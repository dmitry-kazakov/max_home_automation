--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_IO                                      Luebeck            --
--  Implementation                                 Summer, 2015       --
--                                                                    --
--                                Last revision :  13:51 05 Jul 2023  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.IO_Exceptions;            use Ada.IO_Exceptions;
with Ada.Strings.Maps;             use Ada.Strings.Maps;
with GLib.Time_Zone;               use GLib.Time_Zone;
with MAX_Icon_Factory;             use MAX_Icon_Factory;
with MAX_MQTT_Server;              use MAX_MQTT_Server;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Strings_Edit.UTF8.Wildcards;  use Strings_Edit.UTF8.Wildcards;
with Synchronization.Mutexes;      use Synchronization.Mutexes;

with Ada.Tags;
with Ada.Text_IO;
with Cairo;
with Gtk.Handlers;
with Gtk.Recent_Manager_Keys;
with Strings_Edit.Floats;
with System.Storage_Elements;

with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.
     Time_Zones;
with Ada.Calendar.Time_Zones;

package body MAX_IO is
   use Ada.Strings.Unbounded;
   use GLib;
   use GLib.Messages;
   use GNAT.Sockets.MQTT;
   use GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.
       Time_Zones;
   use type Settings_Request_List.Doubly_Linked.Item;

--       procedure Call_Stack (Text : String) is
--          use Ada.Text_IO, GNAT.Traceback, GNAT.Traceback.Symbolic;
--          TB : Tracebacks_Array (1..1_000); Len : Natural;
--       begin
--          Put_Line (Text);
--          Call_Chain (TB, Len);
--          Put_Line (Symbolic_Traceback (TB (1..Len)));
--       end Call_Stack;

   type GUI_Interface_Ptr is access all GUI_Interface'Class;

   Timeout         : constant Duration :=  5.0;
   Setting_Timeout : constant Duration := 10.0;
   Exit_Timeout    : constant Duration :=  1.0;

   GUI    : GUI_Interface_Ptr;
   Filter : Messages_Filter;
   Blanks : constant Character_Set :=
                     To_Set
                     (  Character'Val (9)  -- HT
                     &  Character'Val (10) -- LF
                     &  Character'Val (11) -- VT
                     &  Character'Val (12) -- FF
                     &  Character'Val (13) -- CR
                     &  ' '
                     );

   No_IO : Integer := 0;
   pragma Atomic (No_IO);

   function To_String (Data : Stream_Element_Array) return String is
      use Strings_Edit;
      Length : Natural := 0;
   begin
      for Index in Data'Range loop
         case Data (Index) is
            when 32..36 | 38..126 =>
               Length := Length + 1;
            when others =>
               Length := Length + 3;
         end case;
      end loop;
      declare
         Text : String (1..Length);
         Pointer : Integer := 1;
      begin
         for Index in Data'Range loop
            case Data (Index) is
               when 32..36 | 38..126 =>
                  Text (Pointer) := Character'Val (Data (Index));
                  Pointer := Pointer + 1;
               when others =>
                  Text (Pointer) := '%';
                  Pointer := Pointer + 1;
                  Strings_Edit.Integers.Put
                  (  Destination => Text,
                     Pointer     => Pointer,
                     Value       => Integer (Data (Index)),
                     Base        => 16,
                     Field       => 2,
                     Justify     => Right,
                     Fill        => '0'
                  );
            end case;
         end loop;
         return Text (1..Pointer - 1);
      end;
   end To_String;

   function Where (Name : String) return String is
   begin
      return " in MAX_IO." & Name;
   end Where;

   type Handler_Data is record
      Handler : Settings_Handler_Ptr;
      Request : Settings_Request_List.Doubly_Linked.Item;
      ID      : Gtk.Handlers.Handler_ID;
   end record;
   package ID_To_Data_Maps is
      new Generic_Map (Settings_Sequence_No, Handler_Data);

   Mapping_Mutex : aliased Mutex;
   ID_To_Data    : ID_To_Data_Maps.Map;
   Sequence_No   : Settings_Sequence_No := 0;
   Current_No    : Settings_Sequence_No := Settings_Sequence_No'Last;
   pragma Atomic (Current_No);

   package Request_Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Settings_Sequence_No
          );
   function "not" (Mode : Managed_Mode) return Managed_Mode is
   begin
      case Mode is
         when Automatic =>
            return Manual;
         when Manual =>
            return Automatic;
      end case;
   end "not";

   procedure Add
             (  List       : in out Device_Parameters_Data_Sets.Set;
                Parameters : Device_Parameters;
                Room       : String
             )  is
   begin
      List.Add
      (  new Device_Parameters_Data'
             (  Object.Entity
             with
                Kind_Of     => Parameters.Kind_Of,
                Name_Length => Parameters.Name_Length,
                Room_Length => Room'Length,
                Parameters  => Parameters,
                Room        => Room
      )     );
   end Add;

   procedure Add_Manually (Address : UTF8_String) is
      Start : Integer := Address'First;
   begin
      for Index in Address'Range loop
         case Address (Index) is
            when ' ' | ',' | ';' |
                 Character'Val (9)..Character'Val (13) =>
               if Index > Start then
                  Worker.Add_Manually (Address (Start..Index - 1));
               end if;
               Start := Index + 1;
            when others =>
               null;
         end case;
      end loop;
      if Address'Last >= Start then
         Worker.Add_Manually (Address (Start..Address'Last));
      end if;
   end Add_Manually;

   procedure Adjust (Handle : in out Cube_Client_Handle) is
   begin
      if Handle.Ptr /= null then
         Increment_Count (Handle.Ptr.all);
      end if;
   end Adjust;

   function Compare_Parameters
            (  Client  : Cube_Client;
               Address : RF_Address;
               Data    : Device_Parameters
            )  return Boolean is
   begin
      declare
         Parameters : Device_Parameters renames
                      Client.Get_Device_Parameters (Address);
      begin
         return
         (  Data.Comfort /= Parameters.Comfort
         or else
            Data.Eco /= Parameters.Eco
         or else
            Data.Max /= Parameters.Max
         or else
            Data.Min /= Parameters.Min
         or else
            Data.Offset /= Parameters.Offset
         or else
            Data.Window_Open /= Parameters.Window_Open
         or else
            (  Data.Kind_Of /= Wall_Thermostat
            and then
               Data.Window_Time /= Parameters.Window_Time
         )  );
      end;
   exception
      when others =>
         return False;
   end Compare_Parameters;

   function Compare_Schedule
            (  Client  : Cube_Client;
               Address : RF_Address;
               Day     : Week_Day;
               Data    : Week_Schedule
            )  return Boolean is
   begin
      declare
         Parameters : Device_Parameters renames
                      Client.Get_Device_Parameters (Address);
      begin
         return Parameters.Schedule (Day).Points /= Data (Day).Points;
      end;
   exception
      when others =>
         return False;
   end Compare_Schedule;

   function Compare_Schedule
            (  Client  : Cube_Client;
               Address : RF_Address;
               Day     : Week_Day;
               Data    : Device_Parameters
            )  return Boolean is
   begin
      declare
         Parameters : Device_Parameters renames
                      Client.Get_Device_Parameters (Address);
      begin
         return
         (  Parameters.Schedule (Day).Points
         /= Data.Schedule (Day).Points
         );
      end;
   exception
      when others =>
         return False;
   end Compare_Schedule;

   function Compare_Valve
            (  Client  : Cube_Client;
               Address : RF_Address;
               Data    : Device_Parameters
            )  return Boolean is
   begin
      declare
         Parameters : Device_Parameters renames
                      Client.Get_Device_Parameters (Address);
      begin
         return
         (  Data.Boost_Time /= Parameters.Boost_Time
         or else
            Data.Boost_Valve /= Parameters.Boost_Valve
         or else
            Data.Max_Valve /= Parameters.Max_Valve
         or else
            Data.Valve_Offset /= Parameters.Valve_Offset
         or else
            Data.Decalcification /= Parameters.Decalcification
         );
      end;
   exception
      when others =>
         return False;
   end Compare_Valve;

   procedure Complete
             (  Request  : in out Settings_Request;
                Canceled : Boolean
             )  is
      No : Settings_Sequence_No;
   begin
      declare
         Data  : Handler_Data;
         Lock  : Holder (Mapping_Mutex'Access);
         Index : constant Integer := ID_To_Data.Find (Request.No);
      begin
         if Index <= 0 then
            return;
         end if;
         Data := ID_To_Data.Get (Index);
         if Data.Handler = null then
            ID_To_Data.Remove (Index);
            return;
         end if;
         Data.Request := null;
         ID_To_Data.Replace (Index, Data);
         No := ID_To_Data.Get_Key (Index);
      end;
      Request.Client.Invalidate;
      Current_No := Settings_Sequence_No'Last;
      Completed_Messages.Send (Service'Access, (Canceled, No));
   end Complete;

   procedure Configuration_Updated
             (  Client : in out Cube_Client;
                Update : GNAT.Sockets.Connection_State_Machine.
                         ELV_MAX_Cube_Client.Update_Data
             )  is
      Address : constant RF_Address := Client.Get_RF_Address (True);
   begin
      if Exiting then
         return;
      elsif Address = 0 then
         Trace
         (  (  "Cube at "
            &  Client.Server.Factory.Get_Client_Name (Client)
            &  " is potentially malfunctioning."
            &  " Sending configuration data without handshaking."
            &  " Reconnecting..."
            ),
            Mode_Text
         );
         Client.Reconnect;
         return;
      end if;
      case Update.Kind_Of is
         when Cube_Update =>
            declare
               This : Update_Cube_Request (Client.Get_Serial_No'Length);
            begin
               This.Address   := Address;
               This.Source    := Client.Get_Client_Address;
               This.Serial_No := Client.Get_Serial_No;
               This.Request;
            end;
         when Topology_Update =>
            declare
               This : Update_Devices_Request;
            begin
               This.Address   := Address;
               This.Source    := Client.Get_Client_Address;
               This.Serial_No := Client.Get_Serial_No;
               for Index in 1..Client.Get_Number_Of_Rooms loop
                  if This.Rooms.Is_In (Client.Get_Room_ID (Index)) then
                     Trace_Messages.Send
                     (  Service'Access,
                        (  Error_Text,
                           To_Unbounded_String
                           (  "Cube "
                           &  Client.Get_ID
                           &  " configuration error, room ID "
                           &  Image
                              (  Integer (Client.Get_Room_ID (Index))
                              )
                           &  " is used more than once. You might need"
                           &  " to reset the cube"
                        )  ),
                        Timeout
                     );
                  end if;
                  This.Rooms.Replace
                  (  Client.Get_Room_ID   (Index),
                     Client.Get_Room_Name (Index)
                  );
               end loop;
               for Index in 1..Client.Get_Number_Of_Devices loop
                  if This.Devices.Is_In
                     (  Client.Get_Device_RF_Address (Index)
                     )  then
                     Trace_Messages.Send
                     (  Service'Access,
                        (  Error_Text,
                           To_Unbounded_String
                           (  "Cube "
                           &  Client.Get_ID
                           &  " configuration error, device RF-address "
                           &  Image
                              (  Client.Get_Device_RF_Address (Index)
                              )
                           &  " is used more than once. You might need"
                           &  " to reset the cube"
                        )  ),
                        Timeout
                     );
                  end if;
                  This.Devices.Replace
                  (  Client.Get_Device_RF_Address (Index),
                     Client.Get_Device_Parameters (Index)
                  );
               end loop;
               Update_Devices_Messages.Send
               (  Service'Access,
                  This,
                  Timeout
               );
            end;
         when Detached_Device_Update =>
            Detached_Device_Messages.Send
            (  Service'Access,
               (  Cube      => Address,
                  Kind_Of   => Update.Device,
                  Device    => Update.Address,
                  Serial_No => Update.Serial_No
            )  );
         when Device_Parameters_Update =>
            if Update.Device in Radiator_Thermostat..Eco_Button then
               declare
                  Data : constant Device_Parameters :=
                         Client.Get_Device_Parameters (Update.Address);
                  This : Update_Parameters_Request
                         (  Data.Kind_Of,
                            Data.Name_Length,
                            Address
                         );
               begin
                  This.Parameters := Data;
                  This.Request;
               end;
            end if;
         when Device_Discovery_Update =>
            Pairing_Messages.Send
            (  Service'Access,
               (  Ended     => False,
                  Cube      => Address,
                  Kind_Of   => Update.Device,
                  Address   => Update.Address,
                  Serial_No => Update.Serial_No
            )  );
         when End_Discovery_Update =>
            Pairing_Messages.Send
            (  Service'Access,
               (  Ended => True,
                  Cube  => Address
            )  );
         when NTP_Servers_List_Update =>
            NTP_Update_Messages.Send
            (  Service'Access,
               (  Cube => Address,
                  List => Update.NTP_Servers_List
            )  );
      end case;
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Configuration_Updated")
         )  );
   end Configuration_Updated;

   procedure Data_Received
             (  Client : in out Cube_Client;
                Data   : Device_Data
             )  is
      This : Data_Received_Request (Data.Kind_Of);
   begin
      if Exiting or else Data.Address = 0 then
         return;
      end if;
      if Decoded_Trace then
         Trace_IO_Messages.Send
         (  Service'Access,
            (  Received_Text,
               Clock,
               To_Unbounded_String (Client.Get_ID),
               To_Unbounded_String (Image (Data))
            ),
            Timeout
         );
      end if;
      This.Cube := Client.Get_RF_Address;
      This.Data := Data;
      case This.Kind_Of is
         when Radiator_Thermostat..Wall_Thermostat =>
            begin
               This.Offset :=
                  Client.Get_Device_Parameters (Data.Address).Offset;
            exception
               when others =>
                  This.Offset := 0.0;
            end;
         when others =>
            This.Offset := 0.0;
      end case;
      This.Request;
      if MQTT_Listener /= null then
         declare
            Info : constant Device_Parameters_Data_Handles.
                            Handle :=
               Get_Parameters (Client.Get_RF_Address, Data.Address);
            Parameters : Device_Parameters renames
                         Info.Ptr.Parameters;
         begin
            case Parameters.Kind_Of is
               when Radiator_Thermostat..Wall_Thermostat =>
                  Publish
                  (  Client    => Client,
                     Server    => MQTT_State.all,
                     Data      => Data,
                     Serial_No => Parameters.Serial_No,
                     Name      => Parameters.Name,
                     Offset    => Parameters.Offset,
                     Policy    => MQTT_Device_Policy
                  );
               when others =>
                  Publish
                  (  Client    => Client,
                     Server    => MQTT_State.all,
                     Data      => Data,
                     Serial_No => Parameters.Serial_No,
                     Name      => Parameters.Name,
                     Offset    => 0.0,
                     Policy    => MQTT_Device_Policy
                  );
            end case;
         end;
      end if;
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Error : Gtk.Main.Router.Busy_Error =>
         Trace
         (  "Data_Received timeout: " & Exception_Information (Error),
            Error_Text
         );
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Data_Received")
         )  );
   end Data_Received;

   function Delete_Event_Handler
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk.Event.Gdk_Event
            )  return Boolean is
   begin
      return False;
   end Delete_Event_Handler;

   procedure Destroy_Handler
             (  Widget : access Gtk_Widget_Record'Class
             )  is
   begin
      Trace_To_File (Image (Clock, True) & " Exiting the GUI");
      Exiting := True;
      if not Worker'Terminated then
         declare
            Started : Time := Clock;
         begin
            select
               Worker.Wake_Up;
            or delay Exit_Timeout;
               Trace_To_File
               (  Image (Clock, True)
               &  " Timed out waking up the worker task, ignoring"
               );
            end select;
         exception
            when Error : others =>
               Trace_To_File
               (  Image (Clock, True)
               &  " Error waking up the worker task: "
               &  Exception_Information (Error)
               );
         end;
      end if;
      Gtk.Main.Main_Quit;
   end Destroy_Handler;

   procedure Disconnected (Client : in out Cube_Client) is
      This : Disconnected_Request;
   begin
      ELV_MAX_Cube_Client (Client).Disconnected;
      if Exiting then
         return;
      end if;
      This.ID := Client.ID;
      This.Request;
      Publish
      (  Client,
         MQTT_State.all,
         Client.Get_RF_Address (True),
         False,
         MQTT_Connection_Policy
      );
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
   end Disconnected;

   procedure Exit_Handler is
      Started : constant Time := Clock;
   begin
      if Store_ID /= No_Source_ID then
         Trace_To_File ("Holding the geometry store timer");
         Remove (Store_ID);
         Store_ID := No_Source_ID;
      end if;
      Trace_To_File ("Shutting down updates check server");
      Hosting_Server.Invalidate;
      Trace_To_File ("Shutting down SMTP server");
      SMTP_Server.Invalidate;
      Trace_To_File ("Shutting down the worker task");
      Worker.Shutdown;
      while not Worker'Terminated loop
         delay 0.1;
         if Clock - Started > Exit_Timeout then
            Trace_To_File
            (  Image (Clock, True)
            &  " Timed out waiting for the worker task"
            &  " to exit, ignoring"
            );
            exit;
         end if;
      end loop;
      if HTTP_Server /= null then
         Trace_To_File ("Shutting down the HTTP server");
         Free (HTTP_Server);
      end if;
      if MQTT_Listener /= null then
         Trace_To_File ("Shutting down the MQTT server");
         Free (MQTT_Listener);
      end if;
   end Exit_Handler;

   procedure Faulty_Device_Received
             (  Client      : in out Cube_Client;
                Device      : RF_Address;
                Length      : Natural;
                Error       : Boolean;
                Initialized : Boolean;
                Orphaned    : Boolean
             )  is
      function Is_Initialized return String is
      begin
         if not Initialized then
            return ", uninitialized";
         else
            return "";
         end if;
      end Is_Initialized;

      function Is_Error return String is
      begin
         if Error then
            return ", error";
         else
            return "";
         end if;
      end Is_Error;

      function Is_Orphaned return String is
      begin
         if Orphaned then
            return ", orphaned";
         else
            return "";
         end if;
      end Is_Orphaned;

      This : Faulty_Device_Request;
   begin
      if Decoded_Trace then
         Trace_IO_Messages.Send
         (  Service'Access,
            (  Received_Text,
               Clock,
               To_Unbounded_String (Client.Get_ID),
               To_Unbounded_String
               (  "Faulty device "
               &  Image (Device)
               &  ", data length "
               &  Image (Length)
               &  Is_Error
               &  Is_Initialized
               &  Is_Orphaned
            )  ),
            Timeout
         );
      end if;
      This.Cube        := Client.Get_RF_Address (False);
      This.Device      := Device;
      This.Length      := Length;
      This.Error       := Error;
      This.Initialized := Initialized;
      This.Orphaned    := Orphaned;
      This.Request;
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Error : Gtk.Main.Router.Busy_Error =>
         Trace
         (  (  "Faulty_Device_Received timeout: "
            &  Exception_Information (Error)
            ),
            Error_Text
         );
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Faulty_Device_Received")
         )  );
   end Faulty_Device_Received;

   procedure Finalize (Handle : in out Cube_Client_Handle) is
      procedure Free is new
         Ada.Unchecked_Deallocation
         (  Cube_Client'Class,
            Cube_Client_Ptr
         );
   begin
      if Handle.Ptr /= null then
         declare
            Count : Natural;
         begin
            Decrement_Count (Handle.Ptr.all, Count);
            if Count > 0 then
               Handle.Ptr := null;
            else
               Free (Handle.Ptr);
            end if;
         end;
      end if;
   end Finalize;

   procedure Finalize (Block : in out IO_Blocker) is
   begin
      No_IO := No_IO - 1;
   end Finalize;

   procedure Finalize (Data : in out Server_Data) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Connections_Factory'Class,
                Connections_Factory_Ptr
             );
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Connections_Server'Class,
                Connections_Server_Ptr
             );
   begin
      Free (Data.Server);
      Free (Data.Factory);
      Object.Entity (Data).Finalize;
   end Finalize;

   function From_String (Text : String) return Shortcut_Mode is
   begin
      if Text = "Automatic" then
         return Automatic_Shortcut;
      elsif Text = "Automatic+" then
         return Automatic_With_Temperature_Shortcut;
      elsif Text = "Manual" then
         return Manual_Shortcut;
      elsif Text = "Vacation" then
         return Vacation_Shortcut;
      elsif Text = "Boost" then
         return Boost_Shortcut;
      elsif Text = "Eco" then
         return Eco_Shortcut;
      elsif Text = "Comfort" then
         return Comfort_Shortcut;
      elsif Text = "Airing" then
         return Airing_Shortcut;
      else
         return Unchanged_Shortcut;
      end if;
   end From_String;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Day_Duration
             )  is
      use Strings_Edit;
      Hour   : Integer;
      Minute : Integer := 0;
   begin
      begin
         Get
         (  Source  => Source,
            Pointer => Pointer,
            Value   => Hour,
            First   => 0,
            Last    => 24
         );
      exception
         when Constraint_Error =>
            Raise_Exception
            (  Data_Error'Identity,
               (  "Hour is out of range 0..24 at "
               &  Source (Pointer..Source'Last)
            )  );
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               (  "Hour is expected at "
               &  Source (Pointer..Source'Last)
            )  );
      end;
      Get (Source, Pointer);
      if Pointer <= Source'Last and then Source (Pointer) = ':' then
         Pointer := Pointer + 1;
         Get (Source, Pointer, Blanks);
         declare
            Last : Integer := 59;
         begin
            if Hour = 24 then
               Last := 0;
            end if;
            Get
            (  Source  => Source,
               Pointer => Pointer,
               Value   => Minute,
               First   => 0,
               Last    => Last
            );
         exception
            when Constraint_Error =>
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Minute is out of range 0.."
                  &  Image (Last)
                  &  " at "
                  &  Source (Pointer..Source'Last)
               )  );
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Minute is expected at "
                  &  Source (Pointer..Source'Last)
               )  );
         end;
         Minute := (Minute / 5) * 5;
         if Minute > 55 then
            Minute := 55;
         end if;
      end if;
      declare
         To : constant Duration := Duration ((Hour * 60 + Minute) * 60);
      begin
         if To >= Day_Duration'Last then
            Value := Day_Duration'Last;
         else
            Value := To;
         end if;
      end;
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Centigrade
             )  is
      use Strings_Edit.Floats;
      Number : Float;
   begin
      Get
      (  Source  => Source,
         Pointer => Pointer,
         Value   => Number,
         First   => 0.0,
         Last    => 60.0
      );
      Value := Centigrade (Number);
   exception
      when Constraint_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            (  "Temperature is out of range 0..60 at "
            &  Source (Pointer..Source'Last)
         )  );
      when others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  "Temperature is expected at "
            &  Source (Pointer..Source'Last)
         )  );
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Set_Point
             )  is
      use Strings_Edit;
   begin
      Get (Source, Pointer, Value.Last);
      Get (Source, Pointer, Blanks);
      if Pointer > Source'Last or else Source (Pointer) /= '=' then
         Raise_Exception
         (  Data_Error'Identity,
            (  "= is expected after time at "
            &  Source (Pointer..Source'Last)
         )  );
      end if;
      Pointer := Pointer + 1;
      Get (Source, Pointer, Blanks);
      Get (Source, Pointer, Value.Point);
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Day_Schedule
             )  is
      use Strings_Edit;
      Start  : Integer;
      Length : Point_Count := 0;
      List   : Points_List (Point_Number'Range);
   begin
      loop
         if Length = Point_Count'Last then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Too many scheduling points (more than 13) at "
               &  Source (Pointer..Source'Last)
            )  );
         end if;
         Start := Pointer;
         Get (Source, Pointer, List (Length + 1));
         if Length > List'First then
            if List (Length).Last > List (Length + 1).Last then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "The list of scheduling points is "
                  &  "not ascending at "
                  &  Source (Start..Source'Last)
               )  );
            elsif List (Length).Last = List (Length + 1).Last then
               if List (Length).Point /= List (Length + 1).Point then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "The list of scheduling points has "
                     &  "different temperatures for the same time at "
                     &  Source (Start..Source'Last)
                  )  );
               end if;
            else
               Length := Length + 1;
            end if;
         else
            Length := Length + 1;
         end if;
         Get (Source, Pointer, Blanks);
         exit when Pointer > Source'Last
           or else Source (Pointer) /= ',';
         Pointer := Pointer + 1;
         Get (Source, Pointer, Blanks);
      end loop;
      Value := (Length, List (1..Length));
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Week_Day
             )  is
      use Strings_Edit;
   begin
      if Pointer <= Source'Last then
         case Source (Pointer) is
            when 'F' | 'f' =>
               if Pointer <= Source'Last then
                  case Source (Pointer + 1) is
                     when 'R' | 'r' =>
                        Pointer := Pointer + 2;
                        Value   := Fr;
                        return;
                     when others =>
                        null;
                  end case;
               end if;
            when 'M' | 'm' =>
               if Pointer <= Source'Last then
                  case Source (Pointer + 1) is
                     when 'O' | 'o' =>
                        Pointer := Pointer + 2;
                        Value   := Mo;
                        return;
                     when others =>
                        null;
                  end case;
               end if;
            when 'T' | 't' =>
               if Pointer <= Source'Last then
                  case Source (Pointer + 1) is
                     when 'U' | 'u' =>
                        Pointer := Pointer + 2;
                        Value   := Tu;
                        return;
                     when 'H' | 'h' =>
                        Pointer := Pointer + 2;
                        Value   := Th;
                        return;
                     when others =>
                        null;
                  end case;
               end if;
            when 'W' | 'w' =>
               if Pointer <= Source'Last then
                  case Source (Pointer + 1) is
                     when 'E' | 'e' =>
                        Pointer := Pointer + 2;
                        Value   := We;
                        return;
                     when others =>
                        null;
                  end case;
               end if;
            when 'S' | 's' =>
               if Pointer <= Source'Last then
                  case Source (Pointer + 1) is
                     when 'A' | 'a' =>
                        Pointer := Pointer + 2;
                        Value   := Sa;
                        return;
                     when 'U' | 'u' =>
                        Pointer := Pointer + 2;
                        Value   := Su;
                        return;
                     when others =>
                        null;
                  end case;
               end if;
            when others =>
               null;
         end case;
      end if;
      Raise_Exception
      (  Data_Error'Identity,
         (  "Week day (Mo, Tu etc) is expected at "
         &  Source (Pointer..Source'Last)
      )  );
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Week_Schedule
             )  is
      use Strings_Edit;
      Days  : array (Week_Day) of Boolean := (others => False);
      Start : Integer;
      From  : Week_Day;
      To    : Week_Day;
   begin
      Get (Source, Pointer, Blanks);
      loop
         Start := Pointer;
         Get (Source, Pointer, From);
         To := From;
         Get (Source, Pointer, Blanks);
         if Is_Prefix ("..", Source, Pointer) then
            Pointer := Pointer + 2;
            Get (Source, Pointer, Blanks);
            Get (Source, Pointer, To);
         end if;
         if From > To then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Invalid week days range at "
               &  Source (Start..Source'Last)
            )  );
         end if;
         for Index in From..To loop
            if Days (Index) then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "The week day specified second time at "
                  &  Source (Start..Source'Last)
               )  );
            end if;
            Days (Index) := True;
         end loop;
         Get (Source, Pointer, Blanks);
         if Pointer > Source'Last or else Source (Pointer) /= '(' then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Opening bracket is expected at "
               &  Source (Pointer..Source'Last)
            )  );
         end if;
         Pointer := Pointer + 1;
         Get (Source, Pointer, Blanks);
         Get (Source, Pointer, Value (From));
         Get (Source, Pointer, Blanks);
         if Pointer > Source'Last or else Source (Pointer) /= ')' then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Closing bracket is expected at "
               &  Source (Pointer..Source'Last)
            )  );
         end if;
         Pointer := Pointer + 1;
         Get (Source, Pointer, Blanks);
         if From < To then
            for Index in Week_Day'Succ (From)..To loop
               Value (Index) := Value (From);
            end loop;
         end if;
         exit when Pointer > Source'Last
           or else Source (Pointer) /= ',';
         Pointer := Pointer + 1;
         Get (Source, Pointer, Blanks);
      end loop;
      for Index in Days'Range loop
         if not Days (Index) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "No schedule is specified for "
               &  Image (Index, False)
            )  );
         end if;
      end loop;
   end Get;

   function Get_Cube (Box : RF_Address) return Cube_Client_Handle is
      Result : Cube_Client_Handle;
   begin
      Worker.Get_Cube (Box, Result);
      return Result;
   end Get_Cube;

   function Get_Cube
            (  Address : GNAT.Sockets.Inet_Addr_Type
            )  return Cube_Client_Handle is
      Result : Cube_Client_Handle;
   begin
      Worker.Get_Cube (Address, Result);
      return Result;
   end Get_Cube;

   function Get_Cubes_List return RF_Address_Sets.Set is
      Result : RF_Address_Sets.Set;
   begin
      Worker.Get_Cubes_List (Result);
      return Result;
   end Get_Cubes_List;

   function Get_Device_Name (Box, Device : RF_Address) return String is
      Result : Unbounded_String;
   begin
      Worker.Get_Device_Name (Box, Device, Result);
      return To_String (Result);
   end Get_Device_Name;

   function Get_Device_Data
            (  Box, Device : RF_Address;
               No_Device   : not null access Boolean
            )  return Device_Data is
      Result : Device_Status_Data_Handles.Handle;
   begin
      Worker.Get_Device_Data (Box, Device, Result);
      if Result.Is_Valid then
         No_Device.all := False;
         return Result.Ptr.Status;
      elsif No_Device.all then -- No exception
         return Result : Device_Data (Unknown);
      else
         raise End_Error with
               "No cube "     &
               Image (Box)    &
               " or device "  &
               Image (Device) &
               " found";
      end if;
   end Get_Device_Data;

   function Get_Device_Data
            (  Box       : RF_Address;
               Index     : not null access Positive;
               No_Device : not null access Boolean
            )  return Device_Data is
      Result : Device_Status_Data_Handles.Handle;
   begin
      Worker.Get_Device_Data (Box, Index.all, Result);
      if Result.Is_Valid then
         No_Device.all := False;
         return Result.Ptr.Status;
      elsif No_Device.all then
         return Result : Device_Data (Unknown);
      else
         raise End_Error with
               "No cube "        &
               Image (Box)       &
               " or device no."  &
               Image (Index.all) &
               " found";
      end if;
   end Get_Device_Data;

   function Get_Devices_List
            (  Box     : RF_Address      := 0;
               Filter  : Device_Type_Set := (others => True)
            )  return RF_Address_Sets.Set is
      Result  : RF_Address_Sets.Set;
      No_Cube : Boolean;
   begin
      Worker.Get_Devices_List (Box, Result, Filter, No_Cube);
      return Result;
   end Get_Devices_List;

   function Get_ID (Client : Cube_Client) return Cube_Descriptor is
   begin
      return Client.ID;
   end Get_ID;

   function Get_ID (Client : Cube_Client) return String is
      use GNAT.Sockets;
   begin
      return Image (Client.ID.Address) & ' ' & Client.ID.Serial_No;
   end Get_ID;

   function Get_Parameters (Box, Device : RF_Address)
      return Device_Parameters_Data_Handles.Handle is
      Result : Device_Parameters_Data_Handles.Handle;
   begin
      Worker.Get (Box, Device, Result);
      return Result;
   end Get_Parameters;

   function Get_Radiator_Thermostats
            (  Box  : RF_Address;
               Room : Room_ID
            )  return RF_Address_Sets.Set is
      Result : RF_Address_Sets.Set;
   begin
      Worker.Get_Radiator_Thermostats (Box, Room, Result);
      return Result;
   end Get_Radiator_Thermostats;

   function Get_Rooms_List
            (  Box     : RF_Address      := 0;
               Filter  : Device_Type_Set := (others => True)
            )  return Room_To_Device_List.Map is
      Result  : Room_To_Device_List.Map;
      No_Cube : Boolean;
   begin
      Worker.Get_Rooms_List (Box, Result, Filter, No_Cube);
      return Result;
   end Get_Rooms_List;

   function Get_Thermostats
            (  Box    : RF_Address;
               Room   : Room_ID;
               Filter : Device_Type_Set :=
                        (  Radiator_Thermostat..Wall_Thermostat => True,
                           others => False
            )           )  return RF_Address_Maps.Map is
      Result : RF_Address_Maps.Map;
   begin
      Worker.Get_Room_Thermostats (Box, Room, Filter, Result);
      return Result;
   end Get_Thermostats;

   function Get_Thermostats return RF_Address_Sets.Set is
      Result : RF_Address_Sets.Set;
   begin
      Worker.Get_Thermostats (Result);
      return Result;
   end Get_Thermostats;

   function Get_Topology (Box : RF_Address)
      return Topology_Handles.Handle is
      Result : Topology_Handles.Handle;
   begin
      Worker.Get_Topology (Box, Result);
      return Result;
   end Get_Topology;

   procedure Get_Valves
             (  Box     : RF_Address;
                Average : out Natural;
                Min     : out Natural;
                Max     : out Natural
             )  is
   begin
      Worker.Get_Valves (Box, Average, Min, Max);
   end Get_Valves;

   type Zone_Map is array (-12..12, 1..2) of Zone_Data;
   None : constant Zone_Data := (0, 10, Su, 3, 0.0, "");

   North_Zones : constant Zone_Map :=
                 ( -12 => (None,  None), -11 => ( SST,   SDT),
                   -10 => (HAST,  HADT),  -9 => (AKST,  AKDT),
                    -8 => ( PST,   PDT),  -7 => ( MST,   MDT),
                    -6 => ( CST,   CDT),  -5 => ( EST,   EDT),
                    -4 => ( AST,  None),  -3 => ( WGT,  WGST),
                    -2 => (None,  None),  -1 => ( EGT,  EGST),
                     0 => ( GMT,   BST),   1 => ( CET,  CEST),
                     2 => ( EET,  EEST),   3 => ( FET,  FEST),
                     4 => (SAMT,  SAMT),   5 => (YEKT,  YEKT),
                     6 => (OMST,  OMST),   7 => (KRAT,  KRAT),
                     8 => (IRKT,  IRKT),   9 => (YAKT,  YAKT),
                    10 => (VLAT,  VLAT),  11 => (MAGT,  MAGT),
                    12 => (PETT,  PETT)
                 );
   South_Zones : constant Zone_Map :=
                 (  -6 => (EAST, EASST),  -5 => ( COT,  None),
                    -4 => ( CLT,  CLST),  -3 => ( BRT,  BRST),
                     8 => (AWST,  AWDT),  10 => (AEST,  AEDT),
                    12 => (NZST,  NZDT),  others => (None, None)
                 );

   function Get_Name (TZ : GTime_Zone; Interval : GInt) return String is
      Name : String := Get_Abbreviation (TZ, Interval);
      To   : Integer := Name'First;
   begin
      for From in Name'Range loop
         case Name (From) is
            when Character'Val (0) ..
                 Character'Val (31) |
                 Character'Val (255) =>
               null;
            when others =>
               Name (To) := Name (From);
               To := To + 1;
         end case;
      end loop;
      return Name (Name'First..To - 1);
   end Get_Name;

   procedure Handshake_Received (Client : in out Cube_Client) is
      This    : Connected_Request;
      Drift   : Duration;
      Address : constant RF_Address := Client.Get_RF_Address (True);
   begin
      if Exiting then
         return;
      end if;
      This.ID := Client.ID;
      Publish
      (  Client,
         MQTT_State.all,
         Address,
         True,
         MQTT_Connection_Policy
      );
      Publish
      (  Client,
         MQTT_State.all,
         Address,
         Client.Get_Duty,
         MQTT_Connection_Policy
      );
      This.Request;
      Drift := Client.Get_Clock_Difference;
      Trace
      (  (  "Cube at "
         &  Client.Server.Factory.Get_Client_Name (Client)
         &  " has clock skew "
         &  Image (Drift)
         ),
         Mode_Text
      );
      if abs Drift > 60.0 * 2.0 then
         declare
            type DST_Type is (False, True, Unknown);
            type Time_Data is record
               Zone : Zone_Data := GMT;
               DST  : DST_Type  := False;
            end record;
            function Create
                     (  TZ    : GTime_Zone;
                        Stamp : Time;
                        Start : Month_Number;
                        Day   : Day_Of_Week;
                        Hour  : Hour_Number
                     )  return Time_Data is
               Data     : Time_Data;
               Interval : constant GInt := Find_Interval (TZ, Stamp);
            begin
               if Interval >= 0 then
                  declare
                     use Ada.Calendar.Time_Zones;
                     Name : constant String := Get_Name (TZ, Interval);
                  begin
                     Data.Zone.Offset := Get_Offset (TZ, Interval);
                     if Is_DST (TZ, Interval) then
                        Data.DST := True;
                     else
                        Data.DST := False;
                     end if;
                     if (  Integer (Data.Zone.Offset) / 60
                        /= Integer (UTC_Time_Offset (Stamp))
                        )  then
                        Trace
                        (  (  "Invalid time zone offset "
                           &  Image (Integer (Data.Zone.Offset) / 60)
                           &  "min reported by GLib, expected "
                           &  Image (Integer (UTC_Time_Offset (Stamp)))
                           &  "min"
                           ),
                           Mode_Text
                        );
                        Data.DST := Unknown;
                        Data.Zone.Offset :=
                           Duration (UTC_Time_Offset (Stamp)) * 60.0;
                     end if;
                     if Name'Length <= 5 then
                        Data.Zone :=
                           (  Name'Length,
                              Start,
                              Day,
                              Hour,
                              Data.Zone.Offset,
                              Name
                           );
                     else
                        Data.Zone :=
                           (  0,
                              Start,
                              Su,
                              0,
                              Data.Zone.Offset,
                              ""
                           );
                     end if;
                  end;
               end if;
               return Data;
            end Create;

            function Image (Data : Time_Data) return String is
               function DST return String is
               begin
                  if Data.DST = True then
                     return " DST";
                  else
                     return "";
                  end if;
               end DST;
            begin
               if Data.Zone.Length > 0 then
                  return
                  (  Data.Zone.Name
                  &  " "
                  &  Strings_Edit.Floats.Image
                     (  Value    => Float (Data.Zone.Offset) / 3_600.0,
                        AbsSmall => -2,
                        PutPlus  => True
                     )
                  &  DST
                  );
               else
                  return
                  (  "UTC"
                  &  Strings_Edit.Floats.Image
                     (  Value    => Float (Data.Zone.Offset) / 3_600.0,
                        AbsSmall => -2,
                        PutPlus  => True
                     )
                  &  DST
                  );
               end if;
            end Image;

            procedure Set
                      (  Left  : in out Zone_Data;
                         Right : Zone_Data
                      )  is
            begin
               if Right /= None then
                  Left := Right;
               end if;
            end Set;

            Winter : Time_Data;
            Summer : Time_Data;
         begin
            declare
               TZ       : GTime_Zone renames Gtk_New_Local.all;
               Year     : Year_Number;
               Month    : Month_Number;
               Day      : Day_Number;
               Seconds  : Day_Duration;
               Index    : Integer;
               Now      : constant Time := Clock;
               Interval : constant GInt := Find_Interval (TZ, Now);

               function Is_DST return Boolean is
                  use Ada.Calendar.Time_Zones;
               begin
                  if (  Interval >= 0
                     and then
                        (  Integer (Get_Offset (TZ, Interval))
                        =  Integer (UTC_Time_Offset (Now)) * 60
                     )  )  then
                     return Is_DST (TZ, Interval);
                  else
                     return False;
                  end if;
               end Is_DST;

               function OS_Zone return String is
                  function DST return String is
                  begin
                     if Is_DST then
                        return ", daylight saving time";
                     else
                        return "";
                     end if;
                  end DST;
               begin
                  if Interval >= 0 then
                     return Get_Abbreviation (TZ, Interval) & DST;
                  else
                     return "undefined";
                  end if;
               end OS_Zone;
            begin
               Split (Clock, Year, Month, Day, Seconds);
               Winter :=
                  Create (TZ, Time_Of (Year, 1, 1, 0.0), 10, Su, 3);
               Summer :=
                  Create (TZ, Time_Of (Year, 6, 1, 0.0), 3, Su, 2);
               if Winter.Zone.Offset = Summer.Zone.Offset then
                  -- Same time
                  if Winter.Zone.Length = 0 then -- Guess zone
                     Index := Integer (Winter.Zone.Offset / 3_600.0);
                     if Is_DST and then Index > Zone_Map'First (1) then
                        Set (Winter.Zone, North_Zones (Index - 1, 2));
                        Set (Summer.Zone, North_Zones (Index - 1, 2));
                     else
                        Set (Winter.Zone, North_Zones (Index, 1));
                        Set (Summer.Zone, North_Zones (Index, 1));
                     end if;
                  end if;
               elsif Summer.DST = True then
                  -- North hemisphere
                  if Winter.Zone.Length = 0 then -- Guess name
                     Index := Integer (Winter.Zone.Offset / 3_600.0);
                     if Is_DST and then Index > Zone_Map'First (1) then
                        Index := Index - 1;
                     end if;
                     Set (Winter.Zone, North_Zones (Index, 1));
                     Set (Summer.Zone, North_Zones (Index, 2));
                  end if;
               elsif Winter.DST = True then
                  -- South hemisphere
                  if Summer.Zone.Length = 0 then -- Guess name
                     Index := Integer (Summer.Zone.Offset / 3_600.0);
                     if Is_DST and then Index > Zone_Map'First (1) then
                        Index := Index - 1;
                     end if;
                     Set (Summer.Zone, South_Zones (Index, 1));
                     Set (Winter.Zone, South_Zones (Index, 2));
                  end if;
               else
                  -- No DST
                  if Winter.Zone.Length = 0 then -- Guess zone
                     Index := Integer (Winter.Zone.Offset / 3_600.0);
                     Set (Winter.Zone, North_Zones (Index, 1));
                     Set (Summer.Zone, North_Zones (Index, 1));
                  end if;
               end if;
               Trace
               (  (  "Setting cube at "
                  &  Client.Server.Factory.Get_Client_Name (Client)
                  &  " clock and time zone to "
                  &  Image (Winter)
                  &  ", "
                  &  Image (Summer)
                  &  " ["
                  &  OS_Zone
                  &  "]"
                  ),
                  Error_Text
               );
               Client.Set_Time (Winter.Zone, Summer.Zone);
               Unref (TZ);
            end;
         end;
      end if;
      Client.Query_NTP_Servers;
   end Handshake_Received;

   function Has_Wall_Thermostat
            (  Client : Cube_Client;
               Room   : Room_ID
            )  return Boolean is
   begin
      for Index in 1..Client.Get_Number_Of_Devices (Room) loop
         if (  Client.Get_Device_Type
               (  Client.Get_Device (Room, Index)
               )
            =  Wall_Thermostat
            )
         then
            return True;
         end if;
      end loop;
      return False;
   end Has_Wall_Thermostat;

   Clipboard_Message_Pattern : constant String :=
      "inner_clipboard_window_procedure: assertion * failed";
   Bus_Message_Pattern : constant String :=
      "*Failed to create D-Bus proxy:*";
   Mapped_Message_Pattern : constant String :=
      "GtkWindow * is mapped but visible child GtkTreeMenu * " &
      "is not mapped";

   function Ignore
            (  Filter  : not null access Messages_Filter;
               Domain  : String;
               Level   : Log_Level_Flags;
               Message : UTF8_String
            )  return Boolean is
   begin
      return
      (  Match (Message, Clipboard_Message_Pattern)
      or else
         Match (Message, Bus_Message_Pattern)
      or else
         Match (Message, Mapped_Message_Pattern)
      );
   end Ignore;

   procedure Initialize (Block : in out IO_Blocker) is
   begin
      No_IO := No_IO + 1;
   end Initialize;

   procedure Invalidate (Handle : in out Cube_Client_Handle) is
   begin
      Finalize (Handle);
   end Invalidate;

   function Image (Address : System.Address) return String is
      use System;
      use System.Storage_Elements;
      Figures : constant String := "0123456789ABCDEF";
   begin
      if Address = System.Null_Address then
         return "null";
      else
         declare
            Buffer  : String (1..20);
            Pointer : Integer := Buffer'Last;
            Value   : Integer_Address := To_Integer (Address);
         begin
            loop
               Buffer (Pointer) := Figures (Natural (Value mod 16) + 1);
               Pointer := Pointer - 1;
               Value := Value / 16;
               exit when Value = 0;
            end loop;
            return Buffer (Pointer + 1..Buffer'Last);
         end;
      end if;
   end Image;

   function Image
            (  Handler : not null access
                         Settings_Handler_Interface'Class
            )  return String is
   begin
      return Image (Handler.Get_Object.all'Address);
   end Image;

   function Image (Schedule : Day_Schedule) return String is
      use Strings_Edit;
      Result  : String (1..512);
      Pointer : Integer := Result'First;
      List    : Points_List renames Schedule.Points;
   begin
      for Index in List'Range loop
         declare
            Point : Set_Point renames List (Index);
         begin
            if Index > List'First then
               Put (Result, Pointer, ", ");
            end if;
            Put
            (  Destination => Result,
               Pointer     => Pointer,
               Value       => Integer (Point.Last) / 3600,
               Justify     => Right,
               Field       => 2,
               Fill        => '0'
            );
            Put (Result, Pointer, ":");
            Put
            (  Destination => Result,
               Pointer     => Pointer,
               Value       => (Integer (Point.Last) / 60) mod 60,
               Justify     => Right,
               Field       => 2,
               Fill        => '0'
            );
            Put (Result, Pointer, "=");
            Put (Result, Pointer, Image (Point.Point));
         end;
      end loop;
      return Result (1..Pointer - 1);
   end Image;

   function Image (Schedule : Week_Schedule) return String is
      use Strings_Edit;
      Result  : String (1..1024);
      Pointer : Integer  := Result'First;
      From    : Week_Day := Mo;
      To      : Week_Day := Mo;
   begin
      loop
         To := From;
         while (  To < Week_Day'Last
               and then
                  Schedule (From) = Schedule (Week_Day'Succ (To))
               )  loop
            To := Week_Day'Succ (To);
         end loop;
         if From /= Mo then
            Put (Result, Pointer, ", ");
         end if;
         Put (Result, Pointer, Image (From));
         if From /= To then
            Put (Result, Pointer, "..");
            Put (Result, Pointer, Image (To));
         end if;
         Put (Result, Pointer, "(");
         Put (Result, Pointer, Image (Schedule (From)));
         Put (Result, Pointer, ")");
         exit when To = Week_Day'Last;
         From := Week_Day'Succ (To);
      end loop;
      return Result (1..Pointer - 1);
   end Image;

   function Image (Value : RF_Address_Array) return String is
      use Strings_Edit;
      Result  : String (1..1024);
      Pointer : Integer := 1;
   begin
      for Index in Value'Range loop
         if Index > 1 then
            Put (Result, Pointer, ",");
         end if;
         Put (Result, Pointer, Image (Value (Index)));
      end loop;
      if Pointer = 1 then
         return "<nothing>";
      else
         return Result (1..Pointer - 1);
      end if;
   exception
      when others =>
         return Result (1..Pointer - 1) & "...";
   end Image;

   function Image (Map : RF_Address_Maps.Map) return String is
      use Strings_Edit;
      Result  : String (1..1024);
      Pointer : Integer := 1;
   begin
      for Index in 1..Map.Get_Size loop
         if Index > 1 then
            Put (Result, Pointer, ",");
         end if;
         Put (Result, Pointer, Image (Map.Get_Key (Index)));
      end loop;
      if Pointer = 1 then
         return "<nothing>";
      else
         return Result (1..Pointer - 1);
      end if;
   exception
      when others =>
         return Result (1..Pointer - 1) & "...";
   end Image;

   function Image (Action : Store_Mode) return String is
   begin
      case Action is
         when Store_Start =>
            return "starting thermostat configuration";
         when Store_Schedule =>
            return "storing thermostat schedule";
         when Store_Parameters =>
            return "storing thermostat parameters";
         when Store_Valve =>
            return "storing thermostat valve parameters";
         when Store_Stop =>
            return "stopping thermostat configuration";
      end case;
   end Image;

--     function Is_Completed
--              (  Handler : Settings_Handler_Interface'Class
--              )  return Boolean is
--        Lock : Holder (Mapping_Mutex'Access);
--        Item : constant Settings_Handler_Ref := Handler'Unchecked_Access;
--     begin
--        for Index in 1..ID_To_Data.Get_Size loop
--           declare
--              This : constant Handler_Data := ID_To_Data.Get (Index);
--           begin
--              if This.Handler.all'Access = Item then
--                 return
--                 (  This.Request = null
--                 or else
--                    (  This.Request.Active
--                    and then
--                       This.Request.Expected = 0
--                 )  );
--              end if;
--           end;
--        end loop;
--        return True;
--     end Is_Completed;

   function Is_Free (Block : IO_Blocker) return Boolean is
   begin
      return No_IO <= 1;
   end Is_Free;

   function Is_Handled (Request : Settings_Request) return Boolean is
      Lock : Holder (Mapping_Mutex'Access);
   begin
      return ID_To_Data.Is_In (Request.No);
   end Is_Handled;

   function Is_Valid (Handle : Cube_Client_Handle) return Boolean is
   begin
      return Handle.Ptr /= null;
   end Is_Valid;

   function Next
            (  Action     : Store_Mode;
               Schedule   : Boolean;
               Parameters : Boolean;
               Valve      : Boolean;
               Cycle      : not null access function return Boolean :=
                            Never'Access
            )  return Store_Mode is
   begin
      case Action is
         when Store_Start =>
            if Schedule then
               return Store_Schedule;
            elsif Parameters then
               return Store_Parameters;
            elsif Valve then
               return Store_Valve;
            else
               return Store_Stop;
            end if;
         when Store_Schedule =>
            if Parameters then
               return Store_Parameters;
            elsif Valve then
               return Store_Valve;
            elsif Cycle.all then
               return Store_Schedule;
            else
               return Store_Stop;
            end if;
         when Store_Parameters =>
            if Valve then
               return Store_Valve;
            elsif Cycle.all then
               if Schedule then
                  return Store_Schedule;
               else
                  return Store_Parameters;
               end if;
            else
               return Store_Stop;
            end if;
         when Store_Valve =>
            if Cycle.all then
               if Schedule then
                  return Store_Schedule;
               elsif Parameters then
                  return Store_Parameters;
               else
                  return Store_Valve;
               end if;
            else
               return Store_Stop;
            end if;
         when Store_Stop =>
            return Store_Stop;
      end case;
   end Next;

   function Never return Boolean is
   begin
      return False;
   end Never;

   function Note (Duty : Ratio) return String is
   begin
      if Duty > 0.9 then
         return ". The cube has exhausted its radio traffic "  &
                "limit for this hour. You should wait for an " &
                "hour before another attempt";
      else
         return "";
      end if;
   end Note;

   Last : Time := Clock;

   procedure On_Destroy
             (  Object : access GObject_Record'Class;
                No     : Settings_Sequence_No
             )  is
   begin
      declare
         Lock  : Holder (Mapping_Mutex'Access);
         Index : constant Integer := ID_To_Data.Find (No);
      begin
         if Index <= 0 then
            return;
         end if;
         declare
            Data : Handler_Data := ID_To_Data.Get (Index);
         begin
            Data.Handler := null;
            ID_To_Data.Replace (Index, Data);
         end;
      end;
   end On_Destroy;

   function On_Draw
            (  Widget  : access Gtk_Widget_Record'Class;
               Context : Cairo.Cairo_Context
            )  return Boolean is
      use Ada.Tags;
      use Ada.Text_IO;
      use Strings_Edit.Floats;
      Now : constant Time := Clock;
   begin
      Put_Line
      (  "on draw "
      &  Expanded_Name (Widget.all'Tag)
      &  " "
      &  Image (Float (Now - Last), AbsSmall => -3)
      );
      Last := Now;
      return False;
   end On_Draw;

   procedure Reconnect
             (  Cube    : Cube_Client;
                Connect : Boolean;
                Silent  : Boolean
             )  is
   begin
      Worker.Reconnect (Cube.ID.Address, Connect, Silent);
   end Reconnect;

   procedure Scan (Address : UTF8_String) is
   begin
      Worker.Scan (Address);
   end Scan;

   procedure Register
             (  Handler : Settings_Handler_Ptr;
                Request : Settings_Request_List.Doubly_Linked.Item
             )  is
      Lock : Holder (Mapping_Mutex'Access);
   begin
      Request.No := Sequence_No;
      if Handler = null then
         ID_To_Data.Add
         (  Sequence_No,
            (  Handler  => Handler,
               Request  => Request,
               ID       => (Gtk.Handlers.Null_Handler_Id, null)
         )  );
      else
         ID_To_Data.Add
         (  Sequence_No,
            (  Handler  => Handler,
               Request  => Request,
               ID       =>
                  Request_Handlers.Connect
                  (  Handler.Get_Object,
                     "destroy",
                     Request_Handlers.To_Marshaller (On_Destroy'Access),
                     Sequence_No
         )  )    );
         Handler.Registered (Sequence_No);
      end if;
      Sequence_No := Sequence_No + 1;
   end Register;

   procedure Received
             (  Client  : in out Cube_Client;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      ELV_MAX_Cube_Client (Client).Received (Data, Pointer);
   end Received;

   function Replace_Handler
            (  No      : Settings_Sequence_No;
               Handler : Settings_Handler_Ptr
            )  return Boolean is
      use Gtk.Handlers;
      Data  : Handler_Data;
      Lock  : Holder (Mapping_Mutex'Access);
      Index : constant Integer := ID_To_Data.Find (No);
   begin
      if Index <= 0 then
         return False;
      end if;
      Data := ID_To_Data.Get (Index);
      if Data.Handler /= null then
         Disconnect (Data.Handler.Get_Object, Data.ID);
      end if;
      Data.Handler := Handler;
      Data.ID := Request_Handlers.Connect
                 (  Handler.Get_Object,
                    "destroy",
                    Request_Handlers.To_Marshaller (On_Destroy'Access),
                    No
                 );
      ID_To_Data.Replace (Index, Data);
      return True;
   end Replace_Handler;

   procedure Report
             (  Request : in out Settings_Request;
                Message : String;
                Title   : String := "Communication error"
             )  is
   begin
      if Request.Silent_Mode then
         Trace (Message, Error_Text);
      else
         Say (Message, Title);
      end if;
   end Report;

   function Restore (Key : String; Default : Integer) return GInt is
      use Gtk.Recent_Manager_Keys;
      Result : Integer;
   begin
      Result := Value (Restore (Key, Image (Default)));
      return GInt (Result);
   exception
      when others =>
         return GInt (Default);
   end Restore;

   function Restore (Key : String; Default : Integer) return Integer is
      use Gtk.Recent_Manager_Keys;
      Result : Integer;
   begin
      Result := Value (Restore (Key, Image (Default)));
      return Result;
   exception
      when others =>
         return Default;
   end Restore;

   procedure Service (Request : in out Completed_Message) is
      use Gtk.Handlers;
      Data : Handler_Data;
   begin
      declare
         Lock  : Holder (Mapping_Mutex'Access);
         Index : constant Integer := ID_To_Data.Find (Request.No);
      begin
         if Index <= 0 then
            return;
         end if;
         Data := ID_To_Data.Get (Index);
         ID_To_Data.Remove (Index);
      end;
      if Data.Handler = null then
         return;
      end if;
      Disconnect (Data.Handler.Get_Object, Data.ID); -- null-ID-safe
      if Request.Canceled then
         Data.Handler.Canceled;
      else
         Data.Handler.Finished;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (Completed_Message)")
         )  );
   end Service;

   procedure Service (Request : in out Connected_Request) is
   begin
      if not Exiting and then GUI /= null then
         GUI.Update_Connected (Request.ID);
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (Connected_Request)")
         )  );
   end Service;

   procedure Service (Request : in out Cube_Statistics_Message) is
   begin
      if not Exiting and then GUI /= null then
         GUI.Update_Cube_Statistics
         (  Address => Request.Address,
            Average => Request.Average
         );
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (Cube_Statistics_Messages)")
         )  );
   end Service;

   procedure Service (Request : in out Data_Received_Request) is
   begin
      if not Exiting and then GUI /= null then
         GUI.Update_Data (Request.Cube, Request.Data, Request.Offset);
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (Data_Received_Request)")
         )  );
   end Service;

   procedure Service (Request : in out Detached_Device_Message) is
   begin
      if not Exiting and then GUI /= null then
         GUI.Detached_Device
         (  Cube      => Request.Cube,
            Kind_Of   => Request.Kind_Of,
            Device    => Request.Device,
            Serial_No => Request.Serial_No
         );
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (Detached_Device_Message)")
         )  );
   end Service;

   procedure Service (Request : in out Disconnected_Request) is
   begin
      if not Exiting and then GUI /= null then
         GUI.Update_Disconnected (Request.ID);
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (Disconnected_Request)")
         )  );
   end Service;

   procedure Service (Request : in out Faulty_Device_Request) is
   begin
      if not Exiting and then GUI /= null then
         GUI.Faulty_Device
         (  Cube        => Request.Cube,
            Device      => Request.Device,
            Length      => Request.Length,
            Error       => Request.Error,
            Initialized => Request.Initialized,
            Orphaned    => Request.Orphaned
         );
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (Faulty_Device_Request)")
         )  );
   end Service;

   procedure Service (Request : in out NTP_Update_Message) is
   begin
      if not Exiting and then GUI /= null then
         GUI.Set_NTP_List
         (  Cube => Request.Cube,
            List => Request.List
         );
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (NTP_Update_Message)")
         )  );
   end Service;

   procedure Service (Request : in out Pairing_Update_Message) is
   begin
      if not Exiting and then GUI /= null then
         if Request.Ended then
            if View /= null then
               View.Trace
               (  "Pairing cube " & Image (Request.Cube) & " ended"
               );
            end if;
            GUI.Pairing_Ended;
         else
            if View /= null then
               View.Trace
               (  "Pairing cube "
               &  Image (Request.Cube)
               &  ", a new device found: "
               &  Image (Request.Address)
               &  " "
               &  Image (Request.Kind_Of)
               &  " "
               &  Request.Serial_No
               );
            end if;
            GUI.Paired_Device
            (  Cube      => Request.Cube,
               Device    => Request.Kind_Of,
               Address   => Request.Address,
               Serial_No => Request.Serial_No
            );
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (Pairing_Update_Message)")
         )  );
   end Service;

   procedure Service (Request : in out Status_Received_Message) is
      Completed : Boolean := True;

      procedure Update
                (  Handler : Settings_Handler_Ptr;
                   Data    : in out Settings_Request'Class
                )  is
      begin
         Data.Error := Data.Error or else Request.Error;
         if not Exiting and then GUI /= null then
            GUI.Update_Status
            (  Handler,
               Request.Cube.Ptr.all,
               Request.Error,
               Request.Duty,
               Request.Slots,
               Data.Expected
            );
         elsif Data.Expected > 0 then
            Data.Expected := Data.Expected - 1;
         end if;
         Completed := Data.Expected = 0;
         if (  not Data.Reported
            and then
               not Exiting
            and then
               Handler = null
            and then
               Request.Error
            )
         then
            Data.Reported := True;
            Data.Report
            (  "Failed "
            &  Data.Image
            &  ". The cube rejected some of commands required to "
            &  "perform the operation"
            &  Note (Request.Duty)
            );
         end if;
      end Update;

      procedure Update is
         Expected : Natural := 0;
      begin
         if not Exiting and then GUI /= null then
            GUI.Update_Status
            (  null,
               Request.Cube.Ptr.all,
               Request.Error,
               Request.Duty,
               Request.Slots,
               Expected
            );
         end if;
         Completed := Expected = 0;
      end Update;

   begin
      declare
         Lock  : Holder (Mapping_Mutex'Access);
         Index : constant Integer := ID_To_Data.Find (Current_No);
      begin
         if Index > 0 then
            declare
               Data : constant Handler_Data := ID_To_Data.Get (Index);
            begin
               if Data.Request = null then
                  Update;
               else
                  Update (Data.Handler, Data.Request.all);
               end if;
            end;
         else
            Update;
         end if;
      end;
      if Completed then
         Worker.Dequeue (Settings_Sequence_No'Last, False);
      end if;
   exception
      when Error : others =>
         Worker.Dequeue (Settings_Sequence_No'Last, True);
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (Status_Received_Message)")
         )  );
   end Service;

   procedure Service (Request : in out Trace_Request) is
   begin
      if not Exiting and then View /= null then
         View.Trace (To_String (Request.Message), Request.Mode);
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (Trace_Request)")
         )  );
   end Service;

   procedure Service (Request : in out Trace_IO_Request) is
   begin
      if not Exiting and then View /= null then
         View.Trace
         (  To_String (Request.Message),
            Request.Mode,
            Image (Request.Stamp, True) & " " & To_String (Request.ID)
         );
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (Trace_IO_Request)")
         )  );
   end Service;

   procedure Service (Request : in out Update_Cube_Request) is
   begin
      if not Exiting and then GUI /= null then
         GUI.Update_Cube
         (  Request.Address,
            Request.Serial_No,
            Request.Source
         );
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (Update_Cube_Request)")
         )  );
   end Service;

   procedure Service (Request : in out Update_Devices_Request) is
      Completed : Boolean := True;

      procedure Update
                (  Handler : Settings_Handler_Ptr;
                   Data    : in out Settings_Request'Class
                )  is
      begin
         if not Exiting and then GUI /= null then
            GUI.Update_Devices
            (  Handler,
               Request.Address,
               Request.Serial_No,
               Request.Source,
               Request.Devices,
               Request.Rooms,
               Data.Expected
            );
         elsif Data.Expected > 0 then
            Data.Expected := Data.Expected - 1;
         end if;
         Completed := Data.Expected = 0;
      end Update;

      procedure Update is
         Expected : Natural := 0;
      begin
         if not Exiting and then GUI /= null then
            GUI.Update_Devices
            (  null,
               Request.Address,
               Request.Serial_No,
               Request.Source,
               Request.Devices,
               Request.Rooms,
               Expected
            );
         end if;
         Completed := Expected = 0;
      end Update;

   begin
      declare
         Lock  : Holder (Mapping_Mutex'Access);
         Index : constant Integer := ID_To_Data.Find (Current_No);
      begin
         if Index > 0 then
            declare
               Data : constant Handler_Data := ID_To_Data.Get (Index);
            begin
               if Data.Request = null then
                  Update;
               else
                  Update (Data.Handler, Data.Request.all);
               end if;
            end;
         else
            Update;
         end if;
      end;
      if Completed then
         Worker.Dequeue (Settings_Sequence_No'Last, False);
      end if;
   exception
      when Error : others =>
         Worker.Dequeue (Settings_Sequence_No'Last, True);
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (Update_Devices_Request)")
         )  );
   end Service;

   procedure Service (Request : in out Update_Parameters_Request) is
   begin
      if not Exiting and then GUI /= null then
         GUI.Update_Parameters (Request.Cube, Request.Parameters);
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (Update_Parameters_Request)")
         )  );
   end Service;

   procedure Service (Request : in out Update_Version_Message) is
   begin
      if not Exiting and then GUI /= null then
         GUI.Update_Version
         (  Request.Major,
            Request.Minor,
            Request.Version
         );
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (Update_Version_Message)")
         )  );
   end Service;

   procedure Set
             (  Handle : in out Cube_Client_Handle;
                Cube   : Cube_Client_Ptr
             )  is
   begin
      if Handle.Ptr /= Cube then
         if Cube /= null then
            Increment_Count (Cube.all);
         end if;
         Finalize (Handle);
         Handle.Ptr := Cube;
      end if;
   end Set;

   procedure Set_Auto_Scale is
   begin
      if GUI /= null then
         GUI.Set_Auto_Scale;
      end if;
   end Set_Auto_Scale;

   procedure Set_Fixed_Scale (Low, High : Centigrade) is
   begin
      if GUI /= null then
         GUI.Set_Fixed_Scale (Low, High);
      end if;
   end Set_Fixed_Scale;

   procedure Set_Control
             (  Device  : in out Device_Control;
                Address : RF_Address;
                Cube    : in out Cube_Client'Class
             )  is
   begin
      if Cube.Has_Wall_Thermostat (Cube.Get_Device_Room (Address)) then
         Device.Control := Managed_Room;
      else
         Device.Control := Unmanaged_Room;
      end if;
   exception
      when others =>
         null;
   end Set_Control;

   procedure Set_GUI (List : not null access GUI_Interface'Class) is
   begin
      GUI := List.all'Unchecked_Access;
   end Set_GUI;

   procedure Set_Poll_Time (Poll : Duration) is
   begin
      Worker.Set_Poll (Poll);
   end Set_Poll_Time;

   procedure Set_Trace_Box (Box : Trace_Box) is
   begin
      View := Box;
   end Set_Trace_Box;

   procedure Status_Received
             (  Client : in out Cube_Client;
                Error  : Boolean;
                Duty   : Ratio;
                Slots  : Natural
             )  is
      This    : Status_Received_Message;
      Address : constant RF_Address := Client.Get_RF_Address (True);
   begin
      if Exiting then
         return;
      elsif Address = 0 then
         Trace
         (  (  "Cube at "
            &  Client.Server.Factory.Get_Client_Name (Client)
            &  " is potentially malfunctioning."
            &  " Sending status without handshaking."
            &  " Reconnecting..."
            ),
            Mode_Text
         );
         Client.Reconnect;
         return;
      end if;
      if Decoded_Trace then
         declare
            use Strings_Edit;
            Text    : String (1..40);
            Pointer : Integer := 1;
         begin
            if Error then
               Put (Text, Pointer, "Rejected, ");
            end if;
            Put (Text, Pointer, Integer (Duty * 100.0));
            Put (Text, Pointer, "%, ");
            Put (Text, Pointer, Slots);
            Put (Text, Pointer, " free");
            Trace_IO_Messages.Send
            (  Service'Access,
               (  Received_Text,
                  Clock,
                  To_Unbounded_String (Client.Get_ID),
                  To_Unbounded_String (Text (1..Pointer - 1))
               ),
               Timeout
            );
         end;
      end if;
      if MQTT_Listener /= null then
         Publish
         (  Client => Client,
            Server => MQTT_State.all,
            Cube   => Address,
            Duty   => Duty,
            Policy => MQTT_Cube_Policy
         );
      end if;
      This.Cube.Set (Client'Unchecked_Access);
      This.Error := Error;
      This.Duty  := Duty;
      This.Slots := Slots;
      Status_Messages.Send (Service'Access, This, Timeout);
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Error : Gtk.Main.Router.Busy_Error =>
         Trace
         (  "Status_Received timeout: " & Exception_Information (Error),
            Error_Text
         );
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Status_Received")
         )  );
   end Status_Received;

   procedure Switch
             (  Device  : in out Device_Control;
                Address : RF_Address;
                Cube    : in out Cube_Client'Class;
                Mode    : Managed_Mode
             )  is
   begin
      case Mode is
         when Automatic =>
            Cube.Set_Thermostat_Automatic (Address);
         when Manual =>
            Cube.Set_Thermostat_Temperature
            (  Address,
               Device.Temperature
            );
      end case;
   end Switch;

   procedure Switch_Back
             (  Device  : in out Device_Control;
                Address : RF_Address;
                Data    : Device_Data;
                Now     : Time;
                Cube    : in out Cube_Client'Class
             )  is
   begin
      if Data.Mode not in Managed_Mode then -- Other mode engaged
         Trace
         (  (  "The thermostat "
            &  Image (Address)
            &  " was switched to "
            &  Image (Data.Mode)
            &  ", holding scanning for a while"
            ),
            Message_Text
         );
         Device.Action := No_Action;
      elsif (  Data.Latest_Temperature /= Centigrade'First
            and then
               Now - Data.Received_At <= Temperature_Timeout
            )  then -- Have a new temperature
         if Data.Mode = Device.Mode then -- Already back
            Trace
            (  (  "Have a new temperature from the thermostat "
               &  Image (Address)
               &  ", keeping "
               &  Image (Device.Mode)
               &  " mode"
               ),
               Message_Text
            );
            Device.Action := No_Action;
         else -- Restore previous mode
            Trace
            (  (  "Have a new temperature from the thermostat "
               &  Image (Address)
               &  ", returning it back to "
               &  Image (Device.Mode)
               &  " mode"
               ),
               Message_Text
            );
            Device.Last_Switch := Now;
            Device.Action := Switch_Backward;
            Device.Switch (Address, Cube, Device.Mode);
         end if;
      elsif Device.Last_Switch + Device.Timeout < Now then -- Timed out
         if Data.Mode = Device.Mode then
            if Device.Action = Switch_Forward then -- Failed forward
               Trace
               (  (  "Failed to switch the thermostat "
                  &  Image (Address)
                  &  " to "
                  &  Image (not Device.Mode)
                  &  " mode after "
                  &  Image (Integer (Now - Device.Last_Switch))
                  &  "s, trying again"
                  ),
                  Error_Text
               );
               Device.Last_Switch := Now;
               Device.Switch (Address, Cube, not Device.Mode);
            else
               Trace
               (  (  "Scan timeout expired for the thermostat "
                  &  Image (Address)
                  &  " after "
                  &  Image (Integer (Now - Device.Last_Switch))
                  &  "s, keeping "
                  &  Image (Device.Mode)
                  &  " mode"
                  ),
                  Message_Text
               );
               Device.Action := No_Action;
            end if;
         else
            if Device.Action = Switch_Forward then
               Trace
               (  (  "No new temperature for the thermostat "
                  &  Image (Address)
                  &  " within "
                  &  Image (Integer (Now - Device.Last_Switch))
                  &  "s, returning back to "
                  &  Image (Device.Mode)
                  &  " mode"
                  ),
                  Message_Text
               );
            else
               Trace
               (  (  "Failed to switch the thermostat "
                  &  Image (Address)
                  &  " back to "
                  &  Image (Device.Mode)
                  &  " mode after "
                  &  Image (Integer (Now - Device.Last_Switch))
                  &  "s, trying again"
                  ),
                  Error_Text
               );
            end if;
            Device.Last_Switch := Now;
            Device.Action := Switch_Backward;
            Device.Switch (Address, Cube, Device.Mode);
         end if;
      end if;
   exception
      when Use_Error =>
         Trace
         (  (  "Busy, switching "
            &  Image (Address)
            &  " back later ..."
            ),
            Message_Text
         );
   end Switch_Back;

   procedure Switch_Forth
             (  Device      : in out Device_Control;
                Address     : RF_Address;
                From        : Managed_Mode;
                Temperature : Centigrade;
                Now         : Time;
                Cube        : in out Cube_Client'Class
             )  is
   begin
      Device.Timeout := Duration'Min (Scan_Timeout, Scan_Period / 2.0);
      Device.Mode        := From;
      Device.Action      := Switch_Forward;
      Device.Temperature := Temperature;
      Device.Last_Switch := Now;
      Trace
      (  (  "Set the thermostat "
         &  Image (Address)
         &  " to "
         &  Image (not From)
         &  " forcing the measured temperature report (for "
         &  Image (Integer (Device.Timeout))
         &  "s)"
         ),
         Message_Text
      );
      Device.Switch (Address, Cube, not From);
   exception
      when Use_Error =>
         Trace
         (  (  "Busy, switching "
            &  Image (Address)
            &  " later ..."
            ),
            Message_Text
         );
   end Switch_Forth;

   function To_Array
            (  Set : RF_Address_Sets.Set
            )  return RF_Address_Array is
      Result : RF_Address_Array (1..Set.Get_Size);
   begin
      for Index in Result'Range loop
         Result (Index) := Set.Get (Index);
      end loop;
      return Result;
   end To_Array;

   function To_Array
            (  Map : RF_Address_Maps.Map
            )  return RF_Address_Array is
      Result : RF_Address_Array (1..Map.Get_Size);
   begin
      for Index in Result'Range loop
         Result (Index) := Map.Get_Key (Index);
      end loop;
      return Result;
   end To_Array;

   procedure Trace
             (  Factory : in out ELV_Factory;
                Message : String
             )  is
   begin
      if not Exiting and then ELV_Trace then
         Trace_Messages.Send
         (  Service'Access,
            (Message_Text, To_Unbounded_String (Message)),
            Timeout
         );
      end if;
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Gtk.Main.Router.Busy_Error =>
         Trace_To_File (Message);
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Trace (Factory)")
         )  );
   end Trace;

   procedure Trace
             (  Factory : in out ELV_Secure_Factory;
                Message : String
             )  is
   begin
      if not Exiting and then ELV_Trace then
         Trace_Messages.Send
         (  Service'Access,
            (Message_Text, To_Unbounded_String (Message)),
            Timeout
         );
      end if;
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Gtk.Main.Router.Busy_Error =>
         Trace_To_File (Message);
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Trace (Secure Factory)")
         )  );
   end Trace;

   procedure Trace (Message : String; Mode : Trace_Type) is
   begin
      if not Exiting then
         Trace_Messages.Send
         (  Service'Access,
            (Mode, To_Unbounded_String (Message)),
            Timeout
         );
      end if;
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Gtk.Main.Router.Busy_Error =>
         Trace_To_File (Message, Mode);
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Trace")
         )  );
   end Trace;

   procedure Trace
             (  Client  : in out Cube_Client;
                Message : String
             )  is
      Now : constant Time := Clock;
   begin
      if not Exiting then
         Trace_IO_Messages.Send
         (  Service'Access,
            (  Received_Text,
               Now,
               To_Unbounded_String (Client.Get_ID),
               To_Unbounded_String (Message)
            ),
            Timeout
         );
      end if;
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Gtk.Main.Router.Busy_Error =>
         Trace_To_File
          (  Message,
             Received_Text,
             Image (Now, True) & " " & Client.Get_ID
          );
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Trace (Cube_Client)")
         )  );
   end Trace;

   procedure Trace
             (  Client  : in out MAX_MQTT_Client;
                Session : String;
                Message : String;
                Kind_Of : GNAT.Sockets.MQTT.Trace_Message_Type
             )  is
      use GNAT.Sockets;
      Now    : constant Time := Clock;
      Prefix : constant String := Image (Get_Client_Address (Client));
      Text   : constant String := Message & '|' & Session;
   begin
      case Kind_Of is
         when GNAT.Sockets.MQTT.Received =>
            begin
               if not Exiting and then ELV_Trace then
                  Trace_IO_Messages.Send
                  (  Service'Access,
                     (  Received_Text,
                        Now,
                        To_Unbounded_String (Prefix),
                        To_Unbounded_String (Text)
                     ),
                     Timeout
                  );
               end if;
            exception
               when Gtk.Main.Router.Busy_Error =>
                  Trace_To_File
                  (  Text,
                     Received_Text,
                     (  Image (Now, True)
                     &  " "
                     &  Prefix
                  )  );
            end;
         when GNAT.Sockets.MQTT.Sent =>
            begin
               if not Exiting and then ELV_Trace then
                  Trace_IO_Messages.Send
                  (  Service'Access,
                     (  Sent_Text,
                        Now,
                        To_Unbounded_String (Prefix),
                        To_Unbounded_String (Text)
                     ),
                     Timeout
                  );
               end if;
            exception
               when Gtk.Main.Router.Busy_Error =>
                  Trace_To_File
                  (  Text,
                     Sent_Text,
                     (  Image (Now, True)
                     &  " "
                     &  Prefix
                  )  );
            end;
         when GNAT.Sockets.MQTT.Action =>
            begin
               if not Exiting and then ELV_Trace then
                  Trace_IO_Messages.Send
                  (  Service'Access,
                     (  Message_Text,
                        Now,
                        To_Unbounded_String (Prefix),
                        To_Unbounded_String (Text)
                     ),
                     Timeout
                  );
               end if;
            exception
               when Gtk.Main.Router.Busy_Error =>
                  Trace_To_File
                  (  Text,
                     Message_Text,
                     (  Image (Now, True)
                     &  " "
                     &  Prefix
                  )  );
            end;
      end case;
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Trace (MAX_MQTT_Client)")
         )  );
   end Trace;

   procedure Trace_Draw
             (  Widget : not null access Gtk_Widget_Record'Class
             )  is
   begin
      Widget.On_Draw (On_Draw'Access);
   end Trace_Draw;

   procedure Trace_Error
             (  Factory    : in out ELV_Factory;
                Context    : String;
                Occurrence : Exception_Occurrence
             )  is
   begin
      if not Exiting then
         Trace_Messages.Send
         (  Service'Access,
            (  Error_Text,
               To_Unbounded_String
               (  Context &
                  " fault: " &
                  Exception_Information (Occurrence)
            )  ),
            Timeout
         );
      end if;
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Gtk.Main.Router.Busy_Error =>
         Trace_To_File
         (  (  Context
            &  " fault: "
            &  Exception_Information (Occurrence)
            ),
            Error_Text
         );
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Occurrence)
            &  " [and then "
            &   Exception_Information (Error)
            &  "] "
            &  Where ("Trace_Error (Factory)")
         )  );
   end Trace_Error;

   procedure Trace_Error
             (  Factory    : in out ELV_Secure_Factory;
                Context    : String;
                Occurrence : Exception_Occurrence
             )  is
   begin
      if not Exiting then
         Trace_Messages.Send
         (  Service'Access,
            (  Error_Text,
               To_Unbounded_String
               (  Context &
                  " fault: " &
                  Exception_Information (Occurrence)
            )  ),
            Timeout
         );
      end if;
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Gtk.Main.Router.Busy_Error =>
         Trace_To_File
         (  (  Context
            &  " fault: "
            &  Exception_Information (Occurrence)
            ),
            Error_Text
         );
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Occurrence)
            &  " [and then "
            &   Exception_Information (Error)
            &  "] "
            &  Where ("Trace_Error (Secure Factory)")
         )  );
   end Trace_Error;

   procedure Trace_Error
             (  Context    : String;
                Occurrence : Exception_Occurrence
             )  is
   begin
      if not Exiting then
         Trace_Messages.Send
         (  Service'Access,
            (  Error_Text,
               To_Unbounded_String
               (  Context
               &  " fault: "
               &  Exception_Information (Occurrence)
            )  ),
            Timeout
         );
      end if;
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Gtk.Main.Router.Busy_Error =>
         Trace_To_File
         (  (  Context
            &  " fault: "
            &  Exception_Information (Occurrence)
            ),
            Error_Text
         );
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Occurrence)
            &  " [and then "
            &  Exception_Information (Error)
            &  "] "
            &  Where ("Trace_Error")
         )  );
   end Trace_Error;

   function Trace_ID (Client : Connection'Class) return String is
   begin
      if Client in Cube_Client'Class then
         return Cube_Client'Class (Client).Get_ID;
      else
         return GNAT.Sockets.Image (Client.Get_Client_Address);
      end if;
   end Trace_ID;

   procedure Trace_Received
             (  Factory : in out ELV_Factory;
                Client  : Connection'Class;
                Data    : Stream_Element_Array;
                From    : Stream_Element_Offset;
                To      : Stream_Element_Offset;
                Encoded : Boolean := False
             )  is
      Now : constant Time := Clock;
   begin
      if not Exiting and then ELV_Trace then
         Trace_IO_Messages.Send
         (  Service'Access,
            (  Received_Text,
               Now,
               To_Unbounded_String (Trace_ID (Client)),
               To_Unbounded_String (To_String (Data (From..To)))
            ),
            Timeout
         );
      end if;
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Gtk.Main.Router.Busy_Error =>
         Trace_To_File
          (  To_String (Data (From..To)),
             Received_Text,
             Image (Now, True) & " " & Trace_ID (Client)
          );
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Trace_Received")
         )  );
   end Trace_Received;

   procedure Trace_Received
             (  Factory : in out ELV_Secure_Factory;
                Client  : Connection'Class;
                Data    : Stream_Element_Array;
                From    : Stream_Element_Offset;
                To      : Stream_Element_Offset;
                Encoded : Boolean := False
             )  is
      Now : constant Time := Clock;
   begin
      if not Exiting and then ELV_Trace then
         Trace_IO_Messages.Send
         (  Service'Access,
            (  Received_Text,
               Now,
               To_Unbounded_String (Trace_ID (Client)),
               To_Unbounded_String (To_String (Data (From..To)))
            ),
            Timeout
         );
      end if;
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Gtk.Main.Router.Busy_Error =>
         Trace_To_File
          (  To_String (Data (From..To)),
             Received_Text,
             Image (Now, True) & " " & Trace_ID (Client)
          );
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Trace_Received (Secure)")
         )  );
   end Trace_Received;

   procedure Trace_Sending
             (  Factory : in out ELV_Factory;
                Client  : Connection'Class;
                Enabled : Boolean;
                Reason  : String
             )  is
   begin
      if Exiting or else not ELV_Trace then
         return;
      end if;
      if Enabled then
         if (  Reason /= ", blocking timeout expired"
            and then
               Reason /= ", some data to send"
            )  then
            Trace_IO_Messages.Send
            (  Service'Access,
               (  Mode    => Mode_Text,
                  Stamp   => Clock,
                  ID      => To_Unbounded_String (Trace_ID (Client)),
                  Message => To_Unbounded_String
                             (  "Resume polling"
                             &  Reason
            )  )             );
         end if;
      else
         if Reason /= ", nothing to send" then
            Trace_IO_Messages.Send
            (  Service'Access,
               (  Mode    => Mode_Text,
                  Stamp   => Clock,
                  ID      => To_Unbounded_String (Trace_ID (Client)),
                  Message => To_Unbounded_String
                             (  "Stop polling"
                             &  Reason
            )  )             );
         end if;
      end if;
   end Trace_Sending;

   procedure Trace_Sending
             (  Factory : in out ELV_Secure_Factory;
                Client  : Connection'Class;
                Enabled : Boolean;
                Reason  : String
             )  is
   begin
      if Exiting or else not ELV_Trace then
         return;
      end if;
   end Trace_Sending;

   procedure Trace_Sent
             (  Factory : in out ELV_Factory;
                Client  : Connection'Class;
                Data    : Stream_Element_Array;
                From    : Stream_Element_Offset;
                To      : Stream_Element_Offset;
                Encoded : Boolean := False
             )  is
      Now : constant Time := Clock;
   begin
      if not Exiting and then ELV_Trace  then
         Trace_IO_Messages.Send
         (  Service'Access,
            (  Sent_Text,
               Now,
               To_Unbounded_String (Trace_ID (Client)),
               To_Unbounded_String (To_String (Data (From..To)))
            ),
            Timeout
         );
      end if;
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Gtk.Main.Router.Busy_Error =>
         Trace_To_File
          (  To_String (Data (From..To)),
             Sent_Text,
             Image (Now, True) & " " & Trace_ID (Client)
          );
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Trace_Sent")
         )  );
   end Trace_Sent;

   procedure Trace_Sent
             (  Factory : in out ELV_Secure_Factory;
                Client  : Connection'Class;
                Data    : Stream_Element_Array;
                From    : Stream_Element_Offset;
                To      : Stream_Element_Offset;
                Encoded : Boolean := False
             )  is
      Now : constant Time := Clock;
   begin
      if not Exiting and then ELV_Trace then
         Trace_IO_Messages.Send
         (  Service'Access,
            (  Sent_Text,
               Now,
               To_Unbounded_String (Trace_ID (Client)),
               To_Unbounded_String (To_String (Data (From..To)))
            ),
            Timeout
         );
      end if;
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
      when Gtk.Main.Router.Busy_Error =>
         Trace_To_File
          (  To_String (Data (From..To)),
             Sent_Text,
             Image (Now, True) & " " & Trace_ID (Client)
          );
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Trace_Sent (Secure)")
         )  );
   end Trace_Sent;

   procedure Trace_Service_Loop
             (  Factory : in out ELV_Factory;
                Stage   : Service_Loop_Stage;
                Server  : in out Connections_Server'Class
             )  is
   begin
      null;
--        Trace_To_File
--        (  Service_Loop_Stage'Image (Stage),
--           Mode_Text,
--           "ELV"
--        );
   end Trace_Service_Loop;

   function To_String (Mode : Shortcut_Mode) return String is
   begin
      case Mode is
         when Automatic_Shortcut =>
            return "Automatic";
         when Automatic_With_Temperature_Shortcut =>
            return "Automatic+";
         when Manual_Shortcut =>
            return "Manual";
         when Vacation_Shortcut =>
            return "Vacation";
         when Boost_Shortcut =>
            return "Boost";
         when Eco_Shortcut =>
            return "Eco";
         when Comfort_Shortcut =>
            return "Comfort";
         when Airing_Shortcut =>
            return "Airing";
         when Unchanged_Shortcut =>
            return Unchanged;
      end case;
   end To_String;

   function Value (Text : String) return RF_Address is
   begin
      if Text'Length = 0 then
         return 0;
      end if;
      return RF_Address (Integer'(Value (Source => Text, Base => 16)));
   exception
      when others =>
         return 0;
   end Value;

   procedure Version_Update (Major, Minor : Positive) is
   begin
      Update_Version_Messages.Send
      (  Service'Access,
         (Major, Minor, Outdated),
         Timeout
      );
   end Version_Update;

   procedure Version_Uptodate (Major, Minor : Positive) is
   begin
      Update_Version_Messages.Send
      (  Service'Access,
         (Major, Minor, Uptodate),
         Timeout
      );
   end Version_Uptodate;

   procedure Version_Unknown is
   begin
      Update_Version_Messages.Send
      (  Service'Access,
         (0, 0, Unknown),
         Timeout
      );
   end Version_Unknown;

   procedure Wake_Up is
   begin
      Worker.Wake_Up;
   end Wake_Up;

   function "=" (Left, Right : Cube_Descriptor) return Boolean is
      use GNAT.Sockets;
   begin
      return Image (Left.Address) = Image (Right.Address);
   end "=";

   function "<" (Left, Right : Cube_Descriptor) return Boolean is
      use GNAT.Sockets;
   begin
      return Image (Left.Address) < Image (Right.Address);
   end "<";

   function "<" (Left, Right : Shortcut_Target) return Boolean is
   begin
      return
      (  Left.Cube < Right.Cube
      or else
         (  Left.Cube = Right.Cube
         and then
            Left.Thermostat = Right.Thermostat
      )  );
   end "<";

   task body Worker is
      use Settings_Request_List.Doubly_Linked;
      procedure Free is
         new Ada.Unchecked_Deallocation (Settings_Request'Class, Item);
      Factory        : aliased ELV_Factory;
      Server         : aliased Connections_Server (Factory'Access, 0);
      List           : Cube_List.Map;
      IP_Address     : Unbounded_String;
      Poll_Time      : Duration := 1.0;
      Last_Poll      : Time     := Clock;
      Devices        : Device_Maps.Map;
         -- Queue
      Queue          : Settings_Request_List.Doubly_Linked.List;
      Current        : Settings_Request_List.Doubly_Linked.Item;
      type Dequeue_Action is
           (  Keep_Current,
              Complete_Current,
              Cancel_Current
           );
      Action : Dequeue_Action := Keep_Current;

      procedure Do_Add is
         use GNAT.Sockets;
         use Strings_Edit;
         Host    : constant UTF8_String :=
                            Trim (To_String (IP_Address));
         Address : Inet_Addr_Type (Family_Inet);
      begin
         if Host'Length = 0 then
            return;
         end if;
         Address := To_Addr (Host);
         for Index in 1..List.Get_Size loop
            declare
               This : constant Cube_Descriptor := List.Get_Key (Index);
            begin
               if This.Address = Address then
                  return;
               end if;
            end;
         end loop;
         declare
            Reference : Cube_Client_Handle;
         begin
            Set (Reference, new Cube_Client (Server'Unchecked_Access));
            declare
               Client : Cube_Client'Class renames Reference.Ptr.all;
            begin
               Client.ID.Address   := Address;
               Client.ID.Name      := (others => ' ');
               Client.ID.Serial_No := (others => ' ');
               Connect
               (  Server,
                  Client'Unchecked_Access,
                  Image (Address),
                  ELV_MAX_Cube_Port
               );
               List.Replace (Client.ID, Reference);
            end;
         exception
            when Error : Socket_Error =>
               Say
               (  (  Exception_Message (Error)
                  &  ", when connecting to the cube at "
                  &  Image (Address)
                  ),
                  "Connection error"
               );
         end;
      exception
         when Error : Socket_Error =>
            Say (Exception_Message (Error), "MAX! cube error");
         when Error : Host_Error =>
            Say (Exception_Message (Error), "MAX! cube address error");
         when Error : others =>
            Trace_To_File
            (  (  "MAX! cube fault :"
               &  Exception_Information (Error)
               ),
               Error_Text
            );
      end Do_Add;

      procedure Scan_LAN (Host : UTF8_String) is
      begin
         Trace
         (  "Searching LAN segment for MAX! cubes... [" & Host & "]",
            Message_Text
         );
         declare
            Cubes : constant Cube_Descriptor_Array :=
                             Discover (Host => Host);
         begin
            if Cubes'Length = 0 then
               Trace ("No new MAX! cube discovered", Message_Text);
            else
               for Index in Cubes'Range loop
                  declare
                     This : Cube_Descriptor renames Cubes (Index);
                  begin
                     if not List.Is_In (This) then
                        Trace
                        (  (  "MAX! cube at "
                           &  GNAT.Sockets.Image (This.Address)
                           &  " "
                           &  This.Name
                           &  " "
                           &  This.Serial_No
                           ),
                           Message_Text
                        );
                        declare
                           Reference : Cube_Client_Handle;
                        begin
                           Set
                           (  Reference,
                              new Cube_Client
                                  (  Server'Unchecked_Access
                           )      );
                           declare
                              Client : Cube_Client renames
                                       Cube_Client (Reference.Ptr.all);
                           begin
                              Client.ID := This;
                              Connect
                              (  Server,
                                 Client'Unchecked_Access,
                                 GNAT.Sockets.Image (This.Address),
                                 ELV_MAX_Cube_Port
                              );
                           end;
                           List.Add (This, Reference);
                        exception
                           when Error : GNAT.Sockets.Socket_Error =>
                              Say
                              (  (  Exception_Message (Error)
                                 &  ", when connecting to the cube at "
                                 &  GNAT.Sockets.Image (This.Address)
                                 ),
                                "Connection error"
                              );
                        end;
                     end if;
                  end;
               end loop;
            end if;
         end;
      exception
         when Error : GNAT.Sockets.Socket_Error =>
            Say
            (  Exception_Message (Error),
               "LAN discovery error [" & Host & "]"
            );
         when Error : GNAT.Sockets.Host_Error =>
            Say
            (  Exception_Message (Error),
               "LAN discovery error [" & Host & "]"
            );
         when Error : others =>
            Trace_To_File
            (  (  "LAN discovery fault :"
               &  Exception_Information (Error)
               ),
               Error_Text
            );
      end Scan_LAN;

      procedure Do_Scan is
         use GNAT.Sockets;
         use Strings_Edit;
         Host : constant UTF8_String := Trim (To_String (IP_Address));
      begin
         if Host'Length = 0 then
            declare
               Text : Unbounded_String;
               Host : constant Host_Entry_Type :=
                               Get_Host_By_Name (Host_Name);
            begin
               Append (Text, "Host ");
               Append (Text, Host_Name);
               Append (Text, " has addresses to scan: ");
               for Index in 1..Addresses_Length (Host) loop
                  if Index > 1 then
                     Append (Text, ", ");
                  end if;
                  Append (Text, Image (Addresses (Host, Index)));
               end loop;
               Trace (To_String (Text), Message_Text);
               for Index in 1..Addresses_Length (Host) loop
                  Scan_LAN (Image (Addresses (Host, Index)));
               end loop;
            end;
         else
            Scan_LAN (Host);
         end if;
      end Do_Scan;

      procedure Do_Poll
                (  Cube : in out Cube_Client'Class;
                   Data : Device_Data;
                   Now  : Time
                )  is
         Index : Integer;
         This  : Device_Control_Handles.Handle;
      begin
         if Data.Kind_Of not in Radiator_Thermostat
                             .. Radiator_Thermostat_Plus then
            return;
         end if;
         Index := Devices.Find (Data.Address);
         if Index > 0 then
            This := Devices.Get (Index);
         else
            This.Set (new Device_Control);
            This.Ptr.Last_Switch := Clock - Scan_Period;
            Devices.Add (Data.Address, This);
         end if;
         declare
            Device : Device_Control'Class renames This.Ptr.all;
         begin
            if Device.Control = Unknown then
               Device.Set_Control (Data.Address, Cube);
            end if;
            case Device.Action is
               when No_Action => -- Nothing to do
                  if (  Scan_On
                     and then
                        Data.Mode in Managed_Mode
                     and then
                        Now - Device.Last_Switch >= Scan_Period
                     and then
                        Device.Control = Unmanaged_Room
                     and then
                         (  Data.Latest_Temperature = Centigrade'First
                         or else
                            Now - Data.Received_At > Temperature_Timeout
                     )   )
                  then
                     Device.Switch_Forth
                     (  Data.Address,
                        Data.Mode,
                        Data.Set_Temperature,
                        Now,
                        Cube
                     );
                  end if;
               when others =>
                  Device.Switch_Back
                  (  Data.Address,
                     Data,
                     Now,
                     Cube
                  );
            end case;
         end;
      end Do_Poll;

      procedure Do_Poll is
         This : Cube_Client_Handle;
      begin
         Poll_No := Poll_No + 1;
         for Index in 1..List.Get_Size loop
            This := List.Get (Index);
            if This.Is_Valid then
               declare
                  Cube : Cube_Client'Class renames This.Ptr.all;
               begin
                  if (  Cube.Is_Connected
                     and then
                        Cube.Queued_To_Send = 0
                     and then
                        Cube.Available_To_Process = 0
                     )
                  then
                     if Cube.Is_Configured then
                        Cube.Query_Devices;
                     else
                        if Cube.Configuration_Queried then
                           if Clock - Cube.Created > 20.0 then
                              Cube.Query_Devices; -- Query anyway
                           end if;
                        else
                           if Clock - Cube.Created > 10.0 then
                              Trace
                               (  (  "Querying missing device "
                                  &  "configurations of the cube "
                                  &  Image (Cube.Get_RF_Address (True))
                                  ),
                                  Error_Text
                              );
                              Cube.Configuration_Queried := True;
                              Cube.Query_Unconfigured_Devices;
                           end if;
                        end if;
                     end if;
                  end if;
               exception
                  when others =>
                     null;
               end;
            end if;
         end loop;
         declare
            Now : constant Time := Clock;
         begin
            for Index in 1..List.Get_Size loop
               This := List.Get (Index);
               if This.Is_Valid then
                  begin
                     declare
                        Cube  : Cube_Client'Class renames This.Ptr.all;
                        Min   : Ratio   := 1.0;
                        Max   : Ratio   := 0.0;
                        Sum   : Float   := 0.0;
                        Count : Natural := 0;
                     begin
                        for No in 1..Cube.Get_Number_Of_Devices loop
                           begin
                              if Cube.Has_Device_Data (No) then
                                 declare
                                    Data : constant Device_Data :=
                                           Cube.Get_Device_Data (No);
                                 begin
                                    Do_Poll (Cube, Data, Now);
                                    if Data.Kind_Of in
                                          Radiator_Thermostat
                                       .. Radiator_Thermostat_Plus then
                                       declare
                                          Valve : constant Ratio :=
                                                  Data.Valve_Position;
                                       begin
                                          Count := Count + 1;
                                          Sum := Sum + Float (Valve);
                                          Max := Ratio'Max (Max, Valve);
                                          Min := Ratio'Min (Min, Valve);
                                      end;
                                    end if;
                                 end;
                              end if;
                           exception
                              when Error : others =>
                                 Trace_To_File
                                 (  (  "Poll: getting device data "
                                    &  "fault :"
                                    &  Exception_Information (Error)
                                    ),
                                    Error_Text
                                 );
                           end;
                        end loop;
                        if Count > 0 then
                           Sum := Sum / Float (Count);
                           Cube.Average := Integer (Sum * 100.0);
                           Cube.Min := Integer (Float (Min) * 100.0);
                           Cube.Max := Integer (Float (Max) * 100.0);
                           if MQTT_Listener /= null then
                              Publish
                              (  Client  => Cube,
                                 Server  => MQTT_State.all,
                                 Cube    => Cube.Get_RF_Address,
                                 Min     => Cube.Min,
                                 Max     => Cube.Max,
                                 Average => Cube.Average,
                                 Policy  => MQTT_Cube_Policy
                              );
                           end if;
                           Cube_Statistics_Messages.Send
                           (  Service'Access,
                              (  Address => Cube.Get_RF_Address,
                                 Average => Cube.Average
                           )  );
                        end if;
                     end;
                  exception
                     when Error : others =>
                        Trace_To_File
                        (  (  "Poll: getting cube fault :"
                           &  Exception_Information (Error)
                           ),
                           Error_Text
                        );
                  end;
               end if;
            end loop;
         end;
      end Do_Poll;

      procedure Do_Queue is
      begin
         loop
            Take (Queue, Current);
            exit when Current = null;
            begin
               for Index in 1..List.Get_Size loop
                  declare
                     Cube : constant Cube_Client_Handle :=
                                     List.Get (Index);
                  begin
                     if (  Cube.Is_Valid
                        and then
                           Cube.Ptr.Get_RF_Address (True) = Current.Cube
                        )  then
                        Current.Client := Cube;
                        exit;
                     end if;
                  end;
               end loop;
               if Current.Client.Is_Valid then
                  Current_No := Current.No;   -- Initiate
                  begin
                     Trace_To_File
                     (  Message => "Starting to " & Current.Image,
                        Prefix  => "Queue: "
                     );
                     Current.Started := Clock;
                     Current.Start (Current.Client.Ptr.all, List);
                     if Current.Expected > 0 then
                        return;
                     end if;
                     if not Current.Error then
                        Trace_To_File
                        (  Message => "Continuing to " & Current.Image,
                           Prefix  => "Queue: "
                        );
                        Current.Continue (Current.Client.Ptr.all);
                     end if;
                     if Current.Expected > 0 then
                        return;
                     end if;
                     Trace_To_File
                     (  Message => "Completing to " & Current.Image,
                        Prefix  => "Queue: "
                     );
                     Current.Complete (False);
                  exception
                     when Error : others =>
                        Trace_To_File
                        (  Message => "Failed to " & Current.Image,
                           Error   => Error
                        );
                        Current.Report
                        (  "Unsuccessful attempt to "
                        &  Current.Image
                        &  ": "
                        &  Exception_Information (Error)
                        );
                        Current.Complete (True);
                  end;
               else
                  Trace_To_File
                  (  Message => "No cube found to " & Current.Image,
                     Prefix  => "Queue: "
                  );
                  Current.Complete (True);
               end if;
               Free (Current);
            end;
         end loop;
      end Do_Queue;

      procedure Do_Rooms_List
                (  Box     : RF_Address;
                   Rooms   : in out Room_To_Device_List.Map;
                   Filter  : Device_Type_Set;
                   No_Cube : out Boolean
                )  is
         Cube : Cube_Client_Handle;
      begin
         Rooms.Erase;
         No_Cube := True;
         for Index in 1..List.Get_Size loop
            Cube := List.Get (Index);
            if Cube.Is_Valid then
               declare
                  Client : Cube_Client'Class renames Cube.Ptr.all;
               begin
                  if Box = 0 or else Client.Get_RF_Address (True) = Box
                  then
                     No_Cube := False;
                     for Room in 1..Client.Get_Number_Of_Rooms loop
                        declare
                           This : Room_Devices_List_Handles.Handle;
                        begin -- Creating rooms list
                           declare
                              ID   : constant Room_ID :=
                                              Client.Get_Room_ID (Room);
                              Name : constant String :=
                                              Client.Get_Room_Name (ID);
                           begin
                              This.Set
                              (  new Room_Devices_List (Name'Length)
                              );
                              This.Ptr.Room := Name;
                              Rooms.Add (ID, This);
                           end;
                        end;
                     end loop;
                     for Index in 1..Client.Get_Number_Of_Devices loop
                        declare
                           Offset     : Integer;
                           Parameters : constant Device_Parameters :=
                              Client.Get_Device_Parameters (Index);
                        begin
                           if Filter (Parameters.Kind_Of) then
                              Offset := Rooms.Find (Parameters.Room);
                              if Offset > 0 then
                                 Rooms.Get (Offset).Ptr.List.Add
                                 (  Parameters.Address,
                                    Parameters
                                 );
                              end if;
                           end if;
                        exception
                           when others =>
                              null;
                        end;
                     end loop;
                  end if;
               exception
                  when others =>
                     null;
               end;
            end if;
         end loop;
      exception
         when Error : others =>
            Trace_To_File
            (  (  "Rooms list (devices): getting cube fault :"
               &  Exception_Information (Error)
               ),
               Error_Text
            );
      end Do_Rooms_List;

      IO_Loop_Prefix : constant String := "IO loop: ";
   begin
      Trace_On
      (  Factory  => Factory,
         Received => GNAT.Sockets.Server.Trace_Decoded,
         Sent     => GNAT.Sockets.Server.Trace_Decoded
      );
      while not Exiting loop
         if Current = null then
            Trace_To_File
            (  Message => (  "selecting, to poll "
                          &  Image (Next_Poll - Clock)
                          ),
               Prefix  => IO_Loop_Prefix
            );
         else
            Trace_To_File
            (  Message => (  "selecting to poll "
                          &  Image (Next_Poll - Clock)
                          &  ", to current operation deadline "
                          &  Image
                             (  Current.Started
                             +  Setting_Timeout
                             -  Clock
                          )  ),
               Prefix  => IO_Loop_Prefix
            );
         end if;
         select
            accept Scan (Address : UTF8_String) do
               IP_Address := To_Unbounded_String (Address);
            end Scan;
            Trace_To_File
            (  Message => "accept Scan",
               Prefix  => IO_Loop_Prefix
            );
            Do_Scan;
         or accept Add_Manually (Address : UTF8_String) do
               IP_Address := To_Unbounded_String (Address);
            end Add_Manually;
            Trace_To_File
            (  Message => "accept Add_Manually",
               Prefix  => IO_Loop_Prefix
            );
            Do_Add;
         or accept Dequeue
                   (  No     : Settings_Sequence_No;
                      Cancel : Boolean
                   )  do
               if No = Settings_Sequence_No'Last then
                  if Cancel then
                     Trace_To_File
                     (  Message => "accept Dequeue [cancel current]",
                        Prefix  => IO_Loop_Prefix
                     );
                  else
                     Trace_To_File
                     (  Message => "accept Dequeue [complete current]",
                        Prefix  => IO_Loop_Prefix
                     );
                  end if;
                  if Current = null then
                     Action := Keep_Current;
                  elsif Cancel then
                     Action := Cancel_Current;
                  else
                     Action := Complete_Current;
                  end if;
               else
                  if Cancel then
                     Trace_To_File
                     (  Message => "accept Dequeue [cancel "       &
                                   Settings_Sequence_No'Image (No) &
                                   "]",
                        Prefix  => IO_Loop_Prefix
                     );
                  else
                     Trace_To_File
                     (  Message => "accept Dequeue [complete "     &
                                   Settings_Sequence_No'Image (No) &
                                   "]",
                        Prefix  => IO_Loop_Prefix
                     );
                  end if;
                  if Current = null or else No /= Current.No then
                     Action := Keep_Current;
                  elsif Cancel then
                     Action := Cancel_Current;
                  else
                     Action := Complete_Current;
                  end if;
               end if;
            end Dequeue;
            case Action is
               when Keep_Current =>
                  null;
               when Cancel_Current =>
                  Trace_To_File
                  (  Message => "Canceling to " & Current.Image,
                     Prefix  => IO_Loop_Prefix
                  );
                  Current.Complete (True);
                  Free (Current);
                  Do_Queue;
               when Complete_Current =>
                  begin
                     if not Current.Error then
                        Trace_To_File
                        (  Message => "Continuing to "           &
                                      Current.Image              &
                                      ". Outstanding requests: " &
                                      Image (Current.Expected),
                           Prefix  => IO_Loop_Prefix
                        );
                        Current.Continue (Current.Client.Ptr.all);
                     end if;
                     if Current.Expected = 0 then
                        Current_No := Settings_Sequence_No'Last;
                        Trace_To_File
                        (  Message => "Completing to " & Current.Image,
                           Prefix  => IO_Loop_Prefix
                        );
                        Current.Complete (False);
                        Free (Current);
                        Do_Queue;
                     end if;
                  exception
                     when Error : others =>
                        Trace_To_File
                        (  Message => "Failed to " & Current.Image,
                           Error   => Error
                        );
                        Current.Report
                        (  "Unsuccessful attempt to "
                        &  Current.Image
                        &  ": "
                        &  Exception_Information (Error)
                        );
                        Current.Complete (True);
                        Free (Current);
                        Do_Queue;
                  end;
            end case;
         or accept Enqueue (Request : Item) do
               Append (Queue, Request);
               Trace_To_File
               (  Message => "accept Enqueue " & Image (Request.all),
                  Prefix  => IO_Loop_Prefix
               );
            end Enqueue;
            if Current = null then
               Do_Queue;
            end if;
         or accept Get
                   (  Box    : RF_Address;
                      Device : RF_Address;
                      Data   : in out Device_Parameters_Data_Handles.
                                      Handle
                   )  do
               Trace_To_File
               (  Message => "accept Get",
                  Prefix  => IO_Loop_Prefix
               );
               Data.Invalidate;
               for Index in 1..List.Get_Size loop
                  declare
                     This : constant Cube_Client_Handle :=
                                     List.Get (Index);
                  begin
                     if This.Is_Valid then
                        declare
                           Cube : Cube_Client'Class renames
                                  This.Ptr.all;
                        begin
                           if Cube.Get_RF_Address (True) = Box then
                              declare
                                 Parameters : constant
                                              Device_Parameters :=
                                    Cube.Get_Device_Parameters (Device);
                                 function Get_Room_Name return String is
                                 begin
                                    if Parameters.Room = No_Room then
                                       return "";
                                    else
                                       return Cube.Get_Room_Name
                                              (  Parameters.Room
                                              );
                                    end if;
                                 end Get_Room_Name;
                                 Name : constant String :=
                                                 Get_Room_Name;
                              begin
                                 Data.Set
                                 (  new Device_Parameters_Data'
                                        (  Object.Entity
                                        with
                                           Kind_Of =>
                                              Parameters.Kind_Of,
                                           Room_Length => Name'Length,
                                           Room        => Name,
                                           Parameters  => Parameters,
                                           Name_Length =>
                                              Parameters.Name_Length
                                 )      );
                                 exit;
                              end;
                           end if;
                        exception
                           when others =>
                              null;
                        end;
                     end if;
                  end;
               end loop;
            end Get;
         or accept Get_Cube
                   (  Box  : RF_Address;
                      Cube : in out Cube_Client_Handle
                   )  do
               Trace_To_File
               (  Message => "accept Get_Cube by RF address",
                  Prefix  => IO_Loop_Prefix
               );
               for Index in 1..List.Get_Size loop
                  Cube := List.Get (Index);
                  if Cube.Is_Valid then
                     declare
                        This : Cube_Client'Class renames Cube.Ptr.all;
                     begin
                        if This.Get_RF_Address (True) = Box then
                           return;
                        end if;
                     exception
                        when others =>
                           Cube.Invalidate;
                     end;
                  end if;
               end loop;
               Cube.Invalidate;
            end Get_Cube;
         or accept Get_Cube
                   (  Address : GNAT.Sockets.Inet_Addr_Type;
                      Cube    : in out Cube_Client_Handle
                   )  do
               Trace_To_File
               (  Message => "accept Get_Cube by IP address",
                  Prefix  => IO_Loop_Prefix
               );
               for Index in 1..List.Get_Size loop
                  Cube := List.Get (Index);
                  if Cube.Is_Valid then
                     declare
                        use GNAT.Sockets;
                        This : Cube_Client'Class renames Cube.Ptr.all;
                     begin
                        if This.ID.Address = Address then
                           return;
                        end if;
                     exception
                        when others =>
                           Cube.Invalidate;
                     end;
                  end if;
               end loop;
               Cube.Invalidate;
            end Get_Cube;
         or accept Get_Cubes_List
                   (  Cubes : in out RF_Address_Sets.Set
                   )  do
               Trace_To_File
               (  Message => "accept Get_Cubes_List",
                  Prefix  => IO_Loop_Prefix
               );
               Cubes.Erase;
               for Index in 1..List.Get_Size loop
                  declare
                     This : constant Cube_Client_Handle :=
                                     List.Get (Index);
                  begin
                     if This.Is_Valid then
                        declare
                           Cube : Cube_Client'Class renames
                                  This.Ptr.all;
                        begin
                           Cubes.Add (Cube.Get_RF_Address (True));
                        end;
                     end if;
                  end;
               end loop;
            end Get_Cubes_List;
         or accept Get_Devices_List
                   (  Box     : RF_Address;
                      Devices : in out RF_Address_Sets.Set;
                      Filter  : Device_Type_Set;
                      No_Cube : out Boolean
                   )  do
               Trace_To_File
               (  Message => "accept Get_Devices_List",
                  Prefix  => IO_Loop_Prefix
               );
               Devices.Erase;
               No_Cube := True;
               declare
                  Cube : Cube_Client_Handle;
               begin
                  for Index in 1..List.Get_Size loop
                     Cube := List.Get (Index);
                     if Cube.Is_Valid then
                        declare
                           Client : Cube_Client'Class renames
                                    Cube.Ptr.all;
                        begin
                           if (  Box = 0
                              or else
                                 Client.Get_RF_Address (True) = Box
                              )  then
                              No_Cube := False;
                              for Index in 1
                                        .. Client.Get_Number_Of_Devices
                              loop
                                 declare
                                    Parameters : constant
                                                 Device_Parameters :=
                                       Client.Get_Device_Parameters
                                       (  Index
                                       );
                                 begin
                                    if Filter (Parameters.Kind_Of) then
                                       Devices.Add (Parameters.Address);
                                    end if;
                                 end;
                              end loop;
                           end if;
                        exception
                           when others =>
                              null;
                        end;
                     end if;
                  end loop;
               end;
            end Get_Devices_List;
         or accept Get_Device_Data
                   (  Box    : RF_Address;
                      Device : RF_Address;
                      Data   : in out Device_Status_Data_Handles.Handle
                   )  do
               Trace_To_File
               (  Message => "accept Get_Device_Data by RF address " &
                             Image (Device),
                  Prefix  => IO_Loop_Prefix
               );
               Data.Invalidate;
               for Index in 1..List.Get_Size loop
                  declare
                     This : constant Cube_Client_Handle :=
                                     List.Get (Index);
                  begin
                     if This.Is_Valid then
                        declare
                           Cube : Cube_Client'Class renames
                                  This.Ptr.all;
                        begin
                           if Cube.Get_RF_Address (True) = Box then
                              if Cube.Is_In (Device) then
                                 declare
                                    Status : constant Device_Data :=
                                       Cube.Get_Device_Data (Device);
                                 begin
                                    Data.Set
                                    (  new Device_Status_Data'
                                           (  Object.Entity
                                           with
                                              Kind_Of => Status.Kind_Of,
                                              Status  => Status
                                    )      );
                                 end;
                              end if;
                              exit;
                           end if;
                        exception
                           when Error : others =>
                              Trace_To_File
                              (  Message =>
                                    "Get_Device_Data by RF address " &
                                    Image (Device)                   &
                                    " fault: "                       &
                                    Exception_Information (Error),
                                 Prefix  => IO_Loop_Prefix
                              );
                        end;
                     end if;
                  end;
               end loop;
            end Get_Device_Data;
         or accept Get_Device_Data
                   (  Box   : RF_Address;
                      Index : in out Positive;
                      Data  : in out Device_Status_Data_Handles.Handle
                   )  do
               Trace_To_File
               (  Message => "accept Get_Device_Data by index " &
                             Image (Index),
                  Prefix  => IO_Loop_Prefix
               );
               Data.Invalidate;
               for No in 1..List.Get_Size loop
                  declare
                     This : constant Cube_Client_Handle :=
                                     List.Get (No);
                  begin
                     if This.Is_Valid then
                        declare
                           Cube : Cube_Client'Class renames
                                  This.Ptr.all;
                        begin
                           if Cube.Get_RF_Address (True) = Box then
                              while Index <=
                                    Cube.Get_Number_Of_Devices
                              loop
                                 begin
                                    declare
                                       Status : constant Device_Data :=
                                          Cube.Get_Device_Data (Index);
                                    begin
                                       Data.Set
                                       (  new Device_Status_Data'
                                              (  Object.Entity
                                              with
                                                 Kind_Of =>
                                                    Status.Kind_Of,
                                                 Status =>
                                                    Status
                                       )      );
                                       exit;
                                    end;
                                 exception
                                    when Status_Error =>
                                       null; -- Uninitialized device
                                 end;
                                 Index := Index + 1;
                              end loop;
                              exit;
                           end if;
                        end;
                     end if;
                  exception
                     when Error : others =>
                        Trace_To_File
                        (  Message => "Get_Device_Data by index " &
                                      Image (Index)               &
                                      " fault: "                  &
                                      Exception_Information (Error),
                           Prefix  => IO_Loop_Prefix
                        );
                  end;
               end loop;
            end Get_Device_Data;
         or accept Get_Device_Name
                   (  Box    : RF_Address;
                      Device : RF_Address;
                      Data   : in out Unbounded_String
                   )  do
               Trace_To_File
               (  Message => "accept Get_Device_Name",
                  Prefix  => IO_Loop_Prefix
               );
               for Index in 1..List.Get_Size loop
                  declare
                     This : constant Cube_Client_Handle :=
                                     List.Get (Index);
                  begin
                     if This.Is_Valid then
                        declare
                           Cube : Cube_Client'Class renames
                                  This.Ptr.all;
                        begin
                           if Cube.Get_RF_Address (True) = Box then
                              Data := To_Unbounded_String
                                      (  Cube.Get_Device_Name (Device)
                                      );
                           end if;
                        exception
                           when others =>
                              null;
                        end;
                     end if;
                  end;
               end loop;
            end Get_Device_Name;
         or accept Get_Radiator_Thermostats
                   (  Box  : RF_Address;
                      Room : Room_ID;
                      Set  : in out RF_Address_Sets.Set
                   )  do
               Trace_To_File
               (  Message => "accept Get_Radiator_Thermostats",
                  Prefix  => IO_Loop_Prefix
               );
               if Room = No_Room then
                  return;
               end if;
               declare
                  Cube : Cube_Client_Handle;
               begin
                  for Index in 1..List.Get_Size loop
                     Cube := List.Get (Index);
                     if Cube.Is_Valid then
                        declare
                           Client : Cube_Client'Class renames
                                    Cube.Ptr.all;
                        begin
                           if Client.Get_RF_Address (True) = Box then
                              for Index in
                                        1
                                     .. Client.Get_Number_Of_Devices
                                        (  Room
                                        )
                              loop
                                 declare
                                    Parameters : constant
                                                 Device_Parameters :=
                                       Client.Get_Device_Parameters
                                       (  Client.Get_Device
                                          (  Room,
                                             Index
                                       )  );
                                 begin
                                    if Parameters.Kind_Of
                                          in Radiator_Thermostat
                                          .. Radiator_Thermostat_Plus
                                    then
                                       Set.Add (Parameters.Address);
                                    end if;
                                 end;
                              end loop;
                           end if;
                        exception
                           when others =>
                              null;
                        end;
                     end if;
                  end loop;
               end;
            end Get_Radiator_Thermostats;
         or accept Get_Room_Thermostats
                   (  Box    : RF_Address;
                      Room   : Room_ID;
                      Filter : Device_Type_Set;
                      Set    : in out RF_Address_Maps.Map
                   )  do
               Trace_To_File
               (  Message => "accept Get_Room_Thermostats",
                  Prefix  => IO_Loop_Prefix
               );
               declare
                  Cube : Cube_Client_Handle;
               begin
                  for Index in 1..List.Get_Size loop
                     Cube := List.Get (Index);
                     if Cube.Is_Valid then
                        declare
                           Client : Cube_Client'Class renames
                                   Cube.Ptr.all;
                        begin
                           if Client.Get_RF_Address (True) = Box then
                              if Room = No_Room then
                                 for Index in 1
                                           .. Client.
                                              Get_Number_Of_Devices
                                 loop
                                    declare
                                       Parameters : constant
                                                    Device_Parameters :=
                                          Client.Get_Device_Parameters
                                          (  Index
                                          );
                                    begin
                                       if Filter (Parameters.Kind_Of)
                                       then
                                          Set.Add
                                          (  Parameters.Address,
                                             Parameters.Kind_Of
                                          );
                                       end if;
                                    end;
                                 end loop;
                              else
                                 for Index in 1
                                           .. Client.
                                              Get_Number_Of_Devices
                                              (  Room
                                              )
                                 loop
                                    declare
                                       Parameters : constant
                                                    Device_Parameters :=
                                          Client.Get_Device_Parameters
                                          (  Client.Get_Device
                                             (  Room,
                                                Index
                                          )  );
                                    begin
                                       if Filter (Parameters.Kind_Of)
                                       then
                                          Set.Add
                                          (  Parameters.Address,
                                             Parameters.Kind_Of
                                          );
                                       end if;
                                    end;
                                 end loop;
                              end if;
                           end if;
                        exception
                           when others =>
                              null;
                        end;
                     end if;
                  end loop;
               end;
            end Get_Room_Thermostats;
         or accept Get_Rooms_List
                   (  Box     : RF_Address;
                      Rooms   : in out Room_To_Device_List.Map;
                      Filter  : Device_Type_Set;
                      No_Cube : out Boolean
                   )  do
               Trace_To_File
               (  Message => "accept Get_Rooms_List of " & Image (Box),
                  Prefix  => IO_Loop_Prefix
               );
               Do_Rooms_List (Box, Rooms, Filter, No_Cube);
            end Get_Rooms_List;
         or accept Get_Thermostats (Set : in out RF_Address_Sets.Set) do
               Trace_To_File
               (  Message => "accept Get_Thermostats as a set",
                  Prefix  => IO_Loop_Prefix
               );
               declare
                  Cube : Cube_Client_Handle;
               begin
                  for Index in 1..List.Get_Size loop
                     Cube := List.Get (Index);
                     if Cube.Is_Valid then
                        declare
                           Client : Cube_Client'Class renames
                                    Cube.Ptr.all;
                        begin
                           for Index in 1
                                     .. Client.Get_Number_Of_Devices
                           loop
                              declare
                                 Parameters : constant
                                              Device_Parameters :=
                                    Client.Get_Device_Parameters
                                    (  Index
                                    );
                              begin
                                 case Parameters.Kind_Of is
                                    when Radiator_Thermostat..
                                         Wall_Thermostat =>
                                       Set.Add (Parameters.Address);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           end loop;
                        exception
                           when others =>
                              null;
                        end;
                     end if;
                  end loop;
               end;
            end Get_Thermostats;
         or accept Get_Topology
                   (  Box      : RF_Address;
                      Topology : in out Topology_Handles.Handle
                   )  do
               Trace_To_File
               (  Message => "accept Get_Topology",
                  Prefix  => IO_Loop_Prefix
               );
               Topology.Set (new Topology_Object (100_000));
               for Index in 1..List.Get_Size loop
                  declare
                     This : constant Cube_Client_Handle :=
                                     List.Get (Index);
                  begin
                     if This.Is_Valid then
                        declare
                           Cube    : Cube_Client'Class renames
                                     This.Ptr.all;
                           Pointer : Integer := 1;
                        begin
                           if Cube.Get_RF_Address (True) = Box then
                              Put
                              (  Topology.Ptr.Metadata,
                                 Pointer,
                                 Cube
                              );
                              Topology.Ptr.Length := Pointer - 1;
                              exit;
                           end if;
                        exception
                           when others =>
                              null;
                        end;
                     end if;
                  end;
               end loop;
            end Get_Topology;
         or accept Get_Valves
                   (  Box     : RF_Address;
                      Average : out Natural;
                      Min     : out Natural;
                      Max     : out Natural
                   )  do
               Trace_To_File
               (  Message => "accept Get_Valves",
                  Prefix  => IO_Loop_Prefix
               );
               Average := 0;
               Min     := 0;
               Max     := 0;
               for Index in 1..List.Get_Size loop
                  declare
                     This : constant Cube_Client_Handle :=
                                     List.Get (Index);
                  begin
                     if This.Is_Valid then
                        declare
                           Cube : Cube_Client'Class renames
                                  This.Ptr.all;
                        begin
                           if Cube.Get_RF_Address (True) = Box then
                              Average := Cube.Average;
                              Max     := Cube.Max;
                              Min     := Cube.Min;
                              exit;
                           end if;
                        exception
                           when others =>
                              null;
                        end;
                     end if;
                  end;
               end loop;
            end Get_Valves;
         or accept Reconnect
                   (  Address       : GNAT.Sockets.Inet_Addr_Type;
                      Connect_Again : Boolean;
                      Silent        : Boolean
                   )  do
               Trace_To_File
               (  Message => "accept Reconnect",
                  Prefix  => IO_Loop_Prefix
               );
               declare
                  Cube : Cube_Client_Handle;
               begin
                  for Index in 1..List.Get_Size loop
                     Cube := List.Get (Index);
                     if Cube.Is_Valid then
                        declare
                           use GNAT.Sockets;
                           Client : Cube_Client'Class renames
                                    Cube.Ptr.all;
                        begin
                           if Client.ID.Address = Address then
                              if Connect_Again then
                                 if not Client.Is_Connected then
                                    Trace
                                     (  (  "Connecting to the cube at "
                                        &  Image (Address)
                                        ),
                                        Message_Text
                                    );
                                    begin
                                       Connect
                                       (  Server,
                                          Client'Unchecked_Access,
                                          Image (Address),
                                          ELV_MAX_Cube_Port
                                       );
                                    exception
                                       when Error : Socket_Error =>
                                          if Silent then
                                             Trace
                                             (  (  "Connection to the "
                                                &  "cube at "
                                                &  Image (Address)
                                                &  " failed: "
                                                &  Exception_Message
                                                   (  Error
                                                )  ),
                                                Error_Text
                                             );
                                          else
                                             Say
                                             (  (  Exception_Message
                                                   (  Error
                                                   )
                                                &  ", when reconnecting"
                                                &  " to the cube at "
                                                &  Image (Address)
                                                ),
                                                "Connection error"
                                             );
                                          end if;
                                    end;
                                 end if;
                              else
                                 if Client.Is_Connected then
                                    Trace
                                     (  (  "Disconnecting from the "
                                        &  "cube at "
                                        &  Image (Address)
                                        ),
                                        Message_Text
                                    );
                                    Client.Shutdown;
                                 end if;
                              end if;
                              exit;
                           end if;
                        exception
                           when Status_Error =>
                              null;
                        end;
                     end if;
                  end loop;
               end;
            end Reconnect;
         or accept Wake_Up do
               Trace_To_File
               (  Message => "accept Wake_Up",
                  Prefix  => IO_Loop_Prefix
               );
            end Wake_Up;
            exit when Exiting;
         or accept Set_Poll (Poll : Duration) do
               Trace_To_File
               (  Message => "accept Set_Poll",
                  Prefix  => IO_Loop_Prefix
               );
               if Poll < 0.1 then
                  Poll_Time := 0.1;
               elsif Poll > Day_Duration'Last then
                  Poll_Time := Day_Duration'Last;
               else
                  Poll_Time := Poll;
               end if;
               Next_Poll := Last_Poll + Poll_Time;
            end Set_Poll;
         or when Current = null =>
            delay until Next_Poll;
            Trace_To_File
            (  Message => "polling",
               Prefix  => IO_Loop_Prefix
            );
            Last_Poll := Clock;
            Next_Poll := Last_Poll + Poll_Time;
            Do_Poll;
         or when Current /= null =>
            delay until Current.Started + Setting_Timeout;
            Trace_To_File
            (  Message => "handling operation timeout " &
                          Image (Current.all)           &
                          " started: "                  &
                          Image (Current.Started)       &
                          ", waited "                   &
                          Image (Setting_Timeout),
               Prefix  => IO_Loop_Prefix
            );
            Current.Report
            (  "Timed out when trying to " & Image (Current.all)
            );
            Current.Complete (True);
            Free (Current);
            Do_Queue;
         end select;
         if Current = null and then Clock - Last_Poll >= Poll_Time then
            Last_Poll := Clock;
            Next_Poll := Last_Poll + Poll_Time;
            Do_Poll;
         end if;
--           Dump_Process_Size ("Worker loop end");
      end loop;
      Trace_To_File
      (  Message => "exiting",
         Prefix  => IO_Loop_Prefix
      );
      Current_No := Settings_Sequence_No'Last;
      while Current /= null loop
         Current.Complete (True);
         Free (Current);
         Take (Queue, Current);
      end loop;
      for Index in 1..List.Get_Size loop
         declare
            This : constant Cube_Client_Handle := List.Get (Index);
         begin
            if This.Is_Valid then
               Trace_To_File
               (  Message => "shutting down cube " &
                             Get_ID (This.Ptr.all),
                  Prefix  => "Working task: "
               );
               This.Ptr.Shutdown;
            end if;
         end;
      end loop;
      List.Erase;
      Trace_To_File
      (  Message => "shutting down",
         Prefix  => "Working task: "
      );
      accept Shutdown;
   exception
      when Gtk.Main.Router.Quit_Error =>
         accept Shutdown;
      when Error : others =>
         if Exiting then
            accept Shutdown;
         else
            Log
            (  MAX_Domain,
               Log_Level_Critical,
               (  "Worker task failed: "
               &  Exception_Information (Error)
            )  );
         end if;
   end Worker;
end MAX_IO;
