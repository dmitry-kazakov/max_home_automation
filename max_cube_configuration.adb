--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Cube_Configuration                      Luebeck            --
--  Implementation                                 Autumn, 2016       --
--                                                                    --
--                                Last revision :  16:16 25 Apr 2024  --
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

with Ada.Calendar;                use Ada.Calendar;
with Ada.Exceptions;              use Ada.Exceptions;
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with GLib.Messages;               use GLib.Messages;
with GLib.Values;                 use GLib.Values;
with Gtk.Cell_Renderer_Combo;     use Gtk.Cell_Renderer_Combo;
with Gtk.Cell_Renderer_Pixbuf;    use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Progress;  use Gtk.Cell_Renderer_Progress;
with Gtk.Cell_Renderer_Text;      use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Frame;                   use Gtk.Frame;
with MAX_Cube_Topology;           use MAX_Cube_Topology;
with MAX_IO.Storing_Parameters;   use MAX_IO.Storing_Parameters;
with MAX_Trace;                   use MAX_Trace;
with Strings_Edit.Integers;       use Strings_Edit.Integers;

with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.
     Stream_IO;
use  GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.
     Stream_IO;

with GNAT.Sockets.Connection_State_Machine.Chain_Code.Naturals;
use  GNAT.Sockets.Connection_State_Machine.Chain_Code.Naturals;

with GLib.Properties;
with GLib.Types;
with Strings_Edit.Streams.Naturals;

package body MAX_Cube_Configuration is
   use Gtk.Tree_Model.Extension_Store;

   Magic_1                 : constant := 231261;
   Magic_2                 : constant := 876361;
   Max_Size                : constant := 100_000;
   Source_Selection_Column : constant := Time_Stamp_Column + 1;
   Source_Visible_Column   : constant := Source_Selection_Column + 1;
   Source_Name_Column      : constant := Source_Visible_Column + 1;
   Complete_Column         : constant := Source_Name_Column + 1;
   Ignore                  : constant String := "-";
   Pending                 : constant String := "system-run";
   Failure                 : constant String := "dialog-error";
   Wall_Thermostat_v2      : constant Stream_Element := 255;
   Wall_Thermostat_v3      : constant Stream_Element := 254;

   function Where (Name : String) return String is
   begin
      return " in MAX_Cube_Configuration." & Name;
   end Where;

--     procedure Check_Buttons
--               (  Restore : not null access
--                            Restore_Configuration_Record'Class
--               )  is
--     begin
--        if (  Restore.Selected > 0
--           and then
--              (  Restore.Restore_Schedule.Get_Active
--              or else
--                 Restore.Restore_Parameters.Get_Active
--              or else
--                 Restore.Restore_Valve.Get_Active
--           )  )
--        then
--           Restore.OK.Show;
--        else
--           Restore.OK.Hide;
--        end if;
--     end Check_Buttons;

   procedure Canceled
             (  Restore : in out Restore_Configuration_Record
             )  is
      Row : Gtk_Tree_Iter;
   begin
      Row := Restore.List.To_Extension
             (  Restore.Rooms.Find (Restore.To)
             );
      if Row /= Null_Iter then
         Restore.List.Set_Extension (Row, 4, Failure);
      end if;
      Restore.Repeat := True;
      Restore.Error.Show;
      Restore.OK.Show;
      Restore.Retry.Show;
      Restore.Action.Set_Text
      (  "Timed out while " & Image (Restore.Status)
      );
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Canceled")
         )  );
   end Canceled;

   function Clean_Errors
            (  Model : Gtk_Tree_Model;
               Path  : Gtk_Tree_Path;
               Row   : Gtk_Tree_Iter;
               Data  : Restore_Configuration
            )  return Boolean is
      List : Gtk_Extension_Store renames Data.List;
      This : Gtk_Tree_Iter;
   begin
      This := List.To_Extension (Row);
      if Get_String (List, This, Complete_Column) = Failure then
         Set_Extension (List, This, 4, Pending);
      end if;
      return False;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Clean_Errors")
         )  );
         return False;
   end Clean_Errors;

   procedure Do_Cancel
             (  Restore : in out Restore_Configuration_Record
             )  is
      No : GInt;
   begin
      Restore.Rooms.Set_Pending_Restore (False);
      No := Restore.Find_Page;
      if No >= 0 then
         Restore.Parent.Remove_Page (No);
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Do_Cancel")
         )  );
   end Do_Cancel;

   procedure Do_Restore
             (  Restore : in out Restore_Configuration_Record
             )  is
   begin
      Restore.Warning.Hide;
      Restore.Error.Hide;
      Walker.Foreach
      (  Restore.Rooms.Get_List,
         Find_Next'Access,
         Restore'Unchecked_Access
      );
      if Restore.Current = Null_Iter then -- Nothing to do
         Restore.Action.Set_Text ("Finished");
         Restore.OK.Show;
         Restore.Retry.Show;
         return;
      end if;
      declare
         Row : Gtk_Tree_Iter :=
               Restore.List.From_Extension (Restore.Current);
      begin
         Restore.From := RF_Address
                         (  Strings_Edit.Integers.Value
                            (  Get_String
                               (  Restore.List,
                                  Restore.Current,
                                  1
                               ),
                               Base => 16
                         )  );
         Restore.To := Restore.Rooms.Get
                       (  Restore.List.From_Extension (Restore.Current)
                       );
         Restore.Source := 0;
         for Index in 1..Restore.Parameters.Get_Size loop
             if (  Restore.Parameters.Get (Index).Parameters.Address
                =  Restore.From
                )  then
                Restore.Source := Index;
             end if;
         end loop;
         if (  Restore.Source > 0
            and then
               (  Restore.Restore_Schedule.Get_Active
               or else
                  Restore.Restore_Parameters.Get_Active
               or else
                  Restore.Restore_Valve.Get_Active
            )  )
         then
            Restore.OK.Hide;
            Restore.Retry.Hide;
            Restore.Status   := Store_Start; -- Start over
            Restore.Repeat   := True;        -- Do not skip anything
            Restore.Rejected := False;       -- No error so far
            Restore.Finished;
         else
            Restore.Action.Set_Text ("Finished");
            Restore.OK.Show;
            Restore.Retry.Show;
         end if;
      end;
   exception
      when Use_Error =>
         Restore.Action.Set_Text ("Operation in progress");
         Restore.OK.Show;
         Restore.Retry.Show;
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Do_Restore")
         )  );
   end Do_Restore;

   function Find_Next
            (  Model : Gtk_Tree_Model;
               Path  : Gtk_Tree_Path;
               Row   : Gtk_Tree_Iter;
               Data  : Restore_Configuration
            )  return Boolean is
      List : Gtk_Extension_Store renames Data.List;
   begin
      Data.Current := List.To_Extension (Row);
      if Get_String (List, Data.Current, Complete_Column) = Pending then
         return True;
      else
         Data.Current := Null_Iter;
         return False;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Find_Next")
         )  );
         Data.Current := Null_Iter;
         return False;
   end Find_Next;

   function Find_Page (Restore : Restore_Configuration_Record)
      return GInt is
      type Gtk_Widget_Ref is
         access constant Gtk_Widget_Record'Class;
      This : constant Gtk_Widget_Ref :=
                      Restore.Immediate_Parent.all'Unchecked_Access;
   begin
      if Restore.Parent /= null then
         for Index in 0..Restore.Parent.Get_N_Pages - 1 loop
            if (  This
               =  Restore.Parent.Get_Nth_Page
                  (  Index
                  ) .all'Unchecked_Access
               )
            then
               return Index;
            end if;
         end loop;
      end if;
      return -1;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Find_Page")
         )  );
         return -1;
   end Find_Page;

   procedure Finished (Restore : in out Restore_Configuration_Record) is
      Row : Gtk_Tree_Iter;
   begin
      Row := Restore.List.To_Extension
             (  Restore.Rooms.Find (Restore.To)
             );
      if Row = Null_Iter then
         Restore.Error.Show;
         Restore.OK.Show;
         Restore.Retry.Show;
         Restore.Action.Set_Text
         (  "Failed to restore some of the configurations "
         &  Image (Clock)
         );
         return;
      end if;
      if Restore.Rejected then
         Restore.List.Set_Extension (Row, 4, Failure);
         return;
      end if;
      declare
         Cube : Cube_Client'Class renames
                Get_Cube (Restore.Cube).Ptr.all;
         Data : Device_Parameters renames
                Restore.Parameters.Get (Restore.Source).Parameters;
      begin
         loop
            if Restore.Repeat then
               Restore.Repeat := False;
            else
               Restore.Status :=
                  Next
                  (  Action     => Restore.Status,
                     Schedule   => Restore.Restore_Schedule.Get_Active,
                     Valve      => Restore.Restore_Valve.Get_Active,
                     Parameters => Restore.Restore_Parameters.Get_Active
                  );
            end if;
            case Restore.Status is
               when Store_Start =>
                  null;
               when Store_Schedule =>
                  Restore.Action.Set_Text
                  (  "Restoring schedule of "
                  &  Image (Restore.From)
                  &  " into "
                  &  Image (Restore.To)
                  );
                  exit;
               when Store_Parameters =>
                  Restore.Action.Set_Text
                  (  "Restoring parameters of "
                  &  Image (Restore.From)
                  &  " into "
                  &  Image (Restore.To)
                  );
                  exit;
               when Store_Valve =>
                  if Data.Kind_Of in Radiator_Thermostat
                                  .. Radiator_Thermostat_Plus then
                     Restore.Action.Set_Text
                     (  "Restoring valve settings of "
                     &  Image (Restore.From)
                     &  " into "
                     &  Image (Restore.To)
                     );
                     exit;
                  end if;
               when Store_Stop =>
                  Restore.List.Set_Extension (Row, 1, Ignore);
                  Restore.List.Set_Extension (Row, 3, "");
                  Restore.List.Set_Extension (Row, 4, Success);
                  Restore.Do_Restore;
                  return;
            end case;
         end loop;
      end;
      Restore.Error.Hide;
      Restore.OK.Hide;
      Restore.Retry.Hide;
      Store_Configuration
      (  Box        => Restore.Cube,
         Device     => Restore.To,
         Action     => Restore.Status,
         Handler    => Restore'Unchecked_Access,
         Parameters => Restore.Parameters.Get
                       (  Restore.Source
                       ). Parameters
      );
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finished")
         )  );
   end Finished;

   function Get_Object
            (  Restore : not null access Restore_Configuration_Record
            )  return GObject is
   begin
      return Restore.all'Unchecked_Access;
   end Get_Object;

   procedure Link_Cell_Data
             (  Column  : not null access
                             Gtk_Tree_View_Column_Record'Class;
                Cell    : not null access
                             Gtk_Cell_Renderer_Record'Class;
                Model   : Gtk_Tree_Model;
                Row     : Gtk_Tree_Iter;
                Restore : Restore_Configuration
             )  is
   begin
      Cell.Set_Visible
      (  Cube
      =  Restore.Rooms.Get (Restore.List.From_Extension (Row))
      );
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Link_Cell_Data")
         )  );
   end Link_Cell_Data;

   procedure On_A_Response
             (  Restore  : in out Restore_Configuration_Record;
                Address  : RF_Address;
                Devices  : Devices_Maps.Map;
                List     : Rooms_Maps.Map;
                Expected : in out Natural
             )  is
   begin
      if Expected > 0 then
         Expected := Expected - 1;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_A_Response")
         )  );
   end On_A_Response;

   procedure On_Cancel
             (  Button  : access GObject_Record'Class;
                Params  : GValues;
                Restore : Restore_Configuration
             )  is
   begin
      Restore.Do_Cancel;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Cancel")
         )  );
   end On_Cancel;

   procedure On_Changed
             (  Combo   : access GObject_Record'Class;
                Params  : GValues;
                Restore : Restore_Configuration
             )  is
      Source_Row : Gtk_Tree_Iter;
      Tree_Row   : Gtk_Tree_Iter;
      Selected   : Boolean;
   begin
      Get_Tree_Iter (Nth (Params, 2), Source_Row);
      if Source_Row = Null_Iter then
         return;
      end if;
      Tree_Row :=
         Get_Iter_From_String
         (  To_Interface (Restore.List),
            Get_String (Nth (Params, 1))
         );
      if Tree_Row = Null_Iter then
         return;
      end if;
      Selected :=
         (  Get_String (Restore.List, Tree_Row, Source_Selection_Column)
         /= Ignore
         );
      declare
         Address : constant String :=
                   Restore.Sources.Get_String (Source_Row, 0);
      begin
         Set_Extension (Restore.List, Tree_Row, 1, Address);
         Set_Extension
         (  Restore.List,
            Tree_Row,
            3,
            Restore.Sources.Get_String (Source_Row, 1)
         );
         if Address = Ignore then
            if Selected then
               Restore.Selected := Restore.Selected - 1;
               Set_Extension
               (  Restore.List,
                  Tree_Row,
                  4,
                  ""
               );
--                 Check_Buttons (Restore);
            end if;
         else
            if not Selected then
               Restore.Selected := Restore.Selected + 1;
               Set_Extension
               (  Restore.List,
                  Tree_Row,
                  4,
                  Pending
               );
--                 Check_Buttons (Restore);
            end if;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Changed")
         )  );
   end On_Changed;

   procedure On_Restore
             (  Button  : access GObject_Record'Class;
                Params  : GValues;
                Restore : Restore_Configuration
             )  is
   begin
      Restore.Do_Restore;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Restore")
         )  );
   end On_Restore;

   procedure On_Retry
             (  Button  : access GObject_Record'Class;
                Params  : GValues;
                Restore : Restore_Configuration
             )  is
   begin
      Walker.Foreach
      (  Restore.Rooms.Get_List,
         Clean_Errors'Access,
         Restore
      );
      Restore.Do_Restore;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Retry")
         )  );
   end On_Retry;

   procedure On_Toggled
             (  Combo   : access GObject_Record'Class;
                Params  : GValues;
                Restore : Restore_Configuration
             )  is
   begin
      null;
--        Check_Buttons (Restore);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Toggled")
         )  );
   end On_Toggled;

   procedure On_S_Response
             (  Restore  : in out Restore_Configuration_Record;
                Cube     : access Cube_Client'Class;
                Error    : Boolean;
                Duty     : Ratio;
                Slots    : Natural;
                Expected : in out Natural
             )  is
   begin
      Restore.Rejected := Restore.Rejected or else Error;
      begin
         Restore.Rooms.Get_Logger.Update_Status
         (  Cube.Get_RF_Address,
            Error,
            Duty,
            Slots
         );
      exception
         when Status_Error =>
            null;
      end;
      if Duty >= Max_Duty then
         Restore.Warning.Show;
      else
         Restore.Warning.Hide;
      end if;
      if Error then
         Restore.Error.Show;
         Restore.OK.Show;
      end if;
      if Expected > 0 then
         Expected := Expected - 1;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_S_Response")
         )  );
   end On_S_Response;

   function Read (Stream : not null access Root_Stream_Type'Class)
      return Natural is
      Buffer  : Stream_Element_Array (1..100);
      Pointer : Stream_Element_Offset := 1;
      Result  : Natural;
   begin
      loop
         Stream_Element'Read (Stream, Buffer (Pointer));
         exit when Buffer (Pointer) < 128;
         if Pointer = Buffer'Last then
            raise Data_Error with "Invalid natural value encoding";
         end if;
         Pointer := Pointer + 1;
      end loop;
      Pointer := 1;
      Get (Buffer, Pointer, Result);
      return Result;
   end Read;

   procedure Read
             (  Stream : in out Root_Stream_Type'Class;
                Value  : out Duration
             )  is
   begin
      Value := Duration (Natural'(Read (Stream'Access))) * 60.0;
   end Read;

   procedure Read
             (  Stream      : not null access Root_Stream_Type'Class;
                Thermostats : out Device_Parameters_Data_Sets.Set;
                Topology    : in out Topology_Handles.Handle
             )  is
      use Device_Parameters_Data_Handles;
      Count        : Natural;
      Has_Metadata : Boolean;
      Name_Length  : Natural;
      Room_Length  : Natural;
      Reference    : Handle;
      Kind_Of      : Device_Type;
      Byte         : Stream_Element;
   begin
      Thermostats.Erase;
      Count := Read (Stream);
      declare
         Check : constant Natural := Read (Stream);
      begin
         if Count + Magic_1 = Check then
            Has_Metadata := False;
         elsif Count + Magic_2 = Check then
            Has_Metadata := True;
         else
            raise Data_Error with "Invalid magic cookie";
         end if;
      end;
      if Count > Max_Size then
         raise Data_Error with "Invalid devices count";
      end if;
      for Index in 1..Count loop
         -- Kind_Of     := Read (Stream);
         Byte := Stream_Element'Input (Stream);
         if Byte <= Device_Type'Pos (Device_Type'Last) then
            Kind_Of := Device_Type'Val (Byte);
         elsif Byte in Wall_Thermostat_v3..Wall_Thermostat_v2 then
                -- Extended format
            Kind_Of := Wall_Thermostat;
         else
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid device type"
            );
         end if;
         Name_Length := Character'Pos (Character'Input (Stream));
         Room_Length := Character'Pos (Character'Input (Stream));
         case Kind_Of is
            when Cube | Shutter_Contact | Eco_Button | Unknown =>
               null;
            when Wall_Thermostat =>
               Reference.Set
               (  new Device_Parameters_Data
                      (  Kind_Of     => Wall_Thermostat,
                         Name_Length => Name_Length,
                         Room_Length => Room_Length
               )      );
            when Radiator_Thermostat =>
               Reference.Set
               (  new Device_Parameters_Data
                      (  Kind_Of     => Radiator_Thermostat,
                         Name_Length => Name_Length,
                         Room_Length => Room_Length
               )      );
            when Radiator_Thermostat_Plus =>
               Reference.Set
               (  new Device_Parameters_Data
                      (  Kind_Of     => Radiator_Thermostat_Plus,
                         Name_Length => Name_Length,
                         Room_Length => Room_Length
               )      );
         end case;
         if Kind_Of in Radiator_Thermostat..Wall_Thermostat then
            declare
               Item : Device_Parameters_Data'Class renames
                      Reference.Ptr.all;
               This : Device_Parameters renames Item.Parameters;
            begin
               This.Address := 0;
               This.Room    := Read (Stream);
               This.Address := Read (Stream);
               String'Read (Stream, This.Serial_No);
               String'Read (Stream, This.Name);
               This.Comfort         := Read (Stream);
               This.Eco             := Read (Stream);
               This.Max             := Read (Stream);
               This.Min             := Read (Stream);
               This.Schedule        := Read (Stream);
               if Kind_Of in Radiator_Thermostat
                          .. Radiator_Thermostat_Plus then
                  This.Offset          := Read (Stream);
                  This.Window_Open     := Read (Stream);
                  Read (Stream.all, This.Window_Time);
                  Read (Stream.all, This.Boost_Time);
                  This.Boost_Valve     := Read (Stream);
                  This.Decalcification := Read (Stream);
                  This.Max_Valve       := Read (Stream);
                  This.Valve_Offset    := Read (Stream);
               elsif Byte = Wall_Thermostat_v3 then
                  This.Offset          := Read (Stream);
                  This.Window_Open     := Read (Stream);
               elsif Byte = Wall_Thermostat_v2 then
                  This.Offset          := Read (Stream);
               else                  -- Legacy wall thermostat format
                  This.Offset          := 0.0; -- Default
               end if;
               String'Read (Stream, Item.Room);
            exception
               when Error : others =>
                  raise Data_Error with
                        Exception_Message (Error) &
                        " while loading thermostat " &
                        Image (This.Address);
            end;
         end if;
         Thermostats.Add (Reference);
      end loop;
      if Has_Metadata then
         declare
            use Strings_Edit.Streams.Naturals;
            Length : constant Natural := Input (Stream);
         begin
            if not Topology.Is_Valid then
               Topology.Set (new Topology_Object (100_000));
            end if;
            if Length > Topology.Ptr.Size then
               raise Data_Error with
                     "Metadata size " &
                     Image (Length) &
                     " exceeds " &
                     Image (Topology.Ptr.Size) &
                     " bytes";
            end if;
            String'Read (Stream, Topology.Ptr.Metadata (1..Length));
            Topology.Ptr.Length := Length;
         end;
      else
         Topology.Invalidate;
      end if;
   end Read;

   procedure Restore
             (  Pages      : not null access Gtk_Notebook_Record'Class;
                Rooms      : not null access Rooms_List_Record'Class;
                Cube       : RF_Address;
                Parameters : Device_Parameters_Data_Sets.Set;
                Topology   : Topology_Handles.Handle
             )  is
      Configuration : constant Gtk_Notebook := Gtk_Notebook_New;
      Restore       : Restore_Configuration;
      Metadata      : Restore_Topology;
      Box           : Gtk_Box;
      Label         : Gtk_Label;
   begin
      Configuration.Set_Tab_Pos (Pos_Left);
      ------------------------------------------------------------------
         Gtk_New (Box, Orientation_Horizontal, 3);
         Label := Gtk_Style_Label_New (Topology_Label).
                  all'Unchecked_Access;
         Label.Set_Angle (90.0);
         Box.Pack_Start (Label);
         Box.Show_All;
         if Topology.Is_Valid then
            Metadata := Restore_Topology_New
                        (  Pages,
                           Configuration,
                           Rooms,
                           Cube,
                           Topology
                        );
            Configuration.Append_Page (Metadata, Box);
         else
            Label := Gtk_Style_Label_New (No_Topology_Label).
                     all'Unchecked_Access;
            Configuration.Append_Page (Label, Box);
            Label.Show_All;
         end if;
         ---------------------------------------------------------------
         Gtk_New (Box, Orientation_Horizontal, 3);
         Label := Gtk_Style_Label_New (Configuration_Label).
                  all'Unchecked_Access;
         Label.Set_Angle (90.0);
         Box.Pack_Start (Label);
         Box.Show_All;
         Restore := Restore_Configuration_New
                    (  Pages,
                       Configuration,
                       Rooms,
                       Cube,
                       Parameters
                    );
         Configuration.Append_Page (Restore, Box);
      ------------------------------------------------------------------
      Gtk_New (Box, Orientation_Horizontal, 3);
      Label := Gtk_Style_Label_New (Restore_Label, Image (Cube)).
               all'Unchecked_Access;
      Box.Pack_Start (Label);
      Box.Show_All;
      Configuration.Show_All;
      Restore.Warning.Hide;
      Restore.Error.Hide;
      Restore.Retry.Hide;
      if Metadata /= null then
         Metadata.Warning.Hide;
         Metadata.Error.Hide;
      end if;
      Pages.Set_Current_Page
      (  Pages.Append_Page (Configuration, Box)
      );
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Restore")
         )  );
   end Restore;

   function Restore_Configuration_New
            (  Pages      : not null access Gtk_Notebook_Record'Class;
               Immediate  : not null access Gtk_Notebook_Record'Class;
               Rooms      : not null access Rooms_List_Record'Class;
               Cube       : RF_Address;
               Parameters : Device_Parameters_Data_Sets.Set
            )  return Restore_Configuration is
      Result : constant Restore_Configuration :=
                        new Restore_Configuration_Record;
   begin
      Gtk.Box.Initialize (Result, Orientation_Vertical, 3);
      Result.Parent           := Pages.all'Unchecked_Access;
      Result.Immediate_Parent := Immediate.all'Unchecked_Access;
      Result.Rooms            := Rooms.all'Unchecked_Access;
      Result.Parameters       := Parameters;
      Result.Cube             := Cube;

      declare
         Buttons : Gtk_HBox;
      begin
         Gtk_New_Hbox (Buttons, Spacing => 3);
         Result.Pack_Start (Buttons, False, False);
         Cancel_Restore_Buttons.Gtk_New (Result.Cancel);
         Buttons.Pack_Start (Result.Cancel, False, False);
         Retry_Restore_Buttons.Gtk_New (Result.Retry);
         Buttons.Pack_Start (Result.Retry, False, False);
         Start_Restore_Buttons.Gtk_New (Result.OK);
         Buttons.Pack_Start (Result.OK, False, False);
      end;
      Gtk_New
      (  Result.Sources,
         (  GType_String, -- From (source address)
            GType_String  -- From name
      )  );
      declare
         Row : Gtk_Tree_Iter;
      begin
         Result.Sources.Append (Row);
         Gtk.Missed.Set (Result.Sources, Row, 0, Ignore);
         for Index in 1..Parameters.Get_Size loop
            declare
               Item : Device_Parameters_Data'Class renames
                      Parameters.Get (Index).all;
            begin
               Result.Sources.Append (Row);
               Gtk.Missed.Set
               (  Result.Sources,
                  Row,
                  0,
                  Image (Item.Parameters.Address)
               );
               Gtk.Missed.Set
               (  Result.Sources,
                  Row,
                  1,
                  Item.Parameters.Name
               );
            end;
         end loop;
      end;
      Gtk_New
      (  Result.List,
         Rooms.Get_List,
         (  GType_String,  -- From (source address)
            GType_Boolean, -- Visible
            GType_String,  -- From name
            GType_String   -- Complete
      )  );
      Walker.Foreach (Rooms.Get_List, Visit'Access, Result);
      Gtk_New (Result.Scroll);
      Result.Scroll.Set_Policy (Policy_Never, Policy_Automatic);
      Result.Pack_Start (Result.Scroll);
      Result.View :=
         Gtk_Tree_View_New_With_Model (To_Interface (Result.List));
      declare
         Column    : Gtk_Tree_View_Column;
         Combo     : Gtk_Cell_Renderer_Combo;
         Text      : Gtk_Cell_Renderer_Text;
         Icon      : Gtk_Cell_Renderer_Pixbuf;
         Progress  : Gtk_Cell_Renderer_Progress;
         Column_No : Gint;
      begin
         Gtk_New (Column);
         Column.Set_Title ("Name");
         Gtk_New (Icon);
         Column.Pack_Start (Icon, False);
         Column.Add_Attribute (Icon, "pixbuf", Type_Column);
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Column.Add_Attribute (Text, "text", Name_Column);
         Gtk_New (Progress);
         Column.Pack_Start (Progress, False);
         Column.Add_Attribute (Progress, "value", Duty_Column);
         Column_No := Result.View.Append_Column (Column);
         Column.Set_Resizable (True);
         Set_Mode_Cell_Data.Set_Cell_Data_Func
         (  Column,
            Progress,
            Link_Cell_Data'Access,
            Result
         );

         Gtk_New (Column);
         Column.Set_Title ("Address");
         Gtk_New (Text);
         Column.Pack_Start (Text, False);
         Column.Add_Attribute (Text, "text", Address_Column);
         Column_No := Result.View.Append_Column (Column);
         Column.Set_Resizable (False);

         Gtk_New (Column);
         Column.Set_Title ("Serial No");
         Gtk_New (Text);
         Column.Pack_Start (Text, False);
         Column.Add_Attribute (Text, "text", Serial_No_Column);
         Column_No := Result.View.Append_Column (Column);
         Column.Set_Resizable (False);

         Gtk_New (Column);
         Column.Set_Title ("Get from");
         Gtk_New (Combo);
         Glib.Properties.Set_Property
         (  Combo,
            Gtk.Cell_Renderer_Combo.Model_Property,
            Types.GType_Interface (Gtk_Tree_Model'(+Result.Sources))
         );
         Glib.Properties.Set_Property
         (  Combo,
            Gtk.Cell_Renderer_Combo.Text_Column_Property,
            0
         );
         Glib.Properties.Set_Property
         (  Combo,
            Gtk.Cell_Renderer_Combo.Has_Entry_Property,
            False
         );
         Glib.Properties.Set_Property
         (  Combo,
            Editable_Property,
            True
         );
         Column.Pack_Start (Combo, False);
         Column.Add_Attribute
         (  Combo,
            "text",
            Source_Selection_Column
         );
         Column.Add_Attribute
         (  Combo,
            "visible",
            Source_Visible_Column
         );
         Column_No := Result.View.Append_Column (Column);
         Column.Set_Resizable (False);
         Restore_Handlers.Connect
         (  Combo,
            "changed",
            On_Changed'Access,
            Result
         );
         Result.Sources.Unref;

         Gtk_New (Column);
         Column.Set_Title ("Saved thermostat's name");
         Gtk_New (Text);
         Column.Pack_Start (Text, False);
         Column.Add_Attribute (Text, "text", Source_Name_Column);
         Column_No := Result.View.Append_Column (Column);
         Column.Set_Resizable (True);

         Gtk_New (Column);
         Gtk_New (Icon);
         Column.Pack_Start (Icon, False);
         Column.Add_Attribute (Icon, "icon-name", Complete_Column);
         Column_No := Result.View.Append_Column (Column);
         Column.Set_Resizable (False);
      end;
      Result.Scroll.Add (Result.View);
      Result.View.Expand_All;
      Result.View.Columns_Autosize;  -- Size columns

      declare
         Frame : Gtk_Frame;
         Box   : Gtk_VBox;
         Bar   : Gtk_HBox;
      begin
         Gtk_New (Frame, " Parameters to restore ");
         Frame.Set_Shadow_Type (Shadow_Out);
         Frame.Set_Border_Width (3);
         Result.Pack_Start (Frame, False, False);

         Bar := Gtk_HBox_New;
         Frame.Add (Bar);

         Box := Gtk_VBox_New;
         Bar.Pack_Start (Box);

         Gtk_New_From_Icon_Name
         (  Result.Warning,
            "dialog-warning",
            Icon_Size_Dialog
         );
         Result.Warning.Set_Tooltip_Text
         (  "The radio traffic is close to its limit. "
         &  "This may lead to a delay or failure for the operation "
         &  "to complete because the cube will wait for the radio band "
         &  "to become available again."
         );
         Bar.Pack_Start (Result.Warning, False, False);

         Gtk_New_From_Icon_Name
         (  Result.Error,
            Failure,
            Icon_Size_Dialog
         );
         Result.Error.Set_Tooltip_Text
         (  "The last upload operation failed. "
         &  "It is either because the radio band limit has been "
         &  "reached or the device is in an erroneus state. "
         &  "In the former case you may try to upload remaining "
         &  "configurations into thermostats later "
         );
         Bar.Pack_Start (Result.Error, False, False);

         Gtk_New (Result.Restore_Schedule, "Schedule");
         Result.Restore_Schedule.Set_Tooltip_Text
         (  "This option restores the schedules of the thermostats "
         &  "from the backup. "
         &  "Note that this operation is very radio traffic consuming."
         );
         Result.Restore_Schedule.Set_Active (True);
         Box.Pack_Start (Result.Restore_Schedule, False, False);

         Gtk_New (Result.Restore_Parameters, "Thermostat parameters");
         Result.Restore_Parameters.Set_Tooltip_Text
         (  "This option restores the parameters of the thermostats "
         &  "from the backup. "
         &  "It does not include the schedule and valve settings."
         );
         Result.Restore_Parameters.Set_Active (True);
         Box.Pack_Start (Result.Restore_Parameters, False, False);

         Gtk_New (Result.Restore_Valve, "Valve parameters");
         Result.Restore_Valve.Set_Tooltip_Text
         (  "This option restores the thermostats valve settings "
         &  "from the backup. "
         &  "It does not include the schedule and parameters."
         );
         Result.Restore_Valve.Set_Active (True);
         Box.Pack_Start (Result.Restore_Valve, False, False);

         Restore_Handlers.Connect
         (  Result.Restore_Schedule,
            "toggled",
            On_Toggled'Access,
            Result
         );
         Restore_Handlers.Connect
         (  Result.Restore_Parameters,
            "toggled",
            On_Toggled'Access,
            Result
         );
         Restore_Handlers.Connect
         (  Result.Restore_Valve,
            "toggled",
            On_Toggled'Access,
            Result
         );
      end;
      Gtk_New (Result.Action);
      Result.Pack_Start (Result.Action, False, False);
--        Check_Buttons (Result);
      declare
         Dummy  : GInt;
         Height : GInt;
         Width  : GInt;
      begin
         Result.View.Get_Preferred_Height (Dummy, Height);-- tree Result.View size
         Result.View.Get_Preferred_Width  (Dummy, Width);
         Result.Scroll.Set_Size_Request                   -- Set new size
         (  GInt'Min (Width,  500),
            GInt'Min (Height, 300)
         );
      end;
      Restore_Handlers.Connect
      (  Result.Cancel,
         "clicked",
         On_Cancel'Access,
         Result
      );
      Restore_Handlers.Connect
      (  Result.OK,
         "clicked",
         On_Restore'Access,
         Result
      );
      Restore_Handlers.Connect
      (  Result.Retry,
         "clicked",
         On_Retry'Access,
         Result
      );
      Rooms.Set_Pending_Restore (True);
      return Result;
   end Restore_Configuration_New;

   function Visit
            (  Model : Gtk_Tree_Model;
               Path  : Gtk_Tree_Path;
               Row   : Gtk_Tree_Iter;
               Data  : Restore_Configuration
            )  return Boolean is
      List       : Gtk_Extension_Store renames Data.List;
      Rooms      : Rooms_List renames Data.Rooms;
      Parameters : Device_Parameters_Data_Sets.Set renames
                   Data.Parameters;
   begin
      declare
         Iter    : constant Gtk_Tree_Iter := List.To_Extension (Row);
         Kind_Of : constant Device_Type   := Rooms.Get (Row);
      begin
         Set_Extension (List, Iter, 1, Ignore);
         if Kind_Of in Radiator_Thermostat..Radiator_Thermostat_Plus
         then
            declare
               Address : constant RF_Address := Rooms.Get (Row);
               Name    : constant String :=
                                  Rooms.Get (Row, Name_Column);
            begin
               for Saved in 1..Parameters.Get_Size loop
                  declare
                     Item : Device_Parameters_Data'Class renames
                            Parameters.Get (Saved).all;
                  begin
                     if (  Address = Item.Parameters.Address
                        or Name = Item.Parameters.Name
                        )
                     then
                        Set_Extension
                        (  List,
                           Iter,
                           1,
                           Image (Item.Parameters.Address)
                        );
                        Set_Extension
                        (  List,
                           Iter,
                           3,
                           Item.Parameters.Name
                        );
                        Set_Extension
                        (  List,
                           Iter,
                           4,
                           Pending
                        );
                        Data.Selected := Data.Selected + 1;
                        exit;
                     end if;
                  end;
               end loop;
            end;
            Set_Extension (List, Iter, 2, True);
         else
            Set_Extension (List, Iter, 2, False);
         end if;
      end;
      return False;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Visit")
         )  );
         return False;
   end Visit;

   procedure Write
             (  Stream : in out Root_Stream_Type'Class;
                Value  : Natural
             )  is
      Buffer  : Stream_Element_Array (1..80);
      Pointer : Stream_Element_Offset := 1;
   begin
      Put (Buffer, Pointer, Value);
      Write (Stream, Buffer (1..Pointer - 1));
   end Write;

   procedure Write
             (  Stream : in out Root_Stream_Type'Class;
                Value  : Duration
             )  is
   begin
      Write (Stream, Natural (Value / 60.0));
   end Write;

   procedure Write
             (  Stream      : in out Root_Stream_Type'Class;
                Thermostats : Device_Parameters_Data_Sets.Set;
                Topology    : Topology_Handles.Handle
             )  is
      use Strings_Edit.Streams.Naturals;
      Count : Natural := 0;
   begin
      for Index in 1..Thermostats.Get_Size loop
         if Thermostats.Get (Index).Kind_Of in
            Radiator_Thermostat.. Wall_Thermostat then
            Count := Count + 1;
         end if;
      end loop;
      Write (Stream, Count);
      if Topology.Is_Valid and then Topology.Ptr.Length > 0 then
         Write (Stream, Count + Magic_2);
      else
         Write (Stream, Count + Magic_1);
      end if;
      for Index in 1..Thermostats.Get_Size loop
         declare
            Item : Device_Parameters_Data'Class renames
                   Thermostats.Get (Index).all;
            This : Device_Parameters renames Item.Parameters;
         begin
            case This.Kind_Of is
               when Cube | Shutter_Contact | Eco_Button | Unknown =>
                  null;
               when Wall_Thermostat =>
--                Write (Stream'Access, This.Kind_Of);
                  Stream_Element'Write
                  (  Stream'Access,
                     Wall_Thermostat_v3
                  );
                  String'Write
                  (  Stream'Access,
                     (  Character'Val (This.Name_Length)
                     &  Character'Val (Item.Room_Length)
                  )  );
                  Write (Stream'Access, This.Room);
                  Write (Stream'Access, This.Address);
                  String'Write (Stream'Access, This.Serial_No);
                  String'Write (Stream'Access, This.Name);
                  Write (Stream'Access, This.Comfort);
                  Write (Stream'Access, This.Eco);
                  Write (Stream'Access, This.Max);
                  Write (Stream'Access, This.Min);
                  Write (Stream'Access, This.Schedule);
                  Write (Stream'Access, This.Offset);      -- v2
                  Write (Stream'Access, This.Window_Open); -- v3
                  String'Write (Stream'Access, Item.Room);
               when Radiator_Thermostat..Radiator_Thermostat_Plus =>
                  Write (Stream'Access, This.Kind_Of);
                  String'Write
                  (  Stream'Access,
                     (  Character'Val (This.Name_Length)
                     &  Character'Val (Item.Room_Length)
                  )  );
                  Write (Stream'Access, This.Room);
                  Write (Stream'Access, This.Address);
                  String'Write (Stream'Access, This.Serial_No);
                  String'Write (Stream'Access, This.Name);
                  Write (Stream'Access, This.Comfort);
                  Write (Stream'Access, This.Eco);
                  Write (Stream'Access, This.Max);
                  Write (Stream'Access, This.Min);
                  Write (Stream'Access, This.Schedule);
                  Write (Stream'Access, This.Offset);
                  Write (Stream'Access, This.Window_Open);
                  Write (Stream, This.Window_Time);
                  Write (Stream, This.Boost_Time);
                  Write (Stream'Access, This.Boost_Valve);
                  Write (Stream'Access, This.Decalcification);
                  Write (Stream'Access, This.Max_Valve);
                  Write (Stream'Access, This.Valve_Offset);
                  String'Write (Stream'Access, Item.Room);
            end case;
         exception
            when Error : others =>
               raise Data_Error with
                     Exception_Message (Error) &
                     " while storing thermostat " &
                     Image (This.Address);
         end;
      end loop;
      if Topology.Is_Valid and then Topology.Ptr.Length > 0 then
         Output (Stream'Access, Topology.Ptr.Length);
         String'Write
         (  Stream'Access,
            Topology.Ptr.Metadata (1..Topology.Ptr.Length)
         );
      end if;
   end Write;

end MAX_Cube_Configuration;
