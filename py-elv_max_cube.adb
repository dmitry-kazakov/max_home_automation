--                                                                    --
--  package Py.ELV_MAX_Cube         Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2018       --
--                                                                    --
--                                Last revision :  19:50 04 Jul 2023  --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.IO_Exceptions;         use Ada.IO_Exceptions;
with GNAT.Sockets.MQTT.Server;  use GNAT.Sockets.MQTT.Server;
with MAX_IO;                    use MAX_IO;
with MAX_IO.Set_Mode;           use MAX_IO.Set_Mode;
with MAX_Trace;                 use MAX_Trace;
with Strings_Edit.Quoted;       use Strings_Edit.Quoted;

with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
use  GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;

with Tables.UTF8_Names;

package body Py.ELV_MAX_Cube is

   No_Device_Exists : constant String := "No device exists";
   Not_Thermostat   : constant String := "Not a thermostat";
   Not_Radiator     : constant String := "Not a radiator thermostat";

   procedure Check_Spelling (Name : String) is
   begin
      null;
   end Check_Spelling;

   function Check_Matched
            (  Source  : String;
               Pointer : Integer
            )  return Boolean is
   begin
      case Source (Pointer) is
         when '0'..'9' | 'A'..'Z' | 'a'..'z' =>
            return False;
         when others =>
            return True;
      end case;
   end Check_Matched;

   package Device_Type_Tables_Raw is new Tables (Device_Type);
   package Device_Type_Tables is new Device_Type_Tables_Raw.UTF8_Names;

   package Device_Mode_Tables_Raw is new Tables (Operating_Mode);
   package Device_Mode_Tables is new Device_Mode_Tables_Raw.UTF8_Names;

   package Message_Tables_Raw is new Tables (Message_Type);
   package Message_Tables is new Message_Tables_Raw.UTF8_Names;

   package Temperature_Mode_Tables_Raw is new Tables (Temperature_Mode);
   package Temperature_Mode_Tables is
      new Temperature_Mode_Tables_Raw.UTF8_Names;

   Device_Types      : Device_Type_Tables.Dictionary;
   Device_Modes      : Device_Mode_Tables.Dictionary;
   Message_Types     : Message_Tables.Dictionary;
   Temperature_Modes : Temperature_Mode_Tables.Dictionary;

   function Disconnect (Self : Object; Args : Object) return Object;
   pragma Convention (C, Disconnect);

   function Get_Battery (Self : Object; Args : Object) return Object;
   pragma Convention (C, Get_Battery);

   function Get_Connection (Self : Object; Args : Object) return Object;
   pragma Convention (C, Get_Connection);

   function Get_Cubes_List (Self : Object; Args : Object) return Object;
   pragma Convention (C, Get_Cubes_List);

   function Get_Devices_List (Self : Object; Args : Object)
      return Object;
   pragma Convention (C, Get_Devices_List);

   function Get_Duty (Self : Object; Args : Object) return Object;
   pragma Convention (C, Get_Duty);

   function Get_Link (Self : Object; Args : Object) return Object;
   pragma Convention (C, Get_Link);

   function Get_Mode (Self : Object; Args : Object) return Object;
   pragma Convention (C, Get_Mode);

   function Get_MQTT (Self : Object; Args : Object) return Object;
   pragma Convention (C, Get_MQTT);

   function Get_Parameters (Self : Object; Args : Object) return Object;
   pragma Convention (C, Get_Parameters);

   function Get_Set_Temperature (Self : Object; Args : Object)
      return Object;
   pragma Convention (C, Get_Set_Temperature);

   function Get_Status (Self : Object; Args : Object) return Object;
   pragma Convention (C, Get_Status);

   function Get_Summer_Time (Self : Object; Args : Object)
      return Object;
   pragma Convention (C, Get_Summer_Time);

   function Get_Temperature (Self : Object; Args : Object)
      return Object;
   pragma Convention (C, Get_Temperature);

   function Get_Valve (Self : Object; Args : Object)
      return Object;
   pragma Convention (C, Get_Valve);

   function Get_Valve_Statistics (Self : Object; Args : Object)
      return Object;
   pragma Convention (C, Get_Valve_Statistics);

   function Publish_MQTT (Self : Object; Args : Object) return Object;
   pragma Convention (C, Publish_MQTT);

   function Reboot (Self : Object; Args : Object) return Object;
   pragma Convention (C, Reboot);

   function Reconnect (Self : Object; Args : Object) return Object;
   pragma Convention (C, Reconnect);

   function Set_Mode
            (  Self     : Object;
               Args     : Object;
               Keywords : Object
            )  return Object;
   pragma Convention (C, Set_Mode);

   function Trace (Self : Object; Args : Object) return Object;
   pragma Convention (C, Trace);

   function No_Value return Object is
   begin
      Links.IncRef (Links.None);
      return Links.None;
   end No_Value;

   function "+" (Value : Boolean) return Handle is
      Result : Handle;
   begin
      if Value then
         Result.Ptr := Links.True;
      else
         Result.Ptr := Links.False;
      end if;
      Links.IncRef (Result.Ptr);
      return Result;
   end "+";

   function "+" (Value : Centigrade) return Handle is
   begin
      return (  Ada.Finalization.Controlled
            with
                Links.Float_FromDouble (double (Value))
             );
   end "+";

   function "+" (Kind_Of : Device_Type) return Handle is
   begin
      return Unicode_FromString (Image (Kind_Of));
   end "+";

   function "+" (Value : Natural) return Handle is
   begin
      return Long_FromLong (long (Value));
   end "+";

   function "+" (Mode : Operating_Mode) return Handle is
   begin
      case Mode is
         when Automatic =>
            return Unicode_FromString ("automatic");
         when Manual =>
            return Unicode_FromString ("manual");
         when Vacation =>
            return Unicode_FromString ("vacation");
         when Boost =>
            return Unicode_FromString ("boost");
      end case;
   end "+";

   function "+" (Value : Ratio) return Handle is
   begin
      return (  Ada.Finalization.Controlled
             with
                Links.Float_FromDouble (double (Value))
             );
   end "+";

   function "+" (Address : RF_Address) return Handle is
   begin
      return Long_FromLong (long (Address));
   end "+";

   function "+" (ID : Room_ID) return Handle is
   begin
      return Long_FromLong (long (ID));
   end "+";

   function "+" (Text : String) return Handle
      renames Unicode_FromString;

   function "+" (Value : Week_Time) return Handle is
      Result : Handle;
   begin
      Result := Dict_New;
      Result.Dict_SetItemString ("day",  +Image (Value.Day, False));
      Result.Dict_SetItemString ("time", +Minutes (Value.Time));
      return Result;
   end "+";

   function "+" (Value : Set_Point) return Handle is
      Result : Handle;
   begin
      Result := Dict_New;
      Result.Dict_SetItemString ("until",       +Minutes (Value.Last));
      Result.Dict_SetItemString ("temperature", +Value.Point);
      return Result;
   end "+";

   function "+" (Schedule : Day_Schedule) return Handle is
      Result : Handle;
   begin
      Result := List_New (0);
      for Point in Schedule.Points'Range loop
         Result.List_Append (+Schedule.Points (Point));
      end loop;
      return Result;
   end "+";

   function "+" (Schedule : Week_Schedule) return Handle is
      Result : Handle;
   begin
      Result := Dict_New;
      for Day in Schedule'Range loop
         Result.Dict_SetItemString
         (  Image (Day, False),
            +Schedule (Day)
         );
      end loop;
      return Result;
   end "+";

   function "-" (Value : Object) return Duration is
   begin
      return Duration (Links.Float_AsDouble (Value));
   exception
      when others =>
         Throw_TypeError ("Wrong duration argument");
         raise Python_Error;
   end "-";

   function "-" (Value : Object) return Centigrade is
   begin
      return Centigrade (Links.Float_AsDouble (Value));
   exception
      when others =>
         Throw_TypeError ("Wrong temperature argument");
         raise Python_Error;
   end "-";

   function "-" (Value : Object) return RF_Address is
   begin
      return RF_Address (Links.Long_AsLong (Value));
   exception
      when others =>
         Throw_TypeError ("Wrong RF_Address");
         raise Python_Error;
   end "-";

   function "-" (Value : Object) return String is
   begin
      return As_String (Value);
   exception
      when others =>
         Throw_TypeError ("Wrong string argument");
         raise Python_Error;
   end "-";

   function "-" (Value : Object) return Message_Type is
   begin
      return Message_Types.Find (-Value);
   exception
      when others =>
         Throw_TypeError ("Wrong MQTT publishing policy argument");
         raise Python_Error;
   end "-";

   function "-" (Value : Object) return Temperature_Mode is
   begin
      return Temperature_Modes.Find (-Value);
   exception
      when others =>
         Throw_TypeError ("Wrong disposition argument");
         raise Python_Error;
   end "-";

   function "-" (Value : Object) return Operating_Mode is
   begin
      return Device_Modes.Find (-Value);
   exception
      when others =>
         Throw_TypeError ("Wrong operating mode argument");
         raise Python_Error;
   end "-";

   procedure Check_Arguments_Count
             (  Args     : Object;
                Expected : ssize_t
             )  is
      Length : ssize_t;
   begin
      Length := Links.Tuple_Size (Args);
      Check_Error;
      if Length /= Expected then
         Throw_TypeError
         (  "Wrong number of arguments"
         &  ssize_t'Image (length)
         &  ", expected "
         &  ssize_t'Image (Expected)
         );
         raise Python_Error;
      end if;
   end Check_Arguments_Count;

   function Disconnect (Self : Object; Args : Object)
      return Object is
   begin
      Check_Arguments_Count (Args, 1);
      declare
         This : constant Cube_Client_Handle :=
                         Get_Cube (-Links.Tuple_GetItem (Args, 0));
      begin
         if This.Is_Valid then
            if This.Ptr.Is_Connected then
               This.Ptr.Reconnect (False, True);
            end if;
         else
            Throw_TypeError ("No cube exists");
         end if;
         return Null_Object;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Disconnect;

   function Get_Battery (Self : Object; Args : Object) return Object is
   begin
      Check_Arguments_Count (Args, 2);
      declare
         None : aliased Boolean := True;
         Data : constant Device_Data :=
                   Get_Device_Data
                   (  -Links.Tuple_GetItem (Args, 0),
                      -Links.Tuple_GetItem (Args, 1),
                      None'Access
                   );
      begin
         case Data.Kind_Of is
            when Radiator_Thermostat..Eco_Button =>
               if Data.Battery_Low then
                 return Links.Unicode_FromString ("low" & Nul);
               else
                 return Links.Unicode_FromString ("high" & Nul);
               end if;
            when Cube =>
               Throw_TypeError ("The cube has no battery");
               return Null_Object;
            when Unknown =>
               Throw_TypeError (No_Device_Exists);
               return Null_Object;
         end case;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Get_Battery;

   function Get_Connection (Self : Object; Args : Object)
      return Object is
   begin
      Check_Arguments_Count (Args, 1);
      declare
         This : constant Cube_Client_Handle :=
                         Get_Cube (-Links.Tuple_GetItem (Args, 0));
      begin
         if This.Is_Valid then
            if This.Ptr.Is_Connected then
               return Links.Unicode_FromString ("connected" & Nul);
            else
               return Links.Unicode_FromString ("disconnected" & Nul);
            end if;
         else
            return Links.Unicode_FromString ("unknown" & Nul);
         end if;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Get_Connection;

   function Get_Cubes_List
            (  Self : Object;
               Args : Object
            )  return Object is
   begin
      Check_Arguments_Count (Args, 0);
      declare
         Cubes  : constant RF_Address_Sets.Set := MAX_IO.Get_Cubes_List;
         Result : Handle;
      begin
         Result := List_New (0);
         for Index in 1..Cubes.Get_Size loop
            Result.List_Append (+Cubes.Get (Index));
         end loop;
         Links.IncRef (Result.Ptr);
         return Result.Ptr;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Get_Cubes_List;

   function Get_Devices_List
            (  Self : Object;
               Args : Object
            )  return Object is
      Length : ssize_t;
   begin
      Length := Links.Tuple_Size (Args);
      if Length > 2 then
         Throw_TypeError
         (  "Wrong number of arguments"
         &  ssize_t'Image (length)
         &  ", expected 0 to 2"
         );
         raise Python_Error;
      end if;
      declare
         Box    : RF_Address := 0;
         Filter : Device_Type_Set;
         Result : Handle;
         List   : RF_Address_Sets.Set;
      begin
         if Length > 0 then
            Box := -Links.Tuple_GetItem (Args, 0);
         end if;
         if Length = 2 then
            Filter := (others => False);
            declare
               Types : constant Object := Links.Tuple_GetItem (Args, 1);
               Size  : ssize_t;
            begin
               Size := Links.List_Size (Types);
               Check_Error;
               for Index in 0..Size - 1 loop
                  declare
                     Text : constant String :=
                        As_String (Links.List_GetItem (Types, Index));
                  begin
                     Filter (Device_Types.Find (Text)) := True;
                  exception
                     when others =>
                        Throw_TypeError
                        (  "Unknown device type "
                        &  Quote (Text)
                        );
                  end;
               end loop;
            end;
         else
            Filter := (others => True);
         end if;
         Result := List_New (0);
         List   := MAX_IO.Get_Devices_List (Box, Filter);
         for Index in 1..List.Get_Size loop
            Result.List_Append (+List.Get (Index));
         end loop;
         Links.IncRef (Result.Ptr);
         return Result.Ptr;
      end;
   exception
      when Error : End_Error =>
         Throw_TypeError (Exception_Message (Error));
         return Null_Object;
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Get_Devices_List;

   function Get_Duty (Self : Object; Args : Object) return Object is
   begin
      Check_Arguments_Count (Args, 1);
      declare
         This : constant Cube_Client_Handle :=
                         Get_Cube (-Links.Tuple_GetItem (Args, 0));
      begin
         if This.Is_Valid then
            return Links.Float_FromDouble (double (This.Ptr.Get_Duty));
         else
            Throw_TypeError ("Wrong cube address");
            return Null_Object;
         end if;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Get_Duty;

   function Get_Link (Self : Object; Args : Object) return Object is
   begin
      Check_Arguments_Count (Args, 2);
      declare
         None : aliased Boolean := True;
         Data : constant Device_Data :=
                         Get_Device_Data
                         (  -Links.Tuple_GetItem (Args, 0),
                            -Links.Tuple_GetItem (Args, 1),
                            None'Access
                         );
      begin
         if None then
            Throw_TypeError (No_Device_Exists);
            return Null_Object;
         else
            if Data.Link_Error then
               return Links.Unicode_FromString ("error" & Nul);
            else
               return Links.Unicode_FromString ("ok" & Nul);
            end if;
         end if;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Get_Link;

   function Get_Mode (Self : Object; Args : Object) return Object is
   begin
      Check_Arguments_Count (Args, 2);
      declare
         None : aliased Boolean := True;
         Data : constant Device_Data :=
                         Get_Device_Data
                         (  -Links.Tuple_GetItem (Args, 0),
                            -Links.Tuple_GetItem (Args, 1),
                            None'Access
                         );
      begin
         if None then
            Throw_TypeError (No_Device_Exists);
            return Null_Object;
         else
            case Data.Mode is
               when Automatic =>
                  return Links.Unicode_FromString ("automatic" & Nul);
               when Manual =>
                  return Links.Unicode_FromString ("manual" & Nul);
               when Vacation =>
                  return Links.Unicode_FromString ("vacation" & Nul);
               when Boost =>
                  return Links.Unicode_FromString ("boost" & Nul);
            end case;
         end if;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Get_Mode;

   function Get_MQTT (Self : Object; Args : Object) return Object is
      use GNAT.Sockets.MQTT;
   begin
      Check_Arguments_Count (Args, 1);
      declare
         Topic : constant String := -Links.Tuple_GetItem (Args, 0);
      begin
         declare
            Message : constant MQTT_Message :=
                               MQTT_State.Get_Message (Topic);
         begin
            return Links.Unicode_FromString
                   (  To_C (Message.Get_Message)
                   );
         end;
      exception
         when Reason : Constraint_Error =>
            Throw_TypeError
            (  "Invalid topic: " & Exception_Message (Reason)
            );
            return Null_Object;
         when End_Error =>
            Throw_TypeError ("No topic " & Quote (Topic) & " exists");
            return Null_Object;
      end;
   exception
      when Error : End_Error =>
         Throw_TypeError (Exception_Message (Error));
         return Null_Object;
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Get_MQTT;

   function Get_Parameters
            (  Self : Object;
               Args : Object
            )  return Object is
   begin
      Check_Arguments_Count (Args, 2);
      declare
         Info   : constant Device_Parameters_Data_Handles.Handle :=
                     Get_Parameters
                     (  -Links.Tuple_GetItem (Args, 0),
                        -Links.Tuple_GetItem (Args, 1)
                     );
         Data   : Device_Parameters_Data'Class renames Info.Ptr.all;
         Param  : Device_Parameters renames Data.Parameters;
         Result : Handle;
      begin
         Result := Dict_New;
         Result.Dict_SetItemString ("type",      +Data.Kind_Of);
         Result.Dict_SetItemString ("room id",   +Param.Room);
         Result.Dict_SetItemString ("address",   +Param.Address);
         Result.Dict_SetItemString ("room",      +Data.Room);
         Result.Dict_SetItemString ("serial no", +Param.Serial_No);
         Result.Dict_SetItemString ("name",      +Param.Name);
         case Data.Kind_Of is
            when Cube | Shutter_Contact | Eco_Button | Unknown =>
               null;
            when Radiator_Thermostat..Wall_Thermostat =>
               Result.Dict_SetItemString
               (  "comfort temperature",
                  +Param.Comfort
               );
               Result.Dict_SetItemString
               (  "eco temperature",
                  +Param.Eco
               );
               Result.Dict_SetItemString
               (  "maximum temperature",
                  +Param.Max
               );
               Result.Dict_SetItemString
               ( "minimum temperature",
                 +Param.Min
               );
               Result.Dict_SetItemString
               (  "schedule",
                  +Param.Schedule
               );
               Result.Dict_SetItemString
               (  "temperature offset",
                  +Param.Offset
               );
               Result.Dict_SetItemString
               (  "airing temperature",
                  +Param.Window_Open
               );
               case Data.Kind_Of is
                  when Radiator_Thermostat | Radiator_Thermostat_Plus =>
                     Result.Dict_SetItemString
                     (  "airing mode delay",
                        +Minutes (Param.Window_Time)
                     );
                     Result.Dict_SetItemString
                     (  "boost duration",
                        +Minutes (Param.Boost_Time)
                     );
                     Result.Dict_SetItemString
                     (  "boost valve position",
                        +Param.Boost_Valve
                     );
                     Result.Dict_SetItemString
                     (  "decalcification time",
                        +Param.Decalcification
                     );
                     Result.Dict_SetItemString
                     (  "maximum valve position",
                        +Param.Max_Valve
                     );
                     Result.Dict_SetItemString
                     (  "valve offset",
                        +Param.Valve_Offset
                     );
                  when others =>
                     null;
               end case;
         end case;
         Links.IncRef (Result.Ptr);
         return Result.Ptr;
      end;
   exception
      when Error : End_Error =>
         Throw_TypeError (Exception_Message (Error));
         return Null_Object;
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Get_Parameters;

   function Get_Set_Temperature
            (  Self : Object;
               Args : Object
            )  return Object is
   begin
      Check_Arguments_Count (Args, 2);
      declare
         None : aliased Boolean := True;
         Data : constant Device_Data :=
                         Get_Device_Data
                         (  -Links.Tuple_GetItem (Args, 0),
                            -Links.Tuple_GetItem (Args, 1),
                            None'Access
                         );
      begin
         case Data.Kind_Of is
            when Radiator_Thermostat..Wall_Thermostat =>
               return Links.Float_FromDouble
                      (  double (Data.Set_Temperature)
                      );
            when Cube | Shutter_Contact | Eco_Button =>
               Throw_TypeError ("The device is not a thermostat");
               return Null_Object;
            when Unknown =>
               Throw_TypeError (No_Device_Exists);
               return Null_Object;
         end case;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Get_Set_Temperature;

   function Get_Status (Self : Object; Args : Object) return Object is
   begin
      Check_Arguments_Count (Args, 2);
      declare
         None   : aliased Boolean := True;
         Data   : constant Device_Data :=
                           Get_Device_Data
                           (  -Links.Tuple_GetItem (Args, 0),
                              -Links.Tuple_GetItem (Args, 1),
                              None'Access
                           );
         Result : Handle;
      begin
         Result := Dict_New;
         Result.Dict_SetItemString ("type", +Data.Kind_Of);
         Result.Dict_SetItemString ("address", +Data.Address);
         if Data.Kind_Of in Radiator_Thermostat..Wall_Thermostat then
            Result.Dict_SetItemString ("mode", +Data.Mode);
         end if;
         Result.Dict_SetItemString ("error",       +Data.Error);
         Result.Dict_SetItemString ("initialized", +Data.Initialized);
         case Data.Kind_Of is
            when Cube =>
               null;
            when Eco_Button =>
               null;
            when Shutter_Contact =>
               Result.Dict_SetItemString ("open", +Data.Open);
            when Radiator_Thermostat | Radiator_Thermostat_Plus =>
               Result.Dict_SetItemString
               (  "valve",
                  +Data.Valve_Position
               );
               if Data.Temperature /= Centigrade'First then
                  Result.Dict_SetItemString
                  (  "temperature",
                     +Data.Temperature
                  );
               end if;
               Result.Dict_SetItemString
               (  "set temperature",
                  +Data.Set_Temperature
               );
               Result.Dict_SetItemString
               (  "new temperature",
                  +Data.New_Temperature
               );
            when Wall_Thermostat =>
               if Data.Temperature /= Centigrade'First then
                  Result.Dict_SetItemString
                  (  "temperature",
                     +Data.Temperature
                  );
               end if;
               Result.Dict_SetItemString
               (  "set temperature",
                  +Data.Set_Temperature
               );
               Result.Dict_SetItemString
               (  "new temperature",
                  +Data.New_Temperature
               );
            when Unknown =>
               Throw_TypeError (No_Device_Exists);
               return Null_Object;
         end case;
         if Data.Kind_Of in Radiator_Thermostat..Eco_Button then
            Result.Dict_SetItemString
            (  "panel locked",
               +Data.Panel_Locked
            );
            Result.Dict_SetItemString
            (  "battery low",
               +Data.Battery_Low
            );
            Result.Dict_SetItemString ("link error",  +Data.Link_Error);
            Result.Dict_SetItemString ("summer time", +Data.DST);
         end if;
         Links.IncRef (Result.Ptr);
         return Result.Ptr;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Get_Status;

   function Get_Summer_Time
            (  Self : Object;
               Args : Object
            )  return Object is
   begin
      Check_Arguments_Count (Args, 2);
      declare
         Result : Object;
         None   : aliased Boolean := True;
         Data   : constant Device_Data :=
                           Get_Device_Data
                           (  -Links.Tuple_GetItem (Args, 0),
                              -Links.Tuple_GetItem (Args, 1),
                              None'Access
                           );
      begin
         if Data.Kind_Of = Unknown then
            Throw_TypeError (No_Device_Exists);
            return Null_Object;
         elsif Data.DST then
            Result := Links.True;
         else
            Result := Links.False;
         end if;
         Links.IncRef (Result);
         return Result;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Get_Summer_Time;

   function Get_Temperature
            (  Self : Object;
               Args : Object
            )  return Object is
   begin
      Check_Arguments_Count (Args, 2);
      declare
         None : aliased Boolean := True;
         Data : constant Device_Data :=
                         Get_Device_Data
                         (  -Links.Tuple_GetItem (Args, 0),
                            -Links.Tuple_GetItem (Args, 1),
                            None'Access
                         );
      begin
         case Data.Kind_Of is
            when Radiator_Thermostat..Wall_Thermostat =>
               if Data.Temperature = Centigrade'First then
                  Throw_TypeError
                  (  "The thermostat temperature is yet unknown"
                  );
                  return Null_Object;
               end if;
               return Links.Float_FromDouble
                      (  double (Data.Temperature)
                      );
            when Cube | Shutter_Contact | Eco_Button =>
               Throw_TypeError (Not_Thermostat);
               return Null_Object;
            when Unknown =>
               Throw_TypeError (No_Device_Exists);
               return Null_Object;
         end case;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Get_Temperature;

   function Get_Valve
            (  Self : Object;
               Args : Object
            )  return Object is
   begin
      Check_Arguments_Count (Args, 2);
      declare
         None : aliased Boolean := True;
         Data : constant Device_Data :=
                         Get_Device_Data
                         (  -Links.Tuple_GetItem (Args, 0),
                            -Links.Tuple_GetItem (Args, 1),
                             None'Access
                         );
      begin
         case Data.Kind_Of is
            when Radiator_Thermostat..Radiator_Thermostat_Plus =>
               return Links.Float_FromDouble
                      (  double (Data.Valve_Position)
                      );
            when Cube | Shutter_Contact | Eco_Button |
                 Wall_Thermostat =>
               Throw_TypeError (Not_Radiator);
               return Null_Object;
            when Unknown =>
               Throw_TypeError (No_Device_Exists);
               return Null_Object;
         end case;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Get_Valve;

   function Get_Valve_Statistics
            (  Self : Object;
               Args : Object
            )  return Object is
   begin
      Check_Arguments_Count (Args, 1);
      declare
         Average : Natural := 0;
         Min     : Natural := 0;
         Max     : Natural := 0;
         Result  : Handle;
         function "+" (Value : Natural) return Handle is
         begin
            return (  Ada.Finalization.Controlled
                   with
                      Links.Float_FromDouble (double (Value) / 100.0)
                   );
         end "+";
      begin
         MAX_IO.Get_Valves
         (  Box     => -Links.Tuple_GetItem (Args, 0),
            Average => Average,
            Min     => Min,
            Max     => Max
         );
         Result := Dict_New;
         Result.Dict_SetItemString ("average", +Average);
         Result.Dict_SetItemString ("maximum", +Max);
         Result.Dict_SetItemString ("minimum", +Min);
         Links.IncRef (Result.Ptr);
         return Result.Ptr;
      end;
   exception
      when Error : End_Error =>
         Throw_TypeError (Exception_Message (Error));
         return Null_Object;
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Get_Valve_Statistics;

   function Publish_MQTT (Self : Object; Args : Object) return Object is
      Length : ssize_t;
      Policy : Message_Type := Updated;
   begin
      Length := Links.Tuple_Size (Args);
      Check_Error;
      if Length not in 2..3 then
         Throw_TypeError
         (  "Wrong number of arguments"
         &  ssize_t'Image (length)
         &  ", expected 2 or 3"
         );
         raise Python_Error;
      end if;
      if Length = 3 then
         Policy := -Links.Tuple_GetItem (Args, 2);
      end if;
      MQTT_State.Publish
      (  Topic   => -Links.Tuple_GetItem (Args, 0),
         Message => -Links.Tuple_GetItem (Args, 1),
         Policy  => Policy
      );
      return No_Value;
   exception
      when Error : End_Error =>
         Throw_TypeError (Exception_Message (Error));
         return Null_Object;
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Publish_MQTT;

   function Reboot (Self : Object; Args : Object) return Object is
   begin
      Check_Arguments_Count (Args, 1);
      Reboot (-Links.Tuple_GetItem (Args, 0));
      return No_Value;
   exception
      when Constraint_Error =>
         Throw_TypeError ("Invalid serial number");
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Reboot;

   function Reconnect (Self : Object; Args : Object)
      return Object is
   begin
      Check_Arguments_Count (Args, 1);
      declare
         This : constant Cube_Client_Handle :=
                         Get_Cube (-Links.Tuple_GetItem (Args, 0));
      begin
         if This.Is_Valid then
            if not This.Ptr.Is_Connected then
               This.Ptr.Reconnect (True, True);
            end if;
         else
            Throw_TypeError ("No cube exists");
         end if;
         return Null_Object;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Reconnect;

   function Set_Mode
            (  Self     : Object;
               Args     : Object;
               Keywords : Object
            )  return Object is
      type Argument_Type is
           (  Mode_Argument,
              Cube_Argument,
              Thermostat_Argument,
              Temperature_Argument,
              Disposition_Argument,
              Interval_Argument
           );
      Length      : ssize_t;
      Size        : ssize_t;
      Mode        : Operating_Mode;
      Cube        : RF_Address;
      Thermostat  : RF_Address       := 0;
      Temperature : Centigrade       := Centigrade'First;
      Disposition : Temperature_Mode := Absolute;
      Interval    : Duration         := 0.0;
      Got         : array (Argument_Type) of Boolean :=
                       (others => False);
   begin
      Length := Links.Tuple_Size (Args);
      if Keywords = Null_Object then
         Size := 0;
      else
         Size := Links.Dict_Size (Keywords);
      end if;
      Check_Error;
      if Size + Length > 6 then
         Throw_TypeError
         (  "Wrong number of arguments"
         &  ssize_t'Image (length)
         &  ", more than 6"
         );
         raise Python_Error;
      elsif Size + Length < 2 then
         Throw_TypeError
         (  "Wrong number of arguments"
         &  ssize_t'Image (length)
         &  ", less than 2"
         );
         raise Python_Error;
      end if;
      if Length > 0 then
         Mode := -Links.Tuple_GetItem (Args, 0);
         Got (Mode_Argument) := True;
         if Length > 1 then
            Cube := -Links.Tuple_GetItem (Args, 1);
            Got (Cube_Argument) := True;
            if Length > 2 then
               Thermostat := -Links.Tuple_GetItem (Args, 2);
               Got (Thermostat_Argument) := True;
               if Length > 3 then
                  Temperature := -Links.Tuple_GetItem (Args, 3);
                  Got (Temperature_Argument) := True;
                  if Length > 4 then
                     Disposition := -Links.Tuple_GetItem (Args, 4);
                     Got (Disposition_Argument) := True;
                     if Length > 5 then
                        Interval := -Links.Tuple_GetItem (Args, 5);
                        Got (Interval_Argument) := True;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
      if Size > 0 and then Length < 6 then
         declare
            Value : constant Object := Links.Dict_GetItemString
                                       (  Keywords,
                                          "until" & Nul
                                       );
         begin
            if Value /= Null_Object then
               Interval := -Value;
               Size := Size - 1;
               Got (Interval_Argument) := True;
            end if;
         end;
         if Length < 5 then
            declare
               Value : constant Object := Links.Dict_GetItemString
                                          (  Keywords,
                                             "disposition" & Nul
                                          );
            begin
               if Value /= Null_Object then
                  Disposition := -Value;
                  Size := Size - 1;
                  Got (Disposition_Argument) := True;
               end if;
            end;
            if Length < 4 then
               declare
                  Value : constant Object := Links.Dict_GetItemString
                                             (  Keywords,
                                                "temperature" & Nul
                                             );
               begin
                  if Value /= Null_Object then
                     Temperature := -Value;
                     Size := Size - 1;
                     Got (Temperature_Argument) := True;
                  end if;
               end;
               if Length < 3 then
                  declare
                     Value : constant Object :=
                                       Links.Dict_GetItemString
                                       (  Keywords,
                                          "thermostat" & Nul
                                       );
                  begin
                     if Value = Null_Object then
                        declare
                           Value : constant Object :=
                                            Links.Dict_GetItemString
                                            (  Keywords,
                                               "device" & Nul
                                            );
                        begin
                           if Value /= Null_Object then
                              Thermostat := -Value;
                              Size := Size - 1;
                              Got (Thermostat_Argument) := True;
                           end if;
                        end;
                     else
                        Thermostat := -Value;
                        Size := Size - 1;
                        Got (Thermostat_Argument) := True;
                     end if;
                  end;
                  if Length < 2 then
                     declare
                        Value : constant Object :=
                                         Links.Dict_GetItemString
                                         (  Keywords,
                                            "cube" & Nul
                                         );
                     begin
                        if Value /= Null_Object then
                           Cube := -Value;
                           Size := Size - 1;
                           Got (Cube_Argument) := True;
                        end if;
                     end;
                     if Length < 1 then
                        declare
                           Value : constant Object :=
                                            Links.Dict_GetItemString
                                            (  Keywords,
                                               "mode" & Nul
                                            );
                        begin
                           if Value /= Null_Object then
                              Mode := -Value;
                              Size := Size - 1;
                              Got (Mode_Argument) := True;
                           end if;
                        end;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
      if Size > 0 then
         Throw_TypeError ("Some keyed arguments do not match");
         raise Python_Error;
      end if;
      if not Got (Mode_Argument) then
         Throw_TypeError ("Missing mode argument");
         raise Python_Error;
      elsif not Got (Cube_Argument) then
         Throw_TypeError ("Missing cube RF address argument");
         raise Python_Error;
      end if;
      case Disposition is
         when Absolute | Increment | Decrement =>
            if not Got (Temperature_Argument) then
               Throw_TypeError ("No temperature specified");
               raise Python_Error;
            end if;
         when Airing | Comfort | Eco =>
            if Got (Temperature_Argument) then
               Throw_TypeError
               (  "Temperature cannot be specified for " &
                  "airing, comfort, eco disposition"
               );
               raise Python_Error;
            end if;
            Got (Temperature_Argument) := True;
      end case;
      case Mode is
         when Automatic =>
            if Got (Interval_Argument) then
               Throw_TypeError
               (  "Until argument cannot be specified for "
               &  "automatic mode"
               );
               raise Python_Error;
            end if;
         when Manual =>
            if Got (Interval_Argument) then
               Throw_TypeError
               (  "Until argument cannot be specified for "
               &  "manual mode"
               );
               raise Python_Error;
            elsif not Got (Temperature_Argument) then
               Throw_TypeError
               (  "Temperature must be specified for manual mode"
               );
               raise Python_Error;
            end if;
         when Boost =>
            if Got (Interval_Argument) then
               Throw_TypeError
               (  "Until argument cannot be specified for "
               &  "boost mode"
               );
               raise Python_Error;
            elsif not Got (Temperature_Argument) then
               Throw_TypeError
               (  "Temperature must be specified for boost mode"
               );
               raise Python_Error;
            end if;
         when Vacation =>
            if not Got (Interval_Argument) then
               Throw_TypeError
               (  "Until argument must be specified for "
               &  "vacation mode"
               );
               raise Python_Error;
            elsif not Got (Temperature_Argument) then
               Throw_TypeError
               (  "Temperature must be specified for vacation mode"
               );
               raise Python_Error;
            end if;
      end case;
--        Ada.Text_IO.Put_Line
--        (  "MAX_IO.Set_Mode
--        &  "  cube => " & Image (Cube)
--        &  "  thermostat => " & Image (Thermostat)
--        &  "  mode => " & Operating_Mode'Image (Mode)
--        &  "  temperature => " & Image(Temperature)
--        &  "  until =>" & Float'Image (Float(Interval))
--        &  "  disposition => " & Temperature_Mode'Image (Disposition)
--        );
      Set_Thermostat_Mode
      (  Box         => Cube,
         Device      => Thermostat,
         Mode        => Mode,
         Temperature => Temperature,
         Up_Until    => Clock + Interval,
         Disposition => Disposition
      );
      return No_Value;
   exception
      when Error : End_Error =>
         Throw_TypeError (Exception_Message (Error));
         return Null_Object;
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Set_Mode;

   function Trace (Self : Object; Args : Object) return Object is
   begin
      Check_Arguments_Count (Args, 1);
      MAX_IO.Trace (-Links.Tuple_GetItem (Args, 0), Message_Text);
      return No_Value;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Trace;

   ModuleDef_HEAD_INIT : constant ModuleDef_Base :=
                                  (  Base  => (1, Null_Object),
                                     Init  => null,
                                     Index => 0,
                                     Copy  => Null_Object
                                  );

   Module_Name : aliased char_array := "elv_max_cube" & Nul;
   Module_Doc  : aliased char_array := "elv_max_cube interface" & Nul;

   Disconnect_Name : aliased char_array := "disconnect" & Nul;
   Disconnect_Doc  : aliased char_array :=
                             "Disconnect from a MAX! cube" & Nul;

   Get_Battery_Name : aliased char_array := "get_battery" & Nul;
   Get_Battery_Doc  : aliased char_array :=
                         "Get battery state of a MAX! " &
                         "device by cube and device RF address" & Nul;

   Get_Connection_Name : aliased char_array := "get_connection" & Nul;
   Get_Connection_Doc  : aliased char_array :=
                         "Get connection status of a MAX! cube" & Nul;

   Get_Cubes_List_Name : aliased char_array := "get_cubes_list" & Nul;
   Get_Cubes_List_Doc  : aliased char_array :=
                         "Get list of RF addresses of the MAX! " &
                         "cubes attached" & Nul;

   Get_Devices_List_Name : aliased char_array :=
                                               "get_devices_list" & Nul;
   Get_Devices_List_Doc  : aliased char_array :=
                           "Get list of RF addresses of the MAX! " &
                           "devices connected to a cube or to any " &
                           "cube filtered by the device type" & Nul;

   Get_Duty_Name : aliased char_array := "get_duty" & Nul;
   Get_Duty_Doc  : aliased char_array :=
                         "Get cube's duty. The value in the range " &
                         "0..1 indicates how much of the traffic" &
                         "quota has been used this hour" & Nul;

   Get_Link_Name : aliased char_array := "get_link" & Nul;
   Get_Link_Doc  : aliased char_array :=
                         "Get link state of a MAX! " &
                         "device by cube and device RF address" & Nul;

   Get_Mode_Name : aliased char_array := "get_mode" & Nul;
   Get_Mode_Doc  : aliased char_array :=
                         "Get current mode of a MAX! " &
                         "device by cube and device RF address" & Nul;

   Get_MQTT_Name : aliased char_array := "get_mqtt_message" & Nul;
   Get_MQTT_Doc  : aliased char_array :=
                         "Get a retained MQTT message published on " &
                         "the broker" & Nul;

   Get_Parameters_Name : aliased char_array := "get_parameters" & Nul;
   Get_Parameters_Doc  : aliased char_array :=
                         "Get parameters of a device" & Nul;

   Get_Set_Temperature_Name : aliased char_array :=
                         "get_set_temperature" & Nul;
   Get_Set_Temperature_Doc  : aliased char_array :=
                         "Get set temperature of a thermostat " &
                         "in degree Celsius" & Nul;

   Get_Status_Name : aliased char_array := "get_status" & Nul;
   Get_Status_Doc  : aliased char_array :=
                                         "Get status of a device" & Nul;

   Get_Summer_Time_Name : aliased char_array := "get_summer_time" & Nul;
   Get_Summer_Time_Doc  : aliased char_array :=
                         "Check if the daytime saving mode " &
                         "is active" & Nul;

   Get_Temperature_Name : aliased char_array := "get_temperature" & Nul;
   Get_Temperature_Doc  : aliased char_array :=
                         "Get measured temperature of a thermostat " &
                         "in degree Celsius" & Nul;

   Get_Valve_Name : aliased char_array := "get_valve" & Nul;
   Get_Valve_Doc  : aliased char_array :=
                         "Get valve position of a thermostat. " &
                         "The result is a number in the range 0..1. " &
                         "The value 1 corresponds to a fully open " &
                         "valve" & Nul;

   Get_Valve_Statistics_Name : aliased char_array :=
                         "get_valve_statistics" & Nul;
   Get_Valve_Statistics_Doc  : aliased char_array :=
                         "Get statistics of thermostat valve " &
                         "positions. The result contains minimum, " &
                         "maximum and average of current valve " &
                         "positions" & Nul;

   Publish_MQTT_Name : aliased char_array :=
                         "publish_mqtt_message" & Nul;
   Publish_MQTT_Doc  : aliased char_array :=
                         "Publish a topic on the MQTT broker" & Nul;

   Reconnect_Name : aliased char_array := "reconnect" & Nul;
   Reconnect_Doc  : aliased char_array :=
                            "Reconnect from a MAX! cube" & Nul;

   Set_Mode_Name : aliased char_array := "set_mode" & Nul;
   Set_Mode_Doc  : aliased char_array := "Set thermostat mode" & Nul;

   Trace_Name : aliased char_array := "trace" & Nul;
   Trace_Doc  : aliased char_array :=
                         "Write a message to the MAX! Home " &
                         "automation trace" & Nul;

   Reboot_Name : aliased char_array := "reboot" & Nul;
   Reboot_Doc  : aliased char_array := "Reboot a MAX! cube" & Nul;


   Methods : array (1..22) of aliased MethodDef :=
             (  (  Name  => To_Chars_Ptr (Disconnect_Name'Access),
                   Meth  => (False, Disconnect'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Disconnect_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Get_Battery_Name'Access),
                   Meth  => (False, Get_Battery'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Get_Battery_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Get_Connection_Name'Access),
                   Meth  => (False, Get_Connection'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Get_Connection_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Get_Cubes_List_Name'Access),
                   Meth  => (False, Get_Cubes_List'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Get_Cubes_List_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Get_Devices_List_Name'Access),
                   Meth  => (False, Get_Devices_List'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Get_Devices_List_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Get_Duty_Name'Access),
                   Meth  => (False, Get_Duty'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Get_Duty_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Get_Link_Name'Access),
                   Meth  => (False, Get_Link'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Get_Link_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Get_Mode_Name'Access),
                   Meth  => (False, Get_Mode'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Get_Mode_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Get_MQTT_Name'Access),
                   Meth  => (False, Get_MQTT'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Get_MQTT_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Get_Parameters_Name'Access),
                   Meth  => (False, Get_Parameters'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Get_Parameters_Doc'Access)
                ),
                (  Name  =>
                      To_Chars_Ptr (Get_Set_Temperature_Name'Access),
                   Meth  =>
                      (False, Get_Set_Temperature'Access),
                   Flags =>
                      METH_VARARGS,
                   Doc   =>
                      To_Chars_Ptr (Get_Set_Temperature_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Get_Status_Name'Access),
                   Meth  => (False, Get_Status'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Get_Status_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Get_Summer_Time_Name'Access),
                   Meth  => (False, Get_Summer_Time'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Get_Summer_Time_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Get_Temperature_Name'Access),
                   Meth  => (False, Get_Temperature'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Get_Temperature_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Get_Valve_Name'Access),
                   Meth  => (False, Get_Valve'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Get_Valve_Doc'Access)
                ),
                (  Name  =>
                      To_Chars_Ptr (Get_Valve_Statistics_Name'Access),
                   Meth  =>
                      (False, Get_Valve_Statistics'Access),
                   Flags =>
                      METH_VARARGS,
                   Doc   =>
                      To_Chars_Ptr (Get_Valve_Statistics_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Publish_MQTT_Name'Access),
                   Meth  => (False, Publish_MQTT'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Publish_MQTT_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Set_Mode_Name'Access),
                   Meth  => (True, Set_Mode'Access),
                   Flags => METH_VARARGS + METH_KEYWORDS,
                   Doc   => To_Chars_Ptr (Set_Mode_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Reconnect_Name'Access),
                   Meth  => (False, Reconnect'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Reconnect_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Trace_Name'Access),
                   Meth  => (False, Trace'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Trace_Doc'Access)
                ),
                (  Name  => To_Chars_Ptr (Reboot_Name'Access),
                   Meth  => (False, Reboot'Access),
                   Flags => METH_VARARGS,
                   Doc   => To_Chars_Ptr (Reboot_Doc'Access)
                ),
                (  Null_Ptr,
                   (False, null),
                   0,
                   Null_Ptr
             )  );
   Module : constant ModuleDef :=
                     (  Base     => ModuleDef_HEAD_INIT,
                        Name     => To_Chars_Ptr (Module_Name'Access),
                        Doc      => To_Chars_Ptr (Module_Doc'Access),
                        Size     => -1,
                        Methods  => Methods (1)'Access,
                        Slots    => null,
                        Traverse => null,
                        Clear    => null,
                        Free     => null
                     );

   function ELV_MAX_Cube_Init return Object;
   pragma Convention (C, ELV_MAX_Cube_Init);

   function ELV_MAX_Cube_Init return Object is
   begin
      return Module_Create (Module, 1013);
   end ELV_MAX_Cube_Init;

   procedure Init is
   begin
      if 0 > Import_AppendInittab
             (  Module_Name,
                ELV_MAX_Cube_Init'Access
             )
      then
         Raise_Exception
         (  Python_Error'Identity,
            "Cannot append elv_max_cube"
         );
      end if;
   end Init;

begin
   Device_Types.Add ("cube",                  Cube);
   Device_Types.Add ("eco button",            Eco_Button);
   Device_Types.Add ("radiator thermostat",   Radiator_Thermostat);
   Device_Types.Add ("radiator thermostat plus",
                                              Radiator_Thermostat_Plus);
   Device_Types.Add ("shutter contact",       Shutter_Contact);
   Device_Types.Add ("thermostat",            Radiator_Thermostat);
   Device_Types.Add ("thermostat plus",       Radiator_Thermostat_Plus);
   Device_Types.Add ("wall thermostat",       Wall_Thermostat);

   Device_Modes.Add ("automatic", Automatic);
   Device_Modes.Add ("boost",     Boost);
   Device_Modes.Add ("manual",    Manual);
   Device_Modes.Add ("vacation",  Vacation);

   Message_Types.Add ("ignored",   Ignored);
   Message_Types.Add ("initial",   Initial);
   Message_Types.Add ("retained",  Retained);
   Message_Types.Add ("transient", Transient);
   Message_Types.Add ("updated",   Updated);

   Temperature_Modes.Add ("airing",    Airing);
   Temperature_Modes.Add ("absolute",  Absolute);
   Temperature_Modes.Add ("comfort",   Comfort);
   Temperature_Modes.Add ("decrement", Decrement);
   Temperature_Modes.Add ("eco",       Eco);
   Temperature_Modes.Add ("increment", Increment);

end Py.ELV_MAX_Cube;
