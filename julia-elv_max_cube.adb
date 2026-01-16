--                                                                    --
--  package Julia.ELV_MAX_Cube      Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2019       --
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

with Ada.Streams;            use Ada.Streams;
with Ada.Streams.Stream_IO;  use Ada.Streams.Stream_IO;
with GNAT.Sockets.MQTT;      use GNAT.Sockets.MQTT;
with MAX_IO;                 use MAX_IO;
with MAX_IO.Set_Mode;        use MAX_IO.Set_Mode;
with MAX_Trace;              use MAX_Trace;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
use  GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;

with Ada.Unchecked_Deallocation;
with GNAT.Sockets.MQTT.Server;
with Julia.Generic_1D_Array;

package body Julia.ELV_MAX_Cube is
   CRLF : constant String := Character'Val (13) & Character'Val (10);
   Initialized            : Boolean := False;

   No_Device_Exists  : constant String := "No device exists";
   No_Cube_Exists    : constant String := "No cube exists";
   Not_Radiator      : constant String := "Not a radiator thermostat";
   Not_Thermostat    : constant String := "Not a thermostat";

   To_Connection_State        : function_t;
   To_Device_Type             : function_t;
   To_Link_State              : function_t;
   To_MQTT_Policy             : function_t;
   To_Operating_Mode          : function_t;
   To_Temperature_Disposition : function_t;
   To_Week_Day                : function_t;

   type Uchar_Array is
      array (Positive range <>) of Interfaces.Unsigned_8;
   package Uchar_Arrays is
      new Julia.Generic_1D_Array
          (  Index_Type         => Positive,
             Element_Type       => Interfaces.Unsigned_8,
             Element_Array_Type => Uchar_Array,
             Julia_Type         => UInt8_Type
          );

   type Points_Array is array (Positive range <>) of value_t;
   package Points_Arrays is
      new Julia.Generic_1D_Array
          (  Index_Type         => Positive,
             Element_Type       => value_t,
             Element_Array_Type => Points_Array,
             Julia_Type         => Anytuple_Type
          );

   type Unsigned_Array is
      array (Positive range <>) of Interfaces.Unsigned_32;
   package Unsigned_Arrays is
      new Julia.Generic_1D_Array
          (  Index_Type         => Positive,
             Element_Type       => Interfaces.Unsigned_32,
             Element_Array_Type => Unsigned_Array,
             Julia_Type         => UInt32_Type
          );

   type Char_Buffer (Size : size_t) is record
      Length : size_t := 0;
      Text   : char_array (1..Size);
   end record;
   type Char_Buffer_Ptr is access Char_Buffer;

   procedure Free is
      new Ada.Unchecked_Deallocation (Char_Buffer, Char_Buffer_ptr);

   procedure Append
             (  Buffer : in out Char_Buffer_Ptr;
                Chunk  : Stream_Element_Array
             )  is
   begin
      if Buffer = null then
         Buffer := new Char_Buffer (Chunk'Length + 1);
      elsif Buffer.Length + Chunk'Length + 1 > Buffer.Size then
         declare
            Ptr : constant Char_Buffer_Ptr := new Char_Buffer
                                                  (  Buffer.Length
                                                  +  Chunk'Length
                                                  + 1
                                                  );
         begin
            if Buffer.Length > 0 then
               Ptr.Text (1..Buffer.Length) :=
                  Buffer.Text (1..Buffer.Length);
               Ptr.Length := Buffer.Length;
            end if;
            Free (Buffer);
            Buffer := Ptr;
         end;
      end if;
      declare
         To : size_t renames Buffer.Length;
      begin
         for From in Chunk'Range loop
            To := To + 1;
            Buffer.Text (To) := char'Val (Chunk (From));
         end loop;
         Buffer.Text (To + 1) := NUL;
      end;
   end Append;

   function To_Julia (Value : Centigrade) return value_t is
   begin
      return To_Julia (C_float (Value));
   end To_Julia;

   function To_Julia (Value : Device_Type) return value_t is
      Kind_Of : Unsigned_8;
   begin
      case Value is
         when Cube =>
            Kind_Of := 1;
         when Radiator_Thermostat =>
            Kind_Of := 2;
         when Radiator_Thermostat_Plus =>
            Kind_Of := 3;
         when Wall_Thermostat =>
            Kind_Of := 4;
         when Shutter_Contact =>
            Kind_Of := 5;
         when Eco_Button =>
            Kind_Of := 6;
         when Unknown =>
            Kind_Of := 0;
      end case;
      return Call (To_Device_Type, To_Julia (Kind_Of));
   end To_Julia;

   function To_Julia (Value : Operating_Mode) return value_t is
      Kind_Of : Unsigned_8;
   begin
      case Value is
         when Automatic =>
            Kind_Of := 0;
         when Manual =>
            Kind_Of := 1;
         when Vacation =>
            Kind_Of := 2;
         when Boost =>
            Kind_Of := 3;
      end case;
      return Call (To_Operating_Mode, To_Julia (Kind_Of));
   end To_Julia;

   function To_Julia (Value : Ratio) return value_t is
   begin
      return To_Julia (C_float (Value));
   end To_Julia;

   function To_Julia (Day : Week_Day) return value_t is
      Kind_Of : Unsigned_8;
   begin
      case Day is
         when Mo =>
            Kind_Of := 1;
         when Tu =>
            Kind_Of := 2;
         when We =>
            Kind_Of := 3;
         when Th =>
            Kind_Of := 4;
         when Fr =>
            Kind_Of := 5;
         when Sa =>
            Kind_Of := 6;
         when Su =>
            Kind_Of := 7;
      end case;
      return Call (To_Week_Day, To_Julia (Kind_Of));
   end To_Julia;

   function To_Julia (Value : Week_Time) return value_t is
      Roots  : Holder (2);
      Values : values_array (1..2);
   begin
      Values (1) := To_Julia (Value.Day);
      Set (Roots, 1, Values (1));
      Values (2) := To_Julia (Value.Time);
      Set (Roots, 2, Values (2));
      return To_Julia (Values);
   end To_Julia;

   function To_Julia (Value : Set_Point) return value_t is
      Roots  : Holder (2);
      Values : values_array (1..2);
   begin
      Values (1) := To_Julia (Value.Last);
      Set (Roots, 1, Values (1));
      Values (2) := To_Julia (Value.Point);
      Set (Roots, 2, Values (2));
      return To_Julia (Values);
   end To_Julia;

   function To_Julia (Schedule : Day_Schedule) return value_t is
      use Points_Arrays;
      Roots  : Holder (Schedule.Length);
      Result : Points_Array (1..Schedule.Length);
   begin
      for Point in Schedule.Points'Range loop
         Result (Point) := To_Julia (Schedule.Points (Point));
         Set (Roots, Point, Result (Point));
      end loop;
      return To_Julia (Result);
   end To_Julia;

   function To_Julia (Schedule : Week_Schedule) return value_t is
      Result : Tuple;
      Roots  : Holder (7);
      Value  : value_t;
   begin
      for Day in Schedule'Range loop
         Value := To_Julia (Schedule (Day));
         Set (Roots, Week_Day'Pos (Day) + 1, Value);
         Result.Add (Image (Day), Value);
      end loop;
      return To_Julia (Result);
   end To_Julia;

   procedure Julia_Disconnect (Box : Unsigned_32);
   pragma Convention (C, Julia_Disconnect);

   function Julia_Get_Battery (Box, Device : Unsigned_32)
      return unsigned_char;
   pragma Convention (C, Julia_Get_Battery);

   function Julia_Get_Connection (Box : Unsigned_32)
      return unsigned_char;
   pragma Convention (C, Julia_Get_Connection);

   function Julia_Get_Cubes_List return value_t;
   pragma Convention (C, Julia_Get_Cubes_List);

   function Julia_Get_Devices_List
            (  Box    : Unsigned_32;
               Filter : value_t
            )  return value_t;
   pragma Convention (C, Julia_Get_Devices_List);

   function Julia_Get_Duty (Box : Unsigned_32) return C_float;
   pragma Convention (C, Julia_Get_Duty);

   function Julia_Get_Link (Box, Device : Unsigned_32)
      return unsigned_char;
   pragma Convention (C, Julia_Get_Link);

   function Julia_Get_Mode (Box, Device : Unsigned_32)
      return unsigned_char;
   pragma Convention (C, Julia_Get_Mode);

   function Julia_Get_MQTT_Message (Topic_Name : chars_ptr)
      return value_t;
   pragma Convention (C, Julia_Get_MQTT_Message);

   function Julia_Get_Parameters (Box, Device : Unsigned_32)
      return value_t;
   pragma Convention (C, Julia_Get_Parameters);

   function Julia_Get_Set_Temperature (Box, Device : Unsigned_32)
      return C_float;
   pragma Convention (C, Julia_Get_Set_Temperature);

   function Julia_Get_Status (Box, Device : Unsigned_32)
      return value_t;
   pragma Convention (C, Julia_Get_Status);

   function Julia_Get_Summer_Time (Box, Device : Unsigned_32)
      return value_t;
   pragma Convention (C, Julia_Get_Summer_Time);

   function Julia_Get_Temperature (Box, Device : Unsigned_32)
      return C_float;
   pragma Convention (C, Julia_Get_Temperature);

   function Julia_Get_Valve (Box, Device : Unsigned_32)
      return C_float;
   pragma Convention (C, Julia_Get_Valve);

   function Julia_Get_Valve_Statistics (Box : Unsigned_32)
      return value_t;
   pragma Convention (C, Julia_Get_Valve_Statistics);

   procedure Julia_Publish_MQTT
             (  Topic   : chars_ptr;
                Message : chars_ptr;
                Policy  : unsigned_char
             );
   pragma Convention (C, Julia_Publish_MQTT);

   procedure Julia_Reboot (Box : chars_ptr);
   pragma Convention (C, Julia_Reboot);

   procedure Julia_Reconnect (Box : Unsigned_32);
   pragma Convention (C, Julia_Reconnect);

   procedure Julia_Set_Mode
             (  Mode        : unsigned_char;
                Box         : Unsigned_32;
                Device      : Unsigned_32;
                Temperature : C_float;
                Disposition : unsigned_char;
                Deadline    : value_t
             );
   pragma Convention (C, Julia_Set_Mode);

   procedure Julia_Trace (Text : chars_ptr);
   pragma Convention (C, Julia_Trace);

   procedure Julia_Disconnect (Box : Unsigned_32) is
   begin
      declare
         This : constant Cube_Client_Handle :=
                         Get_Cube (RF_Address (Box));
      begin
         if This.Is_Valid then
            if This.Ptr.Is_Connected then
               This.Ptr.Reconnect (False, True);
            end if;
         else
            Error (No_Cube_Exists);
         end if;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
   end Julia_Disconnect;

   function Julia_Get_Battery (Box, Device : Unsigned_32)
      return unsigned_char is
   begin
      declare
         None : aliased Boolean := True;
         Data : constant Device_Data :=
                         Get_Device_Data
                         (  RF_Address (Box),
                            RF_Address (Device),
                            None'Access
                         );
      begin
         case Data.Kind_Of is
            when Radiator_Thermostat..Eco_Button =>
               if Data.Battery_Low then
                  return 0;
               else
                  return 1;
               end if;
            when Cube =>
               Error ("The cube has no battery");
               return 0;
            when Unknown =>
               Error (No_Device_Exists);
               return 0;
         end case;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
         return 0;
   end Julia_Get_Battery;

   function Julia_Get_Connection (Box : Unsigned_32)
      return unsigned_char is
   begin
      declare
         This : constant Cube_Client_Handle :=
                         Get_Cube (RF_Address (Box));
      begin
         if This.Is_Valid then
            if This.Ptr.Is_Connected then
               return 0;
            else
               return 1;
            end if;
         else
            return 2;
         end if;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
         return 0;
   end Julia_Get_Connection;

   function Julia_Get_Cubes_List return value_t is
      use Unsigned_Arrays;
   begin
      declare
         Cubes  : constant RF_Address_Sets.Set :=
                           MAX_IO.Get_Cubes_List;
         Result : Unsigned_Array (1..Cubes.Get_Size);
      begin
         for Index in Result'Range loop
            Result (Index) := Unsigned_32 (Cubes.Get (Index));
         end loop;
         return To_Julia (Result);
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
         return No_Value;
   end Julia_Get_Cubes_List;

   function Julia_Get_Devices_List
            (  Box    : Unsigned_32;
               Filter : value_t
            )  return value_t is
      use Uchar_Arrays;
      use Unsigned_Arrays;
      Selector : Device_Type_Set;
   begin
      declare
         Object : constant Uchar_Array := Value (Filter);
      begin
         if Object'Length = 0 then
            Selector := (others => True);
         else
            Selector := (others => False);
            for Index in Object'Range loop
               case Object (Index) is
                  when 1 =>
                     Selector (Cube) := True;
                  when 2 =>
                     Selector (Radiator_Thermostat) := True;
                  when 3 =>
                     Selector (Radiator_Thermostat_Plus) := True;
                  when 4 =>
                     Selector (Wall_Thermostat) := True;
                  when 5 =>
                     Selector (Shutter_Contact) := True;
                  when 6 =>
                     Selector (Eco_Button) := True;
                  when others =>
                     Selector := (others => True);
               end case;
            end loop;
         end if;
         declare
            List   : constant RF_Address_Sets.Set :=
                     MAX_IO.Get_Devices_List
                     (  RF_Address (Box),
                        Selector
                     );
            Result : Unsigned_Array (1..List.Get_Size);
         begin
            for Index in Result'Range loop
               Result (Index) := Unsigned_32 (List.Get (Index));
            end loop;
            return To_Julia (Result);
         end;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
         return No_Value;
   end Julia_Get_Devices_List;

   function Julia_Get_Duty (Box : Unsigned_32) return C_float is
   begin
      declare
         This : constant Cube_Client_Handle :=
                         Get_Cube (RF_Address (Box));
      begin
         if This.Is_Valid then
            return C_float (This.Ptr.Get_Duty);
         else
            Error (No_Cube_Exists);
            return 0.0;
         end if;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
         return 0.0;
   end Julia_Get_Duty;

   function Julia_Get_Link (Box, Device : Unsigned_32)
      return unsigned_char is
   begin
      declare
         None : aliased Boolean := True;
         Data : constant Device_Data :=
                         Get_Device_Data
                         (  RF_Address (Box),
                            RF_Address (Device),
                            None'Access
                         );
      begin
         if None then
            Error (No_Device_Exists);
            return 0;
         else
            if Data.Link_Error then
               return 1;
            else
               return 0;
            end if;
         end if;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
         return 0;
   end Julia_Get_Link;

   function Julia_Get_Mode (Box, Device : Unsigned_32)
      return unsigned_char is
   begin
      declare
         None : aliased Boolean := True;
         Data : constant Device_Data :=
                         Get_Device_Data
                         (  RF_Address (Box),
                            RF_Address (Device),
                            None'Access
                         );
      begin
         if None then
            Error (No_Device_Exists);
            return 0;
         else
            case Data.Mode is
               when Automatic =>
                  return 0;
               when Manual =>
                  return 1;
               when Vacation =>
                  return 2;
               when Boost =>
                  return 3;
            end case;
         end if;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
         return 0;
   end Julia_Get_Mode;

   function Julia_Get_MQTT_Message (Topic_Name : chars_ptr)
      return value_t is
   begin
      declare
         Topic : constant String := Value (Topic_Name);
      begin
         declare
            Message : constant MQTT_Message :=
                               MQTT_State.Get_Message (Topic);
         begin
            return To_Julia (Message.Get_Message);
         end;
      exception
         when Reason : Constraint_Error =>
            Error ("Invalid topic: " & Exception_Message (Reason));
            return No_Value;
         when End_Error =>
            Error ("No topic " & Quote (Topic) & " exists");
            return No_Value;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
         return No_Value;
   end Julia_Get_MQTT_Message;

   function Julia_Get_Parameters (Box, Device : Unsigned_32)
      return value_t is
   begin
      declare
         Info   : constant Device_Parameters_Data_Handles.Handle :=
                     Get_Parameters
                     (  RF_Address (Box),
                        RF_Address (Device)
                     );
         Data   : Device_Parameters_Data'Class renames Info.Ptr.all;
         Param  : Device_Parameters renames Data.Parameters;
         Result : Tuple;
         Value  : value_t;
         Roots  : Holder (6);
      begin
         Value := To_Julia (Data.Kind_Of);
         Roots.Set (1, Value);
         Result.Add ("type", Value);
         Value := To_Julia (Unsigned_8 (Param.Room));
         Roots.Set (2, Value);
         Result.Add ("room_id", Value);
         Value := To_Julia (Data.Room);
         Roots.Set (3, Value);
         Result.Add ("room", Value);
         Value := To_Julia (Param.Serial_No);
         Roots.Set (4, Value);
         Result.Add ("serial_no", Value);
         Value := To_Julia (Param.Name);
         Roots.Set (5, Value);
         Result.Add ("name", Value);
         Value := To_Julia (Unsigned_32 (Param.Address));
         Roots.Set (6, Value);
         Result.Add ("address", Value);
         case Data.Kind_Of is
            when Cube | Shutter_Contact | Eco_Button | Unknown =>
               return To_Julia (Result);
            when Radiator_Thermostat..Wall_Thermostat =>
               declare
                  Roots : Holder (7);
               begin
                  Value := To_Julia (Param.Comfort);
                  Roots.Set (1, Value);
                  Result.Add ("comfort_temperature", Value);
                  Value := To_Julia (Param.Eco);
                  Roots.Set (2, Value);
                  Result.Add ("eco_temperature", Value);
                  Value := To_Julia (Param.Max);
                  Roots.Set (3, Value);
                  Result.Add ("maximum_temperature", Value);
                  Value := To_Julia (Param.Min);
                  Roots.Set (4, Value);
                  Result.Add ("minimum_temperature", Value);
                  Value := To_Julia (Param.Schedule);
                  Roots.Set (5, Value);
                  Result.Add ("schedule", Value);
                  Value := To_Julia (Param.Offset);
                  Roots.Set (6, Value);
                  Result.Add ("temperature_offset", Value);
                  Value := To_Julia (Param.Window_Open);
                  Roots.Set (7, Value);
                  Result.Add ("airing_temperature", Value);
                  case Data.Kind_Of is
                     when Radiator_Thermostat      |
                          Radiator_Thermostat_Plus =>
                        declare
                           Roots : Holder (6);
                        begin
                           Value :=
                              To_Julia (Minutes (Param.Window_Time));
                           Roots.Set (1, Value);
                           Result.Add ("airing_mode_delay", Value);
                           Value :=
                              To_Julia (Minutes (Param.Boost_Time));
                           Roots.Set (2, Value);
                           Result.Add ("boost_duration", Value);
                           Value := To_Julia (Param.Boost_Valve);
                           Roots.Set (3, Value);
                           Result.Add ("boost_valve_position", Value);
                           Value := To_Julia (Param.Decalcification);
                           Roots.Set (4, Value);
                           Result.Add ("decalcification_time", Value);
                           Value := To_Julia (Param.Max_Valve);
                           Roots.Set (5, Value);
                           Result.Add ("maximum_valve_position", Value);
                           Value := To_Julia (Param.Valve_Offset);
                           Roots.Set (6, Value);
                           Result.Add ("valve_offset", Value);
                           return To_Julia (Result);
                        end;
                     when others =>
                        return To_Julia (Result);
                  end case;
               end;
         end case;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
         return No_Value;
   end Julia_Get_Parameters;

   function Julia_Get_Set_Temperature (Box, Device : Unsigned_32)
      return C_float is
   begin
      declare
         None : aliased Boolean := True;
         Data : constant Device_Data :=
                         Get_Device_Data
                         (  RF_Address (Box),
                            RF_Address (Device),
                            None'Access
                         );
      begin
         case Data.Kind_Of is
            when Radiator_Thermostat..Wall_Thermostat =>
               return C_float (Data.Set_Temperature);
            when Cube | Shutter_Contact | Eco_Button =>
               Error ("The device is not a thermostat");
               return 0.0;
            when Unknown =>
               Error (No_Device_Exists);
               return 0.0;
         end case;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
         return 0.0;
   end Julia_Get_Set_Temperature;

   function Julia_Get_Status (Box, Device : Unsigned_32)
      return value_t is
   begin
      declare
         None : aliased Boolean := True;
         Data   : constant Device_Data :=
                           Get_Device_Data
                           (  RF_Address (Box),
                              RF_Address (Device),
                              None'Access
                           );
         Result : Tuple;
         Value  : value_t;
         Roots  : Holder (4);
      begin
         Value := To_Julia (Data.Kind_Of);
         Roots.Set (1, Value);
         Result.Add ("type", Value);
         Value := To_Julia (Unsigned_32 (Data.Address));
         Roots.Set (2, Value);
         Result.Add ("address", Value);
         Value := To_Julia (Data.Error);
         Roots.Set (3, Value);
         Result.Add ("error", Value);
         Value := To_Julia (Data.Initialized);
         Roots.Set (4, Value);
         Result.Add ("initialized", Value);
         case Data.Kind_Of is
            when Cube =>
               return To_Julia (Result);
            when Partner_Device_Type =>
               null;
            when Unknown =>
               Error (No_Device_Exists);
               return No_Value;
         end case;
         declare
            Roots : Holder (4);
         begin
            Value := To_Julia (Data.Panel_Locked);
            Roots.Set (1, Value);
            Result.Add ("panel_locked", Value);
            Value := To_Julia (Data.Battery_Low);
            Roots.Set (2, Value);
            Result.Add ("battery_low", Value);
            Value := To_Julia (Data.Link_Error);
            Roots.Set (3, Value);
            Result.Add ("link_error", Value);
            Value := To_Julia (Data.DST);
            Roots.Set (4, Value);
            Result.Add ("summer_time", Value);
            case Data.Kind_Of is
               when Shutter_Contact =>
                  declare
                     Roots : Holder (1);
                  begin
                     Value := To_Julia (Data.Open);
                     Roots.Set (Value);
                     Result.Add ("open", Value);
                     return To_Julia (Result);
                  end;
               when Radiator_Thermostat | Wall_Thermostat =>
                  declare
                     Roots : Holder (3);
                  begin
                     Value := To_Julia (Data.Mode);
                     Roots.Set (1, Value);
                     Result.Add ("mode", Value);
                     Value := To_Julia (Data.Set_Temperature);
                     Roots.Set (2, Value);
                     Result.Add ("set_temperature", Value);
                     Value := To_Julia (Data.New_Temperature);
                     Roots.Set (3, Value);
                     Result.Add ("new_temperature", Value);
                     if Data.Kind_Of = Wall_Thermostat then
                        if Data.Temperature /= Centigrade'First then
                           declare
                              Roots : Holder (1);
                           begin
                              Value := To_Julia (Data.Temperature);
                              Roots.Set (Value);
                              Result.Add ("temperature", Value);
                              return To_Julia (Result);
                           end;
                        else
                           return To_Julia (Result);
                        end if;
                     else
                        if Data.Temperature /= Centigrade'First then
                           declare
                              Roots : Holder (2);
                           begin
                              Value := To_Julia (Data.Valve_Position);
                              Roots.Set (1, Value);
                              Result.Add ("valve", Value);
                              Value := To_Julia (Data.Temperature);
                              Roots.Set (2, Value);
                              Result.Add ("temperature", Value);
                              return To_Julia (Result);
                           end;
                        else
                           declare
                              Roots : Holder (1);
                           begin
                              Value := To_Julia (Data.Valve_Position);
                              Roots.Set (Value);
                              Result.Add ("valve", Value);
                              return To_Julia (Result);
                           end;
                        end if;
                     end if;
                  end;
               when others =>
                  return To_Julia (Result);
            end case;
         end;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
         return No_Value;
   end Julia_Get_Status;

   function Julia_Get_Summer_Time (Box, Device : Unsigned_32)
      return value_t is
   begin
      declare
         None : aliased Boolean := True;
         Data : constant Device_Data :=
                         Get_Device_Data
                         (  RF_Address (Box),
                            RF_Address (Device),
                            None'Access
                         );
      begin
         if None then
            Error (No_Device_Exists);
            return No_Value;
         else
            return To_Julia (Data.DST);
         end if;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
         return No_Value;
   end Julia_Get_Summer_Time;

   function Julia_Get_Temperature (Box, Device : Unsigned_32)
      return C_float is
   begin
      declare
         None : aliased Boolean := True;
         Data : constant Device_Data :=
                         Get_Device_Data
                         (  RF_Address (Box),
                            RF_Address (Device),
                            None'Access
                         );
      begin
         case Data.Kind_Of is
            when Radiator_Thermostat..Wall_Thermostat =>
               if Data.Temperature = Centigrade'First then
                  Error ("The thermostat temperature is yet unknown");
                  return 0.0;
               end if;
               return C_float (Data.Temperature);
            when Cube | Shutter_Contact | Eco_Button =>
               Error (Not_Thermostat);
               return 0.0;
            when Unknown =>
               Error (No_Device_Exists);
               return 0.0;
         end case;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
         return 0.0;
   end Julia_Get_Temperature;

   function Julia_Get_Valve (Box, Device : Unsigned_32)
      return C_float is
   begin
      declare
         None : aliased Boolean := True;
         Data : constant Device_Data :=
                         Get_Device_Data
                         (  RF_Address (Box),
                            RF_Address (Device),
                            None'Access
                         );
      begin
         case Data.Kind_Of is
            when Radiator_Thermostat..Radiator_Thermostat_Plus =>
               return C_float (Data.Valve_Position);
            when Cube | Shutter_Contact | Eco_Button |
                 Wall_Thermostat =>
               Error (Not_Radiator);
               return 0.0;
            when Unknown =>
               Error (No_Device_Exists);
               return 0.0;
         end case;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
         return 0.0;
   end Julia_Get_Valve;

   function Julia_Get_Valve_Statistics (Box : Unsigned_32)
      return value_t is
   begin
      declare
         Average : Natural := 0;
         Min     : Natural := 0;
         Max     : Natural := 0;
      begin
         MAX_IO.Get_Valves
         (  Box     => RF_Address (Box),
            Average => Average,
            Min     => Min,
            Max     => Max
         );
         declare
            Result : Tuple;
            Value  : value_t;
            Roots  : Holder (3);
         begin
            Value := To_Julia (C_float (Average));
            Roots.Set (1, Value);
            Result.Add ("average", Value);
            Value := To_Julia (C_float (Min));
            Roots.Set (2, Value);
            Result.Add ("minimum", Value);
            Value := To_Julia (C_float (Max));
            Roots.Set (3, Value);
            Result.Add ("maximum", Value);
            return To_Julia (Result);
         end;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
         return No_Value;
   end Julia_Get_Valve_Statistics;

   procedure Julia_Publish_MQTT
             (  Topic   : chars_ptr;
                Message : chars_ptr;
                Policy  : unsigned_char
             )  is
      use GNAT.Sockets.MQTT.Server;
      function Get_Policy return Message_Type is
      begin
         case Policy is
            when 0 => return Transient;
            when 1 => return Retained;
            when 2 => return Updated;
            when 3 => return Initial;
            when 4 => return Ignored;
            when others =>
               raise Data_Error with "Invalid policy value";
         end case;
      end Get_Policy;
   begin
      MQTT_State.Publish
      (  Topic   => Value (Topic),
         Message => Value (Message),
         Policy  => Get_Policy
      );
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
   end Julia_Publish_MQTT;

   procedure Julia_Reboot (Box : chars_ptr) is
   begin
      Reboot (Value (Box));
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
   end Julia_Reboot;

   procedure Julia_Reconnect (Box : Unsigned_32) is
   begin
      declare
         This : constant Cube_Client_Handle :=
                         Get_Cube (RF_Address (Box));
      begin
         if This.Is_Valid then
            if not This.Ptr.Is_Connected then
               This.Ptr.Reconnect (True, True);
            end if;
         else
            Error (No_Cube_Exists);
         end if;
      end;
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
   end Julia_Reconnect;

   procedure Julia_Set_Mode
             (  Mode        : unsigned_char;
                Box         : Unsigned_32;
                Device      : Unsigned_32;
                Temperature : C_float;
                Disposition : unsigned_char;
                Deadline    : value_t
             )  is
      The_Mode        : Operating_Mode;
      The_Disposition : Temperature_Mode := Absolute;
      The_Temperature : Centigrade       := Centigrade'First;
      Up_Until        : Time;
   begin
      case Mode is
         when 0 =>
            The_Mode := Automatic;
         when 1 =>
            The_Mode := Manual;
         when 2 =>
            The_Mode := Boost;
         when 3 =>
            The_Mode := Vacation;
         when others =>
            Error ("Invalid operating mode");
            return;
      end case;
      Up_Until := Value (Deadline);
      case Disposition is
         when 0 =>
            The_Disposition := Absolute;
         when 1 =>
            The_Disposition := Airing;
         when 2 =>
            The_Disposition := Comfort;
         when 3 =>
            The_Disposition := Decrement;
         when 4 =>
            The_Disposition := Eco;
         when 5 =>
            The_Disposition := Increment;
         when others =>
            Error ("Invalid temperature disposition");
      end case;
      case The_Disposition is
         when Absolute | Increment | Decrement =>
            if Temperature = 0.0 and then The_Mode /= Automatic then
               Error ("No temperature specified");
            end if;
            begin
               The_Temperature := Centigrade (Temperature);
            exception
               when others =>
                  raise Data_Error with "Temperature is out of range";
            end;
         when Airing | Comfort | Eco =>
            if Temperature /= 0.0 then
               Error
               (  "Temperature cannot be specified for " &
                  "airing, comfort, eco disposition"
               );
            end if;
      end case;
      case The_Mode is
         when Automatic =>
            if Up_Until > Time_Of (2020, 1, 1) then
               Error
               (  "Until argument cannot be specified for "
               &  "automatic mode"
               );
               return;
            end if;
         when Manual =>
            if Up_Until > Time_Of (2020, 1, 1) then
               Error
               (  "Until argument cannot be specified for "
               &  "manual mode"
               );
               return;
            elsif Temperature = 0.0 then
               Error
               (  "Temperature must be specified for manual mode"
               );
               return;
            end if;
         when Boost =>
            if Up_Until > Time_Of (2020, 1, 1) then
               Error
               (  "Until argument cannot be specified for "
               &  "boost mode"
               );
               return;
            elsif Temperature = 0.0 then
               Error ("Temperature must be specified for boost mode");
               return;
            end if;
         when Vacation =>
            if Up_Until <= Time_Of (2020, 1, 1) then
               Error
               (  "Until argument must be specified for "
               &  "vacation mode"
               );
               return;
            elsif Temperature = 0.0 then
               Error
               (  "Temperature must be specified for vacation mode"
               );
               return;
            end if;
      end case;
      Set_Thermostat_Mode
      (  Box         => RF_Address (Box),
         Device      => RF_Address (Device),
         Mode        => The_Mode,
         Temperature => The_Temperature,
         Up_Until    => Up_Until,
         Disposition => The_Disposition
      );
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
   end Julia_Set_Mode;

   procedure Julia_Trace (Text : chars_ptr) is
   begin
      MAX_IO.Trace (Value (Text), Message_Text);
   exception
      when Reason : others =>
         Error (Exception_Message (Reason));
   end Julia_Trace;

   function Init (File : String) return function_t is
      Result : value_t;
      Code   : Char_Buffer_Ptr := null;
   begin
      if not Initialized then
         Init;
         Eval_String
         (  "module ELV_MAX_Cube"                                 & CRLF
         &     "import Dates"                                     & CRLF
         &     "@enum Battery_Charge begin"                       & CRLF
         &        "low=0"                                         & CRLF
         &        "high=1"                                        & CRLF
         &     "end"                                              & CRLF
         &     "@enum Connection_State begin"                     & CRLF
         &        "connected=0"                                   & CRLF
         &        "disconnected=1"                                & CRLF
         &        "undefined=2"                                   & CRLF
         &     "end"                                              & CRLF
         &     "function to_connection_state(value::UInt8);"      & CRLF
         &        "Connection_State(value)"                       & CRLF
         &     "end"                                              & CRLF
         &     "@enum Device_Type begin"                          & CRLF
         &        "unknown=0"                                     & CRLF
         &        "cube=1"                                        & CRLF
         &        "radiator_thermostat=2"                         & CRLF
         &        "radiator_thermostat_plus=3"                    & CRLF
         &        "wall_thermostat=4"                             & CRLF
         &        "shutter_contact=5"                             & CRLF
         &        "eco_button=6"                                  & CRLF
         &     "end"                                              & CRLF
         &     "function to_device_type(value::UInt8);"           & CRLF
         &        "Device_Type(value)"                            & CRLF
         &     "end"                                              & CRLF
         &     "@enum Link_State begin"                           & CRLF
         &        "ok=0"                                          & CRLF
         &        "error=1"                                       & CRLF
         &     "end"                                              & CRLF
         &     "function to_link_state(value::UInt8);"            & CRLF
         &        "Link_State(value)"                             & CRLF
         &     "end"                                              & CRLF
         &     "@enum MQTT_Policy begin"                          & CRLF
         &        "transient=0"                                   & CRLF
         &        "retained=1"                                    & CRLF
         &        "updated=2"                                     & CRLF
         &        "initial=3"                                     & CRLF
         &        "ignored=4"                                     & CRLF
         &     "end"                                              & CRLF
         &     "function to_mqtt_policy(value::UInt8);"           & CRLF
         &        "MQTT_Policy(value)"                            & CRLF
         &     "end"                                              & CRLF
         &     "@enum Operating_Mode begin"                       & CRLF
         &        "automatic=0"                                   & CRLF
         &        "manual=1"                                      & CRLF
         &        "boost=2"                                       & CRLF
         &        "vacation=3"                                    & CRLF
         &     "end"                                              & CRLF
         &     "function to_operating_mode(value::UInt8);"        & CRLF
         &        "Operating_Mode(value)"                         & CRLF
         &     "end"                                              & CRLF
         &     "@enum Temperature_Disposition begin"              & CRLF
         &        "absolute=0"                                    & CRLF
         &        "airing=1"                                      & CRLF
         &        "comfort=2"                                     & CRLF
         &        "decrement=3"                                   & CRLF
         &        "eco=4"                                         & CRLF
         &        "increment=5"                                   & CRLF
         &     "end"                                              & CRLF
         &     "function to_temperature_disposition"
         &              "(value::UInt8);"                         & CRLF
         &        "Temperature_Disposition(value)"                & CRLF
         &     "end"                                              & CRLF
         &     "@enum Week_Day begin"                             & CRLF
         &        "Mo=1"                                          & CRLF
         &        "Tu=2"                                          & CRLF
         &        "We=3"                                          & CRLF
         &        "Th=4"                                          & CRLF
         &        "Fr=5"                                          & CRLF
         &        "Sa=6"                                          & CRLF
         &        "Su=7"                                          & CRLF
         &     "end"                                              & CRLF
         &     "function to_week_day(value::UInt8);"              & CRLF
         &        "Week_Day(value)"                               & CRLF
         &     "end"                                              & CRLF
         &     "export disconnect, get_battery, get_connection,"
         &            "get_cubes_list, get_devices_list, trace, "
         &            "get_duty, get_link, get_mqtt_message, "
         &            "get_parameters, get_status, "
         &            "get_summer_time, get_temparature, get_valve, "
         &            "get_valve_statistics,publish_mqtt,reboot,"
         &            "reconnect"                                 & CRLF
         &     "function disconnect(cube::UInt32); ccall("
         &        CCall_Address (Julia_Disconnect'Address)
         &        ",Cvoid,(Cuint,),cube)"                         & CRLF
         &        "return nothing"                                & CRLF
         &     "end"                                              & CRLF
         &     "function get_battery(cube::UInt32,device::UInt32);"
         &        "Battery_Charge(ccall("
         &        CCall_Address (Julia_Get_Battery'Address)
         &        ",Cuchar,(Cuint,Cuint),cube,device))"           & CRLF
         &     "end"                                              & CRLF
         &     "function get_connection(cube::UInt32);"           & CRLF
         &        "Connection_State(ccall("                       & CRLF
         &        CCall_Address (Julia_Get_Connection'Address)
         &        ",Cuchar,(Cuint,),cube))"                       & CRLF
         &     "end"                                              & CRLF
         &     "function get_cubes_list(); ccall("
         &        CCall_Address (Julia_Get_Cubes_List'Address)
         &        ",Array{Cuint},())"                             & CRLF
         &     "end"                                              & CRLF
         &     "function get_devices_list("
         &              "cube::UInt32,"
         &              "filter::Vector{Device_Type}=Device_Type[]);"
         &        "selector = Array{Int8}(undef,length(filter))"  & CRLF
         &        "for i in 1:length(filter)"                     & CRLF
         &           "selector[i] = Int8(filter[i])"              & CRLF
         &        "end"                                           & CRLF
         &        "ccall("
         &        CCall_Address (Julia_Get_Devices_List'Address)
         &        ",Vector{Cuint},(Cuint,Any),"
         &         "cube,selector)"                               & CRLF
         &     "end"                                              & CRLF
         &     "function get_duty(cube::UInt32); ccall("
         &        CCall_Address (Julia_Get_Duty'Address)
         &        ",Cfloat,(Cuint,),cube)"                        & CRLF
         &     "end"                                              & CRLF
         &     "function get_link(cube::UInt32,device::UInt32);"
         &        "Link_State(ccall("
         &        CCall_Address (Julia_Get_Link'Address)
         &        ",Cuchar,(Cuint,Cuint),cube,device))"           & CRLF
         &     "end"                                              & CRLF
         &     "function get_mode(cube::UInt32,device::UInt32);"
         &        "Operating_Mode(ccall("
         &        CCall_Address (Julia_Get_Mode'Address)
         &        ",Cuchar,(Cuint,Cuint),cube,device))"           & CRLF
         &     "end"                                              & CRLF
         &     "function get_mqtt_message(topic::AbstractString);"
         &        "ccall("
         &        CCall_Address (Julia_Get_MQTT_Message'Address)
         &        ",Cvoid,(Cstring,),topic)"                      & CRLF
         &     "end"                                              & CRLF
         &     "function get_parameters(cube::UInt32,device::UInt32);"
         &        "ccall("
         &        CCall_Address (Julia_Get_Parameters'Address)
         &        ",Any,(Cuint,Cuint),cube,device)"               & CRLF
         &     "end"                                              & CRLF
         &     "function get_set_temperature("
         &        "cube::UInt32,device::UInt32); ccall("
         &        CCall_Address (Julia_Get_Set_Temperature'Address)
         &        ",Cfloat,(Cuint,Cuint),cube,device)"            & CRLF
         &     "end"                                              & CRLF
         &     "function get_status(cube::UInt32,device::UInt32);"
         &        "ccall("
         &        CCall_Address (Julia_Get_Status'Address)
         &        ",Any,(Cuint,Cuint),cube,device)"               & CRLF
         &     "end"                                              & CRLF
         &     "function get_summer_time(cube::UInt32,device::UInt32);"
         &        "ccall("
         &        CCall_Address (Julia_Get_Summer_Time'Address)
         &        ",Bool,(Cuint,Cuint),cube,device)"              & CRLF
         &     "end"                                              & CRLF
         &     "function get_temperature(cube::UInt32,device::UInt32);"
         &        "ccall("
         &        CCall_Address (Julia_Get_Temperature'Address)
         &        ",Float32,(Cuint,Cuint),cube,device)"           & CRLF
         &     "end"                                              & CRLF
         &     "function get_valve(cube::UInt32,device::UInt32);"
         &        "ccall("
         &        CCall_Address (Julia_Get_Valve'Address)
         &        ",Float32,(Cuint,Cuint),cube,device)"           & CRLF
         &     "end"                                              & CRLF
         &     "function get_valve_statistics(cube::UInt32);"
         &        "ccall("
         &        CCall_Address (Julia_Get_Valve_Statistics'Address)
         &        ",Any,(Cuint,),cube)"                           & CRLF
         &     "end"                                              & CRLF
         &     "function publish_mqtt("
         &        "topic::AbstractString,"
         &        "message::AbstractString,"
         &        "policy::MQTT_Policy=updated"
         &        "); ccall("
         &        CCall_Address (Julia_Publish_MQTT'Address)
         &        ",Cvoid,(Cstring,Cstring,Cuchar),"
         &        "topic,message,policy)"                         & CRLF
         &        "return nothing"                                & CRLF
         &     "end"                                              & CRLF
         &     "function reboot(cube::AbstractString); ccall("
         &        CCall_Address (Julia_Reboot'Address)
         &        ",Cvoid,(Cstring,),cube)"                       & CRLF
         &     "end"                                              & CRLF
         &     "function reconnect(cube::UInt32); ccall("
         &        CCall_Address (Julia_Reconnect'Address)
         &        ",Cvoid,(Cuint,),cube)"                         & CRLF
         &     "end"                                              & CRLF
         &     "function set_mode("
         &        "mode::Operating_Mode,"
         &        "cube::UInt32,"
         &        "device::UInt32=0,"
         &        "temperature::Float32=0.0f0,"
         &        "disposition::Temperature_Disposition=absolute,"
         &        "until::Dates.DateTime=Dates.DateTime(2020)"
         &        "); ccall("
         &        CCall_Address (Julia_Set_Mode'Address)
         &        ",Cvoid,(Cuchar,Cuint,Cuint,Cfloat,Cuchar,Any),"
         &        "mode,cube,device,temperature,"
         &        "disposition,until)"                            & CRLF
         &     "end"                                              & CRLF
         &     "function trace(text::AbstractString); ccall("
         &        CCall_Address (Julia_Trace'Address)
         &        ",Cvoid,(Cstring,),text)"                       & CRLF
         &        "return nothing"                                & CRLF
         &     "end"                                              & CRLF
         &  "end"                                                 & CRLF
         &  "using .ELV_MAX_Cube: to_connection_state as "
         &      "elv_max_cube_to_connection_state"                & CRLF
         &  "using .ELV_MAX_Cube: to_device_type as "
         &      "elv_max_cube_to_device_type"                     & CRLF
         &  "using .ELV_MAX_Cube: to_link_state as "
         &      "elv_max_cube_to_link_state"                      & CRLF
         &  "using .ELV_MAX_Cube: to_mqtt_policy as "
         &      "elv_max_cube_to_mqtt_policy"                     & CRLF
         &  "using .ELV_MAX_Cube: to_operating_mode as "
         &      "elv_max_cube_to_operating_mode"                  & CRLF
         &  "using .ELV_MAX_Cube: to_temperature_disposition as "
         &      "elv_max_cube_to_temperature_disposition"         & CRLF
         &  "using .ELV_MAX_Cube: to_week_day as "
         &      "elv_max_cube_to_week_day"
         );
         Initialized := True;
      end if;
      if File = "" then
          raise Name_Error with "No Julia file specified";
      end if;
      if Code /= null then
         Code.Length := 0;
      end if;
      declare
         Buffer : Stream_Element_Array (1..1024);
         Last   : Stream_Element_Offset;
         Stream : File_Type;
      begin
         begin
            Open (Stream, In_File, File);
         exception
            when Error : others =>
               Raise_Exception
               (  Exception_Identity (Error),
                  "Cannot open file " &
                  Quote (File)        &
                  ". "                &
                  Exception_Message (Error)
               );
         end;
         begin
            loop
               Read (Stream, Buffer, Last);
               exit when Last < Buffer'First;
               Append (Code, Buffer (1..Last));
               exit when Last < Buffer'Last;
            end loop;
            Close (Stream);
         exception
            when End_Error =>
               Close (Stream);
            when others =>
               Close (Stream);
               raise;
         end;
      exception
         when Error : others =>
            Raise_Exception
            (  Exception_Identity (Error),
               "Cannot read file " &
               Quote (File)        &
               ". "                &
               Exception_Message (Error)
            );
      end;
      if Code =null or else Code.Length = 0 then
         Raise_Exception
         (  Julia_Error'Identity,
            "File " & Quote (File) & " is empty"
         );
      end if;
      Eval_char_array (Code.Text (1..Code.Length));
      begin
         To_Connection_State :=
            Get_Function
            (  Main_Module,
               "elv_max_cube_to_connection_state"
            );
         To_Device_Type :=
            Get_Function
            (  Main_Module,
               "elv_max_cube_to_device_type"
            );
         To_Link_State :=
            Get_Function
            (  Main_Module,
               "elv_max_cube_to_link_state"
            );
         To_MQTT_Policy :=
            Get_Function
            (  Main_Module,
               "elv_max_cube_to_mqtt_policy"
            );
         To_Operating_Mode :=
            Get_Function
            (  Main_Module,
               "elv_max_cube_to_operating_mode"
            );
         To_Temperature_Disposition :=
            Get_Function
            (  Main_Module,
               "elv_max_cube_to_temperature_disposition"
            );
         To_Week_Day :=
            Get_Function
            (  Main_Module,
               "elv_max_cube_to_week_day"
            );
         return Get_Function (Main_Module, "controller");
      exception
         when End_Error =>
            raise End_Error with
                  "File "      &
                  Quote (File) &
                  " does not contain function 'controller'";
      end;
   end Init;

end Julia.ELV_MAX_Cube;
