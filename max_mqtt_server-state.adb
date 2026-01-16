--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_MQTT_Server.State                       Luebeck            --
--  Implementation                                 Spring, 2016       --
--                                                                    --
--                                Last revision :  10:42 30 Sep 2023  --
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

with Ada.Calendar;                 use Ada.Calendar;
with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Strings.Maps.Constants;   use Ada.Strings.Maps.Constants;
with MAX_Trace;                    use MAX_Trace;
with MAX_IO;                       use MAX_IO;
with MAX_IO.Set_Mode;              use MAX_IO.Set_Mode;
with MAX_IO.Set_Schedule;          use MAX_IO.Set_Schedule;
with MAX_IO.Storing_Parameters;    use MAX_IO.Storing_Parameters;
with Strings_Edit;                 use Strings_Edit;
with Strings_Edit.Floats;          use Strings_Edit.Floats;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Strings_Edit.UTF8.Wildcards;  use Strings_Edit.UTF8.Wildcards;

package body MAX_MQTT_Server.State is

   procedure Disable_Publishing (Server : in out MAX_Server_State) is
   begin
      Server.List.Set (new Topics_List);
   end Disable_Publishing;

   procedure Enable_Publishing
             (  Server : in out MAX_Server_State;
                List   : Topic_Sets.Set
             )  is
      Allowed : Topic_List_Handles.Handle;
   begin
      Allowed.Set (new Topics_List);
      Allowed.Ptr.Enabled := True;
      Allowed.Ptr.List    := List;
      Server.List         := Allowed;
   end Enable_Publishing;

   procedure Received
             (  Server  : in out MAX_Server_State;
                Client  : in out MQTT_Connection'Class;
                Topic   : String;
                Message : Stream_Element_Array;
                QoS     : QoS_Level;
                Policy  : in out Message_Type
             )  is
      procedure Get_Temperature
                (  Text        : String;
                   Pointer     : in out Integer;
                   Temperature : out Centigrade;
                   Disposition : out Temperature_Mode
                )  is
         Value : Float;
      begin
         case Text (Pointer) is
            when '+' =>
               Disposition := Increment;
               Pointer := Pointer + 1;
            when '-' =>
               Disposition := Decrement;
               Pointer := Pointer + 1;
            when '0'..'9' =>
               Disposition := Absolute;
            when 'A' | 'a' =>
               if Is_Prefix
                  (  "airing",
                     Text,
                     Pointer,
                     Lower_Case_Map
                  )  then
                  Temperature := 19.0;
                  Disposition := Airing;
                  Pointer     := Pointer + 6;
                  return;
               else
                  raise Constraint_Error;
               end if;
            when 'E' | 'e' =>
               if Is_Prefix
                  (  "eco",
                     Text,
                     Pointer,
                     Lower_Case_Map
                  )  then
                  Temperature := 19.0;
                  Disposition := Eco;
                  Pointer     := Pointer + 3;
                  return;
               else
                  raise Constraint_Error;
               end if;
            when 'C' | 'c' =>
               if Is_Prefix
                  (  "comfort",
                     Text,
                     Pointer,
                     Lower_Case_Map
                  )  then
                  Temperature := 19.0;
                  Disposition := Comfort;
                  Pointer     := Pointer + 7;
                  return;
               else
                  raise Constraint_Error;
               end if;
            when others =>
               raise Constraint_Error;
         end case;
         Get
         (  Source  => Text,
            Pointer => Pointer,
            Value   => Value,
            First   => 0.0,
            Last    => 60.0
         );
         Temperature := Centigrade (Value);
      end Get_Temperature;

      procedure Check_External_Topics is
         List   : constant Topic_List_Handles.Handle := Server.List;
         Topics : Topics_List'Class renames List.Ptr.all;
      begin
         if not Topics.Enabled then
            Policy := Ignored;
            if Server.Is_Tracing_On (Trace_Pubishing) then
               Trace
               (  (  "Publising disabled, ignoring topic "
                  &  Topic
                  ),
                  Message_Text
               );
            end if;
            return;
         end if;
         if Topics.List.Is_Empty then -- Enable everything
            if Server.Is_Tracing_On (Trace_Pubishing) then
               Trace
               (  (  "Publising "
                  &  Topic
                  &  "="
                  &  To_String (Message)
                  ),
                  Message_Text
               );
            end if;
            return;
         end if;
         for Index in 1..Topics.List.Get_Size loop
            if Match (Topic, Topics.List.Get (Index)) then
               if Server.Is_Tracing_On (Trace_Pubishing) then
                  Trace
                  (  (  "Publising "
                     &  Topic
                     &  "="
                     &  To_String (Message)
                     ),
                     Message_Text
                  );
               end if;
               return;
            end if;
         end loop;
         if Server.Is_Tracing_On (Trace_Pubishing) then
            Trace
            (  (  "Ignoring topic "
               &  Topic
               &  " (not allowed for publishing)"
               ),
               Message_Text
            );
         end if;
      exception
         when Error : others => -- Topic or pattern is illegal
            Policy := Ignored;
            if Server.Is_Tracing_On (Trace_Pubishing) then
               Trace
               (  (  "Publising "
                  &  Topic
                  &  " failed: "
                  &  Exception_Message (Error)
                  ),
                  Error_Text
               );
            end if;
      end Check_External_Topics;

      Cube : RF_Address;

      procedure Do_Cube (Connect : Boolean) is
         This : constant Cube_Client_Handle := Get_Cube (Cube);
      begin
         if This.Is_Valid then
            if Connect then
               if not This.Ptr.Is_Connected then
                  This.Ptr.Reconnect (True, True);
               end if;
            else
               if This.Ptr.Is_Connected then
                  This.Ptr.Reconnect (False, True);
               end if;
            end if;
         else
            Trace
            (  (  "Publishing cube connection control on non-existing "
               &  "cube "
               &  Image (Cube)
               ),
               Message_Text
            );
            Policy := Ignored;
         end if;
      end Do_Cube;

      Device : RF_Address;

      procedure Do_Automatic
                (  Data    : String;
                   Pointer : in out Integer
                )  is
         Temperature : Centigrade;
         Disposition : Temperature_Mode;
      begin
         if Pointer > Data'Last then
            Set_Thermostat_Mode
            (  Box         => Cube,
               Device      => Device,
               Mode        => Automatic,
               Silent      => True
            );
         else
            Get_Temperature
            (  Data,
               Pointer,
               Temperature,
               Disposition
            );
            if Pointer <= Data'Last then
               Policy := Ignored;
               return;
            end if;
            Set_Thermostat_Mode
            (  Box         => Cube,
               Device      => Device,
               Mode        => Automatic,
               Temperature => Temperature,
               Disposition => Disposition,
               Silent      => True
            );
         end if;
      end Do_Automatic;

      procedure Do_Manual
                (  Data    : String;
                   Pointer : in out Integer
                )  is
         Temperature : Centigrade;
         Disposition : Temperature_Mode;
      begin
         Get_Temperature (Data, Pointer, Temperature, Disposition);
         if Pointer <= Data'Last then
            Policy := Ignored;
            return;
         end if;
         Set_Thermostat_Mode
         (  Box         => Cube,
            Device      => Device,
            Mode        => Manual,
            Temperature => Temperature,
            Disposition => Disposition,
            Silent      => True
         );
      end Do_Manual;

      procedure Do_Vacation
                (  Data    : String;
                   Pointer : in out Integer
                )  is
         Value       : Float;
         Temperature : Centigrade;
         Disposition : Temperature_Mode;
      begin
         Get_Temperature
         (  Data,
            Pointer,
            Temperature,
            Disposition
         );
         Get (Data, Pointer);
         Get
         (  Source  => Data,
            Pointer => Pointer,
            Value   => Value,
            First   => 0.0
         );
         Get (Data, Pointer);
         if Data (Pointer..Data'Last) = "weeks" then
            Value := Value * 60.0 * 60.0 * 24.0 * 7.0;
         elsif Data (Pointer..Data'Last) = "days" then
            Value := Value * 60.0 * 60.0 * 24.0;
         elsif Data (Pointer..Data'Last) = "hours" then
            Value := Value * 60.0 * 60.0;
         elsif Data (Pointer..Data'Last) = "minutes" then
            Value := Value * 60.0;
         else
            Policy := Ignored;
            return;
         end if;
         Set_Thermostat_Mode
         (  Box         => Cube,
            Device      => Device,
            Mode        => Vacation,
            Temperature => Temperature,
            Disposition => Disposition,
            Up_Until    => Clock + Duration (Value),
            Silent      => True
         );
      end Do_Vacation;

      Pointer : Integer := Topic'First;
   begin
--        if Topic (Pointer) /= '/' then
--           return;
--        end if;
--        Pointer := Pointer + 1;
      if Pointer > Topic'Last then
         return;
      end if;
      if Topic = "reboot" then
         if Message'Length /= 10 then
            Trace ("Invalid MAX! cube serial number", Message_Text);
         else
            begin
               Trace
               (  "Rebooting the cube " & To_String (Message) & "...",
                  Mode_Text
               );
               Reboot (To_String (Message));
            exception
               when Error : others =>
                  Trace
                  (  (  "Rebooting MAX! cube fault: "
                     &  Exception_Message (Error)
                     ),
                     Message_Text
                  );
            end;
         end if;
         Policy := Ignored;
         return;
      end if;
      case Topic (Pointer) is
         when 'a'..'f' | 'A'..'F' | '0'..'9' =>
            begin
               Get
               (  Source  => Topic,
                  Pointer => Pointer,
                  Value   => Integer (Cube),
                  First   => 1,
                  Last    => 16#FFFFFF#,
                  Base    => 16
               );
            exception
               when others =>
                  Check_External_Topics;
                  return;
            end;
         when others =>
            Check_External_Topics;
            return;
      end case;
      if Is_Prefix ("/set/", Topic, Pointer) then
         Pointer := Pointer + 5;
         case Topic (Pointer) is
            when '0'..'9' =>
               Get
               (  Source  => Topic,
                  Pointer => Pointer,
                  Value   => Integer (Device),
                  First   => 1,
                  Last    => 16#FFFFFF#,
                  Base    => 16
               );
            when others =>
               Device  := 0;
               Pointer := Pointer - 1;
         end case;
         if Topic (Pointer..Topic'Last) = "/automatic" then
            declare
               Data    : constant String := To_String (Message);
               Pointer : Integer := Data'First;
            begin
               Do_Automatic (Data, Pointer);
            end;
         elsif Topic (Pointer..Topic'Last) = "/manual" then
            declare
               Data    : constant String := To_String (Message);
               Pointer : Integer := Data'First;
            begin
               Do_Manual (Data, Pointer);
            end;
         elsif Topic (Pointer..Topic'Last) = "/boost" then
            Set_Thermostat_Mode
            (  Box    => Cube,
               Device => Device,
               Mode   => Boost,
               Silent => True
            );
         elsif Topic (Pointer..Topic'Last) = "/vacation" then
            declare
               Data    : constant String := To_String (Message);
               Pointer : Integer := Data'First;
            begin
               Do_Vacation (Data, Pointer);
            end;
         elsif Topic (Pointer..Topic'Last) = "/mode" then
            declare
               Data    : constant String := To_String (Message);
               Pointer : Integer := Data'First;
            begin
               if Is_Prefix ("boost", Data, Pointer) then
                  Set_Thermostat_Mode
                  (  Box    => Cube,
                     Device => Device,
                     Mode   => Boost,
                     Silent => True
                  );
               elsif Is_Prefix ("automatic", Data, Pointer) then
                  Pointer := Pointer + 9;
                  Get (Data, Pointer);
                  Do_Automatic (Data, Pointer);
               elsif Is_Prefix ("manual", Data, Pointer) then
                  Pointer := Pointer + 6;
                  Get (Data, Pointer);
                  Do_Manual (Data, Pointer);
               elsif Is_Prefix ("vacation", Data, Pointer) then
                  Pointer := Pointer + 8;
                  Get (Data, Pointer);
                  Do_Vacation (Data, Pointer);
               else
                  Policy := Ignored;
               end if;
            end;
         elsif Topic (Pointer..Topic'Last) = "/schedule" then
            declare
               Data     : constant String := To_String (Message);
               Pointer  : Integer := Data'First;
               Schedule : Week_Schedule;
            begin
               Get (Data, Pointer, Schedule);
               if Pointer <= Data'Last then
                  Policy := Ignored;
                  return;
               end if;
               Set_Thermostat_Schedule
               (  Box      => Cube,
                  Device   => Device,
                  Schedule => Schedule,
                  Silent   => True
               );
            exception
               when Error : others =>
                  Trace
                   (  (  "Publishing schedule with the syntax error: "
                      &  Exception_Message (Error)
                      ),
                      Message_Text
                  );
                  Policy := Ignored;
            end;
         elsif Topic (Pointer..Topic'Last) = "/eco" then
            declare
               Data        : constant String := To_String (Message);
               Pointer     : Integer := Data'First;
               Temperature : Centigrade;
               Disposition : Temperature_Mode;
            begin
               Get_Temperature
               (  Data,
                  Pointer,
                  Temperature,
                  Disposition
               );
               if Pointer <= Data'Last then
                  Policy := Ignored;
                  return;
               end if;
               if Disposition /= Absolute then
                  Trace
                   (  "Published eco temperature is not absolute",
                      Message_Text
                  );
                  Policy := Ignored;
               else
                  begin
                     declare
                        Parameters : Device_Parameters :=
                                     Get_Parameters
                                     (  Cube,
                                        Device
                                     ) .Ptr.Parameters;
                     begin
                        Parameters.Eco := Temperature;
                        Store_Configuration
                        (  Box        => Cube,
                           Device     => Device,
                           Parameters => Parameters,
                           Action     => Store_Parameters
                        );
                     end;
                  exception
                     when Error : others =>
                        Trace
                         (  (  "Publishing thermostat eco temperature "
                            &  "error: "
                            &  Exception_Message (Error)
                            ),
                            Message_Text
                        );
                        Policy := Ignored;
                  end;
               end if;
            end;
         else
            Policy := Ignored;
         end if;
      elsif Topic (Pointer..Topic'Last) = "/reconnect" then
         Do_Cube (True);
      elsif Topic (Pointer..Topic'Last) = "/disconnect" then
         Do_Cube (False);
      else
         Check_External_Topics;
      end if;
   exception
      when others =>
         Policy := Ignored;
   end Received;

   State : aliased MAX_Server_State;

begin
   MQTT_State := State'Unchecked_Access;
end MAX_MQTT_Server.State;
