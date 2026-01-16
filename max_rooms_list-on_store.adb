--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.On_Store                     Luebeck            --
--  Separate body                                  Summer, 2015       --
--                                                                    --
--                                Last revision :  08:36 05 May 2020  --
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

separate (MAX_Rooms_List)
   procedure On_Store
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
   function Date return Time is
      use Strings_Edit;
      Text    : constant String := List.Date.Get_Text;
      Pointer : Integer := Text'First;
      Hour    : Integer := 0;
      Minute  : Integer := 0;
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
   begin
      Get (Text, Pointer);
      begin
         Get (Text, Pointer, Year, First => 2000, Last => 2063);
      exception
         when End_Error =>
            Say
            (  (  "Year number is expected at "
               &  Text (Pointer..Text'Last)
               ),
               "End date error"
            );
            raise Data_Error;
         when others =>
            Say
            (  (  "Year number "
               &  Image (Year)
               &  " is not in 2000..2063"
               ),
               "End date error"
            );
            raise Data_Error;
      end;
      Get (Text, Pointer);
      if Pointer > Text'Last and then Text (Pointer) /= '-' then
         Say
         (  "Minus sign (-) is expected after the year number",
            "End date error"
         );
         raise Data_Error;
      end if;
      Pointer := Pointer + 1;
      Get (Text, Pointer);
      begin
         Get (Text, Pointer, Month);
      exception
         when End_Error =>
            Say
            (  (  "Month number is expected at "
               &  Text (Pointer..Text'Last)
               ),
               "End date error"
            );
            raise Data_Error;
         when others =>
            Say
            (  "Month number " & Image (Month) & " is not in 1..12",
               "End date error"
            );
            raise Data_Error;
      end;
      Get (Text, Pointer);
      if Pointer > Text'Last and then Text (Pointer) /= '-' then
         Say
         (  "Minus sign (-) is expected after the month number",
            "End date error"
         );
         raise Data_Error;
      end if;
      Pointer := Pointer + 1;
      Get (Text, Pointer);
      begin
         Get (Text, Pointer, Day);
      exception
         when End_Error =>
            Say
            (  (  "Day number is expected at "
               &  Text (Pointer..Text'Last)
               ),
               "End date error"
            );
            raise Data_Error;
         when others =>
            Say
            (  "Day number " & Image (Day) & " is not in 1..31",
               "End date error"
            );
            raise Data_Error;
      end;
      Get (Text, Pointer);
      if Pointer <= Text'Last then
         begin
            Get (Text, Pointer, Hour, First => 0, Last => 23);
         exception
            when End_Error =>
               Say
               (  (  "Hour number is expected at "
                  &  Text (Pointer..Text'Last)
                  ),
                  "End date error"
               );
               raise Data_Error;
            when others =>
               Say
               (  "Hour number " & Image (Hour) & " is not in 0..23",
                  "End date error"
               );
               raise Data_Error;
         end;
         Get (Text, Pointer);
         if Pointer > Text'Last and then Text (Pointer) /= ':' then
            Say
            (  "Colon (:) is expected after the hour number",
               "End date error"
            );
            raise Data_Error;
         end if;
         Pointer := Pointer + 1;
         Get (Text, Pointer);
         if Pointer <= Text'Last then
            begin
               Get (Text, Pointer, Minute, First => 0, Last => 59);
               Minute := (Minute / 30) * 30;
            exception
               when End_Error =>
                  Say
                  (  (  "Minute number is expected at "
                     &  Text (Pointer..Text'Last)
                     ),
                     "End date error"
                  );
                  raise Data_Error;
               when others =>
                  Say
                  (  (  "Minute number "
                     &  Image (Minute)
                     &  " is not in 0..59"
                     ),
                     "End date error"
                  );
                  raise Data_Error;
            end;
            Get (Text, Pointer);
            if Pointer <= Text'Last then
               Say
               (  "Unrecognized text after the minute number",
                  "End date error"
               );
               raise Data_Error;
            end if;
         end if;
      end if;
      return Time_Of
             (  Year    => Year,
                Month   => Month,
                Day     => Day,
                Seconds => Duration (Hour * 3600 + Minute * 60)
             );
   exception
      when Time_Error =>
         Say
         (  "Illegal date specified",
            "End date error"
         );
         raise Data_Error;
   end Date;

   function Temperature return Centigrade is
      Data : Float;
      Text : constant String := List.Temperature.Get_Text;
   begin
      Data := Value (Text);
      if Data <= 0.0 then
         return 0.0;
      elsif Data >= 63.5 then
         return 63.5;
      else
         return Centigrade (Data);
      end if;
   exception
      when End_Error =>
         Say
         (  "No thermostat set temperature given",
            "Set temperature error"
         );
         raise Data_Error;
      when others =>
         Say
         (  "Wrong thermostat set temperature",
            "Set temperature error"
         );
         raise Data_Error;
   end Temperature;

   package Walker is new Selected_Foreach_User_Data (Boolean);

   procedure Enumerate
             (  Model : Gtk_Tree_Model;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter;
                Data  : Boolean
             )  is
      No_IO  : IO_Blocker;
      Box    : RF_Address;
      Device : RF_Address;
   begin
      if (  No_IO.Is_Free
         and then
            List.Get (Iter) in Radiator_Thermostat..Wall_Thermostat
         )
      then
         Device := List.Get (Iter);
         Box    := List.Get_Cube (Iter);
         case List.Mode.Get_Active is
            when 1 => -- Manual
               Set_Thermostat_Mode
               (  Box         => Box,
                  Device      => Device,
                  Mode        => Manual,
                  Temperature => Temperature,
                  Handler     => List.all'Unchecked_Access
               );
            when 2 => -- Vacation
               Set_Thermostat_Mode
               (  Box         => Box,
                  Device      => Device,
                  Mode        => Vacation,
                  Temperature => Temperature,
                  Up_Until    => Date,
                  Handler     => List.all'Unchecked_Access
               );
            when 3 => -- Boost
               Set_Thermostat_Mode
               (  Box     => Box,
                  Device  => Device,
                  Mode    => Boost,
                  Handler => List.all'Unchecked_Access
               );
            when others =>
               Set_Thermostat_Mode
               (  Box         => Box,
                  Device      => Device,
                  Mode        => Automatic,
                  Temperature => Temperature,
                  Handler     => List.all'Unchecked_Access
               );
         end case;
         List.Thermostats.Add (Device);
      end if;
   exception
      when Data_Error =>
         null;
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Store (Enumerate)")
         )  );
   end Enumerate;
begin
   List.Thermostats.Erase;
   Walker.Selected_Foreach
   (  List.View.Get_Selection,
      Enumerate'Access,
      True
   );
exception
   when Error : others =>
      Log
      (  MAX_Domain,
         Log_Level_Critical,
         (  "Fault: "
         &  Exception_Information (Error)
         &  Where ("On_Store")
      )  );
end On_Store;
