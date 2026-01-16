--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.On_Export_To_Max             Luebeck            --
--  Separate body                                  Spring, 2019       --
--                                                                    --
--                                Last revision :  13:38 14 Sep 2019  --
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

with Strings_Edit.Quoted;  use Strings_Edit.Quoted;
with MAX_User;             use MAX_User;

with Ada.Text_IO;

separate (MAX_Rooms_List)
   procedure On_Export_To_MAX
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
   function Device_Image (Kind_Of : Device_Type) return String is
   begin
      case Kind_Of is
         when Radiator_Thermostat | Radiator_Thermostat_Plus =>
            return "Heizungsthermostat";
         when Wall_Thermostat =>
            return "Wandthermostat";
         when Shutter_Contact =>
            return "Fensterkontakt";
         when Eco_Button =>
            return "Fensterkontakt"; -- Same as shutter contact
         when others =>
            return "";
      end case;
   end Device_Image;

   function Stamp (Date : Time) return String is
      use Strings_Edit;
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
      Minutes : Integer;
      Text    : String (1..60);
      Pointer : Integer := Text'First;
   begin
      Split (Date, Year, Month, Day, Seconds);
      Minutes := Integer (Seconds) / 60;
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => Integer (Year),
         Field       => 4,
         Fill        => '0',
         Justify     => Right
      );
      Put (Text, Pointer, "-");
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => Integer (Month),
         Field       => 2,
         Fill        => '0',
         Justify     => Right
      );
      Put (Text, Pointer, "-");
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => Integer (Day),
         Field       => 2,
         Fill        => '0',
         Justify     => Right
      );
      Put (Text, Pointer, "_");
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => Minutes / 60,
         Field       => 2,
         Fill        => '0',
         Justify     => Right
      );
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => Minutes mod 60,
         Field       => 2,
         Fill        => '0',
         Justify     => Right
      );
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => Integer (Seconds) mod 60,
         Field       => 2,
         Fill        => '0',
         Justify     => Right
      );
      Put (Text, Pointer, "-");
      declare
         S : constant Float := Float (Seconds);
      begin
         Put
         (  Destination => Text,
            Pointer     => Pointer,
            Field       => 2,
            Fill        => '0',
            Justify     => Right,
            Value       => Integer'Min
                           (  99,
                              Integer
                              (  (S - Float'Floor (S)) *  100.0
         )                 )  );
      end;
      return Text (Text'First..Pointer - 1);
   end Stamp;

   Name      : constant String := "devices";
   Extension : constant String := ".txt";
   Cube      : RF_Address;
   Devices   : RF_Address_Maps.Map;
   Path      : constant UTF8_String := MAX_User.User_Path &
                                       MAX_User.Directory_Separator;
   File_Name : constant UTF8_String := Path & Name & Extension;
begin
   Cube := List.Get_Cube (List.Get_Selected);
   if Cube = 0 then
      List.Export_To_MAX.Hide;
      return;
   end if;
   Devices := Get_Thermostats (Cube, No_Room, (others => True));
   if File_Test (Path & Name & Extension, File_Test_Exists) then
      declare
         Save : constant UTF8_String :=
                         Path & Name & "_" & Stamp (Clock) & Extension;
      begin
         Rename (File_Name, Save);
      exception
         when others =>
            Say
            (  (  "Cannot rename "
               &  Quote (File_Name)
               &  " to "
               &  Quote (Save)
               ),
               "Backing up error"
            );
            return;
      end;
   end if;
   declare
      use Ada.Text_IO;
      File : File_Type;
   begin
      begin
         Create (File, Out_File, File_Name);
      exception
         when others =>
            Say
            (  "Cannot open " & Quote (File_Name),
               "Exporting error"
            );
            return;
      end;
      begin
         for Index in 1..Devices.Get_Size loop
            if Devices.Get (Index) in Radiator_Thermostat
                                   .. Eco_Button then
               Put_Line
               (  File,
                  (  "Position: "
                  &  Image (Index)
                  &  " - "
                  &  Device_Image (Devices.Get (Index))
               )  );
               Put_Line
               (  File,
                  (  "Funkadresse: "
                  &  Image (Integer (Devices.Get_Key (Index)))
               )  );
   --              Put_Line ("Status: gültig");
   --              Put_Line ("initialisiert: ja");
   --              Put_Line ("Antwort auf Funkbefehl: ja");
   --              Put_Line ("Fehler: nein");
               Put_Line
               (  File,
                  (  "_________________________________________"
                  &  "_______________________________________"
               )  );
            end if;
         end loop;
      exception
         when others =>
            Say
            (  "Cannot write " & Quote (File_Name),
               "Exporting error"
            );
            Close (File);
            return;
      end;
      Close (File);
   end;
exception
   when Error : others =>
      Log
      (  MAX_Domain,
         Log_Level_Critical,
         (  "Fault: "
         &  Exception_Information (Error)
         &  Where ("On_Export_To_MAX")
      )  );
end On_Export_To_MAX;
