--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.On_Update_Preview            Luebeck            --
--  Separate body                                  Summer, 2015       --
--                                                                    --
--                                Last revision :  22:18 18 Feb 2021  --
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
   procedure On_Update_Preview
             (  Object : access GObject_Record'Class;
                Dialog : Preview_Dialog
             )  is
   LF   : constant Character := Character'Val (13);
   From : Gtk_Text_Iter;
   To   : Gtk_Text_Iter;
   procedure Add (Text : UTF8_String) is
   begin
      Dialog.Buffer.Get_End_Iter (To);
      Dialog.Buffer.Insert (To, Text);
   end;
   procedure Add_Line (Text : UTF8_String) is
   begin
      Dialog.Buffer.Get_End_Iter (To);
      Dialog.Buffer.Insert (To, Text & LF);
   end;
   procedure Add (P : Device_Parameters; Room : String) is
      function Image (Value : Duration) return String is
      begin
         return Minutes (Value);
      end Image;

      function Image (Value : Ratio) return String is
      begin
         return Image (Integer (Float (Value) * 100.0));
      end Image;

   begin
      Add_Line ("  Address: "      & Image (P.Address));
      Add_Line ("  Name: "         & P.Name);
      Add_Line ("  Room ID: "      & Image (Integer (P.Room)));
      Add_Line ("  Room name: "    & Room);
      Add_Line ("  Comfort: "      & Image (P.Comfort));
      Add_Line ("  Eco: "          & Image (P.Eco));
      Add_Line ("  Max: "          & Image (P.Max));
      Add_Line ("  Min: "          & Image (P.Min));
      Add_Line ("  Offset: "       & Image (P.Offset));
      Add_Line ("  Window open: "  & Image (P.Window_Open));
      case P.Kind_Of is
         when Radiator_Thermostat | Radiator_Thermostat_Plus =>
            Add_Line ("  Window time: "  & Image (P.Window_Time));
            Add_Line ("  Boost time: "   & Image (P.Boost_Time));
            Add_Line ("  Boost valve: "  & Image (P.Boost_Valve));
            Add_Line ("  Decalc.: "      & Image (P.Decalcification));
            Add_Line ("  Max valve: "    & Image (P.Max_Valve));
            Add_Line ("  Valve offset: " & Image (P.Valve_Offset));
         when others =>
            null;
      end case;
      for Day in P.Schedule'Range loop
         Add ("  " & Image (Day) & ":");
         for Index in P.Schedule (Day).Points'Range loop
            declare
               Point : Set_Point renames
                       P.Schedule (Day).Points (Index);
            begin
               Add
               (  " "
               &  Image (Point.Last)
               &  "="
               &  Image (Point.Point)
               );
            end;
         end loop;
         Add ("" & LF);
      end loop;
   end Add;

   procedure Show_Toplogy (Data : String) is

      procedure Get_Address
                (  Line    : String;
                   Pointer : in out Integer;
                   Address : out RF_Address
                )  is
      begin
         if Pointer + 2 > Line'Last then
            raise Data_Error with
                  "Invalid response, missing device RF address";
         end if;
         Address := Character'Pos (Line (Pointer    )) * 2**16
                  + Character'Pos (Line (Pointer + 1)) * 2**8
                  + Character'Pos (Line (Pointer + 2));
         Pointer := Pointer + 3;
      end Get_Address;

      procedure Get_Type
                (  Line    : String;
                   Pointer : in out Integer;
                   Kind_Of : out Device_Type
                )  is
      begin
         if Pointer > Line'Last then
            raise Data_Error with
                  "Invalid response, missing device type";
         end if;
         case Character'Pos (Line (Pointer)) is
            when 0      => Kind_Of := Cube;
            when 1      => Kind_Of := Radiator_Thermostat;
            when 2      => Kind_Of := Radiator_Thermostat_Plus;
            when 3      => Kind_Of := Wall_Thermostat;
            when 4      => Kind_Of := Shutter_Contact;
            when 5      => Kind_Of := Eco_Button;
            when others => Kind_Of := Unknown;
         end case;
         Pointer := Pointer + 1;
      end Get_Type;

      procedure Get_Device
                (  Data    : String;
                   Pointer : in out Integer;
                   No      : Positive
                )  is
         Kind_Of : Device_Type;
         Name    : Integer;
         Serial   : Integer;
         Length  : Natural;
         Address : RF_Address;
         ID      : Room_ID;
      begin
         Get_Type (Data, Pointer, Kind_Of);
         Get_Address (Data, Pointer, Address);
         if Pointer + 9 > Data'Last then
            raise Data_Error with
                  "missing device serial number (device " &
                  Image (No) &
                  ')';
         end if;
         Serial  := Pointer;
         Pointer := Pointer + 10;
         Length  := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         Name    := Pointer;
         Pointer := Pointer + Length;
         if Pointer - Data'Last > 1 then
            raise Data_Error with
                  "wrong name length (device " & Image (No) & ')';
         end if;
         if Pointer > Data'Last then
            raise Data_Error with
                  "missing room ID (device " & Image (No) & ')';
         end if;
         ID := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         Add_Line ("  Device name: " & Data (Name..Name + Length - 1));
         Add_Line ("  Address: " & Image (Address));
         Add_Line ("  Serial No: " & Data (Serial..Serial + 9));
         Add_Line ("  Type: " & Image (Kind_Of));
         if ID /= No_Room then
            Add_Line ("  Room ID: " & Image (Integer (ID)));
         end if;
      end Get_Device;

      procedure Get_Room
                (  Data    : String;
                   Pointer : in out Integer;
                   No      : Positive
                )  is
         ID      : Room_ID;
         Name    : Integer;
         Length  : Natural;
         Address : RF_Address;
      begin
         if Pointer > Data'Last then
            raise Data_Error with
                  "missing room ID (room " & Image (No) & ')';
         end if;
         ID := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         if Pointer > Data'Last then
            raise Data_Error with
                  "missing room name length (room " & Image (No) & ')';
         end if;
         Length  := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         Name    := Pointer;
         Pointer := Pointer + Length;
         if Pointer - Data'Last > 1 then
            raise Data_Error with
                  "wrong name length (room " & Image (No) & ')';
         end if;
         Get_Address (Data, Pointer, Address);
         Add_Line ("  Room name: " & Data (Name..Name + Length - 1));
         Add_Line ("  ID: " & Image (Integer (ID)));
         Add_Line ("  Address: " & Image (Address));
      end Get_Room;

      Pointer : Integer := Data'First;
      Count   : Integer;
   begin
      if (  Data (Pointer) /= 'V'
         or else
            Data (Pointer + 1) /= Character'Val (2)
         )  then
         raise Data_Error with "malformed data header";
      end if;
      Pointer := Pointer + 2;
      if Pointer <= Data'Last then
         Count   := Character'Pos (Data (Pointer ));
         Pointer := Pointer + 1;
         Add_Line ("-- Topology metadata --");
         Add_Line ("Total rooms: " & Image (Count));
         for Room in 1..Count loop
            Get_Room (Data, Pointer, Room);
         end loop;
         if Pointer <= Data'Last then
            Count   := Character'Pos (Data (Pointer));
            Pointer := Pointer + 1;
            Add_Line ("Total devices: " & Image (Count));
            for Device in 1..Count loop
               Get_Device (Data, Pointer, Device);
            end loop;
         end if;
      end if;
   exception
      when Error : others =>
         Add_Line (Exception_Message (Error));
   end Show_Toplogy;
begin
   Dialog.Buffer.Get_Start_Iter (From);
   Dialog.Buffer.Get_End_Iter (To);
   Dialog.Buffer.Delete (From, To);
   declare
      use Ada.Streams.Stream_IO;
      use MAX_Cube_Configuration;
      File     : File_Type;
      Config   : Device_Parameters_Data_Sets.Set;
      Topology : Topology_Handles.Handle;
   begin
      begin
         Open (File, In_File, Dialog.Get_Preview_Filename);
      exception
         when Error : others =>
            Set_Preview_Widget_Active (+Dialog, False);
            return;
      end;
      begin
         Read (Stream (File), Config, Topology);
      exception
         when Error : others =>
            Add_Line (Exception_Message (Error));
            Set_Preview_Widget_Active (+Dialog, True);
            Close (File);
            return;
      end;
      Close (File);
      if Topology.Is_Valid then -- Show topology
         Show_Toplogy (Topology.Ptr.Metadata (1..Topology.Ptr.Length));
      end if;
      Add_Line ("-- Device configuration data --");
      for Index in 1..Config.Get_Size loop
         declare
            Parameters : Device_Parameters renames
                         Config.Get (Index).Parameters;
         begin
            case Parameters.Kind_Of is
               when Wall_Thermostat =>
                  Add_Line
                  (  "Wall thermostat "
                  &  Parameters.Serial_No
                  );
                  Add (Parameters, Config.Get (Index).Room);
               when Radiator_Thermostat =>
                  Add_Line
                  (  "Radiator thermostat "
                  &  Parameters.Serial_No
                  );
                  Add (Parameters, Config.Get (Index).Room);
               when Radiator_Thermostat_Plus =>
                  Add_Line
                  (  "Radiator thermostat plus "
                  &  Parameters.Serial_No
                  );
                  Add (Parameters, Config.Get (Index).Room);
               when others =>
                  null;
            end case;
         end;
      end loop;
      Set_Preview_Widget_Active (+Dialog, True);
   end;
exception
   when Error : others =>
      Log
      (  MAX_Domain,
         Log_Level_Critical,
         (  "Fault: "
         &  Exception_Information (Error)
         &  Where ("On_Update_Preview")
      )  );
end On_Update_Preview;
