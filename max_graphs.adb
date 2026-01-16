--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Graphs                                  Luebeck            --
--  Implementation                                 Autumn, 2015       --
--                                                                    --
--                                Last revision :  11:21 18 Jan 2025  --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Gdk.Color;            use Gdk.Color;
with Gdk.Color.IHLS;       use Gdk.Color.IHLS;
with GLib.Messages;        use GLib.Messages;
with Gtk.Drawing_Area;     use Gtk.Drawing_Area;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Frame;            use Gtk.Frame;
with Gtk.Missed;           use Gtk.Missed;
with MAX_Icon_Factory;     use MAX_Icon_Factory;
with Strings_Edit.Floats;  use Strings_Edit.Floats;

with Gtk.Recent_Manager_Keys;

package body MAX_Graphs is
   use Gtk.Recent_Manager_Keys;

   Buffer_Size : constant := 1024 * 64; -- Channel buffer size
   Axis_Height : constant := 20;        -- Lower horizontal axis height
   Axis_Width  : constant := 42;        -- Left vertical axis width
   Background  : constant Gdk_Color := RGB (0.00, 0.00, 0.00);
   Major_Line  : constant Gdk_Color := RGB (0.22, 0.22, 0.22);
   Minor_Line  : constant Gdk_Color := RGB (0.12, 0.12, 0.12);
   T_Color     : constant Gdk_Color := RGB (1.00, 0.00, 0.00);
   Valve_Color : constant Gdk_Color := RGB (1.00, 1.00, 0.00);
   T_Set_Color : constant Gdk_Color := RGB (0.50, 0.50, 0.50);
   Min_Period  : constant Float := 0.01;
   Max_Period  : constant Float := 99.0;
--     Colors      : constant array (GUInt range 0..5) of Gdk_Color :=
--                            (  RGB (1.00, 0.00, 0.00),
--                               RGB (1.00, 1.00, 0.00),
--                               RGB (0.00, 1.00, 0.00),
--                               RGB (0.00, 1.00, 1.00),
--                               RGB (0.00, 0.00, 1.00),
--                               RGB (1.00, 0.00, 1.00)
--                            );
   Averaging_Factor : constant GDouble := 0.8;
   Engine           : aliased MAX_Refresh_Engine;

   Refresh_Tip : constant String :=
                 "The refresh period. The curves are updated "         &
                 "(rendered) with the frequency determined by the "    &
                 "period. Increase the period to reduce load caused "  &
                 "by graphs' visualization";

   function Where (Name : String) return String is
   begin
      return " in MAX_Graphs." & Name;
   end Where;

   function "<" (Left, Right : Room_Key) return Boolean is
   begin
      return
      (  Left.Cube < Right.Cube
      or else
         (  Left.Cube = Right.Cube and then Left.Room < Right.Room
      )  );
   end "<";

   procedure Add
             (  Overview : not null access Graphs_Overview_Record'Class;
                Cube     : RF_Address;
                Room     : Room_ID;
                Name     : String
             )  is
      Valves_Count : Natural := 0;
      Color_Count  : Natural := 0;
      Box          : Gtk_HBox;
      Label        : Gtk_Label;
      Thermostats  : RF_Address_Maps.Map;
      Grid_On      : Boolean;
       Handle      : Room_Data_Handles.Handle;
   begin
      if Overview.Is_In (Cube, Room) then
         return;
      end if;
      Grid_On := Restore ("grid", "on") /= "off";
      Thermostats := Get_Thermostats (Cube, Room);
      if Thermostats.Is_Empty then
         return;
      end if;
      if Overview.Slots.Is_Empty then
         --
         -- Add sweeper and scaler at the bottom
         --
         declare
            Grid : Gtk_VBox;
         begin
            Overview.Insert_Row (0);
            Gtk_New_VBox (Grid, False, 3);
            Overview.Attach (Grid, 0, 0);
            Grid.Set_App_Paintable (True);
            declare
               Box    : Gtk_HBox;
               Filler : Gtk_Drawing_Area;
               Label  : Gtk_Label;
            begin
               Gtk_New_HBox (Box);
               Box.Set_App_Paintable (True);
               Grid.Pack_End (Box, False, False);
               Gtk_New (Filler);
               Filler.Set_Size_Request (Axis_Width, Axis_Height);
               Box.Pack_Start (Filler, False, False);
               Gtk_New (Filler);
               Filler.Set_Size_Request (Axis_Width, Axis_Height);
               Box.Pack_End (Filler, False, False);
               Gtk_New
               (  Widget         => Overview.Scaler,
                  Refresh_Engine => Engine'Access,
                  Buffer_Size    => 10
               );
               Box.Pack_Start (Overview.Scaler);
               Box.Set_App_Paintable (True);
               Overview.Scaler.Set_Page_Span (Lower, Graph_Width);
               Overview.Scaler.Set_Time_Axis (Lower, True);
               Overview.Scaler.Set_Time_Axis_Height
               (  Lower,
                  Axis_Height
               );
               Overview.Scaler.Set_Vexpand (False);

               Gtk_New
               (  Widget       => Overview.Sweeper,
                  Oscilloscope => Overview.Scaler,
                  Show_Buttons => False,
                  Flat         => True
               );
               Overview.Attach (Overview.Sweeper, 0, 1);
               Overview.Sweeper.Show_All;
               declare
                  Rows, Columns : GUInt;
                  Grid_On       : Boolean := True;
               begin
                  Grid_On := Restore ("grid", "on") /= "off";
                  Overview.Sweeper.Get_Size (Rows, Columns);
                  Overview.Sweeper.Resize (Rows, Columns + 3);
                  Gtk_New (Overview.Check, "show graph paper");
                  Overview.Check.Set_Active (Grid_On);
                  Connect
                  (  Overview.Check,
                     "toggled",
                     On_Toggled_Grid'Access,
                     Overview.History.all'Unchecked_Access
                  );
                  Overview.Sweeper.Attach
                  (  Overview.Check,
                     Columns, Columns + 1,
                     0, 1,
                     XOptions => Shrink,
                     YOptions => Shrink
                  );
                  Gtk_New
                  (  Spin_Button => Overview.Period,
                     Min         => GDouble (Min_Period),
                     Max         => GDouble (Max_Period),
                     Step        => GDouble (Min_Period)
                  );
                  Overview.Period.Set_Numeric (True);
                  Overview.Period.Set_Snap_To_Ticks (True);
                  Overview.Period.Set_Value
                  (  GDouble (Engine.Get_Period)
                  );
                  Overview.Period.Set_Tooltip_Text (Refresh_Tip);
                  Overview.Sweeper.Attach
                  (  Overview.Period,
                     Columns + 1, Columns + 2,
                     0, 1,
                     XOptions => Shrink,
                     YOptions => Shrink
                  );
                  Gtk_New (Label, "s");
                  Overview.Sweeper.Attach
                  (  Label,
                     Columns + 2, Columns + 3,
                     0, 1,
                     XOptions => Shrink,
                     YOptions => Shrink
                  );
                  Overview.Sweeper.Show_All;
               end;
               Connect
               (  Overview.Period,
                  "value_changed",
                  On_Refresh_Changed'Access,
                  Overview.History.all'Unchecked_Access
               );
            end;
            Grid.Show_All;
         end;
      end if;
      --
      -- Header
      --
      Gtk_New_Hbox (Box);
      Overview.Insert_Row (0);
      Overview.Attach (Box, 0, 0);

      Gtk_New (Label, Degree & "C ");
      Box.Pack_Start (Label, False, False);
      --
      -- Oscilloscope
      --
      Handle.Set (new Room_Data);
      declare
         Oscilloscope    : Gtk_Oscilloscope renames
                              Handle.Ptr.Oscilloscope;
         Start           : constant Gdk_IHLS_Color := To_IHLS (T_Color);

         procedure Add_Temperature (Address : RF_Address) is
            Channel : Channel_Number;
            Color   : constant Gdk_Color :=
                               To_RGB (Val (Start, Color_Count, 4));
         begin
            Color_Count := Color_Count + 1;
         --
         -- Is temperature
         --
            Channel :=
               Oscilloscope.Add_Channel
               (  Sweeper => Lower,
                  Color   => Color
               );
            Gtk_New (Label, "  ");
            Set_Background_Color (Label, To_RGBA (Color));
            Box.Pack_Start (Label, False, False);
            Oscilloscope.Set_Group
            (  Left,
               Oscilloscope.Get_Group (Channel)
            );
            Oscilloscope.Set_Tooltip_Annotation
            (  Channel,
               "Temperature "
            );
            Oscilloscope.Set_Values_Tooltip_Suffix
            (  Channel,
               Degree & "C [" & Image (Address) & "]"
            );
            Oscilloscope.Set_Values_Axis (Left, True);
            Oscilloscope.Set_Values_Axis_Width (Left, Axis_Width);
            -- Control scales
            Oscilloscope.Set_Time_Scale   (Lower, Grid_On);
            Oscilloscope.Set_Time_Scale   (Upper, False);
            Oscilloscope.Set_Values_Scale (Left,  Grid_On);
            Oscilloscope.Set_Values_Scale (Right, False);
            Oscilloscope.Set_Grid_Colors  (Major_Line, Minor_Line);
            Oscilloscope.Set_Time_Grid    (Lower, Grid_On);
            Oscilloscope.Set_Values_Grid  (Left,  Grid_On);
            if Graph_Fixed_Scale then
               Oscilloscope.Set_Auto_Scaling (Left, False);
               declare
                  Low  : constant GDouble :=
                                  GDouble (Temperature_Low);
                  High : constant GDouble :=
                                  GDouble (Temperature_High);
               begin
                  Oscilloscope.Set_Values_Scale (Left, False);
                  Oscilloscope.Get_Amplifier (Left).Configure
                  (  Value          => (High + Low) / 2.0,
                     Lower          => Low,
                     Upper          => High,
                     Step_Increment => (High - Low) / 5.0,
                     Page_Increment => (High - Low) / 2.0,
                     Page_Size      => High - Low
                  );
               end;
            end if;
            Overview.Temperatures.Replace
            (  Address,
               Graph_Data'(Oscilloscope, Channel, null, null)
            );
         --
         -- Set temperature
         --
            Channel :=
               Oscilloscope.Add_Channel
               (  Sweeper => Lower,
                  Group   => Oscilloscope.Get_Group (Channel),
                  Color   => T_Set_Color
               );
            Oscilloscope.Set_Tooltip_Annotation
            (  Channel,
               "Set temperature "
            );
            if Graph_Add_Offset then
               Oscilloscope.Set_Values_Tooltip_Suffix
               (  Channel,
                  Degree & "C - offset"
               );
            else
               Oscilloscope.Set_Values_Tooltip_Suffix
               (  Channel,
                  Degree & "C"
               );
            end if;
            Overview.Schedules.Replace
            (  Address,
               Graph_Data'(Oscilloscope, Channel, null, null)
            );
            Oscilloscope.Show_All;
         end Add_Temperature;

         procedure Add_Label (Box : Gtk_HBox; Address : RF_Address) is
            Index : Integer := Overview.Temperatures.Find (Address);
         begin
            if Index > 0 then
               declare
                  Data : Graph_Data :=
                         Overview.Temperatures.Get (Index);
               begin
                  Gtk_New (Data.Is_Temperature);
                  Box.Pack_Start
                  (  Data.Is_Temperature,
                     False,
                     False
                  );
                  Overview.Temperatures.Replace (Index, Data);
               end;
            end if;
            Index := Overview.Schedules.Find (Address);
            if Index > 0 then
               declare
                  Data : Graph_Data :=
                         Overview.Schedules.Get (Index);
               begin
                  Gtk_New (Data.Set_Temperature);
                  Box.Pack_Start
                  (  Data.Set_Temperature,
                     False,
                     False
                  );
                  Overview.Schedules.Replace (Index, Data);
               end;
            end if;
         end Add_Label;

         Address : RF_Address := 0;
      begin
         Gtk.Oscilloscope.Gtk_New
         (  Widget         => Oscilloscope,
            Background     => Background,
            Lower_Sweeper  => Overview.Scaler.Get_Sweeper (Lower),
            Refresh_Engine => Engine'Access,
            Buffer_Size    => Buffer_Size
         );
         Overview.Insert_Row (1);
         Overview.Attach (Oscilloscope, 0, 1);
         Overview.Slots.Add ((Cube, Room), Handle);
         for Index in 1..Thermostats.Get_Size loop
            case Thermostats.Get (Index) is
               when Wall_Thermostat => -- There is a wall thermostat
                  Address := Thermostats.Get_Key (Index);
                  exit;
               when others =>
                  null;
            end case;
         end loop;
         --
         -- Adding valve averager
         --
         declare
            Channel : Channel_Number;
         begin
            for Index in 1..Thermostats.Get_Size loop
               case Thermostats.Get (Index) is
                  when Radiator_Thermostat..Radiator_Thermostat_Plus =>
                     Valves_Count := Valves_Count + 1;
                  when others =>
                     null;
               end case;
            end loop;
            --
            -- Set valve
            --
            Channel :=
               Oscilloscope.Add_Channel
               (  Sweeper => Lower,
                  Color   => Valve_Color
               );
            if Valves_Count = 1 then
               Oscilloscope.Set_Tooltip_Annotation
               (  Channel,
                  "Valve "
               );
            else
               Oscilloscope.Set_Tooltip_Annotation
               (  Channel,
                  (  "Average of"
                  &  Natural'Image (Valves_Count)
                  &  " valves "
               )  );
            end if;
            Oscilloscope.Set_Values_Tooltip_Suffix
            (  Channel,
               "%"
            );
            Oscilloscope.Set_Group
            (  Right,
               Oscilloscope.Get_Group (Channel)
            );
            Oscilloscope.Set_Values_Axis (Right, True);
            Oscilloscope.Set_Values_Axis_Width
            (  Right,
               Axis_Width
            );
               -- Control scales
            Oscilloscope.Set_Values_Scale (Right, False);
            Oscilloscope.Set_Values_Grid  (Right, False);
            Oscilloscope.Set_Auto_Scaling (Right, False);
            Oscilloscope.Get_Amplifier (Right).Configure
            (  Value          =>  50.0,
               Lower          =>  -5.0,
               Upper          => 105.0,
               Step_Increment =>  10.0,
               Page_Increment =>  50.0,
               Page_Size      => 110.0
            );
            for Index in 1..Thermostats.Get_Size loop
               case Thermostats.Get (Index) is
                  when Radiator_Thermostat ..
                       Radiator_Thermostat_Plus =>
                     declare
                        Address : constant RF_Address :=
                                     Thermostats.Get_Key (Index);
                     begin
                        if not Overview.Valves.Is_In (Address) then
                           Overview.Valves.Add
                           (  Address,
                              (Channel, Handle.Ptr)
                           );
                        end if;
                     end;
                  when others =>
                     null;
               end case;
            end loop;
         end;
         if Address = 0 then -- No wall thermostats here
            for Index in 1..Thermostats.Get_Size loop
               case Thermostats.Get (Index) is
                  when Radiator_Thermostat..Radiator_Thermostat_Plus =>
                     Add_Temperature (Thermostats.Get_Key (Index));
                  when others =>
                     null;
               end case;
            end loop;
         else
            Add_Temperature (Address);
         end if;
         Oscilloscope.Show_All;
         declare
            Header : Gtk_HBox;
            Title  : Gtk_Label renames Handle.Ptr.Title;
         begin
            Gtk_New_Hbox (Box => Header, Spacing => 3);
            Gtk_New (Title, Name);
            Header.Pack_Start (Title);
            if Address = 0 then -- No wall thermostats here
               for Index in 1..Thermostats.Get_Size loop
                  case Thermostats.Get (Index) is
                     when Radiator_Thermostat
                       .. Radiator_Thermostat_Plus =>
                        Add_Label (Header, Thermostats.Get_Key (Index));
                     when others =>
                        null;
                  end case;
               end loop;
            else
               Add_Label (Header, Address);
            end if;
            Box.Pack_Start (Header);
            if Valves_Count > 0 then
               Gtk_New (Label, "  ");
               Set_Background_Color (Label, To_RGBA (Valve_Color));
               Box.Pack_Start (Label, False, False);
               Gtk_New (Label, " %");
               Box.Pack_Start (Label, False, False);
            end if;
         end;
      end;
      Box.Show_All;
   end Add;

   procedure Add
             (  History : not null access Graphs_Record;
                Cube    : RF_Address;
                Room    : Room_ID;
                Name    : String
             )  is
      Page   : Room_Graphs;
      Header : Gtk_HBox;
   begin
      if History.Is_In (Cube, Room) then
         return;
      end if;
      Page := Gtk_Room_Graphs_New (History, Cube, Room);
      Gtk_New (Header, Orientation_Horizontal, 3);
      Page.Name := Gtk_Label_New (Name);
      Header.Pack_Start (Page.Name);
      Header.Show_All;
      Page.Show_All;
      History.Pages.Append_Page (Page, Header);
      Add (History.Overview, Cube, Room, Name);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add")
         )  );
   end Add;

   procedure Delete
             (  History : not null access Graphs_Record;
                Cube    : RF_Address;
                Room    : Room_ID
             )  is
      Page : Gtk_Widget;
   begin
      for Index in reverse 0..History.Pages.Get_N_Pages - 1 loop
         Page := History.Pages.Get_Nth_Page (Index);
         if (  Page /= null
            and then
               Page.all in Room_Graphs_Record'Class
            )  then
            declare
               This : Room_Graphs_Record'Class renames
                      Room_Graphs_Record'Class (Page.all);
            begin
               if This.Cube = Cube and then This.Room = Room then
                  for Index in 1..This.Thermostats.Get_Size loop
                     declare
                        Address : constant RF_Address :=
                                  This.Thermostats.Get_Key (Index);
                     begin
                        History.Temperatures.Remove (Address);
                        History.Valves.Remove (Address);
                     end;
                  end loop;
                  History.Pages.Remove_Page (Index);
               end if;
            end;
         end if;
      end loop;
      declare
         Overview : Graphs_Overview_Record'Class renames
                    History.Overview.all;
         Offset   : constant Integer :=
                    Overview.Slots.Find ((Cube, Room));
      begin
         if Offset <= 0 then
            return;
         end if;
         declare
            This : constant Gtk_Oscilloscope :=
                   Overview.Slots.Get (Offset).Ptr.Oscilloscope;
         begin
            for Index in reverse 1..Overview.Temperatures.Get_Size loop
               if Overview.Temperatures.Get (Index).Oscilloscope = This
               then
                  Overview.Temperatures.Remove (Index);
               end if;
            end loop;
            for Index in reverse 1..Overview.Valves.Get_Size loop
               if Overview.Valves.Get (Index).Room.Oscilloscope = This
               then
                  Overview.Valves.Remove (Index);
               end if;
            end loop;
            for Slot in 1..Overview.Slots.Get_Size loop
               declare
                  Row : constant GInt := GInt ((Slot - 1) * 2);
               begin
                  if (  Overview.Get_Child_At (0, Row + 1)
                     =  Gtk_Widget_Record'Class
                        (  This.all
                        ) 'Unchecked_Access
                     )  then
                     Overview.Remove_Row (Row); -- Header
                     Overview.Remove_Row (Row); -- Oscilloscope
                     exit;
                  end if;
               end;
            end loop;
         end;
         Overview.Slots.Remove (Offset);
         if Overview.Slots.Is_Empty then
            Erase (Overview'Access);
            Overview.Check   := null;
            Overview.Period  := null;
            Overview.Scaler  := null;
            Overview.Sweeper := null;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Delete")
         )  );
   end Delete;

   procedure Feed
             (  History     : not null access Graphs_Record;
                Address     : RF_Address;
                Temperature : Centigrade;
                Stamp       : Time
             )  is
      Index : Integer;
   begin
      Index := History.Temperatures.Find (Address);
      if Index > 0 then
         declare
            Data : Graph_Data := History.Temperatures.Get (Index);
         begin
            Data.Oscilloscope.Feed
            (  Data.Channel,
               Stamp,
               GDouble (Temperature)
            );
         end;
      end if;
      Index := History.Overview.Temperatures.Find (Address);
      if Index > 0 then
         declare
            Data : Graph_Data := History.Overview.
                                 Temperatures.Get (Index);
         begin
            Data.Oscilloscope.Feed
            (  Data.Channel,
               Stamp,
               GDouble (Temperature)
            );
            if Data.Is_Temperature /= null then
               Data.Is_Temperature.Set_Text
               (  Image (Temperature) & Degree & "C"
               );
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Feed (Temperature)")
         )  );
   end Feed;

   procedure Feed
             (  History : not null access Graphs_Record;
                Address : RF_Address;
                Valve   : Ratio;
                Stamp   : Time
             )  is
      Index : Integer;
   begin
      Index := History.Valves.Find (Address);
      if Index > 0 then
         declare
            Data : Graph_Data := History.Valves.Get (Index);
         begin
            Data.Oscilloscope.Feed
            (  Data.Channel,
               Stamp,
               GDouble (Valve) * 100.0
            );
         end;
      end if;
      Index := History.Overview.Valves.Find (Address);
      if Index > 0 then
         declare
            Data : constant Valve_Data :=
                            History.Overview.Valves.Get (Index);
         begin
            Data.Room.Average :=
               (  Data.Room.Average       *        Averaging_Factor
               +  GDouble (Valve) * 100.0 * (1.0 - Averaging_Factor)
               );
            Data.Room.Oscilloscope.Feed
            (  Data.Channel,
               Stamp,
               Data.Room.Average
            );
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Feed (Valve)")
         )  );
   end Feed;

   procedure Feed
             (  History     : not null access Graphs_Record;
                Address     : RF_Address;
                Temperature : Centigrade;
                Offset      : Centigrade;
                Stamp       : Time
             )  is
      Index : Integer;
   begin
      Index := History.Schedules.Find (Address);
      if Index > 0 then
         declare
            Data : Graph_Data := History.Schedules.Get (Index);
         begin
            if Graph_Add_Offset then
               Data.Oscilloscope.Feed
               (  Data.Channel,
                  Stamp,
                  GDouble (Temperature) - GDouble (Offset)
               );
            else
               Data.Oscilloscope.Feed
               (  Data.Channel,
                  Stamp,
                  GDouble (Temperature)
               );
            end if;
         end;
      end if;
      Index := History.Overview.Schedules.Find (Address);
      if Index > 0 then
         declare
            Data : Graph_Data := History.Overview.Schedules.Get (Index);
         begin
            if Graph_Add_Offset then
               Data.Oscilloscope.Feed
               (  Data.Channel,
                  Stamp,
                  GDouble (Temperature) - GDouble (Offset)
               );
            else
               Data.Oscilloscope.Feed
               (  Data.Channel,
                  Stamp,
                  GDouble (Temperature)
               );
            end if;
            if Data.Set_Temperature /= null then
               if Graph_Add_Offset then
                  Data.Set_Temperature.Set_Text
                  (  Set_T & Image (Temperature - Offset) & Degree & "C"
                  );
               else
                  Data.Set_Temperature.Set_Text
                  (  Set_T & Image (Temperature) & Degree & "C"
                  );
               end if;
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Feed (Temperature - Offset)")
         )  );
   end Feed;

   function Gtk_Room_Graphs_New
            (  History : not null access Graphs_Record;
               Cube    : RF_Address;
               Room    : Room_ID
            )  return Room_Graphs is
      Result      : Room_Graphs;
      Thermostats : RF_Address_Maps.Map;
      Address     : RF_Address;
      Grid_On     : Boolean := True;
   begin
      Grid_On := Restore ("grid", "on") /= "off";
      Thermostats := Get_Thermostats (Cube, Room);
      if Thermostats.Is_Empty then
         declare
            Label : Gtk_Label;
         begin
            Result := new Room_Graphs_Record (0);
            Gtk.Box.Initialize (Result, Orientation_Vertical, 3);
            Result.Cube := Cube;
            Result.Room := Room;
            Gtk_New (Label, "No thermostats in this room");
            Result.Pack_Start (Label);
            return Result;
         end;
      end if;
      declare
         Grid    : Gtk_VBox;
         Scaler  : Gtk_Oscilloscope;
         Sweeper : Gtk_Oscilloscope_Sweeper_Panel;
         Frame   : Gtk_Frame;
      begin
         Result := new Room_Graphs_Record (Thermostats.Get_Size);
         Gtk.Box.Initialize (Result, Orientation_Vertical, 3);
         Result.Cube := Cube;
         Result.Room := Room;
         Frame := Gtk_Frame_New;
         Frame.Set_Shadow_Type (Shadow_Out);
         Result.Pack_Start (Frame);
         Gtk_New_VBox (Grid, False, 3);
         Frame.Add (Grid);
         Frame.Set_App_Paintable (True);
         declare
            Box    : Gtk_HBox;
            Filler : Gtk_Drawing_Area;
            Label  : Gtk_Label;
         begin
            Gtk_New_HBox (Box);
            Box.Set_App_Paintable (True);
            Grid.Pack_End (Box, False, False);
            Gtk_New (Filler);
            Filler.Set_Size_Request (Axis_Width, Axis_Height);
            Box.Pack_Start (Filler, False, False);
            Gtk_New (Filler);
            Filler.Set_Size_Request (Axis_Width, Axis_Height);
            Box.Pack_End (Filler, False, False);
            Gtk_New
            (  Widget         => Scaler,
               Refresh_Engine => Engine'Access,
               Buffer_Size    => 10
            );
            Box.Pack_Start (Scaler);
            Box.Set_App_Paintable (True);
            Scaler.Set_Page_Span (Lower, Graph_Width);
            Scaler.Set_Time_Axis (Lower, True);
            Scaler.Set_Time_Axis_Height (Lower, Axis_Height);
            Scaler.Set_Vexpand (False);

            Gtk_New
            (  Widget       => Sweeper,
               Oscilloscope => Scaler,
               Show_Buttons => False,
               Flat         => True
            );
            Result.Pack_Start (Sweeper, False, False);
            declare
               Rows, Columns : GUInt;
            begin
               Sweeper.Get_Size (Rows, Columns);
               Sweeper.Resize (Rows, Columns + 3);
               Gtk_New (Result.Check, "show graph paper");
               Result.Check.Set_Active (Grid_On);
               Connect
               (  Result.Check,
                  "toggled",
                  On_Toggled_Grid'Access,
                  History.all'Unchecked_Access
               );
               Sweeper.Attach
               (  Result.Check,
                  Columns, Columns + 1,
                  0, 1,
                  XOptions => Shrink,
                  YOptions => Shrink
               );
               Gtk_New
               (  Spin_Button => Result.Period,
                  Min         => GDouble (Min_Period),
                  Max         => GDouble (Max_Period),
                  Step        => GDouble (Min_Period)
               );
               Result.Period.Set_Numeric (True);
               Result.Period.Set_Snap_To_Ticks (True);
               Result.Period.Set_Value (GDouble (Engine.Get_Period));
               Result.Period.Set_Tooltip_Text (Refresh_Tip);
               Sweeper.Attach
               (  Result.Period,
                  Columns + 1, Columns + 2,
                  0, 1,
                  XOptions => Shrink,
                  YOptions => Shrink
               );
               Gtk_New (Label, "s");
               Sweeper.Attach
               (  Label,
                  Columns + 2, Columns + 3,
                  0, 1,
                  XOptions => Shrink,
                  YOptions => Shrink
               );
            end;
            Connect
            (  Result.Period,
               "value_changed",
               On_Refresh_Changed'Access,
               History.all'Unchecked_Access
            );
         end;
         for Slot in 1..GUInt (Thermostats.Get_Size) loop
            Address := Thermostats.Get_Key (Integer (Slot));
            declare -- Header
               Box     : Gtk_HBox;
               Label   : Gtk_Label;
            begin
               Gtk_New_Hbox (Box);
               Grid.Pack_Start (Box, False, False);

               Gtk_New (Label, Degree & "C ");
               Box.Pack_Start (Label, False, False);
               Gtk_New (Label, "  ");
               Set_Background_Color (Label, To_RGBA (T_Color));
               Box.Pack_Start (Label, False, False);

--                 Gtk_New (Label, " " & Set_T & " + offset ");
--                 Box.Pack_Start (Label, False, False);
--                 Gtk_New (Label, "  ");
--                 Set_Background_Color (Label, To_RGBA (T_Set_Color));
--                 Box.Pack_Start (Label, False, False);

               Gtk_New
               (  Label,
                  (  Get_Device_Name (Cube, Address)
                  &  " ["
                  &  Image (Address)
                  &  "]"
               )  );
               Box.Pack_Start (Label);
               Result.Thermostats.Add
               (  Address,
                  (Thermostats.Get (Integer (Slot)), Label)
               );

               Gtk_New (Label, "  ");
               Set_Background_Color (Label, To_RGBA (Valve_Color));
               Box.Pack_Start (Label, False, False);
               Gtk_New (Label, " %");
               Box.Pack_Start (Label, False, False);
            end;
            declare -- Graph
               Oscilloscope : Gtk_Oscilloscope renames
                              Result.Oscilloscopes (Positive (Slot));
               Channel      : Channel_Number;
            begin
               -- Create an oscilloscope for this slot
               Gtk.Oscilloscope.Gtk_New
               (  Widget         => Oscilloscope,
                  Background     => Background,
                  Lower_Sweeper  => Scaler.Get_Sweeper (Lower),
                  Refresh_Engine => Engine'Access,
                  Buffer_Size    => Buffer_Size
               );
               Grid.Pack_Start (Oscilloscope, True, True);
--                Oscilloscope.Set_Size_Request (300, 50);
               Channel :=
                  Oscilloscope.Add_Channel
                  (  Sweeper => Lower,
                     Color   => T_Color
                  );
               Oscilloscope.Set_Group
               (  Left,
                  Oscilloscope.Get_Group (Channel)
               );
               Oscilloscope.Set_Tooltip_Annotation
               (  Channel,
                  "Temperature "
               );
               Oscilloscope.Set_Values_Tooltip_Suffix
               (  Channel,
                  Degree & "C"
               );
               Oscilloscope.Set_Values_Axis (Left, True);
               Oscilloscope.Set_Values_Axis_Width (Left, Axis_Width);
               -- Control scales
               Oscilloscope.Set_Time_Scale   (Lower, Grid_On);
               Oscilloscope.Set_Time_Scale   (Upper, False);
               Oscilloscope.Set_Values_Scale (Left,  Grid_On);
               Oscilloscope.Set_Values_Scale (Right, False);
               Oscilloscope.Set_Grid_Colors  (Major_Line, Minor_Line);
               Oscilloscope.Set_Time_Grid    (Lower, Grid_On);
               Oscilloscope.Set_Values_Grid  (Left,  Grid_On);
               if Graph_Fixed_Scale then
                  Oscilloscope.Set_Auto_Scaling (Left, False);
                  declare
                     Low  : constant GDouble :=
                                     GDouble (Temperature_Low);
                     High : constant GDouble :=
                                     GDouble (Temperature_High);
                  begin
                     Oscilloscope.Set_Values_Scale (Left, False);
                     Oscilloscope.Get_Amplifier (Left).Configure
                     (  Value          => (High + Low) / 2.0,
                        Lower          => Low,
                        Upper          => High,
                        Step_Increment => (High - Low) / 5.0,
                        Page_Increment => (High - Low) / 2.0,
                        Page_Size      => High - Low
                     );
                  end;
               end if;
               History.Temperatures.Replace
               (  Address,
                  Graph_Data'(Oscilloscope, Channel, null, null)
               );
            --
            -- Set temperature
            --
               Channel :=
                  Oscilloscope.Add_Channel
                  (  Sweeper => Lower,
                     Group   => Oscilloscope.Get_Group (Channel),
                     Color   => T_Set_Color
                  );
               Oscilloscope.Set_Tooltip_Annotation
               (  Channel,
                  "Set temperature "
               );
               if Graph_Add_Offset then
                  Oscilloscope.Set_Values_Tooltip_Suffix
                  (  Channel,
                     Degree & "C - offset"
                  );
               else
                  Oscilloscope.Set_Values_Tooltip_Suffix
                  (  Channel,
                     Degree & "C"
                  );
               end if;
               History.Schedules.Replace
               (  Address,
                  Graph_Data'(Oscilloscope, Channel, null, null)
               );
            --
            -- Set valve
            --
               Channel :=
                  Oscilloscope.Add_Channel
                  (  Sweeper => Lower,
                     Color   => Valve_Color
                  );
               Oscilloscope.Set_Tooltip_Annotation
               (  Channel,
                  "Valve "
               );
               Oscilloscope.Set_Values_Tooltip_Suffix
               (  Channel,
                  "%"
               );
               Oscilloscope.Set_Group
               (  Right,
                  Oscilloscope.Get_Group (Channel)
               );
               Oscilloscope.Set_Values_Axis (Right, True);
               Oscilloscope.Set_Values_Axis_Width
               (  Right,
                  Axis_Width
               );
               -- Control scales
               Oscilloscope.Set_Values_Scale (Right, False);
               Oscilloscope.Set_Values_Grid  (Right, False);
               Oscilloscope.Set_Auto_Scaling (Right, False);
               Oscilloscope.Get_Amplifier (Right).Configure
               (  Value          =>  50.0,
                  Lower          =>  -5.0,
                  Upper          => 105.0,
                  Step_Increment =>  10.0,
                  Page_Increment =>  50.0,
                  Page_Size      => 110.0
               );
               History.Valves.Replace
               (  Address,
                  Graph_Data'(Oscilloscope, Channel, null, null)
               );
            end;
         end loop;
      end;
      return Result;
   end Gtk_Room_Graphs_New;

   procedure Gtk_Graphs_New
             (  History  : out Graphs;
                Overview : out Graphs_Overview
             )  is
      Period : Float; -- Refresh period
   begin
      begin
         Period := Value (Restore ("graphs-refresh", "0.02"));
         if Period < Min_Period then
            Period := Min_Period;
         elsif Period > Max_Period then
            Period := Max_Period;
         end if;
      exception
         when others =>
            Period := 0.02;
      end;
      Engine.Set_Period (Duration (Period));
      History := new Graphs_Record;
      Gtk.Box.Initialize (History, Orientation_Vertical, 3);
      Gtk_New (History.Pages);
      History.Pack_Start (History.Pages);
      History.Pages.Set_Tab_Pos (Pos_Bottom);
--      Gtk_New_Hbox (Result.Buttons, Spacing => 3);
--      Result.Pack_Start (Result.Buttons, False, False);
--        Connect
--        (  Result,
--           "destroy",
--           On_Destroy'Access,
--           Result
--        );

      Overview := new Graphs_Overview_Record;
      Overview.History := History;
      History.Overview := Overview;
      Gtk.Grid.Initialize (Overview);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Gtk_Graphs_New")
         )  );
   end Gtk_Graphs_New;

   function Is_In
            (  History : not null access Graphs_Record;
               Cube    : RF_Address;
               Room    : Room_ID
            )  return Boolean is
      Page : Gtk_Widget;
   begin
      for Index in reverse 0..History.Pages.Get_N_Pages - 1 loop
         Page := History.Pages.Get_Nth_Page (Index);
         if (  Page /= null
            and then
               Page.all in Room_Graphs_Record'Class
            )  then
            declare
               This : Room_Graphs_Record'Class renames
                      Room_Graphs_Record'Class (Page.all);
            begin
               if This.Cube = Cube and then This.Room = Room then
                  return True;
               end if;
            end;
         end if;
      end loop;
      return False;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Is_In")
         )  );
         return False;
   end Is_In;

   function Is_In
            (  Overview : not null access Graphs_Overview_Record;
               Cube     : RF_Address;
               Room     : Room_ID
            )  return Boolean is
   begin
      return Overview.Slots.Is_In ((Cube, Room));
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Is_In (Overview)")
         )  );
         return False;
   end Is_In;

   procedure On_Refresh_Changed
             (  Widget  : access Gtk_Widget_Record'Class;
                History : Graphs
             )  is
      Period : GDouble;
      Page   : Gtk_Widget;
   begin
      if History.In_On_Refresh then
         return;
      end if;
      History.In_On_Refresh := True;
      Period := Gtk_Spin_Button_Record'Class (Widget.all).Get_Value;
      for Index in 0..History.Pages.Get_N_Pages - 1 loop
         Page := History.Pages.Get_Nth_Page (Index);
         if (  Page /= null
            and then
               Page.all in Room_Graphs_Record'Class
            )  then
            declare
               This : Room_Graphs_Record'Class renames
                      Room_Graphs_Record'Class (Page.all);
            begin
               This.Period.Set_Value (Period);
            end;
         end if;
      end loop;
      History.Overview.Period.Set_Value (Period);
      Engine.Set_Period (Duration (Period));
      Store ("graphs-refresh", Image (Float (Period)));
      History.In_On_Refresh := False;
   exception
      when Error : others =>
         History.In_On_Refresh := False;
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Refresh_Changed")
         )  );
   end On_Refresh_Changed;

   procedure On_Toggled_Grid
             (  Widget  : access Gtk_Widget_Record'Class;
                History : Graphs
             )  is
      This  : constant Gtk_Check_Button :=
                       Gtk_Check_Button_Record'Class
                       (  Widget.all
                       ) 'Unchecked_Access;
      State : constant Boolean := This.Get_Active;

      function Changed (Check : Gtk_Check_Button) return Boolean is
      begin
         return Check = This or else (Check.Get_Active xor State);
      end Changed;

      procedure Toggle (Oscilloscope : Gtk_Oscilloscope) is
      begin
         Oscilloscope.Set_Time_Grid (Lower, State);
         Oscilloscope.Set_Values_Grid (Left, State);
      end Toggle;

      Page  : Gtk_Widget;
   begin
      for Index in 0..History.Pages.Get_N_Pages - 1 loop
         Page := History.Pages.Get_Nth_Page (Index);
         if (  Page /= null
            and then
               Page.all in Room_Graphs_Record'Class
            )  then
            declare
               This : Room_Graphs_Record'Class renames
                      Room_Graphs_Record'Class (Page.all);
            begin
               if Changed (This.Check) then
                  for Index in This.Oscilloscopes'Range loop
                     Toggle (This.Oscilloscopes (Index));
                  end loop;
                  This.Check.Set_Active (State);
               end if;
            end;
         end if;
      end loop;
      if Changed (History.Overview.Check) then
         for Index in 1..History.Overview.Slots.Get_Size loop
            Toggle
            (  History.Overview.Slots.Get (Index).Ptr.Oscilloscope
            );
         end loop;
         History.Overview.Check.Set_Active (State);
      end if;
      if State then
         Store ("grid", "on");
      else
         Store ("grid", "off");
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Toggled_Grid")
         )  );
   end On_Toggled_Grid;

   procedure Refresh (Engine : in out MAX_Refresh_Engine) is
   begin
      Layered_Refresh_Engine (Engine).Refresh;
   end Refresh;

   procedure Rename
             (  History : not null access Graphs_Record;
                Cube    : RF_Address;
                Room    : Room_ID;
                Name    : String
             )  is
      Page : Gtk_Widget;
   begin
      for Index in reverse 0..History.Pages.Get_N_Pages - 1 loop
         Page := History.Pages.Get_Nth_Page (Index);
         if (  Page /= null
            and then
               Page.all in Room_Graphs_Record'Class
            )  then
            declare
               This : Room_Graphs_Record'Class renames
                      Room_Graphs_Record'Class (Page.all);
            begin
               if This.Cube = Cube and then This.Room = Room then
                  This.Name.Set_Text (Name);
                  exit;
               end if;
            end;
         end if;
      end loop;

      declare
         Overview : Graphs_Overview_Record'Class renames
                    History.Overview.all;
         Offset   : constant Integer :=
                    History.Overview.Slots.Find ((Cube, Room));
      begin
         if Offset <= 0 then
            return;
         end if;
         Overview.Slots.Get (Offset).Ptr.Title.Set_Text (Name);
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Rename (Room)")
         )  );
   end Rename;

   procedure Rename
             (  History : not null access Graphs_Record;
                Cube    : RF_Address;
                Address : RF_Address;
                Name    : String
             )  is
      Page : Gtk_Widget;
   begin
      for Index in reverse 0..History.Pages.Get_N_Pages - 1 loop
         Page := History.Pages.Get_Nth_Page (Index);
         if (  Page /= null
            and then
               Page.all in Room_Graphs_Record'Class
            )  then
            declare
               This : Room_Graphs_Record'Class renames
                      Room_Graphs_Record'Class (Page.all);
            begin
               if This.Cube = Cube then
                  declare
                     Index : constant Integer :=
                                      This.Thermostats.Find (Address);
                  begin
                     if Index > 0 then
                        This.Thermostats.Get (Index).Name.Set_Text
                        (  Name & " [" & Image (Address) & "]"
                        );
                     end if;
                  end;
               end if;
            end;
         end if;
      end loop;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Rename (Device)")
         )  );
   end Rename;

   procedure Reorder
             (  History  : not null access Graphs_Record;
                Cube     : RF_Address;
                Room     : Room_ID;
                Position : GLib.GInt
             )  is
      Page : Gtk_Widget;
   begin
      for Index in reverse 0..History.Pages.Get_N_Pages - 1 loop
         Page := History.Pages.Get_Nth_Page (Index);
         if (  Page /= null
            and then
               Page.all in Room_Graphs_Record'Class
            )  then
            declare
               This : Room_Graphs_Record'Class renames
                      Room_Graphs_Record'Class (Page.all);
            begin
               if This.Cube = Cube and then This.Room = Room then
                  History.Pages.Reorder_Child (Page, Position);
                  exit;
               end if;
            end;
         end if;
      end loop;
      declare
         Overview : Graphs_Overview_Record'Class renames
                    History.Overview.all;
         Offset   : constant Integer :=
                    History.Overview.Slots.Find ((Cube, Room));
         Location : constant Integer :=
                    Integer'Min
                    (  Integer'Max
                       (  0,
                          Integer (Position + 1)
                       ),
                       History.Overview.Slots.Get_Size
                    );
      begin
         if Offset <= 0 then
            return;
         end if;
         declare
            Row  : GInt;
            This : constant Gtk_Oscilloscope :=
                   Overview.Slots.Get (Offset).Ptr.Oscilloscope;
         begin
            for Slot in 1..Overview.Slots.Get_Size loop
               Row := GInt ((Slot - 1) * 2); -- The slot's location
               if (  Overview.Get_Child_At (0, Row + 1)
                  =  Gtk_Widget_Record'Class (This.all)'Unchecked_Access
                  )  then
                  exit when Slot = Location;
                  declare
                     Header : constant Gtk_Widget :=
                                       Overview.Get_Child_At (0, Row);
                  begin
                     Header.Ref;
                     This.Ref;
                     Overview.Remove_Row (Row);
                     Overview.Remove_Row (Row);
                     Row := GInt (Location - 1) * 2; -- The new location
                     Overview.Insert_Row (Row);      -- Insert two rows
                     Overview.Insert_Row (Row);
                     Overview.Attach (Header, 0, Row,     1, 1);
                     Overview.Attach (This,   0, Row + 1, 1, 1);
                     This.Unref;
                     Header.Unref;
                  end;
                  exit;
               end if;
            end loop;
         end;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Reorder")
         )  );
   end Reorder;

   procedure Set_Auto_Scale
             (  History : not null access Graphs_Record
             )  is
      procedure Set (Oscilloscope : Gtk_Oscilloscope) is
      begin
         Oscilloscope.Set_Values_Scale
         (  Left,
            Oscilloscope.Get_Values_Grid (Left)
         );
         Oscilloscope.Set_Auto_Scaling (Left, True);
      end Set;
   begin
      for Index in 1..History.Temperatures.Get_Size loop
         Set (History.Temperatures.Get (Index).Oscilloscope);
      end loop;
      for Index in 1..History.Overview.Temperatures.Get_Size loop
         Set (History.Overview.Temperatures.Get (Index).Oscilloscope);
      end loop;
   end Set_Auto_Scale;

   procedure Set_Fixed_Scale
             (  History : not null access Graphs_Record;
                Low     : Centigrade;
                High    : Centigrade
             )  is
      From : constant GDouble := GDouble (Low);
      To   : constant GDouble := GDouble (High);
      procedure Set (Oscilloscope : Gtk_Oscilloscope) is
      begin
         Oscilloscope.Set_Values_Scale (Left, False);
         Oscilloscope.Set_Auto_Scaling (Left, False);
         Oscilloscope.Get_Amplifier (Left).Configure
         (  Value          => (To + From) / 2.0,
            Lower          => From,
            Upper          => To,
            Step_Increment => (To - From) / 5.0,
            Page_Increment => (To - From) / 2.0,
            Page_Size      => To - From
         );
      end Set;
   begin
      for Index in 1..History.Temperatures.Get_Size loop
         Set (History.Temperatures.Get (Index).Oscilloscope);
      end loop;
      for Index in 1..History.Overview.Temperatures.Get_Size loop
         Set (History.Overview.Temperatures.Get (Index).Oscilloscope);
      end loop;
   end Set_Fixed_Scale;

end MAX_Graphs;
