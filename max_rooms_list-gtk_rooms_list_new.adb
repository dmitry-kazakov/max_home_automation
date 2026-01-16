--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.Gtk_Rooms_List_New           Luebeck            --
--  Separate body                                  Summer, 2015       --
--                                                                    --
--                                Last revision :  15:58 12 Jan 2021  --
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

with Gtk.Cell_Renderer_Fixed;     use Gtk.Cell_Renderer_Fixed;
with Gtk.Cell_Renderer_Progress;  use Gtk.Cell_Renderer_Progress;
with Gtk.Cell_Renderer_Text;      use Gtk.Cell_Renderer_Text;

separate (MAX_Rooms_List)
   function Gtk_Rooms_List_New
            (  History  : not null access Graphs_Record'Class;
               Overview : not null access Graphs_Overview_Record'Class;
               Mails    : not null access MAX_Mail_Record'Class;
               Logger   : not null access MAX_Database_Record'Class;
               Control  : not null access MAX_Control_Record'Class
            )  return Rooms_List is
   Result  : constant Rooms_List := new Rooms_List_Record;
   Scroll  : Gtk_Scrolled_Window;
   Frame   : Gtk_Frame;
   Buttons : Gtk_HBox;
begin
   Gtk.Box.Initialize (Result, Orientation_Vertical, 3);
   -- Buttons ---------------------------
   Buttons := Gtk_HBox_New (Spacing => 3);
   Result.Pack_Start (Buttons, False, False);

      Add_Buttons.Gtk_New (Result.Add_Cube);
      Buttons.Pack_Start (Result.Add_Cube, False, False);
      List_Handlers.Connect
      (  Result.Add_Cube,
         "clicked",
         On_Add'Access,
         Result
      );
      Configure_Buttons.Gtk_New (Result.Configure);
      Result.Configure.Set_Sensitive (False);
      Buttons.Pack_Start (Result.Configure, False, False);
      List_Handlers.Connect
      (  Result.Configure,
         "clicked",
         On_Configure'Access,
         Result
      );
      Select_Thermostats_Buttons.Gtk_New (Result.All_Thermostats);
      Buttons.Pack_Start (Result.All_Thermostats, False, False);
      List_Handlers.Connect
      (  Result.All_Thermostats,
         "clicked",
         On_Select_All_Thermostats'Access,
         Result
      );
      Disconnect_Buttons.Gtk_New (Result.Disconnect);
      Buttons.Pack_Start (Result.Disconnect, False, False);
      List_Handlers.Connect
      (  Result.Disconnect,
         "clicked",
         On_Disconnect'Access,
         Result
      );
      Reconnect_Buttons.Gtk_New (Result.Reconnect);
      Buttons.Pack_Start (Result.Reconnect, False, False);
      List_Handlers.Connect
      (  Result.Reconnect,
         "clicked",
         On_Reconnect'Access,
         Result
      );
      Reboot_Buttons.Gtk_New (Result.Reboot);
      Buttons.Pack_Start (Result.Reboot, False, False);
      List_Handlers.Connect
      (  Result.Reboot,
         "clicked",
         On_Reboot'Access,
         Result
      );
      Network_Scan_Buttons.Gtk_New (Result.Scan);
      Buttons.Pack_Start (Result.Scan, False, False);
      List_Handlers.Connect
      (  Result.Scan,
         "clicked",
         On_Scan'Access,
         Result
      );
      Save_As_Buttons.Gtk_New (Result.Save);
      Buttons.Pack_Start (Result.Save, False, False);
      List_Handlers.Connect
      (  Result.Save,
         "clicked",
         On_Save'Access,
         Result
      );
      MAX_Buttons.Gtk_New (Result.Export_To_MAX);
      Buttons.Pack_Start (Result.Export_To_MAX, False, False);
      List_Handlers.Connect
      (  Result.Export_To_MAX,
         "clicked",
         On_Export_To_MAX'Access,
         Result
      );
      NTP_Buttons.Gtk_New (Result.Set_NTP);
      Buttons.Pack_Start (Result.Set_NTP, False, False);
      List_Handlers.Connect
      (  Result.Set_NTP,
         "clicked",
         On_Set_NTP'Access,
         Result
      );
      Restore_Buttons.Gtk_New (Result.Restore);
      Buttons.Pack_Start (Result.Restore, False, False);
      List_Handlers.Connect
      (  Result.Restore,
         "clicked",
         On_Restore'Access,
         Result
      );
      Down_Buttons.Gtk_New (Result.Move_Down);
      Result.Move_Down.Set_Sensitive (False);
      Buttons.Pack_Start (Result.Move_Down, False, False);
      List_Handlers.Connect
      (  Result.Move_Down,
         "clicked",
         On_Move_Down'Access,
         Result
      );
      Up_Buttons.Gtk_New (Result.Move_Up);
      Result.Move_Up.Set_Sensitive (False);
      Buttons.Pack_Start (Result.Move_Up, False, False);
      List_Handlers.Connect
      (  Result.Move_Up,
         "clicked",
         On_Move_Up'Access,
         Result
      );
      ---------------------------------------------------------------
      Add_Device_Buttons.Gtk_New (Result.Add_Device);
      Buttons.Pack_Start (Result.Add_Device, False, False);
      List_Handlers.Connect
      (  Result.Add_Device,
         "clicked",
         On_Add_Device'Access,
         Result
      );
      Move_Buttons.Gtk_New (Result.Move);
      Buttons.Pack_Start (Result.Move, False, False);
      List_Handlers.Connect
      (  Result.Move,
         "clicked",
         On_Move'Access,
         Result
      );
      Rename_Buttons.Gtk_New (Result.Rename);
      Buttons.Pack_Start (Result.Rename, False, False);
      List_Handlers.Connect
      (  Result.Rename,
         "clicked",
         On_Rename'Access,
         Result
      );
      Delete_Buttons.Gtk_New (Result.Delete_Device);
      Buttons.Pack_Start (Result.Delete_Device, False, False);
      List_Handlers.Connect
      (  Result.Delete_Device,
         "clicked",
         On_Delete'Access,
         Result
      );
      ---------------------------------------------------------------
      Wake_Up_Buttons.Gtk_New (Result.Wake_Up);
      Buttons.Pack_Start (Result.Wake_Up, False, False);
      List_Handlers.Connect
      (  Result.Wake_Up,
         "clicked",
         On_Wake_Up'Access,
         Result
      );
      Reset_Buttons.Gtk_New (Result.Reset);
      Buttons.Pack_Start (Result.Reset, False, False);
      List_Handlers.Connect
      (  Result.Reset,
         "clicked",
         On_Delete'Access,
         Result
      );
      Faulty_Devices_Buttons.Gtk_New (Result.Faulty);
      Buttons.Pack_Start (Result.Faulty, False, False);
      List_Handlers.Connect
      (  Result.Faulty,
         "clicked",
         On_Faulty'Access,
         Result
      );
      About_Buttons.Gtk_New (Result.About);
      Buttons.Pack_Start (Result.About, False, False);
      List_Handlers.Connect
      (  Result.About,
         "clicked",
         On_About'Access,
         Result
      );
         -- Ping images
      Result.Ping_Icons := (  Image_Ping_0_XPM.Image,
                              Image_Ping_1_XPM.Image,
                              Image_Ping_2_XPM.Image,
                              Image_Ping_3_XPM.Image,
                              Image_Ping_4_XPM.Image,
                              Image_Ping_5_XPM.Image
                           );
      for Index in Result.Ping_Icons'Range loop
         Buttons.Pack_End (Result.Ping_Icons (Index), False, False);
      end loop;
         -- Next poll
      Gtk_New (Result.Next_Poll);
      Result.Next_Poll.Set_Tooltip_Text
      (  "Time to the cube next polling"
      );
      Result.Next_Poll.Set_Width_Chars (4);
      Buttons.Pack_End (Result.Next_Poll, False, False);

         -- Display mode
      Gtk_New (Result.Display);
      Result.Display.Set_Tooltip_Text
      (  "The temperature to display by the wall mounted thermostat"
      );
      Result.Display.Set_Border_Width (0);
      Result.Display.Append_Text ("Keep");
      Result.Display.Append_Text ("Measured");
      Result.Display.Append_Text ("Set");
      Result.Display.Set_Active (0);
      Buttons.Pack_End (Result.Display, False, False);
      List_Handlers.Connect
      (  Result.Display,
         "changed",
         Display_Changed'Access,
         Result
      );
          -- Store button
      Set_Thermostat_Buttons.Gtk_New (Result.Store);
      Buttons.Pack_End (Result.Store, False, False);
      List_Handlers.Connect
      (  Result.Store,
         "clicked",
         On_Store'Access,
         Result
      );
         -- Set temperature
      Gtk_New (Result.Degree_Label, Degree & "C");
      Buttons.Pack_End (Result.Degree_Label, False, False);
      Gtk_New (Result.Temperature);
      Result.Temperature.Set_Alignment (0.5);
      Result.Temperature.Set_Width_Chars (5);
      if (  Find_Property (Result.Temperature, "max-width-chars")
         /= null
         )
      then
         Set_Property
         (  Result.Temperature,
            Build ("max-width-chars"),
            GInt'(5)
         );
      end if;
      Result.Temperature.Set_Text ("18.0");
      Buttons.Pack_End (Result.Temperature, False, False);
         -- Calendar button
      Calendar_Buttons.Gtk_New (Result.Calendar);
      Buttons.Pack_End (Result.Calendar, False, False);
      List_Handlers.Connect
      (  Result.Calendar,
         "clicked",
         On_Calendar'Access,
         Result
      );
         -- Date entry
      Gtk_New (Result.Date);
      Result.Date.Set_Alignment (0.5);
      Result.Date.Set_Width_Chars (15);
      if Find_Property (Result.Date, "max-width-chars") /= null then
         Set_Property
         (  Result.Date,
            Build ("max-width-chars"),
            GInt'(15)
         );
      end if;
      Result.Date.Set_Text (Image (Clock + 3600.0 * 24.0 * 14.0));
      Buttons.Pack_End (Result.Date, False, False);
         -- Operating mode
      Gtk_New (Result.Mode);
      Result.Mode.Set_Tooltip_Text ("Thermostat operating mode");
      Result.Mode.Set_Border_Width (0);
      Result.Mode.Append_Text ("Automatic");
      Result.Mode.Append_Text ("Manual");
      Result.Mode.Append_Text ("Vacation");
      Result.Mode.Append_Text ("Boost");
      Buttons.Pack_End (Result.Mode, False, False);
      Gtk_New (Result.Mode_Label, "Mode");
      Buttons.Pack_End (Result.Mode_Label, False, False);
      List_Handlers.Connect
      (  Result.Mode,
         "changed",
         Mode_Changed'Access,
         Result
      );
         -- Shortcut buttons
      declare
         Button : Play_Buttons_10.Gtk_Style_Button;
      begin
         Play_Buttons_10.Gtk_New (Button);
         Result.Play (10) := Button.all'Unchecked_Access;
         Buttons.Pack_End (Result.Play (10), False, False);
      end;
      declare
         Button : Play_Buttons_9.Gtk_Style_Button;
      begin
         Play_Buttons_9.Gtk_New (Button);
         Result.Play (9) := Button.all'Unchecked_Access;
         Buttons.Pack_End (Result.Play (9), False, False);
      end;
      declare
         Button : Play_Buttons_8.Gtk_Style_Button;
      begin
         Play_Buttons_8.Gtk_New (Button);
         Result.Play (8) := Button.all'Unchecked_Access;
         Buttons.Pack_End (Result.Play (8), False, False);
      end;
      declare
         Button : Play_Buttons_7.Gtk_Style_Button;
      begin
         Play_Buttons_7.Gtk_New (Button);
         Result.Play (7) := Button.all'Unchecked_Access;
         Buttons.Pack_End (Result.Play (7), False, False);
      end;
      declare
         Button : Play_Buttons_6.Gtk_Style_Button;
      begin
         Play_Buttons_6.Gtk_New (Button);
         Result.Play (6) := Button.all'Unchecked_Access;
         Buttons.Pack_End (Result.Play (6), False, False);
      end;
      declare
         Button : Play_Buttons_5.Gtk_Style_Button;
      begin
         Play_Buttons_5.Gtk_New (Button);
         Result.Play (5) := Button.all'Unchecked_Access;
         Buttons.Pack_End (Result.Play (5), False, False);
      end;
      declare
         Button : Play_Buttons_4.Gtk_Style_Button;
      begin
         Play_Buttons_4.Gtk_New (Button);
         Result.Play (4) := Button.all'Unchecked_Access;
         Buttons.Pack_End (Result.Play (4), False, False);
      end;
      declare
         Button : Play_Buttons_3.Gtk_Style_Button;
      begin
         Play_Buttons_3.Gtk_New (Button);
         Result.Play (3) := Button.all'Unchecked_Access;
         Buttons.Pack_End (Result.Play (3), False, False);
      end;
      declare
         Button : Play_Buttons_2.Gtk_Style_Button;
      begin
         Play_Buttons_2.Gtk_New (Button);
         Result.Play (2) := Button.all'Unchecked_Access;
         Buttons.Pack_End (Result.Play (2), False, False);
      end;
      declare
         Button : Play_Buttons_1.Gtk_Style_Button;
      begin
         Play_Buttons_1.Gtk_New (Button);
         Result.Play (1) := Button.all'Unchecked_Access;
         Buttons.Pack_End (Result.Play (1), False, False);
      end;
   -- View ---------------------------
   Frame := Gtk_Frame_New;
   Frame.Set_Shadow_Type (Shadow_Out);
   Result.Pack_Start (Frame);
   Scroll := Gtk_Scrolled_Window_New;
   Frame.Add (Scroll);
   Result.List := Gtk_Tree_Store_Newv
                  (  (  Gdk.Pixbuf.Get_Type, -- Icon
                        GType_String,        -- Address
                        GType_Double,        -- Is-temperature
                        GType_Double,        -- Set-temperature
                        GType_String,        -- Text column
                        GType_String,        -- Serial No
                        GType_String,        -- Valve %
                        GType_Int,           -- Duty %
                        Gdk.Pixbuf.Get_Type, -- Battery
                        Gdk.Pixbuf.Get_Type, -- Link
                        GType_String,        -- Name
                        GType_String,        -- IP address
                        GType_Int,           -- Room ID
                        GType_Boolean,       -- Show is-temperature
                        GType_Boolean,       -- Show set-temperature
                        GType_Int,           -- Device type
                        GType_Int,           -- Thermostat mode
                        GType_String         -- Time stamp
                  )  );
   Result.View := Gtk_Tree_View_New_With_Model (+Result.List);
   Result.View.Get_Selection.Set_Mode (Selection_Multiple);
   Result.View.Set_Activate_On_Single_Click (False);
   Result.List.Unref;
   Scroll.Add (Result.View);
   declare
      Column      : Gtk_Tree_View_Column;
      Text        : Gtk_Cell_Renderer_Text;
      Icon        : Gtk_Cell_Renderer_Pixbuf;
      Progress    : Gtk_Cell_Renderer_Progress;
      Temperature : Gtk_Cell_Renderer_Fixed;
      Image       : Gtk_Image;
      Column_No   : Gint;
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
      Column.Set_Title (Degree & "C");
      Gtk_New (Temperature, 1);
      Column.Pack_Start (Temperature, False);
      Column.Add_Attribute
      (  Temperature,
         "value",
         Is_Temperature_Column
      );
      Column.Add_Attribute
      (  Temperature,
         "empty",
         Drop_Is_Temperature_Column
      );
      Column_No := Result.View.Append_Column (Column);
      Column.Set_Resizable (False);

      Gtk_New (Column);
      Column.Set_Title (Set_T & Degree & "C");
      Gtk_New (Temperature, 1);
      Column.Pack_Start (Temperature, False);
      Column.Add_Attribute
      (  Temperature,
         "value",
         Set_Temperature_Column
      );
      Column.Add_Attribute
      (  Temperature,
         "empty",
         Drop_Set_Temperature_Column
      );
      Column_No := Result.View.Append_Column (Column);
      Column.Set_Resizable (False);

--           Gtk_New (Column);
--           Gtk_New (Text);
--           Column.Set_Title ("State");
--           Column.Pack_Start (Text, False);
--           Column.Add_Attribute (Text, "text", Text_Column);
--           Column_No := Result.View.Append_Column (Column);
--           Column.Set_Resizable (False);

      Gtk_New (Column);
      Image := Image_Operating_Mode_XPM.Image;
      Column.Set_Widget (Image);
      Image.Show_All;
      Gtk_New (Icon);
      Column.Pack_Start (Icon, False);
--       Column.Add_Attribute (Icon, "pixbuf", Mode_Column);
      Column_No := Result.View.Append_Column (Column);
      Column.Set_Resizable (False);
      Set_Mode_Cell_Data.Set_Cell_Data_Func
      (  Column,
         Icon,
         Mode_Cell_Data'Access,
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
      Column.Set_Title ("Valve");
      Gtk_New (Text);
      Set_Property (Text, Gtk.Cell_Renderer.Xalign_Property, 1.0);
      Column.Pack_Start (Text, False);
      Column.Add_Attribute (Text, "text", Valve_Column);
      Column_No := Result.View.Append_Column (Column);
      Column.Set_Resizable (False);

      Gtk_New (Column);
      Image := Image_Battery_XPM.Image;
      Column.Set_Widget (Image);
      Image.Show_All;
      Gtk_New (Icon);
      Column.Pack_Start (Icon, False);
      Column.Add_Attribute (Icon, "pixbuf", Battery_Column);
      Column_No := Result.View.Append_Column (Column);
      Column.Set_Resizable (False);

      Gtk_New (Column);
      Image := Image_Link_XPM.Image;
      Column.Set_Widget (Image);
      Image.Show_All;
      Gtk_New (Icon);
      Column.Pack_Start (Icon, False);
      Column.Add_Attribute (Icon, "pixbuf", Link_Column);
      Column_No := Result.View.Append_Column (Column);
      Column.Set_Resizable (False);

      Gtk_New (Column);
      Column.Set_Title ("Last measurement");
      Gtk_New (Text);
      Column.Pack_Start (Text, False);
      Column.Add_Attribute (Text, "text", Time_Stamp_Column);
      Column_No := Result.View.Append_Column (Column);
      Column.Set_Resizable (True);

   end;
   Result.History  := History.all'Unchecked_Access;
   Result.Overview := Overview.all'Unchecked_Access;
   Result.Mails    := Mails.all'Unchecked_Access;
   Result.Logger   := Logger.all'Unchecked_Access;
   History.Ref;
   Overview.Ref;
   List_Handlers.Connect
   (  Result.View,
      "row-activated",
      Activated'Access,
      Result
   );
   Event_Handlers.Connect
   (  Result.View,
      "button-press-event",
      Event_Handlers.To_Marshaller (On_Button_Press'Access),
      Result
   );
   List_Handlers.Connect
   (  Result.View.Get_Selection,
      "changed",
      List_Handlers.To_Marshaller (Device_Changed'Access),
      Result
   );
   List_Handlers.Connect
   (  Result,
      "destroy",
      On_Destroy'Access,
      Result.all'Unchecked_Access
   );
   List_Handlers.Connect
   (  Pages,
      "switch-page",
      On_Page_Switch'Access,
      Result.all'Unchecked_Access
   );
   Result.Timer :=
      Timers.Timeout_Add
      (  GUInt (Ping_Interval * 1_000.0),
         Do_Ping'Access,
         Result
      );
   return Result;
end Gtk_Rooms_List_New;
