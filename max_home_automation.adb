--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--                                                 Luebeck            --
--                                                 Winter, 2007       --
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

with Ada.Calendar;           use Ada.Calendar;
with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with MAX_Control_Page;       use MAX_Control_Page;
with MAX_Database_Page;      use MAX_Database_Page;
with MAX_Graphs;             use MAX_Graphs;
with MAX_Icon_Factory;       use MAX_Icon_Factory;
with MAX_IO;                 use MAX_IO;
with MAX_Mail_Page;          use MAX_Mail_Page;
with MAX_Rooms_List;         use MAX_Rooms_List;
with MAX_Settings_Page;      use MAX_Settings_Page;
with MAX_Shortcuts_Page;     use MAX_Shortcuts_Page;
with MAX_Trace;              use MAX_Trace;
with GLib;                   use GLib;
with GLib.Main;              use GLib.Main;
with GLib.Messages;          use GLib.Messages;
with Gtk.Box;                use Gtk.Box;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Label;              use Gtk.Label;
with Gtk.Notebook;           use Gtk.Notebook;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;
with Gtk.Window;             use Gtk.Window;
with Gtk.Widget;             use Gtk.Widget;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

with Ada.Command_Line;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Image_Automation_16_XPM;
with Image_Automation_24_XPM;
with Image_Automation_32_XPM;
with GLib.Object;
--with GNAT.Exception_Traces;
with Gtk.Main.Router.GNAT_Stack;
with Gtk.Missed;
with Gtk.Style_Provider;
with Gtk.Recent_Manager_Keys;
with GtkAda.Style;
with MAX_Home_Automation_Version;
with MAX_User;
with Strings_Edit.Floats;

procedure MAX_Home_Automation is
   use Gtk.Main.Router.GNAT_Stack;
   use Gtk.Recent_Manager_Keys;

   Min_Width  : constant := 200;
   Min_Height : constant := 100;
   CSS_File   : Unbounded_String :=
                   To_Unbounded_String ("max_home_automation.css");

   type Local is access function return Boolean;
   function "+" is new Ada.Unchecked_Conversion (Local, G_Source_Func);

--     function Idle_Hook return Boolean is
--     begin
--        Memory_Trace ("Idle");
--        return True;
--     exception
--        when others =>
--           return False;
--     end Idle_Hook;

   function Restore_Layout return Boolean is
      Screen_Height : GInt;
      Screen_Width  : GInt;
      Height        : GInt;
      Width         : GInt;
      X, Y          : GInt;
   begin
      Screen_Height := Window.Get_Screen.Get_Height;
      Screen_Width  := Window.Get_Screen.Get_Width;
      Height        := Restore ("height", Min_Height * 2);
      Width         := Restore ("width",  Min_Width * 2);
      X             := Restore ("x", 0);
      Y             := Restore ("y", 0);

      if Width < Min_Width then
         Width := Min_Width;
      end if;
      if Width > Screen_Width then
         Width := Screen_Width;
      end if;
      if Height < Min_Height then
         Height := Min_Height;
      end if;
      if Height > Screen_Height then
         Height := Screen_Height;
      end if;
      if X < 0 then
         X := 0;
      end if;
      if Y < 0 then
         Y := 0;
      end if;
      if X + Width > Screen_Width then
         X := Screen_Width - Width;
      end if;
      if Y + Height > Screen_Height then
         Y := Screen_Height - Height;
      end if;
      Window.Resize (Width, Height);
      Window.Move (X, Y);
      if Discovery_On then
         Scan (To_String (Host));
      else
         Add_Manually (To_String (Cube_Address));
      end if;
      return False;
   exception
      when others =>
         return False;
   end Restore_Layout;

   function Store_Layout return Boolean is
      use Gtk.Recent_Manager_Keys;
      X, Y : GInt;
      Width, Height : GInt;
   begin
      if not Exiting then
         Window.Get_Position (X, Y);
         Store ("x", Image (Integer (X)));
         Store ("y", Image (Integer (Y)));
         Window.Get_Size (Width, Height);
         Store ("width",  Image (Integer (Width)));
         Store ("height", Image (Integer (Height)));
--       Store ("host",   To_String (Host));
      end if;
      return True;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Warning,
            (  "Failed to store window layout: "
            &  Exception_Information (Error)
         )  );
         return False;
   end Store_Layout;

--     package Clipboard_Data is new Request_Targets_User_Data (Integer);
--     procedure Clipboard_Changed
--               (  Clipboard : not null access Gtk_Clipboard_Record'Class;
--                  Atoms     : in out Gdk.Types.Gdk_Atom;
--                  N_Atoms   : Gint;
--                  Data      : Integer
--               )  is
--     begin
--     end Clipboard_Changed;

   Queued_Messages : Messages_Queues.Doubly_Linked.List;
begin
   Trace_Exceptions (True);
   Gtk.Main.Init;
--
-- Check command line arguments:
--
--    --restore=<setting-file-name>
--
   declare
      use Ada.Command_Line;
      use Gtk.Missed;
   begin
      for Index in 1..Argument_Count loop
         declare
            use Ada.Strings.Maps.Constants;
            use Strings_Edit;
            This : constant String := Argument (Index);
         begin
            if Is_Prefix ("--restore=", This, Lower_Case_Map) then
               Queued_Messages :=
                  Replace_Settings (This (This'First + 10..This'Last));
            elsif Is_Prefix ("--load-css=", This, Lower_Case_Map) then
               CSS_File :=
                  To_Unbounded_String
                  (  This (This'First + 11..This'Last)
                  );
               if not File_Test
                      (  To_String (CSS_File),
                         File_Test_Exists
                      )  then
                  Ada.Text_IO.Put_Line
                  (  "CSS file "
                  &  Quote (To_String (CSS_File))
                  &  " does not exist"
                  );
                  Exit_Handler;
                  Set_Exit_Status (Failure);
                  return;
               end if;
            end if;
         end;
      end loop;
   end;
   Set_Log_Trace ("Gtk",          GLib.Messages.Log_Level_Flags'Last);
   Set_Log_Trace ("Gdk",          GLib.Messages.Log_Level_Flags'Last);
   Set_Log_Trace ("GLib-GObject", GLib.Messages.Log_Level_Flags'Last);
   Set_Log_Trace ("Gtk",          GLib.Messages.Log_Level_Flags'Last);
   Set_Log_Trace ("GtkAda+",      GLib.Messages.Log_Level_Flags'Last);
   Set_Log_Trace ("EHA",          GLib.Messages.Log_Level_Flags'Last);
--
-- Check for CSS file
--
   declare
      use Gtk.Missed;
      use Gtk.Style_Provider;
      use GtkAda.Style;
      File_Name : constant String := To_String (CSS_File);

      procedure On_Error (Message : String) is
         use Messages_Queues.Doubly_Linked;
      begin
         Append
         (  Queued_Messages,
            new Trace_Request'
                (  Error_Text,
                   To_Unbounded_String
                   (  "CSS loading from "
                   &  Quote (File_Name)
                   &  " fault: "
                   &  Message
         )      )  );
      end;
   begin
      if File_Test (File_Name, File_Test_Exists) then
         Load_CSS_File (File_Name, On_Error'Access, Priority_User);
      end if;
   exception
      when others =>
         null;
   end;
   Gtk.Window.Gtk_New (Window);
   Window.Set_Title
   (  "MAX! Home automation "
   &  MAX_Home_Automation_Version.Value
   );
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Destroy_Handler'Access);
   declare
      Height : GInt;
      Width  : GInt;
   begin
      Height := Restore ("height", Min_Height * 2);
      Width  := Restore ("width",  Min_Width  * 2);
      if Width < Min_Width then
         Width := Min_Width;
      end if;
      if Height < Min_Height then
         Height := Min_Height;
      end if;
      Window.Set_Default_Size (Width, Height);
   end;
   declare
      use Glib.Object.Object_Simple_List;
      Icon_List : Glist;
   begin
      Append (Icon_List, Image_Automation_16_XPM.Get_Pixbuf.all'Access);
      Append (Icon_List, Image_Automation_24_XPM.Get_Pixbuf.all'Access);
      Append (Icon_List, Image_Automation_32_XPM.Get_Pixbuf.all'Access);
      Window.Set_Icon_List (Icon_List);
   end;
   Gtk.Main.Router.Init (Window);
   Max_Icon_Factory.Init;

   Host         := To_Unbounded_String (Restore ("host", ""));
   Cube_Address := To_Unbounded_String (Restore ("cube", ""));
   Discovery_On := Restore ("discovery", "on") = "on";
   ELV_Trace    := Restore ("elv-trace", "on") = "on";
   declare
      use Strings_Edit;
      Name : constant String := Trim (Restore ("trace-file", ""));
   begin
      if Name'Length > 0 then
         Open_Trace (Name);
      end if;
   end;
   declare
      Label     : Gtk_Label;
      Box       : Gtk_HBox;
      Rooms     : Rooms_List;
      History   : Graphs;
      Overview  : Graphs_Overview;
      Messages  : Trace_Box;
      Mails     : MAX_Mail;
      Logger    : MAX_Database;
      Control   : MAX_Control;
      Shortcuts : MAX_Shortcuts;
      Page_No  : GInt;
   begin
      Pages := Gtk_Notebook_New;
      Window.Add (Pages);
      begin
         Graph_Width :=
            Duration
            (  Strings_Edit.Floats.Value (Restore ("graph-width", "1"))
            *  60.0
            );
      exception
         when others =>
            null;
      end;
      begin
         Temperature_Timeout :=
            Duration
            (  Strings_Edit.Floats.Value
               (  Restore ("temperature-timeout", "5")
               )
            *  60.0
            );
      exception
         when others =>
            null;
      end;
      begin
         Scan_Timeout :=
            Duration
            (  Strings_Edit.Floats.Value
               (  Restore ("scan-timeout", "2")
               )
            *  60.0
            );
      exception
         when others =>
            null;
      end;
      begin
         Scan_Period :=
            Duration
            (  Strings_Edit.Floats.Value
               (  Restore ("scan-period", "5")
               )
            *  60.0
            );
      exception
         when others =>
            null;
      end;
         -- Trace page
         Messages := Gtk_Trace_Box_New;
         MAX_IO.Set_Trace_Box (Messages);
         MAX_Rooms_List.Set_Trace_Box (Messages);
         Gtk_New (Box, Orientation_Horizontal, 3);
--           Gtk_New (Image, "gtk-network", Icon_Size_Small_Toolbar);
--           Box.Pack_Start (Image);
         Label :=
            Gtk_Style_Label_New (Trace_Label).all'Unchecked_Access;
         Box.Pack_Start (Label);
         Box.Show_All;
         Pages.Append_Page (Messages, Box);
         -- Graphs overview
         Gtk_Graphs_New (History, Overview);
         Gtk_New (Box, Orientation_Horizontal, 3);
         Label :=
            Gtk_Style_Label_New (Monitor_Label).all'Unchecked_Access;
         Box.Pack_Start (Label);
         Box.Show_All;
         Pages.Append_Page (Overview, Box);
         -- Graphs, one per room
         Gtk_New (Box, Orientation_Horizontal, 3);
         Label :=
            Gtk_Style_Label_New (Graphs_Label).all'Unchecked_Access;
         Box.Pack_Start (Label);
         Box.Show_All;
         Pages.Append_Page (History, Box);
         -- Settings
         declare
            Scrolled : Gtk_Scrolled_Window;
            Settings : constant Gtk_Notebook := Gtk_Notebook_New;
         begin
            Settings.Set_Tab_Pos (Pos_Left);
            Gtk_New (Box, Orientation_Horizontal, 3);
            Label := Gtk_Style_Label_New (Settings_Label).
                     all'Unchecked_Access;
            Box.Pack_Start (Label);
            Box.Show_All;
            Pages.Append_Page (Settings, Box);
            -- General
            Gtk_New (Box, Orientation_Vertical, 3);
            Label := Gtk_Style_Label_New (General_Label).
                     all'Unchecked_Access;
            Label.Set_Angle (90.0);
            Box.Pack_Start (Label);
            Box.Show_All;
            Gtk_New (Scrolled);
            Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
            Scrolled.Add (Gtk_Settings_New);
            Settings.Append_Page (Scrolled, Box);
            -- E-Mail
            Gtk_New (Box, Orientation_Vertical, 3);
            Label := Gtk_Style_Label_New (E_Mail_Label).
                     all'Unchecked_Access;
            Label.Set_Angle (90.0);
            Box.Pack_Start (Label);
            Box.Show_All;
            Mails := Gtk_Mail_New;
            Gtk_New (Scrolled);
            Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
            Scrolled.Add (Mails);
            Settings.Append_Page (Scrolled, Box);
            -- Logging
            Gtk_New (Box, Orientation_Vertical, 3);
            Label := Gtk_Style_Label_New (Database_Label).
                     all'Unchecked_Access;
            Label.Set_Angle (90.0);
            Box.Pack_Start (Label);
            Box.Show_All;
            Logger := Gtk_Database_New;
            Gtk_New (Scrolled);
            Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
            Scrolled.Add (Logger);
            Settings.Append_Page (Scrolled, Box);
            -- Scripting
            Gtk_New (Box, Orientation_Vertical, 3);
            Label := Gtk_Style_Label_New (Control_Label).
                     all'Unchecked_Access;
            Label.Set_Angle (90.0);
            Box.Pack_Start (Label);
            Box.Show_All;
            Control := Gtk_Control_New;
            Gtk_New (Scrolled);
            Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
            Scrolled.Add (Control);
            Settings.Append_Page (Scrolled, Box);

            -- Overview page
            Rooms :=
               Gtk_Rooms_List_New
               (  History,
                  Overview,
                  Mails,
                  Logger,
                  Control
               );

            -- Shortcuts
            Gtk_New (Box, Orientation_Vertical, 3);
            Label := Gtk_Style_Label_New (Presets_Label).
                     all'Unchecked_Access;
            Label.Set_Angle (90.0);
            Box.Pack_Start (Label);
            Box.Show_All;
            Shortcuts := Gtk_Shortcuts_New (Rooms, Number_Of_Shortcuts);
            for Index in 1..Shortcuts.Shorcut_Count loop
               Shortcuts.Add_Button
               (  Index,
                  Rooms.Get_Play_Button (Index)
               );
            end loop;
            Gtk_New (Scrolled);
            Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
            Scrolled.Add (Shortcuts);
            Settings.Append_Page (Scrolled, Box);
         end;
         Set_GUI (Rooms);
         Gtk_New (Box, Orientation_Horizontal, 3);
         Label := Gtk_Style_Label_New (Overview_Label).
                  all'Unchecked_Access;
         Box.Pack_Start (Label);
         Box.Show_All;
         Page_No := Pages.Insert_Page (Rooms, Box, 0);

         if 0 = Idle_Add (+Restore_Layout'Access) then
            null;
         end if;
      Window.Show_All;
      Rooms.Hide_Mode;
   end;
-- Clipboard_Data.Request_Targets (Get, Clipboard_Changed'Access, 0);
   Store_ID := Timeout_Add (2_000, +Store_Layout'Access);
   Show (Window);
   declare
      use Messages_Queues.Doubly_Linked;
      Message : Item;
   begin
      while not Is_Empty (Queued_Messages) loop
         Message := Item (Queued_Messages);
         Service (Message.all);
         Delete (Queued_Messages, Message);
      end loop;
   end;

--     if 0 = Idle_Add (+Idle_Hook'Access) then
--        null;
--     end if;
   -- Enter the events processing loop
   Gtk.Main.Main;
   Exit_Handler;
   -- Close trace
   if Trace_Open then
      begin
         Trace_To_File
         (  Image (Clock, True)
         &  " Exiting and closing the trace file"
         );
         Ada.Text_IO.Close (Trace_File);
      exception
         when others =>
            null;
      end;
   end if;
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
exception
   when Error : others =>
      Log
      (  MAX_Domain,
         Log_Level_Critical,
         (  "Fatal error: "
         &  Exception_Information (Error)
      )  );
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end MAX_Home_Automation;
