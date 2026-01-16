--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Trace                                   Luebeck            --
--  Implementation                                 Summer, 2015       --
--                                                                    --
--                                Last revision :  10:14 10 Dec 2023  --
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

with Ada.Streams;               use Ada.Streams;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.RGBA;                  use Gdk.RGBA;
with GLib.Messages;             use GLib.Messages;
with GLib.Object;               use GLib.Object;
with Glib.Properties.Creation;  use Glib.Properties.Creation;
with GNAT.Exception_Actions;    use GNAT.Exception_Actions;
with Gtk.Button;                use Gtk.Button;
with Gtk.File_Filter;           use Gtk.File_Filter;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Recent_Manager_Keys;   use Gtk.Recent_Manager_Keys;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Style_Context;         use Gtk.Style_Context;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Strings_Edit;              use Strings_Edit;
with Strings_Edit.Base64;       use Strings_Edit.Base64;
with Strings_Edit.Floats;       use Strings_Edit.Floats;
with Strings_Edit.Integers;     use Strings_Edit.Integers;
with Strings_Edit.Quoted;       use Strings_Edit.Quoted;

with GNAT.Sockets;
with GNAT.Traceback.Symbolic;
with GLib.Object.Checked_Destroy;
with GLib.Types;
with Gtk.Widget.Styles;

package body MAX_Trace is

   LF        : constant Character := Character'Val (13);
   Max_Size  : constant := 1024 * 64;
   In_Action : Boolean := False;
   pragma Atomic (In_Action);

   function Where (Name : String) return String is
   begin
      return " in MAX_Trace." & Name;
   end Where;

   package Style_Handlers is
      new Gtk.Handlers.Callback (Gtk_Style_Label_Record);

   Class_Record : array (Gtk_Style_Label_Kind) of
                     aliased Ada_GObject_Class :=
                         (others => Uninitialized_Class);

   function To_String (Data : Stream_Element_Array) return String is
      Result  : String (1..Data'Length);
      Item    : Stream_Element;
      Pointer : Stream_Element_Offset := Data'First;
   begin
      for Index in Result'Range loop
         Item    := Data (Pointer);
         Pointer := Pointer + 1;
         if Item in 32..126 then
            Result (Index) := Character'Val (Item);
         else
            Result (Index) := '.';
         end if;
      end loop;
      return Result;
   end To_String;

   procedure Clear_Trace
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Trace_Box
             )  is
      From : Gtk_Text_Iter;
      To   : Gtk_Text_Iter;
   begin
      Box.Buffer.Get_Start_Iter (From);
      Box.Buffer.Get_End_Iter (To);
      Box.Buffer.Delete (From, To);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Clear_Trace")
         )  );
   end Clear_Trace;

   procedure Continue_Trace
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Trace_Box
             )  is
   begin
      if Box.Frozen then
         Box.Hold.Set_Sensitive (True);
         Box.Continue.Set_Sensitive (False);
         Box.Frozen := False;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Continue_Trace")
         )  );
   end Continue_Trace;

   procedure Do_Destroy
             (  Dialog   : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             )  is
   begin
      GLib.Object.Checked_Destroy (Dialog);
   end Do_Destroy;

   function Get_Label (Kind_Of : Gtk_Style_Label_Kind)
      return UTF8_String is
   begin
      case Kind_Of is
         when Topology_Label      => return "Topology";
         when Configuration_Label => return "Configuration";
         when Trace_Label         => return "Trace";
         when Monitor_Label       => return "Monitor";
         when Graphs_Label        => return "Graphs";
         when Settings_Label      => return "Settings";
         when General_Label       => return "General";
         when E_Mail_Label        => return "E-Mail";
         when Database_Label      => return "Database";
         when Control_Label       => return "Control";
         when Presets_Label       => return "Presets";
         when Overview_Label      => return "Overview";
         when Restore_Label       => return "Restore";
         when No_Topology_Label   =>
            return "There were no topology data stored";
      end case;
   end Get_Label;

   function Get_Type (Kind_Of : Gtk_Style_Label_Kind) return GType is
      Label : constant String := Get_Label (Kind_Of);
      Class : Ada_GObject_Class renames Class_Record (Kind_Of);
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Label.Get_Type,
            Class_Record => Class'Access,
            Type_Name    => "MAX_" & Label & "_label"
         )
      then
         Install_Style_Property
         (  GLib.Types.Class_Ref (Class.The_Type),
            Gnew_String
            (  Name    => "label",
               Nick    => "label",
               Blurb   => Label & " label",
               Default => Label
         )  );
      end if;
      return Class.The_Type;
   end Get_Type;

   function Gtk_New
            (  Title      : String;
               Action     : Gtk_File_Chooser_Action;
               File       : not null Gtk_GEntry;
               Accept_Tip : String := "Use the selected file";
               Close_Tip  : String := "Close the file selection dialog"
            )  return not null File_Dialog is
      Result : File_Dialog;
   begin
      Result := new Entry_Dialog_Record;
      declare
         Dialog : Entry_Dialog_Record renames
                  Entry_Dialog_Record (Result.all);
      begin
         Initialize (Result, Title, Window, Action);
         Add_Button_From_Stock
         (  Dialog   => Result,
            Response => Gtk_Response_Cancel,
            Icon     => Stock_Cancel,
            Label    => "_Cancel",
            Tip      => Close_Tip
         );
         Add_Button_From_Stock
         (  Dialog   => Result,
            Response => Gtk_Response_Accept,
            Icon     => Stock_OK,
            Label    => "_OK",
            Tip      => Accept_Tip
         ) .Set_Can_Default (True);
         Dialog.Set_Default_Response (Gtk_Response_Accept);
         Dialog.On_Response (On_Response'Access);
         Dialog.Set_Modal (True);
         Dialog.File := File;
         File.Ref;
      end;
      return Result;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Gtk_New (Entry)")
         )  );
         raise;
   end Gtk_New;

   function Gtk_New
            (  Title      : String;
               Action     : Gtk_File_Chooser_Action;
               Handler    : not null Cube_Select_Handler;
               Cube       : RF_Address := 0;
               Accept_Tip : String := "Use the selected file";
               Close_Tip  : String := "Close the file selection dialog"
            )  return not null File_Dialog is
      Result : File_Dialog;
   begin
      Result := new Cube_Dialog_Record;
      declare
         Dialog : Cube_Dialog_Record renames
                  Cube_Dialog_Record (Result.all);
      begin
         Initialize (Result, Title, Window, Action);
         Add_Button_From_Stock
         (  Dialog   => Result,
            Response => Gtk_Response_Cancel,
            Icon     => Stock_Cancel,
            Label    => "_Cancel",
            Tip      => Close_Tip
         );
         Add_Button_From_Stock
         (  Dialog   => Result,
            Response => Gtk_Response_Accept,
            Icon     => Stock_OK,
            Label    => "_OK",
            Tip      => Accept_Tip
         ) .Set_Can_Default (True);
         Dialog.Set_Default_Response (Gtk_Response_Accept);
         Dialog.On_Response (On_Response'Access);
         Dialog.Set_Modal   (True);
         Dialog.Cube_Handler := Handler;
         Dialog.Cube         := Cube;
      end;
      return Result;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Gtk_New (Cube Handler)")
         )  );
         raise;
   end Gtk_New;

   function Gtk_New
            (  Title      : String;
               Action     : Gtk_File_Chooser_Action;
               Handler    : not null Widget_Select_Handler;
               Widget     : not null access Gtk_Widget_Record'Class;
               Accept_Tip : String := "Use the selected file";
               Close_Tip  : String := "Close the file selection dialog"
            )  return not null File_Dialog is
      Result : File_Dialog;
   begin
      Result := new Widget_Dialog_Record;
      declare
         Dialog : Widget_Dialog_Record renames
                  Widget_Dialog_Record (Result.all);
      begin
         Initialize (Result, Title, Window, Action);
         Add_Button_From_Stock
         (  Dialog   => Result,
            Response => Gtk_Response_Cancel,
            Icon     => Stock_Cancel,
            Label    => "_Cancel",
            Tip      => Close_Tip
         );
         Add_Button_From_Stock
         (  Dialog   => Result,
            Response => Gtk_Response_Accept,
            Icon     => Stock_OK,
            Label    => "_OK",
            Tip      => Accept_Tip
         ) .Set_Can_Default (True);
         Dialog.Set_Default_Response (Gtk_Response_Accept);
         Dialog.On_Response (On_Response'Access);
         Dialog.Set_Modal   (True);
         Dialog.Widget_Handler := Handler;
         Dialog.Widget         := Widget.all'Unchecked_Access;
         Widget.Ref;
      end;
      return Result;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Gtk_New (Cube Handler)")
         )  );
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Label   : out Gtk_Style_Label;
                Kind_Of : Gtk_Style_Label_Kind;
                Suffix  : UTF8_String := ""
             )  is
      Widget : Gtk_Style_Label;
   begin
      Widget := new Gtk_Style_Label_Record;
      Widget.Kind_Of := Kind_Of;
      Append (Widget.Suffix, Suffix);
      Max_Trace.Initialize (Widget);
      Label := Widget;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Gtk_New (Style_Label)")
         )  );
         if Widget /= null then
            Unref (Widget);
         end if;
         raise;
   end Gtk_New;

   function Gtk_Style_Label_New
            (  Kind_Of : Gtk_Style_Label_Kind;
               Suffix  : UTF8_String := ""
            )  return access Gtk_Label_Record'Class is
      Widget : Gtk_Style_Label;
   begin
      Gtk_New (Widget, Kind_Of, Suffix);
      return Widget.all'Unchecked_Access;
   end Gtk_Style_Label_New;

   function Gtk_Trace_Box_New return Trace_Box is
      Result  : constant Trace_Box := new Trace_Box_Record;
      Scroll  : Gtk_Scrolled_Window;
      Frame   : Gtk_Frame;
      Buttons : Gtk_HBox;
   begin
      Gtk.Box.Initialize (Result, Orientation_Vertical, 3);
      Buttons := Gtk_HBox_New (Spacing => 3);
      Buttons.Set_Border_Width (3);

      Result.Pack_Start (Buttons, False, False);
         Clear_Buttons.Gtk_New (Result.Clear);
         Buttons.Pack_Start (Result.Clear, False, False);
         Trace_Handlers.Connect
         (  Result.Clear,
            "clicked",
            Clear_Trace'Access,
            Result.all'Unchecked_Access
         );
         Trace_Handlers.Connect
         (  Result.Clear,
            "clicked",
            Clear_Trace'Access,
            Result.all'Unchecked_Access
         );

         Hold_Buttons.Gtk_New (Result.Hold);
         Buttons.Pack_Start (Result.Hold, False, False);
         Trace_Handlers.Connect
         (  Result.Hold,
            "clicked",
            Hold_Trace'Access,
            Result.all'Unchecked_Access
         );

         Continue_Buttons.Gtk_New (Result.Continue);
         Buttons.Pack_Start (Result.Continue, False, False);
         Trace_Handlers.Connect
         (  Result.Continue,
            "clicked",
            Continue_Trace'Access,
            Result.all'Unchecked_Access
         );

         Gtk_New (Result.Enable_File, "Trace to file");
         Buttons.Pack_Start (Result.Enable_File, False, False);
         Result.Enable_File.Set_Active (Trace_Open);
         Trace_Handlers.Connect
         (  Result.Enable_File,
            "clicked",
            On_Trace_To_File'Access,
            Result.all'Unchecked_Access
         );

         Gtk_New (Result.Append_File, "Append");
         Result.Append_File.Set_Tooltip_Text
         (  "When checked the trace file is appended if exists. "
         &  "Otherwise the file is overwritten each time when "
         &  "opened"
         );
         Buttons.Pack_Start (Result.Append_File, False, False);
         Result.Append_File.Set_Active
         (  Restore ("trace-append", "off") = "on"
         );
         Result.Append_File.Set_Sensitive (not Trace_Open);

         Gtk_New (Result.File_Name);
         Buttons.Pack_Start (Result.File_Name, True, True);
         Result.File_Name.Set_Tooltip_Text ("The trace file name");
         Result.File_Name.Set_Text (Restore ("trace-file", ""));
         Result.File_Name.Set_Sensitive (not Trace_Open);

         Trace_File_Buttons.Gtk_New (Result.Browse);
         Buttons.Pack_Start (Result.Browse, False, False);
         Result.Browse.Set_Sensitive (not Trace_Open);
         Trace_Handlers.Connect
         (  Result.Browse,
            "clicked",
            On_Browse_File'Access,
            Result.all'Unchecked_Access
         );

      Frame := Gtk_Frame_New;
      Frame.Set_Shadow_Type (Shadow_Out);
      Result.Pack_Start (Frame);
      Scroll := Gtk_Scrolled_Window_New;
      Frame.Add (Scroll);
      Result.Buffer := Gtk_Text_Buffer_New;
      Result.View := Gtk_Text_View_New_With_Buffer (Result.Buffer);
      Result.Buffer.Unref;
      Result.View.Set_Editable (False);
      Scroll.Add (Result.View);
      return Result;
   end Gtk_Trace_Box_New;

   procedure Hold_Trace
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Trace_Box
             )  is
   begin
      if not Box.Frozen then
         Box.Hold.Set_Sensitive (False);
         Box.Continue.Set_Sensitive (True);
         Box.Frozen := True;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Hold_Trace")
         )  );
   end Hold_Trace;

   function Image (Date : Time; Fraction : Boolean := False)
      return String is
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
      Put (Text, Pointer, " ");
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => Minutes / 60,
         Field       => 2,
         Fill        => '0',
         Justify     => Right
      );
      Put (Text, Pointer, ":");
      if Fraction then
         Put
         (  Destination => Text,
            Pointer     => Pointer,
            Value       => Minutes mod 60,
            Field       => 2,
            Fill        => '0',
            Justify     => Right
         );
         Put (Text, Pointer, ":");
         Put
         (  Destination => Text,
            Pointer     => Pointer,
            Value       => Integer (Seconds) mod 60,
            Field       => 2,
            Fill        => '0',
            Justify     => Right
         );
         Put (Text, Pointer, ".");
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
      else
         Put
         (  Destination => Text,
            Pointer     => Pointer,
            Value       => (Minutes mod 60 / 30) * 30,
            Field       => 2,
            Fill        => '0',
            Justify     => Right
         );
      end if;
      return Text (Text'First..Pointer - 1);
   end Image;

   function Image (Interval : Duration) return String is
      Value : Float := Float (Interval);
   begin
      if abs Value < 60.0 then
         return Image (Value, AbsSmall => 0) & "s";
      end if;
      Value := Value / 60.0;
      if abs Value < 60.0 then
         return Image (Value, AbsSmall => 0) & "min";
      end if;
      Value := Value / 60.0;
      if abs Value < 24.0 then
         return Image (Value, AbsSmall => 0) & "h";
      end if;
      Value := Value / 24.0;
      if abs Value < 7.0 then
         return Image (Value, AbsSmall => 0) & " days";
      end if;
      if abs Value < 31.0 then
         return Image (Value / 7.0, AbsSmall => 0) & " weeks";
      end if;
      if abs Value < 365.0 then
         return Image (Value / 31.0, AbsSmall => 0) & " months";
      else
         return Image (Value / 365.0, AbsSmall => 0) & " years";
      end if;
   end Image;

   procedure Initialize
             (  Label : not null access Gtk_Style_Label_Record'Class
             )  is
   begin
      G_New (Label, Get_Type (Label.Kind_Of));
      Gtk.Label.Initialize (Label);
      Label.Set_Name (Get_Label (Label.Kind_Of));
      Style_Handlers.Connect
      (  Label,
         "style-updated",
         Style_Handlers.To_Marshaller (Style_Updated'Access)
      );
      Style_Updated (Label);
   end Initialize;

   procedure On_Browse_File
             (  Object : access Gtk_Widget_Record'Class;
                Box    : Trace_Box
             )  is
      Dialog : File_Dialog;
      Filter : Gtk_File_Filter;
   begin
      Dialog :=
         Gtk_New
         (  "Select a file to trace into",
            Action_Save,
            Box.File_Name
         );
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name (Log_Filter_Name);
      Filter.Add_Pattern ("*" & Log_Extension);
      Dialog.Add_Filter (Filter);
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name ("Any file");
      Filter.Add_Pattern ("*");
      Dialog.Add_Filter (Filter);
      Dialog.Show_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Browse_File")
         )  );
   end On_Browse_File;

   procedure On_Response
             (  Dialog   : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             )  is
   begin
      declare
         This : File_Dialog_Record'Class renames
                File_Dialog_Record'Class (Dialog.all);

         function "/" (Left, Right : String) return String is
         begin
            if Right'Length = 0 then
               return Left;
            elsif Left'Length < Right'Length then
               return Left & Right;
            elsif Left (Left'Last - Right'Length + 1..Left'Last) =
                  Right then
               return Left;
            else
               return Left & Right;
            end if;
         end "/";

         function Add_Extension return String is
         begin
            if This.Get_Action = Action_Save then
               if This.Get_Filter = null then
                  return "";
               end if;
               declare
                  Name : constant String := This.Get_Filter.Get_Name;
               begin
                  if Name = Sch_Filter_Name then
                     return Sch_Extension;
                  elsif Name = Max_Filter_Name then
                     return Max_Extension;
                  elsif Name = Log_Filter_Name then
                     return Log_Extension;
                  elsif Name = Txt_Filter_Name then
                     return Txt_Extension;
                  elsif Name = HTML_Filter_Name then
                     return HTML_Filter_Name;
                  elsif Name = CSS_Filter_Name then
                     return CSS_Extension;
                  elsif Name = Py_Filter_Name then
                     return Py_Extension;
                  elsif Name = Jl_Filter_Name then
                     return Jl_Extension;
                  else
                     return "";
                  end if;
               end;
            else
               return "";
            end if;
         end Add_Extension;

         function Overwrite (Name : String) return Boolean is
         begin
            if This.Get_Filename = Name then
               return True;
            elsif File_Test (Name, File_Test_Exists)  then
               declare
                  Dialog : Gtk_Dialog;
                  Label  : Gtk_Label;
                  Box    : Gtk_Box;
                  Image  : Gtk_Image;
                  Result : Gtk_Response_Type;
                  Top    : constant Gtk_Widget := Window.Get_Toplevel;
               begin
                   Gtk_New
                   (  Dialog => Dialog,
                      Title  => "Confirm file overwrite",
                      Flags  => Modal or Destroy_With_Parent,
                      Parent => Gtk_Window_Record'Class (Top.all)'Unchecked_Access
                   );
                   Dialog.Realize;
                   Gtk_New_Hbox (Box);
                   Dialog.Get_Content_Area.Set_Spacing (3);
                   Dialog.Get_Content_Area.Pack_Start (Box, Padding => 10);
                   Gtk_New
                   (  Image,
                      Stock_Dialog_Warning,
                      Icon_Size_Dialog
                   );
                   Box.Pack_Start (Image, Padding => 10);

                   Add_Button_From_Stock
                   (  Dialog   => Dialog,
                      Response => Gtk_Response_Yes,
                      Icon     => Stock_Yes,
                      Label    => "_Yes"
                   );
                   Add_Button_From_Stock
                   (  Dialog   => Dialog,
                      Response => Gtk_Response_No,
                      Icon     => Stock_Cancel,
                      Label    => "_No"
                   );
                   Gtk_New
                   (  Label,
                      (  "The file "
                      &  Name
                      &  " already exists. Do you want to replace it?"
                   )  );
                   Label.Set_Selectable (True);
                   Label.Set_Justify (Justify_Left);
                   Box.Pack_Start (Label, Padding => 10);
                   Dialog.Show_All;
                   Result := Dialog.Run;
                   GLib.Object.Checked_Destroy (Dialog);
                   return Result = Gtk_Response_Yes;
               end;
            else
               return True;
            end if;
         end Overwrite;
      begin
         case This.Mode is
            when Entry_Mode =>
               if Response = Gtk_Response_Accept then
                  declare
                     Name : constant String :=
                            Trim (This.Get_Filename) / Add_Extension;
                  begin
                     if not Overwrite (Name) then
                        return;
                     end if;
                     This.File.Set_Text (Name);
                  end;
               end if;
               This.File.Unref;
            when Cube_Mode =>
               if Response = Gtk_Response_Accept then
                  declare
                     Name : constant String :=
                            Trim (This.Get_Filename) / Add_Extension;
                  begin
                     if not Overwrite (Name) then
                        return;
                     end if;
                     This.Cube_Handler (Name, This.Cube);
                  end;
               end if;
            when Widget_Mode =>
               if Response = Gtk_Response_Accept then
                  declare
                     Name : constant String :=
                            Trim (This.Get_Filename) / Add_Extension;
                  begin
                     if not Overwrite (Name) then
                        return;
                     end if;
                     This.Widget_Handler (Name, This.Widget.all);
                  end;
               end if;
               This.Widget.Unref;
         end case;
      end;
      GLib.Object.Checked_Destroy (Dialog);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Response")
         )  );
         GLib.Object.Checked_Destroy (Dialog);
   end On_Response;

   procedure On_Trace_To_File
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Trace_Box
             )  is
      use Ada.Text_IO;
   begin
      if Box.Enable_File.Get_Active then
         declare
            Name : constant String := Trim (Box.File_Name.Get_Text);
         begin
            if Name'Length = 0 then
               Say ("No trace file specified");
               Box.Enable_File.Set_Active (False);
               return;
            end if;
            Open_Trace (Name, Box);
            Box.File_Name.Set_Sensitive   (False);
            Box.Browse.Set_Sensitive      (False);
            Box.Append_File.Set_Sensitive (False);
            Store ("trace-file", Name);
            if Box.Append_File.Get_Active then
               Store ("trace-append", "on");
            else
               Store ("trace-append", "off");
            end if;
         end;
      else
         if Trace_Open then
            begin
               Close (Trace_File);
            exception
               when Error : others =>
                  Log
                  (  MAX_Domain,
                     Log_Level_Critical,
                     (  "Failed to close trace file "
                     &  Quote (Box.File_Name.Get_Text)
                     &  ": "
                     &  Exception_Message (Error)
                  )  );
                  Box.Enable_File.Set_Active (True);
                  return;
            end;
            Trace_Open := False;
         end if;
         Box.File_Name.Set_Sensitive   (True);
         Box.Browse.Set_Sensitive      (True);
         Box.Append_File.Set_Sensitive (True);
         Store ("trace-file", "");
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Trace_To_File")
         )  );
   end On_Trace_To_File;

   procedure Open_Trace
             (  File : UTF8_String;
                Box  : Trace_Box := null
             )  is
      use Ada.Text_IO;
   begin
      if Restore ("trace-append", "off") = "on" then
         begin
            Open (Trace_File, Append_File, File);
            Put_Line
            (  Trace_File,
               (  "----------------------------- Appended "
               &  Image (Clock)
               &  " -----------------------------"
            )  );
         exception
            when others =>
               Create (Trace_File, Out_File, File);
         end;
      else
         Create (Trace_File, Out_File, File);
      end if;
      Trace_Open := True;
   exception
      when Error : others =>
         if Box /= null then
            Say
            (  "Cannot open trace file "
            &  Quote (File)
            &  ": "
            &  Exception_Message (Error)
            );
            Box.Enable_File.Set_Active (False);
         end if;
   end Open_Trace;

   procedure Say
             (  Text  : String;
                Title : String := "Error";
                Mode  : String := "gtk-dialog-error";
                Modal : Boolean := False
             )  is
      Data : Say_Data;
   begin
      Append (Data.Text,  Text);
      Append (Data.Title, Title);
      Append (Data.Mode,  Mode);
      Data.Modal := Modal;
      Say_Messages.Send (Service'Access, Data);
   exception
      when Gtk.Main.Router.Quit_Error =>
         null;
   end Say;

   procedure Service (Message : in out Say_Data) is
      function Get_Title return String is
      begin
         if Length (Message.Title) > 0 then
            return To_String (Message.Title);
         elsif Message.Mode = Stock_Dialog_Error then
            return "Error";
         elsif Message.Mode = Stock_Dialog_Info then
            return "Information";
         elsif Message.Mode = Stock_Dialog_Question then
            return "Question";
         elsif Message.Mode = Stock_Dialog_Warning then
            return "Warning";
         else
            return "";
         end if;
      end Get_Title;
      Dialog : Gtk_Dialog;
      View   : Gtk_Text_View;
      Row    : Gtk_Text_Iter;
      Buffer : Gtk_Text_Buffer;
      Box    : Gtk_Box;
      Image  : Gtk_Image;
      Top    : constant Gtk_Widget := Window.Get_Toplevel;
   begin
      if Top = null or else Top.all not in Gtk_Window_Record'Class then
         return;
      end if;
      Gtk_New
      (  Dialog => Dialog,
         Title  => To_String (Message.Title),
         Flags  => Modal or Destroy_With_Parent,
         Parent => Gtk_Window_Record'Class (Top.all)'Unchecked_Access
      );
      Dialog.Realize;
      Gtk_New_Hbox (Box);
      Dialog.Get_Content_Area.Set_Spacing (3);
      Dialog.Get_Content_Area.Pack_Start (Box, Padding => 10);
      Gtk_New (Image, To_String (Message.Mode), Icon_Size_Dialog);
      Box.Pack_Start (Image, False, False, 10);

      if To_String (Message.Mode) = Stock_Dialog_Question then
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Response => Gtk_Response_Yes,
            Icon     => Stock_Yes,
            Label    => "_Yes"
         );
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Response => Gtk_Response_No,
            Icon     => Stock_Cancel,
            Label    => "_No"
         );
      else
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Response => Gtk_Response_OK,
            Icon     => Stock_OK,
            Label    => "_OK"
         );
      end if;
      Gtk_New (Buffer);
      Buffer.Get_End_Iter (Row);
      Buffer.Insert (Row, To_String (Message.Text));
      Gtk_New (View, Buffer);
      View.Set_Wrap_Mode (Wrap_Word);
      Buffer.Unref;
      Box.Pack_Start (View, Padding => 10);
      View.Set_Size_Request (400);
      Dialog.Show_All;
      declare -- Copy background color from dialog
         Color : Gdk_RGBA;
      begin
         Dialog.Realize;
         Get_Style_Context (Dialog).Get_Background_Color
         (  Gtk_State_Flag_Normal,
            Color
         );
         View.Override_Background_Color
         (  Gtk_State_Flag_Normal,
            Color
         );
      end;
      if Message.Modal then
         Dialog.Set_Modal (True);
      end if;
      Dialog.On_Response (Do_Destroy'Access);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service (say)")
         )  );
   end Service;

   function Stack_Traceback return String is
      use GNAT.Traceback;
      use GNAT.Traceback.Symbolic;
      TB  : Tracebacks_Array (1..1_000);
      Len : Natural;
   begin
       Call_Chain (TB, Len);
       return Symbolic_Traceback (TB (1..Len));
   end Stack_Traceback;

   procedure Style_Updated
             (  Label : access Gtk_Style_Label_Record'Class
             )  is
   begin
      Label.Set_Text
      (  Gtk.Widget.Styles.Style_Get (Label, "label")
      &  To_String (Label.Suffix)
      );
   end Style_Updated;

   function To_Pixbuf
            (  Box  : not null access Trace_Box_Record;
               Mode : Trace_Type
            )  return Gdk_Pixbuf is
   begin
      case Mode is
         when Message_Text =>
            return Box.Render_Icon
                   (  "gtk-discard",
                      Icon_Size_Small_Toolbar
                   );
         when Received_Text =>
            return Box.Render_Icon
                   (  "gtk-go-forward",
                      Icon_Size_Small_Toolbar
                   );
         when Sent_Text =>
            return Box.Render_Icon
                   (  "gtk-go-back",
                      Icon_Size_Small_Toolbar
                   );
         when Error_Text =>
            return Box.Render_Icon
                   (  "gtk-dialog-error",
                      Icon_Size_Small_Toolbar
                   );
         when Mode_Text =>
            return Box.Render_Icon
                   (  "gtk-execute",
                      Icon_Size_Small_Toolbar
                   );
      end case;
   end To_Pixbuf;

   procedure Trace
             (  Box     : not null access Trace_Box_Record;
                Message : UTF8_String;
                Mode    : Trace_Type  := Message_Text;
                Prefix  : UTF8_String := ""
             )  is
      function Dump (Encoded : String) return String is
      begin
         declare
            Decoded : constant String := From_Base64 (Encoded);
            Result  : String (1..Decoded'Length * 3);
            Pointer : Integer := Result'First;
         begin
            for Index in Decoded'Range loop
               Put (Result, Pointer, " ");
               Put
               (  Destination => Result,
                  Pointer     => Pointer,
                  Value       => Character'Pos (Decoded (Index)),
                  Base        => 16,
                  Fill        => '0',
                  Field       => 2,
                  Justify     => Right
               );
            end loop;
            return Result;
         end;
      exception
         when others =>
            return Encoded;
      end Dump;

      Pixbuf : Gdk_Pixbuf;
      Last   : Gtk_Text_Iter;
      From   : Integer := Message'First;
      To     : Integer := Message'First;
      Area   : Gdk_Rectangle;
      Screen : Gdk_Rectangle;
      Scroll : Boolean;
   begin
      if Trace_Open then
         Trace_To_File (Message, Mode, Prefix);
      end if;
      if Exiting or else Box.Frozen then
         return;
      end if;
      Pixbuf := To_Pixbuf (Box, Mode);
      Box.Buffer.Get_End_Iter (Last);
      Box.View.Get_Iter_Location (Last, Area);
      Box.View.Get_Visible_Rect (Screen);
      Scroll := (  Area.Y + Area.Width <= Screen.Y + Screen.Width
                and then
                   Area.Y >= Screen.Y
                );
      loop
         if (  To > Message'Last
            or else
               not (  Character'Pos (Message (To)) in 32..127
                   or else
                      Character'Pos (Message (To)) in 128..255
            )      )  then
            if To - From >= 1 then
               Box.Buffer.Get_End_Iter (Last);
               Box.Buffer.Insert (Last, Prefix);
               if Pixbuf /= null then
                  Box.Buffer.Get_End_Iter (Last);
                  Box.Buffer.Insert_Pixbuf (Last, Pixbuf);
               end if;
               Box.Buffer.Get_End_Iter (Last);
--                 Done := False;
--                 if To - From > 2 and then Message (From + 1) = ':' then
--                    case Message (From) is
--                       when 'L' | 'l' | 'S' | 's' =>
--                          Box.Buffer.Insert
--                          (  Last,
--                             (  Message (From..From + 1)
--                             &  Dump (Message (From + 3..To - 1))
--                             &  LF
--                          )  );
--                          Done := True;
--                       when 'C' =>
--                          for Index in From + 2..To - 1 loop
--                             if Message (Index) = ',' then
--                                Box.Buffer.Insert
--                                (  Last,
--                                   (  Message (From..Index)
--                                   &  Dump (Message (Index + 1..To - 1))
--                                   &  LF
--                                )  );
--                                Done := True;
--                                exit;
--                             end if;
--                          end loop;
--                       when others =>
--                          null;
--                    end case;
--                 end if;
--                 if not Done then
--                    Box.Buffer.Insert (Last, Message (From..To - 1) & LF);
--                 end if;
               Box.Buffer.Insert (Last, Message (From..To - 1) & LF);
            end if;
            exit when To > Message'Last;
            To   := To + 1;
            From := To;
         else
            To := To + 1;
         end if;
      end loop;
      if Pixbuf /= null then
         Pixbuf.Unref;
      end if;
      Trim (Box.Buffer);
      if Scroll then
         Box.Buffer.Get_End_Iter (Last);
         if Box.View.Scroll_To_Iter (Last, 0.25, False, 0.0, 0.0) then
            null;
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Trace")
         )  );
   end Trace;

   procedure Trace
             (  Box     : not null access Trace_Box_Record;
                Message : UTF8_String;
                Error   : Exception_Occurrence
             )  is
      use GNAT.Traceback.Symbolic;
      Pixbuf : constant Gdk_Pixbuf := To_Pixbuf (Box, Error_Text);
      Text   : constant String := Message & LF &
                                  Exception_Information (Error) & LF &
                                  Symbolic_Traceback (Error) & LF;
      Last   : Gtk_Text_Iter;
   begin
      if Trace_Open then
         declare
            use Ada.Text_IO;
         begin
            Put_Line (Trace_File, "*** " & Text);
         exception
            when others =>
               null;
         end;
      end if;
      if Exiting then
         return;
      end if;
      Box.Buffer.Get_End_Iter (Last);
      if Pixbuf /= null then
         Box.Buffer.Insert_Pixbuf (Last, Pixbuf);
         Pixbuf.Unref;
      end if;
      Box.Buffer.Get_End_Iter (Last);
      Box.Buffer.Insert (Last, Text);
      Trim (Box.Buffer);
      Box.Buffer.Get_End_Iter (Last);
      if Box.View.Scroll_To_Iter (Last, 0.25, False, 0.0, 0.0) then
         null;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Trace (error)")
         )  );
   end Trace;

   procedure Trace_Exception_Action (Error : Exception_Occurrence) is
      use Ada.Text_IO;
      use GNAT.Sockets;
--    use GNAT.Traceback.Symbolic;
      use Gtk.Main.Router;
   begin
      if not Trace_Open or else In_Action then
         return;
      end if;
      In_Action := True;
      begin
         if Exception_Identity (Error) = Socket_Error'Identity then
            case Resolve_Exception (Error) is
               when Connection_Timed_Out => -- Ignore this one
                  null;
               when others =>
                  Put_Line
                  (  Trace_File,
                     "++! " & Exception_Information (Error)
                  );
                  Flush (Trace_File);
            end case;
         elsif Exception_Identity (Error) = Quit_Error'Identity then
            null;
         else
            Put_Line
            (  Trace_File,
               "++! " & Exception_Information (Error)
            );
            Flush (Trace_File);
         end if;
      exception
         when others =>
            null;
      end;
      In_Action := False;
   end Trace_Exception_Action;

   procedure Trace_Exceptions (Enable : Boolean) is
   begin
      if Enable then
         Register_Global_Action (Trace_Exception_Action'Access);
      else
         Register_Global_Action (null);
      end if;
   end Trace_Exceptions;

   procedure Trace_Location (Message : UTF8_String) is
      use Ada.Text_IO;
   begin
      if not Trace_Open or else In_Action then
         return;
      end if;
       Put_Line (Message);
       Put_Line (Stack_Traceback);
   end Trace_Location;

   procedure Trace_To_File
             (  Message : UTF8_String;
                Mode    : Trace_Type  := Message_Text;
                Prefix  : UTF8_String := ""
             )  is
      use Ada.Text_IO;
      function Sign return String is
      begin
         case Mode is
            when Message_Text =>
               return Prefix;
            when Received_Text =>
               return Prefix & " > ";
            when Sent_Text =>
               return Prefix & " < ";
            when Error_Text =>
               if Prefix'Length = 0 then
                  return "! ";
               else
                  return Prefix & " ! ";
               end if;
            when Mode_Text =>
               if Prefix'Length = 0 then
                  return ": ";
               else
                  return Prefix & " : ";
               end if;
         end case;
      end Sign;
   begin
      if not Trace_Open then
         return;
      end if;
      for Index in reverse Message'Range loop
         case Message (Index) is
            when Character'Val (8)..Character'Val (13) | ' ' =>
               null;
            when others =>
               Put_Line
               (  Trace_File,
                  Sign & Message (Message'First..Index)
               );
               Flush (Trace_File);
               exit;
         end case;
      end loop;
   exception
      when others =>
         null;
   end Trace_To_File;

   procedure Trace_To_File
             (  Message : UTF8_String;
                Error   : Exception_Occurrence
             )  is
      use Ada.Text_IO;
      use GNAT.Traceback.Symbolic;
   begin
      if not Trace_Open then
         return;
      end if;
      Put_Line
      (  Trace_File,
         (  "*** "
         &  Message & LF
         &  Exception_Information (Error) & LF
         &  Symbolic_Traceback    (Error) & LF
      )  );
   exception
      when others =>
         null;
   end Trace_To_File;

   procedure Trim
             (  Buffer : not null access Gtk_Text_Buffer_Record'Class
             )  is
      Start : Gtk_Text_Iter;
      Stop  : Gtk_Text_Iter;
   begin
      while Buffer.Get_Char_Count >= Max_Size loop
         Buffer.Get_Start_Iter (Start);
         Buffer.Get_Iter_At_Line (Stop, 2);
         Buffer.Delete (Start, Stop);
      end loop;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Trim")
         )  );
   end Trim;
------------------------------------------------------------------------
--
-- Memory tracing, monitors the working set under Windows.
--
--     type BOOL      is new Interfaces.C.int;
--     type DWORD     is new Interfaces.C.unsigned_long;
--     type HANDLE    is new System.Address;
--     type SIZE_T    is mod 2**Standard'Address_Size;
--     type DWORD_PTR is mod 2**Standard'Address_Size;
--     pragma Convention (C, SIZE_T);
--     type PROCESS_MEMORY_COUNTERS is record
--        cb                         : DWORD;
--        PageFaultCount             : DWORD;
--        PeakWorkingSetSize         : SIZE_T;
--        WorkingSetSize             : SIZE_T;
--        QuotaPeakPagedPoolUsage    : SIZE_T;
--        QuotaPagedPoolUsage        : SIZE_T;
--        QuotaPeakNonPagedPoolUsage : SIZE_T;
--        QuotaNonPagedPoolUsage     : SIZE_T;
--        PagefileUsage              : SIZE_T;
--        PeakPagefileUsage          : SIZE_T;
--        PrivateUsage               : SIZE_T;
--     end record;
--     pragma Convention (C, PROCESS_MEMORY_COUNTERS);
--
--     function GetCurrentProcess return HANDLE;
--     pragma Import (Stdcall, GetCurrentProcess, "GetCurrentProcess");
--
--     function GetLastError return DWORD;
--     pragma Import (Stdcall, GetLastError, "GetLastError");
--
--     function GetProcessMemoryInfo
--              (  Process  : HANDLE := GetCurrentProcess;
--                 Counters : access PROCESS_MEMORY_COUNTERS;
--                 Size     : DWORD  := PROCESS_MEMORY_COUNTERS'Size / 8
--              )  return BOOL;
--     pragma Import
--            (  Stdcall,
--               GetProcessMemoryInfo,
--               "GetProcessMemoryInfo"
--            );
--     function GetProcessAffinityMask
--              (  Process             : HANDLE := GetCurrentProcess;
--                 ProcessAffinityMask : access DWORD_PTR;
--                 SystemAffinityMask  : access DWORD_PTR
--              )  return BOOL;
--     pragma Import
--            (  Stdcall,
--               GetProcessAffinityMask,
--               "GetProcessAffinityMask"
--            );
--     function SetProcessAffinityMask
--              (  Process             : HANDLE := GetCurrentProcess;
--                 ProcessAffinityMask : DWORD_PTR
--              )  return BOOL;
--     pragma Import
--            (  Stdcall,
--               SetProcessAffinityMask,
--               "SetProcessAffinityMask"
--            );
--     pragma Linker_Options ("-lpsapi");
--
--     function Memory_Use return Integer is
--        Result : BOOL;
--        Data   : aliased PROCESS_MEMORY_COUNTERS;
--     begin
--        Result := GetProcessMemoryInfo (Counters => Data'Access);
--        return Integer (Data.WorkingSetSize);
--     end Memory_Use;
--
--     function Caller (Offs : Integer := 4) return String is
--        use GNAT.Traceback;
--        use GNAT.Traceback.Symbolic;
--        TB  : Tracebacks_Array (1..1_000);
--        Len : Natural;
--     begin
--        Call_Chain (TB, Len);
--        declare
--           Trace : constant String :=
--                            Symbolic_Traceback (TB (Offs..Offs));
--           First : Integer := Trace'First;
--           Last  : Integer := Trace'Last;
--        begin
--           for Index in reverse Trace'Range loop
--              if Trace (Index) = Character'Val (10) then
--                 Last := Index - 1;
--                 exit;
--              end if;
--           end loop;
--           for Index in reverse Trace'First..Last loop
--              if Trace (Index) = Character'Val (10) then
--                 First := Index + 1;
--                 exit;
--              end if;
--           end loop;
--           return Trace (First..Last);
--        end;
--     end Caller;
--
--     Old_Size : Integer := 0;
--     Old_Text : String (1..1024);
--     Old_Last : Integer := 0;
--     Mutex    : aliased Synchronization.Mutexes.Mutex;
--
--     procedure Memory_Trace
--               (  Text         : String  := "";
--                  Request_Info : Boolean := False
--               )  is
--        use Strings_Edit.Integers;
--        Lock     : Synchronization.Mutexes.Holder (Mutex'Access);
--        New_Size : constant Integer := Memory_Use;
--     begin
--        if New_Size > Old_Size then
--           if Request_Info then
--              Trace_To_File
--              (  Message => (  Image (Old_Size)
--                            &  " [+"
--                            &  Image (New_Size - Old_Size)
--                            &  " "
--                            &  Old_Text (1..Old_Last)
--                            &  "] "
--                            &  Text
--                            &  " "
--                            &  Gtk.Main.Router.Get_Request_Info
--                            &  " in "
--                            &  Caller
--                            ),
--                 Prefix  => "MemUse: "
--              );
--           else
--              Trace_To_File
--              (  Message => (  Image (Old_Size)
--                            &  " [+"
--                            &  Image (New_Size - Old_Size)
--                            &  " "
--                            &  Old_Text (1..Old_Last)
--                            &  "] "
--                            &  Text
--                            &  " in "
--                            &  Caller
--                            ),
--                 Prefix  => "MemUse: "
--              );
--           end if;
--           Old_Size := New_Size;
--        end if;
--        Old_Text (1..Text'Length) := Text;
--        Old_Last := Text'Length;
--     end Memory_Trace;
--
--  begin
--     declare
--        Result       : BOOL;
--        Process_Mask : aliased DWORD_PTR := 0;
--        System_Mask  : aliased DWORD_PTR := 0;
--     begin
--        Result :=
--           SetProcessAffinityMask
--           (  ProcessAffinityMask => 1
--           );
--        Result :=
--           GetProcessAffinityMask
--           (  ProcessAffinityMask => Process_Mask'Access,
--              SystemAffinityMask  => System_Mask'Access
--           );
--     end;
end MAX_Trace;
