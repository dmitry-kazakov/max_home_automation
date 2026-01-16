--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Schedule                                Luebeck            --
--  Implementation                                 Summer, 2015       --
--                                                                    --
--                                Last revision :  17:11 07 Feb 2021  --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Streams.Stream_IO;      use Ada.Streams.Stream_IO;
with Gdk.Event;                  use Gdk.Event;
with GLib.Messages;              use GLib.Messages;
with GLib.Properties;            use GLib.Properties;
with GLib.Values;                use GLib.Values;
with Gtk.Cell_Renderer;          use Gtk.Cell_Renderer;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.File_Chooser;           use Gtk.File_Chooser;
with Gtk.File_Filter;            use Gtk.File_Filter;
with Gtk.Grid;                   use Gtk.Grid;
with Gtk.Missed;                 use Gtk.Missed;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Separator;              use Gtk.Separator;
with Gtk.Stock;                  use Gtk.Stock;
with Gtk.Style_Context;          use Gtk.Style_Context;
with Gtk.Window;                 use Gtk.Window;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with MAX_IO.Storing_Parameters;  use MAX_IO.Storing_Parameters;
with MAX_Trace;                  use MAX_Trace;
with Strings_Edit;               use Strings_Edit;
with Strings_Edit.Floats;        use Strings_Edit.Floats;
with Strings_Edit.Integers;      use Strings_Edit.Integers;
with System;                     use System;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GLib.Object.Checked_Destroy;
with Image_Cancel_XPM.Image;

package body MAX_Schedule is
   use Gtk.Enums;
   use Max_Icon_Factory;
   use Gtk.Image;

   No_Column          : constant := 0;
   Time_Column        : constant := 1;
   Temperature_Column : constant := 2;
   Unset_Column       : constant := 3;

   Once   : Boolean := True;
   Degree : constant UTF8_String := Character'Val (16#C2#) &
                                    Character'Val (16#B0#);

   function Where (Name : String) return String is
   begin
      return " in MAX_Schedule." & Name;
   end Where;

   List      : Scheduled_Devices_Maps.Map;
   Clipboard : Day_Schedule;

   function Image
            (  Store : not null access Gtk_List_Store_Record'Class;
               Row   : Gtk_Tree_Iter
            )  return String is
      Path : constant Gtk_Tree_Path := Store.Get_Path (Row);
   begin
      return Result : constant String := To_String (Path) do
         Path_Free (Path);
      end return;
   end Image;

   function Can_Close
            (  This : not null access Schedule_Record
            )  return Boolean is
      Dialog : Gtk_Dialog;
      Label  : Gtk_Label;
      Box    : Gtk_Box;
      Image  : Gtk_Image;
      Result : Gtk_Response_Type;
      Top    : constant Gtk_Widget := Window.Get_Toplevel;
   begin
      if This.Data.Undo.Is_Empty then
         return True;
      end if;
      if Top = null or else Top.all not in Gtk_Window_Record'Class then
         return True;
      end if;
      Gtk_New
      (  Dialog => Dialog,
         Title  => "Schedule changed",
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
         "The thermostat schedule was not saved. "     &
         "Changes will be lost if you close the tab. " &
         "Continue anyway?"
      );
      Label.Set_Selectable (True);
      Label.Set_Justify (Justify_Left);
      Box.Pack_Start (Label, Padding => 10);
      Dialog.Show_All;
      Result := Dialog.Run;
      GLib.Object.Checked_Destroy (Dialog);
      return Result = Gtk_Response_Yes;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Can_Close")
         )  );
         return True;
   end Can_Close;

   procedure Canceled (Page : in out Schedule_Record) is
   begin
      Say
      (  "Timed out " & Image (Page.Status),
         "Store error"
      );
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Canceled")
         )  );
   end Canceled;

   procedure Changed_Page
             (  Widget : access Gtk_Notebook_Record'Class;
                Page   : not null access Gtk_Widget_Record'Class;
                No     : GUInt
             )  is
      This : Gtk_Widget;
   begin
      for Index in 0..Widget.Get_N_Pages - 1 loop
         This := Widget.Get_Nth_Page (Index);
         if This.all in Schedule_Record'Class then
            declare
               Page : constant Schedule :=
                               Schedule_Record'Class
                               (  This.all
                               ) 'Unchecked_Access;
            begin
               if Index = GInt (No) then
                  if not Is_In (Page.Header, Page.Close) then
                     Page.Header.Pack_Start (Page.Close);
                  end if;
                  Page.Set_Action_Buttons; -- Refresh button status
               else
                  if Is_In (Page.Header, Page.Close) then
                     Remove (Page.Header, Page.Close);
                  end if;
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
            &  Where ("Changed_Page")
         )  );
   end Changed_Page;

   procedure Close
             (  Pages  : not null access Gtk_Notebook_Record'Class;
                Cube   : RF_Address;
                Device : RF_Address
             )  is
      Key  : constant Key_Type := (Cube, Device);
      No   : GInt;
      Page : Schedule;
   begin
      if List.Is_In (Key) then
         Page := List.Get (Key);
         No   := Find (Page);
         if No >= 0 then
            Page.Parent.Remove_Page (No);
         end if;
      end if;
   end Close;

   procedure Configure
             (  Pages : not null access Gtk_Notebook_Record'Class;
                Rooms : not null access Rooms_List_Record'Class;
                Cube  : RF_Address;
                Data  : Device_Parameters_Data_Handles.Handle
             )  is
      Parameters : Device_Parameters renames Data.Ptr.Parameters;
      Key        : constant Key_Type := (Cube, Parameters.Address);
      This       : Schedule;
   begin
      if Parameters.Kind_Of not in Radiator_Thermostat..Wall_Thermostat
      then
         return;
      end if;
      if List.Is_In (Key) then
         This := List.Get (Key);
         Pages.Set_Current_Page (Find (This));
      else
         This := Gtk_Schedule_New (Pages, Rooms, Parameters);
         This.Key := Key;
         if This.All_Room /= null then
            This.All_Room.Set_Label
            (  "into in all thermostats in '"
            &  Data.Ptr.Room
            &  '''
            );
         end if;
         List.Add (Key, This);
         Gtk_New (This.Header, Orientation_Horizontal, 3);
         Gtk_New (This.Intact, "gtk-file", Icon_Size_Small_Toolbar);
         This.Header.Pack_Start (This.Intact);
         Gtk_New (This.Modified, "gtk-edit", Icon_Size_Small_Toolbar);
         This.Header.Pack_Start (This.Modified);
         This.Name := Gtk_Label_New
                      (  Image (Parameters.Address)
                      &  " "
                      &  Parameters.Name
                      );
         This.Header.Pack_Start (This.Name);
         This.Header.Pack_Start (This.Close);
         This.Close.Ref;
         This.Parent := Pages.all'Unchecked_Access;
         This.Header.Show_All;
         This.Modified.Hide;
         declare
            Context : constant Gtk_Style_Context :=
                               Get_Style_Context (This.View (Mo).View);
         begin
            This.Selected :=
               Get_Background_Color (Context, Gtk_State_Flag_Selected);
            This.Unselected :=
               Get_Background_Color (Context, Gtk_State_Flag_Normal);
         end;
         This.Show_All;
         Pages.Set_Current_Page (Pages.Append_Page (This, This.Header));
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Configure")
         )  );
   end Configure;

   procedure Commit
             (  Cell : access Gtk_Cell_Renderer_Fixed_Record'Class;
                Page : Day_View_Ptr
             )  is
      Depth : Integer := 0;
      Row   : Gtk_Tree_Iter;
      Value : GDouble;
      Point : Set_Point;
   begin
      Row := Get_Iter_From_String (Page.List, Get_Path (Cell));
      if Row /= Null_Iter then
         Value := Get_Property (Cell, Build ("value"));
         if Value <= 0.0 then
            Point.Point := 0.0;
         elsif Value >= 63.5 then
            Point.Point := 63.5;
         else
            Point.Point := Centigrade (Value);
         end if;
         if Get (Page.List, Row) then
            Say
            (  (  "Until time must be set before the set temperature ("
               &  Image (Point.Point)
               &  Degree
               &  "C)"
               ),
               "Error input"
            );
         else
            Point.Last := Get (Page.List, Row);
            Page.Parent.Execute
            (  (Replace_Item, Page.Day, Pack (Point)),
               Depth,
               Page.Parent.Data.Undo
            );
            Page.Parent.Data.Redo.Erase;
            Page.Parent.Set_Action_Buttons;
            Page.List.Ref;
            Page.View.Ref;
            Focus_Messages.Send
            (  On_Focus_Until'Access,
               (Page, Point)
            );
         end if;
      end if;
   exception
      when End_Error =>
         raise;
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Commit")
         )  );
   end Commit;

   function Delete
            (  Store : not null access Gtk_List_Store_Record'Class;
               Row   : Gtk_Tree_Iter
            )  return Set_Point is
      Point : Set_Point     := (0.0, 18.0);
      This  : Gtk_Tree_Iter := Row;
      Next  : Gtk_Tree_Iter;
   begin
      if not Get (Store, Row) then -- Deleted value
         Point.Last  := Get (Store, Row);
         Point.Point := Get (Store, Row);
      end if;
      loop
         Next := This;
         Store.Next (Next);
         exit when Next = Null_Iter or else Get (Store, Next);
         Set (Store, This, Day_Duration'(Get (Store, Next)));
         Set (Store, This, Centigrade'(Get (Store, Next)));
         This := Next;
      end loop;
      Set (Store, This, True);
      return Point;
   end Delete;

   procedure Edited
             (  Cell   : access Gtk_Cell_Renderer_Text_Record'Class;
                Params : GValues;
                Page   : Day_View_Ptr
             )  is
      Row       : Gtk_Tree_Iter;
      Old_Point : Set_Point;
      New_Point : Set_Point;
      Updated   : Boolean := False;
      Depth     : Natural := 0;
      Path      : constant String  := Get_String (Nth (Params, 1));
      Text      : constant String  := Get_String (Nth (Params, 2));
   begin
      Row := Get_Iter_From_String (Page.List, Path);
      if Row = Null_Iter then
         return;
      end if;
      begin
         New_Point.Last := To_Duration (Text, True);
      exception
         when Error : others =>
            if Trim (Text) /= "" then
               Say (Exception_Message (Error), "Illegal day time");
            end if;
            return;
      end;
      if Get (Page.List, Row) then -- Empty row
         New_Point.Point := 18.0;
         Page.Parent.Execute
         (  (Insert_Item, Page.Day, Pack (New_Point)),
            Depth,
            Page.Parent.Data.Undo
         );
         Page.Parent.Data.Redo.Erase;
         Page.Parent.Set_Action_Buttons;
         Updated := True;
      else
         Old_Point.Last := Get (Page.List, Row);
         if Old_Point.Last /= New_Point.Last then -- Changed
            Old_Point.Point := Get (Page.List, Row);
            New_Point.Point := Old_Point.Point;
            Page.Parent.Data.Undo.Push ((End_Item, Page.Day));
            Page.Parent.Execute
            (  (Delete_Item, Page.Day, Pack (Old_Point)),
               Depth,
               Page.Parent.Data.Undo
            );
            Page.Parent.Execute
            (  (Insert_Item, Page.Day, Pack (New_Point)),
               Depth,
               Page.Parent.Data.Undo
            );
            Page.Parent.Data.Undo.Push ((Begin_Item, Page.Day));
            Page.Parent.Data.Redo.Erase;
            Page.Parent.Set_Action_Buttons;
            Updated := True;
         end if;
      end if;
      if Updated then
         Page.List.Ref;
         Page.View.Ref;
         Focus_Messages.Send
         (  On_Focus_Temperature'Access,
            (Page, New_Point)
         );
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edited")
         )  );
   end Edited;

   function Erase
            (  Store : not null access Gtk_List_Store_Record'Class
            )  return Points_List is
      Points : Points_List (Point_Number'Range);
      Length : Point_Count := 0;
      This   : Gtk_Tree_Iter;
   begin
      This := Store.Get_Iter_First;
      while This /= Null_Iter and then not Get (Store, This) loop
         Length := Length + 1;
         Points (Length).Last  := Get (Store, This);
         Points (Length).Point := Get (Store, This);
         Set (Store, This, True);
         Store.Next (This);
      end loop;
      return Points (1..Length);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Erase")
         )  );
         return Points (1..0);
   end Erase;

   procedure Execute
             (  Widget : not null access Schedule_Record;
                Action : Action_Item;
                Depth  : in out Integer;
                Undo   : in out Action_Stack.Segmented_Stack.Stack
             )  is
      Row   : Gtk_Tree_Iter;
      Equal : Boolean;
   begin
      case Action.To_Do is
         when Begin_Item =>
            Undo.Push (Action);
            Depth := Depth + 1;
         when End_Item =>
            Undo.Push (Action);
            Depth := Depth - 1;
         when Erase_Item =>
            declare
               Store  : constant Gtk_List_Store :=
                                 Widget.View (Action.Day).List;
               Points : constant Points_List := Erase (Store);
            begin
               if Points'Length > 0 then
                  if Depth = 0 then
                     Undo.Push ((End_Item, Action.Day));
                  end if;
                  for Index in reverse Points'Range loop
                     Undo.Push
                     (  (  Insert_Item,
                           Action.Day,
                           Pack (Points (Index))
                     )  );
                  end loop;
                  if Depth = 0 then
                     Undo.Push ((Begin_Item, Action.Day));
                  end if;
               end if;
            end;
         when Delete_Item =>
            declare
               Store : constant Gtk_List_Store :=
                                Widget.View (Action.Day).List;
               Point : Set_Point;
            begin
               Point := Unpack (Action.Deleted);
               Find (Store, Point.Last, Row, Equal);
               if Equal then
                  Point := Delete (Store, Row);
                  Undo.Push ((Insert_Item, Action.Day, Pack (Point)));
               end if;
            end;
         when Insert_Item =>
            declare
               Store : constant Gtk_List_Store :=
                                Widget.View (Action.Day).List;
               Point : Set_Point;
               Old   : Set_Point;
               Full  : Boolean;
            begin
               Point := Unpack (Action.Inserted);
               Find (Store, Point.Last, Row, Equal);
               if Equal then
                  Replace (Store, Row, Point);
                  Undo.Push ((Replace_Item, Action.Day, Pack (Point)));
               else
                  Insert (Store, Row, Point, Old, Full);
                  if Full then
                     Undo.Push ((End_Item, Action.Day));
                     Undo.Push
                     (  (  Delete_Item,
                           Action.Day,
                           Pack (Point)
                     )  );
                     Undo.Push ((Insert_Item, Action.Day, Pack (Old)));
                     Undo.Push ((Begin_Item, Action.Day));
                  else
                     Undo.Push
                     (  (  Delete_Item,
                           Action.Day,
                           Pack (Point)
                     )  );
                  end if;
               end if;
            end;
         when Replace_Item =>
            declare
               Store : constant Gtk_List_Store :=
                                Widget.View (Action.Day).List;
               Point : Set_Point;
            begin
               Point := Unpack (Action.Replaced);
               Find (Store, Point.Last, Row, Equal);
               if Equal and then Point.Point /= Get (Store, Row) then
                  Replace (Store, Row, Point);
                  Undo.Push ((Replace_Item, Action.Day, Pack (Point)));
               end if;
            end;
      end case;
   end Execute;

   procedure Execute
             (  Widget : not null access Schedule_Record;
                Redo   : in out Action_Stack.Segmented_Stack.Stack;
                Undo   : in out Action_Stack.Segmented_Stack.Stack
             )  is
      Depth  : Integer := 0;
      Action : Action_Item;
      Row    : Gtk_Tree_Iter;
   begin
      while not Redo.Is_Empty loop
         Action := Redo.Top;
         Redo.Pop;
         Widget.Execute (Action, Depth, Undo);
         exit when Depth <= 0;
      end loop;
      Widget.Set_Action_Buttons;
   end Execute;

   procedure Find
             (  Store : not null access Gtk_List_Store_Record'Class;
                Time  : Day_Duration;
                Row   : out Gtk_Tree_Iter;
                Equal : out Boolean
             )  is
      This : Day_Duration;
   begin
      Row := Store.Get_Iter_First;
      while Row /= Null_Iter and then not Get (Store, Row) loop
         This := Get (Store, Row);
         if Time <= This then
            Equal := This = Time;
            return;
         end if;
         Next (Store, Row);
      end loop;
      Equal := False;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Find")
         )  );
         Row   := Null_Iter;
         Equal := False;
   end Find;

   function Find (This : not null access Schedule_Record) return GInt is
   begin
      if This.Parent /= null then
         for Index in 0..This.Parent.Get_N_Pages - 1 loop
            if (  This.Parent.Get_Nth_Page (Index)
               =  This.all'Unchecked_Access
               )
            then
               return Index;
            end if;
         end loop;
      end if;
      return -1;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Find (page)")
         )  );
         return -1;
   end Find;

   procedure Finished (Page : in out Schedule_Record) is
      function Cycle return Boolean is
      begin
         case Page.Thermostats.Get_Size is
            when 0 =>
               return False;
            when 1 =>
               Page.Thermostats.Remove (Positive'(1));
               return False;
            when others =>
               Page.Thermostats.Remove (Positive'(1));
               return True;
         end case;
      end Cycle;
   begin
      if Page.Reported then
         return;
      end if;
      declare
         Data : Device_Parameters renames Page.Parameters;
         To   : constant RF_Address := Page.Thermostats.Get (1);
      begin
         loop
            Page.Status :=
               Next
               (  Action     => Page.Status,
                  Schedule   => not Page.Data.Undo.Is_Empty,
                  Parameters => Page.Changed,
                  Valve      => Page.Valve,
                  Cycle      => Cycle'Access
               );
            case Page.Status is
               when Store_Start =>
                  null;
               when Store_Schedule =>
                  exit;
               when Store_Parameters =>
                  exit;
               when Store_Valve =>
                  exit;
               when Store_Stop =>
                  Page.Changed := False;
                  Page.Edited  := False;
                  Page.Valve   := False;
                  Page.Data.Redo.Erase;
                  Page.Data.Undo.Erase;
                  Page.Set_Action_Buttons;
                  Say
                  (  "Configuration has been successfully set",
                     "Success",
                     "gtk-dialog-info"
                  );
                  return;
            end case;
         end loop;
         Store_Configuration
         (  Box        => Page.Key.Box,
            Device     => To,
            Action     => Page.Status,
            Parameters => Data,
            Handler    => Page'Unchecked_Access
         );
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_S_Response")
         )  );
   end Finished;

   function Get
            (  Widget : not null access Schedule_Record
            )  return Week_Schedule is
      Result : Week_Schedule;
   begin
      for Day in Result'Range loop
         declare
            Points : constant Points_List :=
                              Get (Widget.View (Day).List);
         begin
            Result (Day) := (Points'Length, Points);
         end;
      end loop;
      return Result;
   end Get;

   function Get
            (  Store : not null access Gtk_List_Store_Record'Class
            )  return Points_List is
      Points : Points_List (Point_Number'Range);
      Length : Point_Count := 0;
      This   : Gtk_Tree_Iter;
   begin
      This := Store.Get_Iter_First;
      while This /= Null_Iter and then not Get (Store, This) loop
         Length := Length + 1;
         Points (Length).Last  := Get (Store, This);
         Points (Length).Point := Get (Store, This);
         Store.Next (This);
      end loop;
      return Points (1..Length);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get")
         )  );
         return Points (1..0);
   end Get;

   function Get
            (  Store : not null access Gtk_List_Store_Record'Class;
               Row   : Gtk_Tree_Iter
            )  return Boolean is
      Value  : GValue;
      Result : Boolean;
   begin
      Store.Get_Value (Row, Unset_Column, Value);
      Result := Get_Boolean (Value);
      Unset (Value);
      return Result;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get (Boolean)")
         )  );
         return True;
   end Get;

   function Get
            (  Store : not null access Gtk_List_Store_Record'Class;
               Row   : Gtk_Tree_Iter
            )  return Centigrade is
      Value  : GValue;
      Result : GDouble;
   begin
      Store.Get_Value (Row, Temperature_Column, Value);
      Result := Get_Double (Value);
      Unset (Value);
      if Result <= GDouble (Centigrade'First) then
         return Centigrade'First;
      elsif Result >= GDouble (Centigrade'Last) then
         return Centigrade'Last;
      else
         return Centigrade (Result);
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get (Centigrade)")
         )  );
         return 18.0;
   end Get;

   function Get
            (  Store : not null access Gtk_List_Store_Record'Class;
               Row   : Gtk_Tree_Iter
            )  return Day_Duration is
      Value  : GValue;
      Result : Day_Duration;
   begin
      Store.Get_Value (Row, Time_Column, Value);
      Result := To_Duration (Get_String (Value), True);
      Unset (Value);
      return Result;
   exception
      when Data_Error =>
         Unset (Value);
         return 0.0;
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get (Centigrade)")
         )  );
         Unset (Value);
         return 0.0;
   end Get;

   function Get_Last
            (  Store : not null access Gtk_List_Store_Record'Class
            )  return Gtk_Tree_Iter is
   begin
      return Store.Nth_Child (Null_Iter, 12);
   end Get_Last;

   function Get_Object
            (  Page : not null access Schedule_Record
            )  return GObject is
   begin
       return Page.all'Unchecked_Access;
   end Get_Object;

   function Gtk_Schedule_New
            (  Pages      : not null access Gtk_Notebook_Record'Class;
               Rooms      : not null access Rooms_List_Record'Class;
               Parameters : Device_Parameters
            )  return Schedule is
      Result    : Schedule := new Schedule_Record
                                  (  Parameters.Kind_Of,
                                     Parameters.Name_Length
                                  );
      Buttons   : Gtk_HBox;
      Separator : Gtk_HSeparator;
   begin
      Result.Parameters := Parameters;
      Result.Rooms   := Rooms.all'Unchecked_Access;
      Gtk.Box.Initialize (Result, Orientation_Vertical, 3);
      Gtk_New_Hbox (Buttons, Spacing => 3);
      Result.Pack_Start (Buttons, False, False);
         Open_Buttons.Gtk_New (Result.Open);
         Buttons.Pack_Start (Result.Open, False, False);
         Save_Buttons.Gtk_New (Result.Save);
         Buttons.Pack_Start (Result.Save, False, False);
         Result.Save.Set_Sensitive (False);
         Save_As_Buttons.Gtk_New (Result.Save_As);
         Buttons.Pack_Start (Result.Save_As, False, False);
         Copy_From_Buttons.Gtk_New (Result.Copy_From);
         Result.Copy_From.Set_Sensitive (Get_Thermostats.Get_Size > 1);
         Buttons.Pack_Start (Result.Copy_From, False, False);
         Gtk_New (Separator, Orientation_Vertical);
         Buttons.Pack_Start (Separator, False, False);

         Copy_Buttons.Gtk_New (Result.Copy);
         Buttons.Pack_Start (Result.Copy, False, False);
         Result.Copy.Set_Sensitive (False);
         Cut_Buttons.Gtk_New (Result.Cut);
         Buttons.Pack_Start (Result.Cut, False, False);
         Result.Cut.Set_Sensitive (False);
         Paste_Buttons.Gtk_New (Result.Paste);
         Buttons.Pack_Start (Result.Paste, False, False);
         Result.Paste.Set_Sensitive (False);

         Gtk_New (Separator, Orientation_Vertical);
         Buttons.Pack_Start (Separator, False, False);
         Undo_Buttons.Gtk_New (Result.Undo);
         Buttons.Pack_Start (Result.Undo, False, False);
         Redo_Buttons.Gtk_New (Result.Redo);
         Buttons.Pack_Start (Result.Redo, False, False);
         Delete_Buttons.Gtk_New (Result.Delete);
         Buttons.Pack_Start (Result.Delete, False, False);
         Result.Delete.Set_Sensitive (False);
         Erase_Buttons.Gtk_New (Result.Erase);
         Buttons.Pack_Start (Result.Erase, False, False);
         Result.Erase.Set_Sensitive (False);
         Select_All_Buttons.Gtk_New (Result.Select_All);
         Buttons.Pack_Start (Result.Select_All, False, False);
         Result.Select_All.Set_Sensitive (False);
         Set_Thermostat_Buttons.Gtk_New (Result.Store);
         Buttons.Pack_Start (Result.Store, False, False);

         if Parameters.Kind_Of in Radiator_Thermostat
                               .. Radiator_Thermostat_Plus then
            Gtk_New (Result.All_Room);
            Buttons.Pack_Start (Result.All_Room, False, False);
         end if;
      ------------------------------------------------------------------
      Gtk_New_Hbox (Result.Schedule, Spacing => 3);
      Result.Pack_Start (Result.Schedule, False, False);
      for Day in Result.View'Range loop
         declare
            Box   : Gtk_VBox;
            Frame : Gtk_Frame;
            Label : Gtk_Label;
            This  : Day_View renames Result.View (Day);
         begin
            This.Day    := Day;
            This.Parent := Result;
            Gtk_New (Frame);
            Frame.Set_Shadow_Type (Shadow_Out);
            Result.Schedule.Pack_Start (Frame, False, False);

            Gtk_New_Vbox (Box, Spacing => 3);
            Frame.Add (Box);

            Gtk_New (This.Box);
            Gtk_New (Label, Image (Day, True));
            This.Box.Add (Label);
            This.Box.Set_Events (Key_Press_Mask);
            Box.Pack_Start (This.Box, False, False);
            Gtk_New (Frame);
            Frame.Set_Shadow_Type (Shadow_Out);
            Box.Pack_Start (Frame);

            Gtk_New
            (  This.List,
               (GType_Int, GType_String, GType_Double, GType_Boolean)
            );
            Gtk_New (This.View, +This.List);
            This.View.Get_Selection.Set_Mode (Selection_Multiple);
            This.List.Unref;
            Frame.Add (This.View);

            declare
               Column      : Gtk_Tree_View_Column;
               Text        : Gtk_Cell_Renderer_Text;
               Temperature : Gtk_Cell_Renderer_Fixed;
               Column_No   : Gint;
            begin
               Gtk_New (Column);
               Column.Set_Title ("No");
               Gtk_New (Text);
               Column.Pack_Start (Text, False);
               Set_Property
               (  Text,
                  Gtk.Cell_Renderer.Xalign_Property,
                  1.0
               );
               Column.Add_Attribute (Text, "text", No_Column);
               Column_No := This.View.Append_Column (Column);
               Column.Set_Resizable (False);

               Gtk_New (Column);
               Column.Set_Title ("Until");
               Gtk_New (Text);
               Column.Pack_Start (Text, True);
               Column.Add_Attribute (Text, "text", Time_Column);
               Column_No := This.View.Append_Column (Column);
               Column.Set_Resizable (False);
               Set_Property (Text, Build ("editable"), True);
               Edited_Handlers.Connect
               (  Text,
                  "edited",
                  Edited'Access,
                  Result.View (Day)'Unchecked_Access,
                  True
               );

               Gtk_New (Column);
               Column.Set_Title ("C" & Degree);
               Gtk_New (Temperature, 1);
               Temperature.Set_Mode (Cell_Renderer_Mode_Editable);
               Column.Pack_Start (Temperature, False);
               Column.Add_Attribute
               (  Temperature,
                  "value",
                  Temperature_Column
               );
               Column.Add_Attribute
               (  Temperature,
                  "empty",
                  Unset_Column
               );
               Column_No := This.View.Append_Column (Column);
               Column.Set_Resizable (False);
               Temperature_Handlers.Connect
               (  Temperature,
                  "commit",
                  Commit'Access,
                  Result.View (Day)'Unchecked_Access
               );
               Selection_Handlers.Connect
               (  This.View.Get_Selection,
                  "changed",
                  Selection_Handlers.To_Marshaller
                  (  On_Selection'Access
                  ),
                  Result.View (Day)'Unchecked_Access
               );
               Event_Handlers.Connect
               (  This.Box,
                  "button-press-event",
                  On_Selection'Access,
                  Result
               );
            end;
            declare
               Row : Gtk_Tree_Iter;
            begin
               for Point in GInt range 1..13 loop
                  This.List.Append (Row);
                  This.List.Set (Row, No_Column, Point);
                  Set (This.List, Row, False);
               end loop;
            end;
            Result.Set (Day, Parameters.Schedule (Day));
         end;
      end loop;
      ------------------------------------------------------------------
      declare
         Frame : Gtk_Frame;
         Grid  : Gtk_Grid;
         Label : Gtk_Label;
      begin
         Gtk_New (Frame, " Thermostat and valve parameters ");
         Frame.Set_Border_Width (3);
         Result.Pack_Start (Frame);
         Gtk_New (Grid);
         Grid.Set_Border_Width (3);
         Frame.Add (Grid);
         Grid.Set_Column_Spacing (3);
         Grid.Set_Row_Spacing (3);

         Gtk_New (Label, "Comfort");
         Label.Set_Halign (Align_End);
         Label.Set_Valign (Align_Center);
         Grid.Attach (Label, 0, 0, 1, 1);
         Gtk_New (Result.Comfort);
         Result.Comfort.Set_Alignment (0.5);
         Result.Comfort.Set_Text (Image (Parameters.Comfort));
         Result.Comfort.Set_Width_Chars (5);
         if Find_Property (Result.Comfort, "max-width-chars") /= null
         then
            Set_Property
            (  Result.Comfort,
               Build ("max-width-chars"),
               GInt'(5)
            );
         end if;
         Grid.Attach (Result.Comfort, 1, 0, 1, 1);
         Gtk_New (Label, "C" & Degree);
         Label.Set_Halign (Align_Start);
         Label.Set_Valign (Align_Center);
         Grid.Attach (Label, 2, 0, 1, 1);

         Gtk_New (Label, "Eco");
         Label.Set_Halign (Align_End);
         Label.Set_Valign (Align_Center);
         Grid.Attach (Label, 0, 1, 1, 1);
         Gtk_New (Result.Eco);
         Result.Eco.Set_Alignment (0.5);
         Result.Eco.Set_Text (Image (Parameters.Eco));
         Result.Eco.Set_Width_Chars (5);
         if Find_Property (Result.Eco, "max-width-chars") /= null then
            Set_Property
            (  Result.Eco,
               Build ("max-width-chars"),
               GInt'(5)
            );
         end if;
         Grid.Attach (Result.Eco, 1, 1, 1, 1);
         Gtk_New (Label, "C" & Degree);
         Label.Set_Halign (Align_Start);
         Label.Set_Valign (Align_Center);
         Grid.Attach (Label, 2, 1, 1, 1);

         Gtk_New (Label, "Maximum");
         Label.Set_Halign (Align_End);
         Label.Set_Valign (Align_Center);
         Grid.Attach (Label, 3, 0, 1, 1);
         Gtk_New (Result.Max);
         Result.Max.Set_Alignment (0.5);
         Result.Max.Set_Text (Image (Parameters.Max));
         Result.Max.Set_Width_Chars (5);
         if Find_Property (Result.Max, "max-width-chars") /= null
         then
            Set_Property
            (  Result.Max,
               Build ("max-width-chars"),
               GInt'(5)
            );
         end if;
         Grid.Attach (Result.Max, 4, 0, 1, 1);
         Gtk_New (Label, "C" & Degree);
         Label.Set_Halign (Align_Start);
         Label.Set_Valign (Align_Center);
         Grid.Attach (Label, 5, 0, 1, 1);

         Gtk_New (Label, "Minimum");
         Label.Set_Halign (Align_End);
         Label.Set_Valign (Align_Center);
         Grid.Attach (Label, 3, 1, 1, 1);
         Gtk_New (Result.Min);
         Result.Min.Set_Alignment (0.5);
         Result.Min.Set_Text (Image (Parameters.Min));
         Result.Min.Set_Width_Chars (5);
         if Find_Property (Result.Min, "max-width-chars") /= null
         then
            Set_Property
            (  Result.Min,
               Build ("max-width-chars"),
               GInt'(5)
            );
         end if;
         Grid.Attach (Result.Min, 4, 1, 1, 1);
         Gtk_New (Label, "C" & Degree);
         Label.Set_Halign (Align_Start);
         Label.Set_Valign (Align_Center);
         Grid.Attach (Label, 5, 1, 1, 1);

         if Parameters.Kind_Of in Radiator_Thermostat
                               .. Wall_Thermostat then
            Gtk_New (Label, "Offset");
            Label.Set_Halign (Align_End);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 6, 0, 1, 1);
            Gtk_New (Result.Offset);
            Result.Offset.Set_Alignment (0.5);
            Result.Offset.Set_Text (Image (Parameters.Offset));
            Result.Offset.Set_Width_Chars (5);
            if Find_Property (Result.Offset, "max-width-chars") /= null
            then
               Set_Property
               (  Result.Offset,
                  Build ("max-width-chars"),
                  GInt'(5)
               );
            end if;
            Grid.Attach (Result.Offset, 7, 0, 1, 1);
            Gtk_New (Label, "C" & Degree);
            Label.Set_Halign (Align_Start);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 8, 0, 1, 1);

            Gtk_New (Label, "Airing");
            Label.Set_Halign (Align_End);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 6, 1, 1, 1);
            Gtk_New (Result.Window_Open);
            Result.Window_Open.Set_Tooltip_Text
            (  "The temperature used when a window in the same room "
            &  "is open"
            );
            Result.Window_Open.Set_Alignment (0.5);
            Result.Window_Open.Set_Text
            (  Image (Parameters.Window_Open)
            );
            Result.Window_Open.Set_Width_Chars (5);
            if (  Find_Property (Result.Window_Open, "max-width-chars")
               /= null
               )
            then
               Set_Property
               (  Result.Window_Open,
                  Build ("max-width-chars"),
                  GInt'(5)
               );
            end if;
            Grid.Attach (Result.Window_Open, 7, 1, 1, 1);
            Gtk_New (Label, "C" & Degree);
            Label.Set_Halign (Align_Start);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 8, 1, 1, 1);
         end if;
         if Parameters.Kind_Of in Radiator_Thermostat
                               .. Radiator_Thermostat_Plus then
            Gtk_New (Label, "Airing time");
            Label.Set_Halign (Align_End);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 6, 2, 1, 1);
            Gtk_New (Result.Window_Time);
            Result.Window_Time.Set_Tooltip_Text
            (  "The airing time has 5 minutes step. So the minimal "
            &  "positive time is 5."
            );
            Result.Window_Time.Set_Alignment (0.5);
            Result.Window_Time.Set_Text
            (  Minutes (Parameters.Window_Time)
            );
            Result.Window_Time.Set_Width_Chars (5);
            if (  Find_Property (Result.Window_Time, "max-width-chars")
               /= null
               )
            then
               Set_Property
               (  Result.Window_Time,
                  Build ("max-width-chars"),
                  GInt'(5)
               );
            end if;
            Grid.Attach (Result.Window_Time, 7, 2, 1, 1);
            Gtk_New (Label, "min");
            Label.Set_Halign (Align_Start);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 8, 2, 1, 1);

            Gtk_New (Label, "Boost time");
            Label.Set_Halign (Align_End);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 9, 0, 1, 1);
            Gtk_New (Result.Boost_Time);
            Result.Boost_Time.Set_Alignment (0.5);
            Result.Boost_Time.Set_Text
            (  Minutes (Parameters.Boost_Time)
            );
            Result.Boost_Time.Set_Width_Chars (5);
            if (  Find_Property (Result.Boost_Time, "max-width-chars")
               /= null
               )
            then
               Set_Property
               (  Result.Boost_Time,
                  Build ("max-width-chars"),
                  GInt'(5)
               );
            end if;
            Grid.Attach (Result.Boost_Time, 10, 0, 1, 1);
            Gtk_New (Label, "min");
            Label.Set_Halign (Align_Start);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 11, 0, 1, 1);

            Gtk_New (Label, "Boost valve");
            Label.Set_Halign (Align_End);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 9, 1, 1, 1);
            Gtk_New (Result.Boost_Valve);
            Result.Boost_Valve.Set_Alignment (0.5);
            Result.Boost_Valve.Set_Text
            (  Image (Integer (Float (Parameters.Boost_Valve) * 100.0))
            );
            Result.Boost_Valve.Set_Width_Chars (5);
            if (  Find_Property (Result.Boost_Valve, "max-width-chars")
               /= null
               )
            then
               Set_Property
               (  Result.Boost_Valve,
                  Build ("max-width-chars"),
                  GInt'(5)
               );
            end if;
            Grid.Attach (Result.Boost_Valve, 10, 1, 1, 1);
            Gtk_New (Label, "%");
            Label.Set_Halign (Align_Start);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 11, 1, 1, 1);

            Gtk_New (Label, "Max valve");
            Label.Set_Halign (Align_End);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 12, 0, 1, 1);
            Gtk_New (Result.Max_Valve);
            Result.Max_Valve.Set_Alignment (0.5);
            Result.Max_Valve.Set_Text
            (  Image (Integer (Float (Parameters.Max_Valve) * 100.0))
            );
            Result.Max_Valve.Set_Width_Chars (5);
            if (  Find_Property (Result.Max_Valve, "max-width-chars")
               /= null
               )
            then
               Set_Property
               (  Result.Max_Valve,
                  Build ("max-width-chars"),
                  GInt'(5)
               );
            end if;
            Grid.Attach (Result.Max_Valve, 13, 0, 1, 1);
            Gtk_New (Label, "%");
            Label.Set_Halign (Align_Start);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 14, 0, 1, 1);

            Gtk_New (Label, "Valve offset");
            Label.Set_Halign (Align_End);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 12, 1, 1, 1);
            Gtk_New (Result.Valve_Offset);
            Result.Valve_Offset.Set_Alignment (0.5);
            Result.Valve_Offset.Set_Text
            (  Image (Integer (Float (Parameters.Valve_Offset) * 100.0))
            );
            Result.Valve_Offset.Set_Width_Chars (5);
            if (  Find_Property (Result.Valve_Offset, "max-width-chars")
               /= null
               )
            then
               Set_Property
               (  Result.Valve_Offset,
                  Build ("max-width-chars"),
                  GInt'(5)
               );
            end if;
            Grid.Attach (Result.Valve_Offset, 13, 1, 1, 1);
            Gtk_New (Label, "%");
            Label.Set_Halign (Align_Start);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 14, 1, 1, 1);

            Gtk_New (Label, "Decalcification");
            Label.Set_Halign (Align_End);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 9, 2, 3, 1);
            Gtk_New (Result.Decalc_Day);
            Result.Decalc_Day.Append_Text ("Mo");
            Result.Decalc_Day.Append_Text ("Tu");
            Result.Decalc_Day.Append_Text ("We");
            Result.Decalc_Day.Append_Text ("Th");
            Result.Decalc_Day.Append_Text ("Fr");
            Result.Decalc_Day.Append_Text ("Sa");
            Result.Decalc_Day.Append_Text ("Su");
            Result.Decalc_Day.Set_Active
            (  Week_Day'Pos (Parameters.Decalcification.Day)
            );
            Grid.Attach (Result.Decalc_Day, 12, 2, 1, 1);
            Gtk_New (Result.Decalc_Time);
            Result.Decalc_Time.Append_Text ("00");
            Result.Decalc_Time.Append_Text ("01");
            Result.Decalc_Time.Append_Text ("02");
            Result.Decalc_Time.Append_Text ("03");
            Result.Decalc_Time.Append_Text ("04");
            Result.Decalc_Time.Append_Text ("05");
            Result.Decalc_Time.Append_Text ("06");
            Result.Decalc_Time.Append_Text ("07");
            Result.Decalc_Time.Append_Text ("08");
            Result.Decalc_Time.Append_Text ("09");
            Result.Decalc_Time.Append_Text ("10");
            Result.Decalc_Time.Append_Text ("11");
            Result.Decalc_Time.Append_Text ("12");
            Result.Decalc_Time.Append_Text ("13");
            Result.Decalc_Time.Append_Text ("14");
            Result.Decalc_Time.Append_Text ("15");
            Result.Decalc_Time.Append_Text ("16");
            Result.Decalc_Time.Append_Text ("17");
            Result.Decalc_Time.Append_Text ("18");
            Result.Decalc_Time.Append_Text ("19");
            Result.Decalc_Time.Append_Text ("20");
            Result.Decalc_Time.Append_Text ("21");
            Result.Decalc_Time.Append_Text ("22");
            Result.Decalc_Time.Append_Text ("23");
            Result.Decalc_Time.Set_Active
            (  GInt ((Parameters.Decalcification.Time) / 3600.0)
            );
            Grid.Attach (Result.Decalc_Time, 13, 2, 1, 1);
            Gtk_New (Label, "hour");
            Label.Set_Halign (Align_Start);
            Label.Set_Valign (Align_Center);
            Grid.Attach (Label, 14, 2, 1, 1);
         end if;
      end;
      ------------------------------------------------------------------
      Gtk_New (Result.Close);
      Result.Close.Set_Can_Default (False);
      Result.Close.Set_Can_Focus   (False);
      declare
         Image : constant Gtk_Image := Image_Cancel_XPM.Image;
      begin
         Result.Close.Add (Image);
         Event_Handlers.Connect
         (  Result.Close,
            "button-press-event",
            On_Close'Access,
            Result
         );
         Result.Close.Set_Events (Key_Press_Mask);
      end;
      Schedule_Handlers.Connect
      (  Result.Copy,
         "clicked",
         On_Copy'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Copy_From,
         "clicked",
         On_Copy_From'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Cut,
         "clicked",
         On_Cut'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Delete,
         "clicked",
         On_Delete'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result,
         "destroy",
         On_Destroy'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Open,
         "clicked",
         On_Open'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Paste,
         "clicked",
         On_Paste'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Redo,
         "clicked",
         On_Redo'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Save,
         "clicked",
         On_Save'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Save_As,
         "clicked",
         On_Save_As'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Select_All,
         "clicked",
         On_Select_All'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Store,
         "clicked",
         On_Store'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Undo,
         "clicked",
         On_Undo'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Erase,
         "clicked",
         On_Erase'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Comfort,
         "changed",
         On_Parameter_Changed'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Eco,
         "changed",
         On_Parameter_Changed'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Max,
         "changed",
         On_Parameter_Changed'Access,
         Result
      );
      Schedule_Handlers.Connect
      (  Result.Min,
         "changed",
         On_Parameter_Changed'Access,
         Result
      );
      if Parameters.Kind_Of in Radiator_Thermostat
                            .. Wall_Thermostat then
         Schedule_Handlers.Connect
         (  Result.Offset,
            "changed",
            On_Parameter_Changed'Access,
            Result
         );
         Schedule_Handlers.Connect
         (  Result.Window_Open,
            "changed",
            On_Parameter_Changed'Access,
            Result
         );
      end if;
      if Parameters.Kind_Of in Radiator_Thermostat
                            .. Radiator_Thermostat_Plus then
         Schedule_Handlers.Connect
         (  Result.Window_Time,
            "changed",
            On_Parameter_Changed'Access,
            Result
         );
         Schedule_Handlers.Connect
         (  Result.Offset,
            "changed",
            On_Parameter_Changed'Access,
            Result
         );
         Schedule_Handlers.Connect
         (  Result.Boost_Time,
            "changed",
            On_Valve_Changed'Access,
            Result
         );
         Schedule_Handlers.Connect
         (  Result.Boost_Valve,
            "changed",
            On_Valve_Changed'Access,
            Result
         );
         Schedule_Handlers.Connect
         (  Result.Max_Valve,
            "changed",
            On_Valve_Changed'Access,
            Result
         );
         Schedule_Handlers.Connect
         (  Result.Valve_Offset,
            "changed",
            On_Valve_Changed'Access,
            Result
         );
         Schedule_Handlers.Connect
         (  Result.Decalc_Time,
            "changed",
            On_Valve_Changed'Access,
            Result
         );
         Schedule_Handlers.Connect
         (  Result.Decalc_Day,
            "changed",
            On_Valve_Changed'Access,
            Result
         );
      end if;
      if Once then
         Once := False;
         Pages.On_Switch_Page (Changed_Page'Access);
      end if;
      return Result;
   end Gtk_Schedule_New;

   procedure Insert
             (  Store     : not null access Gtk_List_Store_Record'Class;
                Row       : Gtk_Tree_Iter;
                New_Point : Set_Point;
                Old_Point : out Set_Point;
                Full      : out Boolean
             )  is
      This     : Gtk_Tree_Iter := Row;
      Previous : Set_Point     := New_Point;
   begin
      Full := False;
      while This /= Null_Iter loop
         Full := not Get (Store, This);
         if Full then
            Old_Point.Last  := Get (Store, This);
            Old_Point.Point := Get (Store, This);
         else
            Set (Store, This, False);
         end if;
         Set (Store, This, Previous.Last);
         Set (Store, This, Previous.Point);
         exit when not Full;
         Store.Next (This);
         Previous := Old_Point;
      end loop;
   end Insert;

   procedure On_A_Response
             (  Page     : in out Schedule_Record;
                Address  : RF_Address;
                Devices  : Devices_Maps.Map;
                List     : Rooms_Maps.Map;
                Expected : in out Natural
             )  is
   begin
      if Expected > 0 then
         Expected := Expected - 1;
      end if;
   end On_A_Response;

   function On_Close
            (  Object : access GObject_Record'Class;
               Page   : Schedule
            )  return Boolean is
      No : GInt;
   begin
      No := Find (Page);
      if No >= 0 then
         if Page.Can_Close then
            Page.Parent.Remove_Page (No);
         end if;
      end if;
      return False;
   end On_Close;

   procedure On_Copy
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
   begin
      declare
         Points : constant Points_List :=
                  Selected
                  (  Page.View (Page.Current).List,
                     Page.View (Page.Current).View.Get_Selection
                  );
      begin
         Clipboard := (Points'Length, Points);
         Page.Set_Action_Buttons;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Copy")
         )  );
   end On_Copy;

   procedure On_Copy_From_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             )  is
      use Gtk.Missed;
   begin
      declare
          Dialog : Copy_From_Dialog_Record'Class renames
                   Copy_From_Dialog_Record'Class (Self.all);
      begin
         if Gtk_Response_Accept = Response then
            declare
               Row   : Gtk_Tree_Iter;
               Model : Gtk_Tree_Model;
            begin
               Dialog.View.Get_Selection.Get_Selected (Model, Row);
               if Row /= Null_Iter then
                  declare
                     Data : constant Device_Parameters_Data_Handles.
                                     Handle :=
                            Get_Parameters
                            (  RF_Address
                               (  Dialog.List.Get_Int (Row, 3)
                               ),
                               RF_Address
                               (  Integer'
                                  (  Value
                                     (  Get (Dialog.List, Row, 1),
                                        Base => 16
                            )  )  )  );
                     Parameters : Device_Parameters renames
                                  Data.Ptr.Parameters;
                  begin
                     if Parameters.Kind_Of in Radiator_Thermostat
                                           .. Radiator_Thermostat_Plus
                     then
                        Dialog.Page.Replace (Parameters.Schedule);
                     end if;
                  end;
               end if;
            exception
               when others =>
                  null;
            end;
         end if;
      end;
      GLib.Object.Checked_Destroy (Self);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Copy_From_Response")
         )  );
   end On_Copy_From_Response;

   procedure On_Copy_From
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
      Scroll        : Gtk_Scrolled_Window;
      Thermostats   : RF_Address_Sets.Set;
      Accept_Button : Gtk_Button;
      Dialog        : Copy_From_Dialog;
   begin
      Dialog := new Copy_From_Dialog_Record;
      Initialize
      (  Dialog,
         "Select a thermostat to copy from",
          Window,
          Modal
      );
      Gtk_New (Scroll);
      Gtk_New
      (  Dialog.List,
         (  GType_String, -- Name
            GType_String, -- Address
            GType_String, -- Room
            GType_Int     -- Cube address
      )  );
      Dialog.Page := Page;
      Dialog.View := Gtk_Tree_View_New_With_Model (+Dialog.List);
      Dialog.List.Unref;
      Scroll.Add (Dialog.View);
      declare
         Column    : Gtk_Tree_View_Column;
         Text      : Gtk_Cell_Renderer_Text;
         Column_No : Gint;
      begin
         Gtk_New (Column);
         Column.Set_Title ("Name");
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Column.Add_Attribute (Text, "text", 0);
         Column_No := Dialog.View.Append_Column (Column);
         Column.Set_Resizable (True);

         Gtk_New (Column);
         Column.Set_Title ("Address");
         Gtk_New (Text);
         Column.Pack_Start (Text, False);
         Column.Add_Attribute (Text, "text", 1);
         Column_No := Dialog.View.Append_Column (Column);
         Column.Set_Resizable (False);

         Gtk_New (Column);
         Column.Set_Title ("Room");
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Column.Add_Attribute (Text, "text", 2);
         Column_No := Dialog.View.Append_Column (Column);
         Column.Set_Resizable (True);
      end;
      Page.Rooms.Get_Thermostats (Dialog.List, Page.Parameters.Address);
      Dialog.Get_Content_Area.Pack_Start (Scroll);
      Accept_Button :=
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Response => Gtk_Response_Accept,
            Icon     => Stock_OK,
            Label    => "_OK",
            Tip      => "Copy the thermostat's week schedule"
         );
      Accept_Button.Set_Can_Default (True);
      Add_Button_From_Stock
      (  Dialog   => Dialog,
         Response => Gtk_Response_Cancel,
         Icon     => Stock_Cancel,
         Label    => "_Cancel",
         Tip      => "Close the selection dialog"
      );
      Chooser_Handlers.Connect
      (  Dialog.View.Get_Selection,
         "changed",
         Chooser_Handlers.To_Marshaller (On_Thermostat'Access),
         Accept_Button
      );
      Dialog.On_Response (On_Copy_From_Response'Access);
      Dialog.Show_All;
      declare
         Dummy  : GInt;
         Height : GInt;
         Width  : GInt;
      begin
         Dialog.View.Columns_Autosize;
         Dialog.View.Get_Preferred_Height (Dummy, Height);
         Dialog.View.Get_Preferred_Width  (Dummy, Width);
         Scroll.Set_Size_Request
         (  500, --GInt'Min (Width,  500),
            GInt'Min (Height, 300)
         );
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Copy_From")
         )  );
   end On_Copy_From;

   procedure On_Cut
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
      Depth  : Integer := 1;
   begin
      declare
         Points : constant Points_List :=
                  Selected
                  (  Page.View (Page.Current).List,
                     Page.View (Page.Current).View.Get_Selection
                  );
      begin
         if Points'Length > 0 then
            if Points'Length > 1 then
               Page.Data.Undo.Push ((End_Item, Page.Current));
            end if;
            for Index in reverse Points'Range loop
               Page.Execute
               (  (  Delete_Item,
                     Page.Current,
                     Pack (Points (Index))
                  ),
                  Depth,
                  Page.Data.Undo
               );
            end loop;
            if Points'Length > 1 then
               Page.Data.Undo.Push ((Begin_Item, Page.Current));
            end if;
            Page.Data.Redo.Erase;
            Page.View (Page.Current).View.Get_Selection.Unselect_All;
         end if;
         Clipboard := (Points'Length, Points);
         Page.Set_Action_Buttons;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Cut")
         )  );
   end On_Cut;

   procedure On_Destroy
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
      procedure Free is
         new Ada.Unchecked_Deallocation (Edit_Data, Edit_Data_Ptr);
   begin
      Free (Page.Data);
      if Page.Close /= null then
         Page.Close.Unref;
         Page.Close := null;
         for Index in 1..List.Get_Size loop
            if List.Get (Index) = Page then
               List.Remove (Index);
               exit;
            end if;
         end loop;
      end if;
   end On_Destroy;

   procedure On_Delete
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
      Depth : Integer := 1;
   begin
      declare
         Points : constant Points_List :=
                  Selected
                  (  Page.View (Page.Current).List,
                     Page.View (Page.Current).View.Get_Selection
                  );
      begin
         if Points'Length > 0 then
            if Points'Length > 1 then
               Page.Data.Undo.Push ((End_Item, Page.Current));
            end if;
            for Index in reverse Points'Range loop
               Page.Execute
               (  (  Delete_Item,
                     Page.Current,
                     Pack (Points (Index))
                  ),
                  Depth,
                  Page.Data.Undo
               );
            end loop;
            if Points'Length > 1 then
               Page.Data.Undo.Push ((Begin_Item, Page.Current));
            end if;
            Page.Data.Redo.Erase;
            Page.Set_Action_Buttons;
            Page.View (Page.Current).View.Get_Selection.Unselect_All;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Delete")
         )  );
   end On_Delete;

   procedure On_Erase
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
      Depth : Natural := 0;
   begin
      Page.Execute
      (  (Erase_Item, Page.Current),
         Depth,
         Page.Data.Undo
      );
      Page.Data.Redo.Erase;
      Page.Set_Action_Buttons;
      Page.View (Page.Current).View.Get_Selection.Unselect_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Erase")
         )  );
   end On_Erase;

   procedure On_Focus_Temperature (Data : in out Focus_Data) is
      Row   : Gtk_Tree_Iter;
      Equal : Boolean;
   begin
      Find (Data.Page.List, Data.Point.Last, Row, Equal);
      if Equal then
         declare
            Path : constant Gtk_Tree_Path :=
                            Data.Page.List.Get_Path (Row);
         begin
            Data.Page.View.Set_Cursor
            (  Path,
               Data.Page.View.Get_Column (2),
               True
            );
            Path_Free (Path);
         end;
      end if;
      Data.Page.List.Unref;
      Data.Page.View.Unref;
   exception
      when Error : others =>
         Data.Page.List.Unref;
         Data.Page.View.Unref;
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Focus_Temperature")
         )  );
   end On_Focus_Temperature;

   procedure On_Focus_Until (Data : in out Focus_Data) is
      Row   : Gtk_Tree_Iter;
      Equal : Boolean;
   begin
      Find (Data.Page.List, Data.Point.Last, Row, Equal);
      if Equal then
         Data.Page.List.Next (Row);
         if Row /= Null_Iter then
            declare
               Path : constant Gtk_Tree_Path :=
                               Data.Page.List.Get_Path (Row);
            begin
               Data.Page.View.Set_Cursor
               (  Path,
                  Data.Page.View.Get_Column (1),
                  Get (Data.Page.List, Row)
               );
               Path_Free (Path);
            end;
         end if;
      end if;
      Data.Page.List.Unref;
      Data.Page.View.Unref;
   exception
      when Error : others =>
         Data.Page.List.Unref;
         Data.Page.View.Unref;
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Focus_Until")
         )  );
   end On_Focus_Until;

   procedure On_Open_Schedule
             (  Name   : String;
                Widget : in out Gtk_Widget_Record'Class
             )  is
      File  : File_Type;
      Data  : Week_Schedule;
      Depth : Integer := 1;
   begin
      declare
         Page : Schedule_Record'Class renames
                Schedule_Record'Class (Widget);
      begin
         begin
            Open (File, In_File, Name);
         exception
            when Error : others =>
               Say
               (  "File " & Name & ": " & Exception_Message (Error),
                  "Open file error"
               );
               return;
         end;
         begin
            Data := Read (Stream (File));
         exception
            when Error : others =>
               Say
               (  "File " & Name & ": " & Exception_Message (Error),
                  "File read error"
               );
               Close (File);
               return;
         end;
         Close (File);
         Page.Replace (Data);
         Page.File := To_Unbounded_String (Name);
         Page.Set_Action_Buttons;
         Page.View (Page.Current).View.Get_Selection.Unselect_All;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Open_Schedule")
         )  );
   end On_Open_Schedule;

   procedure On_Open
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
      Dialog : File_Dialog;
      Filter : Gtk_File_Filter;
   begin
      Dialog :=
         Gtk_New
         (  "Select schedule file",
            Action_Open,
            On_Open_Schedule'Access,
            Page,
            "Open the selected file"
         );
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name (Sch_Filter_Name);
      Filter.Add_Pattern ("*" & Sch_Extension);
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
            &  Where ("On_Open")
         )  );
   end On_Open;

   procedure On_Parameter_Changed
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
   begin
      Page.Changed := True;
      Page.Edited  := True;
      Page.Set_Action_Buttons;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Parameter_Changed")
         )  );
   end On_Parameter_Changed;

   procedure On_Paste
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
      Depth : Integer := 1;
   begin
      declare
         Deleted : constant Points_List :=
                   Selected
                   (  Page.View (Page.Current).List,
                      Page.View (Page.Current).View.Get_Selection
                   );
      begin
         if Deleted'Length + Clipboard.Points'Length = 0 then
            return;
         end if;
         if Deleted'Length + Clipboard.Points'Length > 1 then
            Page.Data.Undo.Push ((End_Item, Page.Current));
         end if;
         for Index in reverse Deleted'Range loop
            Page.Execute
            (  (  Delete_Item,
                  Page.Current,
                  Pack (Deleted (Index))
               ),
               Depth,
               Page.Data.Undo
            );
         end loop;
         for Index in Clipboard.Points'Range loop
            Page.Execute
            (  (  Insert_Item,
                  Page.Current,
                  Pack (Clipboard.Points (Index))
               ),
               Depth,
               Page.Data.Undo
            );
         end loop;
         if Deleted'Length + Clipboard.Points'Length > 1 then
            Page.Data.Undo.Push ((Begin_Item, Page.Current));
         end if;
         Page.Data.Redo.Erase;
         Page.Set_Action_Buttons;
         Page.View (Page.Current).View.Get_Selection.Unselect_All;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Paste")
         )  );
   end On_Paste;

   procedure On_Redo
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
   begin
      Page.Execute (Page.Data.Redo, Page.Data.Undo);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Redo")
         )  );
   end On_Redo;

   procedure On_S_Response
             (  Page     : in out Schedule_Record;
                Cube     : access Cube_Client'Class;
                Error    : Boolean;
                Duty     : Ratio;
                Slots    : Natural;
                Expected : in out Natural
             )  is
   begin
      begin
         Page.Rooms.Get_Logger.Update_Status
         (  Cube.Get_RF_Address,
            Error,
            Duty,
            Slots
         );
      exception
         when Status_Error =>
            null;
      end;
      if Error then
         if not Page.Reported then
            Page.Reported := True;
            if Page.Thermostats.Is_Empty then
               Say
               (  "Failure " & Image (Page.Status),
                  "Store error"
               );
            else
               Say
               (  (  "Failure " & Image (Page.Status) & " into "
                  &  Image (Page.Thermostats.Get (1))
                  ),
                  "Store error"
               );
            end if;
         end if;
      end if;
      if Expected > 0 then
         Expected := Expected - 1;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_S_Response")
         )  );
   end On_S_Response;

   function On_Selection
            (  Object : access GObject_Record'Class;
               Page   : Schedule
            )  return Boolean is
   begin
      for Day in Page.View'Range loop
         if Page.View (Day).Box.all'Address = Object.all'Address then
            On_Selection
            (  Page.View (Day).View.Get_Selection,
               Page.View (Day)'Unchecked_Access
            );
            exit;
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
            &  Where ("On_Selection (title)")
         )  );
         return False;
   end On_Selection;

   procedure On_Selection
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                Page      : Day_View_Ptr
             )  is
   begin
      for Day in Page.Parent.View'Range loop
         if Day = Page.Day then
            Set_Background_Color
            (  Page.Parent.View (Day).Box,
               Page.Parent.Selected
            );
         else
            Set_Background_Color
            (  Page.Parent.View (Day).Box,
               Page.Parent.Unselected
            );
            Page.Parent.View (Day).View.Get_Selection.Unselect_All;
         end if;
      end loop;
      if Page.View.Get_Selection.Count_Selected_Rows > 0 then
         Page.Parent.Delete.Set_Sensitive (True);
         Page.Parent.Copy.Set_Sensitive   (True);
         Page.Parent.Cut.Set_Sensitive    (True);
      else
         Page.Parent.Delete.Set_Sensitive (False);
         Page.Parent.Copy.Set_Sensitive   (False);
         Page.Parent.Cut.Set_Sensitive    (False);
      end if;
      Page.Parent.Erase.Set_Sensitive      (True);
      Page.Parent.Select_All.Set_Sensitive (True);
      Page.Parent.Current := Page.Day;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Selection (inside)")
         )  );
   end On_Selection;

   procedure On_Select_All
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
      Store     : Gtk_List_Store;
      Selection : Gtk_Tree_Selection;
      Row       : Gtk_Tree_Iter;
   begin
      Selection := Page.View (Page.Current).View.Get_Selection;
      Store     := Page.View (Page.Current).List;
      Row       := Store.Get_Iter_First;
      Selection.Unselect_All;
      while Row /= Null_Iter and then not Get (Store, Row) loop
         Selection.Select_Iter (Row);
         Store.Next (Row);
      end loop;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Select_All")
         )  );
   end On_Select_All;

   procedure On_Save
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
   begin
      Page.Save_To_File (To_String (Page.File));
      Page.Data.Redo.Erase;
      Page.Data.Undo.Erase;
      Page.Set_Action_Buttons;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Save")
         )  );
   end On_Save;

   procedure On_Save_As_Handler
             (  File   : String;
                Widget : in out Gtk_Widget_Record'Class
             )  is
   begin
      declare
         Page : Schedule_Record'Class renames
                Schedule_Record'Class (Widget);
      begin
         Page.Save_To_File (File);
      exception
         when Error : others =>
            Log
            (  MAX_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("On_Save_As")
            )  );
      end;
   end On_Save_As_Handler;

   procedure On_Save_As
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
      Dialog : File_Dialog;
      Filter : Gtk_File_Filter;
   begin
      Dialog :=
         Gtk_New
         (  "Select or create a schedule file",
            Action_Save,
            On_Save_As_Handler'Access,
            Page,
            "Store into the selected file"
         );
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name (Sch_Filter_Name);
      Filter.Add_Pattern ("*" & Sch_Extension);
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
            &  Where ("On_Save_As")
         )  );
   end On_Save_As;

   procedure On_Store
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
      function Get_Minutes
               (  Text     : String;
                  Context  : String;
                  Truncate : Boolean
               )  return Day_Duration is
      begin
         return To_Duration (Text, Truncate);
      exception
         when Error : others =>
            Say (Exception_Message (Error), "Parameter field error");
            raise End_Error;
      end Get_Minutes;

      function Get_Ratio
               (  Text    : String;
                  Context : String
               )  return Ratio is
         Result : Float;
      begin
         Result := Value (Text);
         if Result <= 0.0 then
            return 0.0;
         elsif Result >= 100.0 then
            return 1.0;
         else
            return Ratio (Result / 100.0);
         end if;
      exception
         when End_Error =>
            Say
            (  "No " & Context & " valve position specified",
               "Parameter field error"
            );
            raise End_Error;
         when others =>
            Say
            (  "Wrong " & Context & " valve position: " & Text,
               "Parameter field error"
            );
            raise End_Error;
      end Get_Ratio;

      function Get_Temperature
               (  Text    : String;
                  Context : String;
                  Lower   : Centigrade
               )  return Centigrade is
         Temperature : Float;
      begin
         Temperature := Value (Text);
         if Temperature <= Float (Lower) then
            return Lower;
         elsif Temperature >= 63.5 + Float (Lower) then
            if Lower >= 0.0 then
               return 63.5;
            else
               return 63.5 + Lower;
            end if;
         else
            return Centigrade (Temperature);
         end if;
      exception
         when End_Error =>
            Say
            (  "No " & Context & " temperature specified",
               "Parameter field error"
            );
            raise End_Error;
         when others =>
            Say
            (  "Wrong " & Context & " temperature: " & Text,
               "Parameter field error"
            );
            raise End_Error;
      end Get_Temperature;

      Room    : Room_ID    := No_Room;
      Address : RF_Address := 0;
   begin
      for Day in Week_Day'Range loop
         declare
            Points : constant Points_List := Get (Page.View (Day).List);
         begin
            Page.Parameters.Schedule (Day) := (Points'Length, Points);
         end;
      end loop;
      if Page.All_Room /= null and then Page.All_Room.Get_Active then
         Room := Page.Rooms.Get (Page.Key.Device);
         Page.Thermostats :=
            Get_Radiator_Thermostats (Page.Key.Box, Room);
      else
         Address := Page.Key.Device;
         Page.Thermostats.Erase;
         Page.Thermostats.Add (Address);
      end if;
      if Page.Changed then
         Page.Parameters.Comfort :=
            Get_Temperature (Page.Comfort.Get_Text, "comfort", 0.0);
         Page.Parameters.Eco :=
            Get_Temperature (Page.Eco.Get_Text, "eco", 0.0);
         Page.Parameters.Max :=
            Get_Temperature (Page.Max.Get_Text, "maximum", 0.0);
         Page.Parameters.Min :=
            Get_Temperature (Page.Min.Get_Text, "minimum", 0.0);
         if Page.Parameters.Kind_Of in Radiator_Thermostat
                                    .. Wall_Thermostat then
            Page.Parameters.Offset :=
               Get_Temperature (Page.Offset.Get_Text, "offset", -3.5);
            Page.Parameters.Window_Open :=
               Get_Temperature
               (  Page.Window_Open.Get_Text,
                  "airing",
                  0.0
               );
         end if;
         if Page.Parameters.Kind_Of in Radiator_Thermostat
                                    .. Radiator_Thermostat_Plus then
            Page.Parameters.Window_Time :=
               Get_Minutes
               (  Page.Window_Time.Get_Text,
                  "airing time",
                  False
               );
         end if;
      end if;
      if Page.Valve then
         Page.Parameters.Boost_Time :=
            Get_Minutes (Page.Boost_Time.Get_Text, "boost time", True);
         Page.Parameters.Boost_Valve :=
            Get_Ratio (Page.Boost_Valve.Get_Text, "boost valve");
         Page.Parameters.Max_Valve :=
            Get_Ratio
            (  Page.Max_Valve.Get_Text,
               "maximum valve position"
            );
         Page.Parameters.Valve_Offset :=
            Get_Ratio
            (  Page.Valve_Offset.Get_Text,
               "valve position offset"
            );
         Page.Parameters.Decalcification.Day :=
            Week_Day'Val (Page.Decalc_Day.Get_Active);
         Page.Parameters.Decalcification.Time :=
            Duration (Page.Decalc_Time.Get_Active) * 3600.0;
      end if;
      if (  not Page.Thermostats.Is_Empty
         and then
            (  not Page.Data.Undo.Is_Empty
            or else
               Page.Changed
            or else
               Page.Valve
         )  )
      then
         Page.Reported := False;
         Page.Status   := Store_Start;
         Page.Finished;
      end if;
   exception
      when Use_Error =>
         null;
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Store")
         )  );
   end On_Store;

   procedure On_Thermostat
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                Button    : Gtk_Button
             )  is
   begin
      Button.Set_Sensitive (Selection.Count_Selected_Rows = 1);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Thermostat")
         )  );
   end On_Thermostat;

   procedure On_Undo
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
   begin
      Page.Execute (Page.Data.Undo, Page.Data.Redo);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Undo")
         )  );
   end On_Undo;

   procedure On_Valve_Changed
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             )  is
   begin
      Page.Valve  := True;
      Page.Edited := True;
      Page.Set_Action_Buttons;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Valve_Changed")
         )  );
   end On_Valve_Changed;

   procedure Rename
             (  Pages  : not null access Gtk_Notebook_Record'Class;
                Cube   : RF_Address;
                Device : RF_Address;
                Name   : String
             )  is
      Key  : constant Key_Type := (Cube, Device);
      Page : Schedule;
   begin
      if List.Is_In (Key) then
         Page := List.Get (Key);
         Page.Name.Set_Text (Image (Device) & " " & Name);
      end if;
   end Rename;

   procedure Replace
             (  Widget   : not null access Schedule_Record;
                Schedule : Week_Schedule
             )  is
      Depth : Integer := 1;
   begin
      Widget.Data.Undo.Push ((End_Item, Mo));
      for Day in Week_Day'Range loop
         declare
            Points : constant Points_List := Schedule (Day).Points;
         begin
            Widget.Execute
            (  (Erase_Item, Day),
               Depth,
               Widget.Data.Undo
            );
            for Index in Points'Range loop
               Widget.Execute
               (  (Insert_Item, Day, Pack (Points (Index))),
                  Depth,
                  Widget.Data.Undo
               );
            end loop;
         end;
      end loop;
      Widget.Data.Undo.Push ((Begin_Item, Mo));
      Widget.Data.Redo.Erase;
      Widget.Set_Action_Buttons;
   end Replace;

   procedure Replace
             (  Store : not null access Gtk_List_Store_Record'Class;
                Row   : Gtk_Tree_Iter;
                Point : in out Set_Point
             )  is
      Old : Set_Point;
   begin
      Old.Last  := Get (Store, Row);
      Old.Point := Get (Store, Row);
      Set (Store, Row, Point.Last);
      Set (Store, Row, Point.Point);
      Point := Old;
   end Replace;

   procedure Save_To_File
             (  Widget : not null access Schedule_Record;
                Name   : UTF8_String
             )  is
      File : File_Type;
   begin
      begin
         Create (File, Out_File, Name);
      exception
         when Error : others =>
            Say
            (  (  "File "
               &  Name
               &  ": "
               &  Exception_Message (Error)
               ),
               "Output file open error"
            );
            return;
      end;
      declare
         Data : constant Week_Schedule := Widget.Get;
      begin
         Write (Stream (File), Data);
      exception
         when Error : others =>
            Say
            (  (  "File "
               &  Name
               &  ": "
               &  Exception_Message (Error)
               ),
               "File write error"
            );
            begin
               Close (File);
            exception
               when others =>
                  null;
            end;
            return;
      end;
      begin
         Close (File);
      exception
         when Error : others =>
            Say
            (  (  "File "
               &  Name
               &  ": "
               &  Exception_Message (Error)
               ),
               "File close error"
            );
            return;
      end;
      Widget.Data.Undo.Erase;
      Widget.Data.Redo.Erase;
      Widget.Set_Action_Buttons;
   end Save_To_File;

   function Selected
            (  Store : not null access Gtk_List_Store_Record'Class;
               From  : not null access Gtk_Tree_Selection_Record'Class
            )  return Points_List is
      Points : Points_List (Point_Number'Range);
      Length : Point_Count;
      Row    : Gtk_Tree_Iter;
   begin
      Length := 0;
      Row    := Store.Get_Iter_First;
      while Row /= Null_Iter and then not Get (Store, Row) loop
         if From.Iter_Is_Selected (Row) then
            Length := Length + 1;
            Points (Length).Last  := Get (Store, Row);
            Points (Length).Point := Get (Store, Row);
         end if;
         Store.Next (Row);
      end loop;
      return Points (1..Length);
   end Selected;

   procedure Set
             (  Widget   : not null access Schedule_Record;
                Day      : Week_Day;
                Schedule : Day_Schedule
             )  is
      Points : Points_List renames Schedule.Points;
      Index  : Point_Count := 0;
      Last   : Duration    := -1.0;
      List   : constant Gtk_List_Store := Widget.View (Day).List;
      Row    : Gtk_Tree_Iter := List.Get_Iter_First;
   begin
      for Item in Points'Range loop
         if Points (Item).Last > Last then
            Last  := Points (Item).Last;
            Index := Index + 1;
            Set (List, Row, False);
            Gtk.Missed.Set
            (  List,
               Row,
               Time_Column,
               Minutes (Points (Item).Last)
            );
            Set (List, Row, Points (Item).Point);
            List.Next (Row);
         end if;
      end loop;
      while Index < Point_Count'Last loop
         Index := Index + 1;
         Gtk.Missed.Set (List, Row, Time_Column, "");
         Set (List, Row, True);
         List.Next (Row);
      end loop;
      Widget.Set_Action_Buttons;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set (schedule)")
         )  );
   end Set;

   procedure Set
             (  Store    : not null access Gtk_List_Store_Record'Class;
                Row      : Gtk_Tree_Iter;
                Until_Up : Day_Duration
             )  is
   begin
      Gtk.Missed.Set (Store, Row, Time_Column, Minutes (Until_Up));
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set (until)")
         )  );
   end Set;

   procedure Set
             (  Store : not null access Gtk_List_Store_Record'Class;
                Row   : Gtk_Tree_Iter;
                Temperature : Centigrade
             )  is
      Value : GValue;
   begin
      Init (Value, GType_Double);
      Set_Double (Value, GDouble (Temperature));
      Store.Set_Value (Row, Temperature_Column, Value);
      Unset (Value);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set (temperature)")
         )  );
   end Set;

   procedure Set
             (  Store : not null access Gtk_List_Store_Record'Class;
                Row   : Gtk_Tree_Iter;
                Flag  : Boolean
             )  is
      Value : GValue;
   begin
      Init (Value, GType_Boolean);
      Set_Boolean (Value, Flag);
      Store.Set_Value (Row, Unset_Column, Value);
      Unset (Value);
      if Flag then
         Gtk.Missed.Set (Store, Row, Time_Column, "");
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set (Boolean)")
         )  );
   end Set;

   procedure Set_Action_Buttons
             (  Widget : not null access Schedule_Record
             )  is
   begin
      if Widget.Data.Redo.Is_Empty then
         Widget.Redo.Set_Sensitive (False);
      else
         Widget.Redo.Set_Sensitive (True);
      end if;
      if Widget.Data.Undo.Is_Empty then
         Widget.Undo.Set_Sensitive  (False);
         if Widget.Edited then
            Widget.Edited := False;
            Widget.Modified.Hide;
            Widget.Intact.Show;
         end if;
      else
         Widget.Undo.Set_Sensitive  (True);
         if not Widget.Edited then
            Widget.Edited := True;
            Widget.Modified.Show;
            Widget.Intact.Hide;
         end if;
      end if;
      if Clipboard.Length > 0 then
         Widget.Paste.Set_Sensitive (True);
      else
         Widget.Paste.Set_Sensitive (False);
      end if;
      Widget.Save.Set_Sensitive (Length (Widget.File) > 0);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Action_Buttons")
         )  );
   end Set_Action_Buttons;

   function To_Duration
            (  Text     : String;
               Truncate : Boolean
            )  return Day_Duration is
      Pointer : Integer := Text'First;
      Minutes : Integer := 0;
      Hour    : Integer;
   begin
      Get (Text, Pointer);
      begin
         Get (Text, Pointer, Hour, 10, 0, 24);
      exception
         when End_Error =>
            raise Data_Error with
                  "Missing hour number at " & Text (Pointer..Text'Last);
         when Constraint_Error =>
            raise Data_Error with
                  "Hour number " & Image (Hour) & " is not in 0..24";
         when Data_Error =>
            raise Data_Error with
                  "Invalid hour number at " & Text (Pointer..Text'Last);
      end;
      Get (Text, Pointer);
      if Pointer <= Text'Last then
         if Text (Pointer) /= ':' then
            raise Data_Error with
                  "Colon ':' is expected at " &
                  Text (Pointer..Text'Last);
         end if;
         Pointer := Pointer + 1;
         Get (Text, Pointer);
         begin
            Get (Text, Pointer, Minutes, 10, 0, 59);
         exception
            when End_Error =>
               raise Data_Error with
                     "Missing minutes number at " &
                     Text (Pointer..Text'Last);
            when Constraint_Error =>
               raise Data_Error with
                     "Hour minutes " &
                     Image (Hour) &
                      " is not in 0..59";
            when Data_Error =>
               raise Data_Error with
                     "Invalid minutes number at " &
                     Text (Pointer..Text'Last);
         end;
         Get (Text, Pointer);
         if Pointer <= Text'Last then
            raise Data_Error with
                  "Unrecognized text after the minutes number";
         end if;
      end if;
      if Hour = 24 and Minutes > 0 then
         raise Data_Error with
               "Invalid time not in the range 00:00..24:00";
      end if;
      if Truncate then
         Minutes := (Minutes / 5) * 5;
      else
         Minutes := ((Minutes + 4) / 5) * 5;
      end if;
      return
      (  Duration (Hour) * 3600.0
      +  Duration (Minutes) * 60.0 -- Five minutes step
      );
   exception
      when Data_Error =>
         raise;
      when Error : others =>
         raise Data_Error with Exception_Message (Error);
   end To_Duration;

   function "<" (Left, Right : Key_Type) return Boolean is
   begin
      return
      (  Left.Box < Right.Box
      or else
         (  Left.Box = Right.Box
         and then
            Left.Device < Right.Device
      )  );
   end "<";

end MAX_Schedule;
