--                                                                    --
--  package MAX_Shortcuts_Page      Copyright (c)  Dmitry A. Kazakov  --
--     Shortcuts page                              Luebeck            --
--  Implementation                                 Autumn, 2018       --
--                                                                    --
--                                Last revision :  14:41 02 Dec 2020  --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.IO_Exceptions;         use Ada.IO_Exceptions;
with GLib.Messages;             use GLib.Messages;
with GLib.Properties;           use GLib.Properties;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Recent_Manager_Keys;   use Gtk.Recent_Manager_Keys;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Max_Icon_Factory;          use Max_Icon_Factory;
with Max_IO.Set_Mode;           use Max_IO.Set_Mode;
with MAX_Trace;                 use MAX_Trace;
with Pango.Enums;               use Pango.Enums;
with Strings_Edit;              use Strings_Edit;
with Strings_Edit.Integers;     use Strings_Edit.Integers;

with GLib.Types;
with Strings_Edit.Integers.Superscript;
with Strings_Edit.Floats;

package body MAX_Shortcuts_Page is

   Edit_Width   : constant := 5;
   Default_Time : constant GDouble := 0.5;
   First_Time   : constant GDouble := 0.5;
   Last_Time    : constant GDouble := 24.0 * 365.0 * 10.0;

   Default_T    : constant GDouble := 19.0;
   First_T      : constant GDouble := 0.0;
   Last_T       : constant GDouble := 63.5;

   function Where (Name : String) return String is
   begin
      return " in MAX_Shortcuts_Page." & Name;
   end Where;

   function "+" (Value : GDouble) return Duration is
   begin
      return Duration (Value * 3600.0);
   end "+";

   function "-" (Value : Duration) return GDouble is
   begin
      return GDouble (Value) / 3600.0;
   end "-";

   procedure Add_Button
             (  Data   : not null access MAX_Shortcuts_Record'Class;
                No     : Positive;
                Button : not null access Gtk_Button_Record'Class
             )  is
   begin
      Data.Button (No) := Button.all'Unchecked_Access;
      Shortcuts_Handlers.Connect
      (  Button,
         "clicked",
         On_Play'Access,
         Data.all'Unchecked_Access
      );
      if Data.Shortcuts (No).Is_Empty then
         Button.Hide;
      else
         Button.Show;
      end if;
      Button.Set_Label (Data.Entries (No).Get_Text);
   end Add_Button;

   function Button_To_No
            (  Data   : MAX_Shortcuts_Record;
               Button : not null access GObject_Record'Class
            )  return Natural is
   begin
      for No in Data.Button'Range loop
         if Data.Button (No) = Button then
            return No;
         end if;
      end loop;
      return 0;
   end Button_To_No;

   procedure Changed
             (  Model : not null access Shortcut_Store_Record;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter
             )  is
   begin
      null; -- Suppress event to prevent canceling editing
   end Changed;

   procedure Combo_Cell_Data
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Combo  : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Row    : Gtk_Tree_Iter;
                Data   : MAX_Shortcuts
             )  is
      No     : constant Natural := Data.Combo_To_No (Combo);
      Iter   : Gtk_Tree_Iter;
      Offset : Natural;

      procedure Restore is
         Prefix : constant String :=
                     Get_Prefix (No, Data.Rooms.Get (Iter));
         Mode   : constant Shortcut_Mode :=
                     From_String (Restore (Prefix & "mode", ""));
         Value  : GValue;
         Key    : Shortcut_Target;
      begin
         Key := Data.Get_Key (Row);
         Init (Value, GType_Double);
         Data.Store.Set_Extension (Row, Offset + 1, To_String (Mode));
         case Mode is
            when Unchanged_Shortcut =>
               Data.Store.Set_Extension (Row, Offset + 2, True);
               Data.Store.Set_Extension (Row, Offset + 4, True);
            when Manual_Shortcut =>
               declare
                  Temperature : constant Centigrade := Restore (Prefix);
               begin
                  Set_Double (Value, GDouble (Temperature));
                  Data.Store.Set_Extension (Row, Offset + 2, False);
                  Data.Store.Set_Extension (Row, Offset + 3, Value);
                  Data.Shortcuts (No).Replace
                  (  Key,
                     (Manual_Shortcut, Temperature)
                  );
               end;
            when Vacation_Shortcut =>
               declare
                  Temperature : constant Centigrade := Restore (Prefix);
                  Timeout     : constant Duration   := Restore (Prefix);
               begin
                  Set_Double (Value, GDouble (Temperature));
                  Data.Store.Set_Extension (Row, Offset + 2, False);
                  Data.Store.Set_Extension (Row, Offset + 3, Value);
                  Set_Double (Value, -Timeout);
                  Data.Store.Set_Extension (Row, Offset + 4, False);
                  Data.Store.Set_Extension (Row, Offset + 5, Value);
                  Data.Shortcuts (No).Replace
                  (  Key,
                     (Vacation_Shortcut, Temperature, Timeout)
                  );
               end;
            when Automatic_Shortcut =>
               Data.Store.Set_Extension (Row, Offset + 2, True);
               Data.Store.Set_Extension (Row, Offset + 4, True);
               Data.Shortcuts (No).Replace
               (  Key,
                  (Mode => Automatic_Shortcut)
               );
            when Automatic_With_Temperature_Shortcut =>
               declare
                  Temperature : constant Centigrade := Restore (Prefix);
               begin
                  Set_Double (Value, GDouble (Temperature));
                  Data.Store.Set_Extension (Row, Offset + 2, False);
                  Data.Store.Set_Extension (Row, Offset + 3, Value);
                  Data.Shortcuts (No).Replace
                  (  Key,
                     (Automatic_With_Temperature_Shortcut, Temperature)
                  );
               end;
            when Boost_Shortcut =>
               Data.Store.Set_Extension (Row, Offset + 2, True);
               Data.Store.Set_Extension (Row, Offset + 4, True);
               Data.Shortcuts (No).Replace
               (  Key,
                  (Mode => Boost_Shortcut)
               );
            when Eco_Shortcut =>
               Data.Store.Set_Extension (Row, Offset + 2, True);
               Data.Store.Set_Extension (Row, Offset + 4, True);
               Data.Shortcuts (No).Replace
               (  Key,
                  (Mode => Eco_Shortcut)
               );
            when Comfort_Shortcut =>
               Data.Store.Set_Extension (Row, Offset + 2, True);
               Data.Shortcuts (No).Replace
               (  Key,
                  (Mode => Comfort_Shortcut)
               );
               Data.Store.Set_Extension (Row, Offset + 4, True);
            when Airing_Shortcut =>
               Data.Store.Set_Extension (Row, Offset + 2, True);
               Data.Shortcuts (No).Replace
               (  Key,
                  (Mode => Airing_Shortcut)
               );
               Data.Store.Set_Extension (Row, Offset + 4, True);
         end case;
         Unset (Value);
      end Restore;
   begin
      if No = 0 then
         return;
      end if;
      Offset := (No - 1) * 5;
      Iter := Data.Store.From_Extension (Row);
      case Device_Type'(Data.Rooms.Get (Iter)) is
         when Radiator_Thermostat      |
              Radiator_Thermostat_Plus |
              Wall_Thermostat =>
            declare
               Mode : constant String :=
                         Gtk.Missed.Get
                         (  Gtk_Tree_Model'(To_Interface (Data.Store)),
                            Row,
                            Time_Stamp_Column + 1 + GInt (Offset)
                         );
            begin
               if Mode = "" then
                  Restore;
                  if Data.Shortcuts (No).Is_Empty then
                     Data.Button (No).Hide;
                  else
                     Data.Button (No).Show;
                  end if;
               end if;
            end;
            case From_String
                 (  Gtk.Missed.Get
                    (  Gtk_Tree_Model'(To_Interface (Data.Store)),
                       Row,
                       Time_Stamp_Column + 1 + GInt (Offset)
                 )  )  is
              when Automatic_With_Temperature_Shortcut |
                   Manual_Shortcut =>
                 Data.Temperature (No).Set_Mode
                 (  Cell_Renderer_Mode_Editable
                 );
                 Data.Timeout (No).Set_Mode
                 (  Cell_Renderer_Mode_Inert
                 );
              when Vacation_Shortcut =>
                 Data.Temperature (No).Set_Mode
                 (  Cell_Renderer_Mode_Editable
                 );
                 Data.Timeout (No).Set_Mode
                 (  Cell_Renderer_Mode_Editable
                 );
              when others =>
                 Data.Temperature (No).Set_Mode
                 (  Cell_Renderer_Mode_Inert
                 );
                 Data.Timeout (No).Set_Mode (Cell_Renderer_Mode_Inert);
            end case;
         when others =>
            Data.Temperature (No).Set_Mode (Cell_Renderer_Mode_Inert);
            Data.Timeout     (No).Set_Mode (Cell_Renderer_Mode_Inert);
      end case;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Combo_Cell_Data")
         )  );
   end Combo_Cell_Data;

   function Combo_To_No
            (  Data : MAX_Shortcuts_Record;
               Cell : not null access Gtk_Cell_Renderer_Record'Class
            )  return Natural is
   begin
      for No in Data.Combo'Range loop
         if Data.Combo (No) = Cell then
            return No;
         end if;
      end loop;
      return 0;
   end Combo_To_No;

   procedure Commit_Temperature
             (  Cell : access Gtk_Cell_Renderer_Fixed_Record'Class;
                Data : MAX_Shortcuts
             )  is
      No          : constant Natural := Data.Temperature_To_No (Cell);
      Row         : Gtk_Tree_Iter;
      Temperature : GDouble;
      Value       : GValue;
      Key         : Shortcut_Target;
      Action      : Shortcut_Action;
   begin
      Row := Get_Iter_From_String
             (  Gtk_Tree_Model'(To_Interface (Data.Store)),
                Get_Path (Cell)
             );
      if Row = Null_Iter or else No = 0 then
         return;
      end if;
      Temperature :=
         GDouble'Max
         (  GDouble'Min
            (  Get_Property (Cell, Build ("value")),
               Last_T
            ),
            First_T
         );
      Key := Data.Get_Key (Row);
      case Data.Get_Mode (Row, No) is
         when Manual_Shortcut =>
            Action := (Manual_Shortcut, Centigrade (Temperature));
            Data.Shortcuts (No).Replace (Key, Action);
         when Automatic_With_Temperature_Shortcut =>
            Action := (  Automatic_With_Temperature_Shortcut,
                         Centigrade (Temperature)
                      );
            Data.Shortcuts (No).Replace (Key, Action);
         when Vacation_Shortcut =>
            declare
               Offset : constant Integer :=
                                 Data.Shortcuts (No).Find (Key);
            begin
               if Offset > 0 then
                  Action := Data.Shortcuts (No).Get (Offset);
                  case Action.Mode is
                     when Automatic_With_Temperature_Shortcut |
                          Vacation_Shortcut                   |
                          Manual_Shortcut                     =>
                        Action.Vacation_Temperature :=
                           Centigrade (Temperature);
                     when others =>
                        Action := (  Vacation_Shortcut,
                                     Centigrade (Temperature),
                                     +Default_Time
                                  );
                  end case;
                  Data.Shortcuts (No).Replace (Offset, Action);
               else
                  Action := (  Vacation_Shortcut,
                               Centigrade (Temperature),
                               +Default_Time
                            );
                  Data.Shortcuts (No).Replace (Key, Action);
               end if;
            end;
         when others =>
            return;
      end case;
      Init (Value, GType_Double);
      Set_Double (Value, Temperature);
      Data.Store.Set_Extension (Row, (No - 1) * 5 + 3, Value);
      Unset (Value);
      Data.Store_Shortcut (No, Key, Action);
   exception
      when End_Error =>
         raise;
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Commit_Temperature")
         )  );
   end Commit_Temperature;

   procedure Commit_Timeout
             (  Cell : access Gtk_Cell_Renderer_Fixed_Record'Class;
                Data : MAX_Shortcuts
             )  is
      No     : constant Natural := Data.Timeout_To_No (Cell);
      Row    : Gtk_Tree_Iter;
      Time   : GDouble;
      Value  : GValue;
      Key    : Shortcut_Target;
      Action : Shortcut_Action;
   begin
      Row := Get_Iter_From_String
             (  Gtk_Tree_Model'(To_Interface (Data.Store)),
                Get_Path (Cell)
             );
      if Row = Null_Iter or else No = 0 then
         return;
      end if;
      Time := Get_Property (Cell, Build ("value"));
      Time := GDouble'Min (GDouble'Max (Time, First_Time), Last_Time);
      Key := Data.Get_Key (Row);
      case Data.Get_Mode (Row, No) is
         when Vacation_Shortcut =>
            declare
               Offset : constant Integer :=
                                 Data.Shortcuts (No).Find (Key);
            begin
               if Offset > 0 then
                  Action := Data.Shortcuts (No).Get (Offset);
                  case Action.Mode is
                     when Vacation_Shortcut =>
                        Action.Vacation_Time := +Time;
                     when others =>
                        Action := (  Vacation_Shortcut,
                                     Centigrade (Default_T),
                                     +Time
                                  );
                  end case;
                  Data.Shortcuts (No).Replace (Offset, Action);
               else
                  Action :=
                     (Vacation_Shortcut, Centigrade (Default_T), +Time);
                  Data.Shortcuts (No).Replace (Key, Action);
               end if;
            end;
         when others =>
            return;
      end case;
      Init (Value, GType_Double);
      Set_Double (Value, Time);
      Data.Store.Set_Extension (Row, (No - 1) * 5 + 5, Value);
      Unset (Value);
      Data.Store_Shortcut (No, Key, Action);
   exception
      when End_Error =>
         raise;
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Commit_Timeout")
         )  );
   end Commit_Timeout;

   procedure Device_Added
             (  Data   : in out MAX_Shortcuts_Record;
                Cube   : RF_Address;
                Device : Device_Parameters
             )  is
      Key : constant Shortcut_Target := (Cube, Device.Address);
   begin
      case Device.Kind_Of is
         when Radiator_Thermostat | Radiator_Thermostat_Plus |
              Wall_Thermostat     =>
            for No in 1..Data.Shorcut_Count loop
               declare
                  Prefix : constant String :=
                           Get_Prefix (No, Device.Address);
                  Mode   : constant Shortcut_Mode :=
                           From_String (Restore (Prefix & "mode", ""));
               begin
                  case Mode is
                     when Unchanged_Shortcut =>
                        null;
                     when Manual_Shortcut =>
                        Data.Shortcuts (No).Replace
                        (  Key,
                           (Manual_Shortcut, Restore (Prefix))
                        );
                     when Vacation_Shortcut =>
                        Data.Shortcuts (No).Replace
                        (  Key,
                           (  Vacation_Shortcut,
                              Restore (Prefix),
                              Restore (Prefix)
                        )  );
                     when Automatic_Shortcut =>
                        Data.Shortcuts (No).Replace
                        (  Key,
                           (Mode => Automatic_Shortcut)
                        );
                     when Automatic_With_Temperature_Shortcut =>
                        Data.Shortcuts (No).Replace
                        (  Key,
                           (  Automatic_With_Temperature_Shortcut,
                              Restore (Prefix)
                        )  );
                     when Boost_Shortcut =>
                        Data.Shortcuts (No).Replace
                        (  Key,
                           (Mode => Boost_Shortcut)
                        );
                     when Eco_Shortcut =>
                        Data.Shortcuts (No).Replace
                        (  Key,
                           (Mode => Eco_Shortcut)
                        );
                     when Comfort_Shortcut =>
                        Data.Shortcuts (No).Replace
                        (  Key,
                           (Mode => Comfort_Shortcut)
                        );
                     when Airing_Shortcut =>
                        Data.Shortcuts (No).Replace
                        (  Key,
                           (Mode => Airing_Shortcut)
                        );
                  end case;
               end;
               if Data.Shortcuts (No).Is_Empty then
                  Data.Button (No).Hide;
               else
                  Data.Button (No).Show;
               end if;
            end loop;
         when others =>
            null;
      end case;
   end Device_Added;

   function Entry_To_No
            (  Data  : MAX_Shortcuts_Record;
               Edit : not null access GObject_Record'Class
            )  return Natural is
   begin
      for No in Data.Combo'Range loop
         if Data.Entries (No) = Edit then
            return No;
         end if;
      end loop;
      return 0;
   end Entry_To_No;

   procedure Execute
             (  Data : not null access MAX_Shortcuts_Record;
                No   : Positive
             )  is
   begin
      if No not in Data.Shortcuts'Range then
         return;
      end if;
      declare
         No_IO  : IO_Blocker;
         Now    : Time := Clock;
         This   : Shortcut renames Data.Shortcuts (No);
         Target : Shortcut_Target;
         Action : Shortcut_Action;
      begin
         for Index in 1..This.Get_Size loop
            Target := This.Get_Key (Index);
            Action := This.Get (Index);
            case Action.Mode is
               when Unchanged_Shortcut =>
                  null;
               when Manual_Shortcut =>
                  Set_Thermostat_Mode
                  (  Box         => Target.Cube,
                     Device      => Target.Thermostat,
                     Mode        => Manual,
                     Temperature => Action.Manual_Temperature,
                     Handler     => Data.Rooms.all'Unchecked_Access
                  );
               when Vacation_Shortcut =>
                  Set_Thermostat_Mode
                  (  Box         => Target.Cube,
                     Device      => Target.Thermostat,
                     Mode        => Vacation,
                     Temperature => Action.Vacation_Temperature,
                     Up_Until    => Now + Action.Vacation_Time,
                     Handler     => Data.Rooms.all'Unchecked_Access
                  );
               when Automatic_Shortcut =>
                  Set_Thermostat_Mode
                  (  Box     => Target.Cube,
                     Device  => Target.Thermostat,
                     Mode    => Automatic,
                     Handler => Data.Rooms.all'Unchecked_Access
                  );
               when Automatic_With_Temperature_Shortcut =>
                  Set_Thermostat_Mode
                  (  Box         => Target.Cube,
                     Device      => Target.Thermostat,
                     Mode        => Automatic,
                     Temperature => Action.Automatic_Temperature,
                     Handler     => Data.Rooms.all'Unchecked_Access
                  );
               when Boost_Shortcut =>
                  Set_Thermostat_Mode
                  (  Box     => Target.Cube,
                     Device  => Target.Thermostat,
                     Mode    => Boost,
                     Handler => Data.Rooms.all'Unchecked_Access
                  );
               when Eco_Shortcut =>
                  Set_Thermostat_Mode
                  (  Box         => Target.Cube,
                     Device      => Target.Thermostat,
                     Mode        => Manual,
                     Disposition => Eco,
                     Handler     => Data.Rooms.all'Unchecked_Access
                  );
               when Comfort_Shortcut =>
                  Set_Thermostat_Mode
                  (  Box         => Target.Cube,
                     Device      => Target.Thermostat,
                     Mode        => Manual,
                     Disposition => Comfort,
                     Handler     => Data.Rooms.all'Unchecked_Access
                  );
               when Airing_Shortcut =>
                  Set_Thermostat_Mode
                  (  Box         => Target.Cube,
                     Device      => Target.Thermostat,
                     Mode        => Manual,
                     Disposition => Airing,
                     Handler     => Data.Rooms.all'Unchecked_Access
                  );
            end case;
         end loop;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Execute")
         )  );
   end Execute;

   function Get
            (  Model  : not null access Shortcut_Store_Record;
               Row    : Gtk_Tree_Iter;
               Column : Positive;
               First  : GDouble;
               Last   : GDouble
            )  return GDouble is
      Result : GDouble;
      Value  : GValue;
   begin
      Get_Value (Model, Row, GInt (Time_Stamp_Column + Column), Value);
      Result :=
         GDouble'Max (GDouble'Min (Get_Double (Value), Last), First);
      Unset (Value);
      return Result;
   end Get;

   function Get_Key
            (  Data : MAX_Shortcuts_Record;
               Row  : Gtk_Tree_Iter
            )  return Shortcut_Target is
      Iter : constant Gtk_Tree_Iter := Data.Store.From_Extension (Row);
   begin
      return
      (  Cube       => Data.Rooms.Get_Cube (Iter),
         Thermostat => Data.Rooms.Get (Iter)
      );
   end Get_Key;

   function Get_Mode
            (  Data : MAX_Shortcuts_Record;
               Row  : Gtk_Tree_Iter;
               No   : Positive
            )  return Shortcut_Mode is
   begin
      return From_String
             (  Gtk.Missed.Get
                (  Gtk_Tree_Model'(To_Interface (Data.Store)),
                   Row,
                   Time_Stamp_Column + 1 + GInt (No - 1) * 5
             )  );
   end Get_Mode;

   function Get_Prefix
            (  No      : Positive;
               Address : RF_Address
            )  return String is
   begin
      return "shortcut" & Image (No) & "#" & Image (Address) & "#";
   end Get_Prefix;

   function Gtk_Shortcuts_New
            (  Rooms         : not null access Rooms_List_Record'Class;
               Shorcut_Count : Positive
            )  return MAX_Shortcuts is
      Result : constant MAX_Shortcuts :=
                    new MAX_Shortcuts_Record (Shorcut_Count);
      Scroll : Gtk_Scrolled_Window;
   begin
      Result.Rooms := Rooms.all'Unchecked_Access;
      Gtk.Box.Initialize (Result, Orientation_Vertical, 6);
      Result.Set_Border_Width (5);
      Scroll := Gtk_Scrolled_Window_New;
      Result.Pack_Start (Scroll);
      Result.Store := new Shortcut_Store_Record;
      declare
         List  : GType_Array (0..GUInt (Shorcut_Count) * 5 - 1);
         Index : GUInt := 0;
      begin
         for No in 1..Shorcut_Count loop
            List (Index    ) := GType_String;  -- Thermostat mode
            List (Index + 1) := GType_Boolean; -- No teemperature
            List (Index + 2) := GType_Double;  -- Temperature
            List (Index + 3) := GType_Boolean; -- No time
            List (Index + 4) := GType_Double;  -- Time
            Index := Index + 5;
         end loop;
         Initialize (Result.Store, Rooms.Get_List, List);
      end;
      Result.View :=
         Gtk_Tree_View_New_With_Model (To_Interface (Result.Store));
      Result.View.Get_Selection.Set_Mode (Selection_Multiple);
      Result.View.Set_Activate_On_Single_Click (False);
      Result.Store.Unref;
      Scroll.Add (Result.View);

      Gtk_New (Result.Modes, (GType_String, GType_Int));
      declare
         Row : Gtk_Tree_Iter;
      begin
         for Mode in Shortcut_Mode'Range loop
            Result.Modes.Append (Row);
            Gtk.Missed.Set (Result.Modes, Row, 0, To_String (Mode));
            Set (Result.Modes, Row, 1, GInt (Shortcut_Mode'Pos (Mode)));
         end loop;
      end;
      declare
         Column    : Gtk_Tree_View_Column;
         Text      : Gtk_Cell_Renderer_Text;
         Icon      : Gtk_Cell_Renderer_Pixbuf;
         Column_No : Gint;
      begin
         Gtk_New (Column);
         Column.Set_Title ("Name");
         Gtk_New (Icon);
         Column.Pack_Start (Icon, False);
         Column.Add_Attribute (Icon, "pixbuf", Type_Column);
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Column.Add_Attribute (Text, "text", Name_Column);
         Column_No := Result.View.Append_Column (Column);
         Column.Set_Resizable (True);

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

         for No in 1..Shorcut_Count loop
            declare
               Combo       : Gtk_Cell_Renderer_Combo renames
                                Result.Combo (No);
               Temperature : Gtk_Cell_Renderer_Fixed renames
                                Result.Temperature (No);
               Timeout     : Gtk_Cell_Renderer_Fixed renames
                                Result.Timeout (No);
               Offset      : constant GInt :=
                                Time_Stamp_Column + GInt (No - 1) * 5;
            begin
               Gtk_New (Column);
               Column.Set_Title ("Button #" & Image (No));
               Gtk_New (Combo);
               Glib.Properties.Set_Property
               (  Combo,
                  Gtk.Cell_Renderer_Combo.Model_Property,
                  Types.GType_Interface
                  (  Gtk_Tree_Model'(To_Interface (Result.Modes))
               )  );
               Glib.Properties.Set_Property
               (  Combo,
                  Gtk.Cell_Renderer_Combo.Text_Column_Property,
                  0
               );
               Glib.Properties.Set_Property
               (  Combo,
                  Gtk.Cell_Renderer_Combo.Has_Entry_Property,
                  False
               );
               Glib.Properties.Set_Property
               (  Combo,
                  Gtk.Cell_Renderer_Text.Editable_Property,
                  True
               );
               Glib.Properties.Set_Property
               (  Combo,
                  Gtk.Cell_Renderer.Xalign_Property,
                  0.5
               );
               Column.Pack_Start (Combo, False);
               Column.Add_Attribute
               (  Combo,
                  "text",
                  Offset + 1
               );
               Combo_Handlers.Connect
               (  Combo,
                  "changed",
                  On_Changed'Access,
                  Result.all'Unchecked_Access,
                  True
               );
               Set_Mode_Cell_Data.Set_Cell_Data_Func
               (  Column,
                  Combo,
                  Combo_Cell_Data'Access,
                  Result
               );
               Column_No := Result.View.Append_Column (Column);
               Column.Set_Resizable (False);

               Gtk_New (Column);
               Column.Set_Title ("  " & Degree & "C  ");
               Gtk_New (Temperature, 1);
               Column.Pack_Start (Temperature, False);
               Column.Add_Attribute (Temperature, "empty", Offset + 2);
               Column.Add_Attribute (Temperature, "value", Offset + 3);
               Value_Handlers.Connect
               (  Temperature,
                  "commit",
                  Commit_Temperature'Access,
                  Result.all'Unchecked_Access
               );
               Column_No := Result.View.Append_Column (Column);
               Column.Set_Resizable (False);

               Gtk_New (Column);
               Column.Set_Title ("hours");
               Gtk_New (Timeout, 1);
               Column.Pack_Start (Timeout, False);
               Column.Add_Attribute (Timeout, "empty", Offset + 4);
               Column.Add_Attribute (Timeout, "value", Offset + 5);
               Value_Handlers.Connect
               (  Timeout,
                  "commit",
                  Commit_Timeout'Access,
                  Result.all'Unchecked_Access
               );
               Column_No := Result.View.Append_Column (Column);
               Column.Set_Resizable (False);
            end;
         end loop;
         Gtk_New (Column);
         Column_No := Result.View.Append_Column (Column);
         Column.Set_Resizable (True);
      end;
      Tree_Handlers.Connect
      (  Result.Store,
         "row-inserted",
         On_Row_Inserted'Access,
         Result.all'Unchecked_Access,
         True
      );
      Result.Modes.Unref;
      Result.View.Expand_All;
      declare
         Box   : Gtk_HBox;
         Label : Gtk_Label;
      begin
         Gtk_New_HBox (Box);
         Box.Set_Spacing (3);
         Result.Pack_Start (Box, False, False);
         Gtk_New (Label, "Button names");
         Box.Pack_Start (Label, False, False);
         for No in 1..Shorcut_Count loop
            declare
               Edit : Gtk_GEntry renames Result.Entries (No);
               Text : constant String :=
                         Restore
                         (  "Shortcut_Button_Name#" & Image (No),
                            Strings_Edit.Integers.Superscript.Image (No)
                         );
            begin
               Gtk_New (Edit);
               Edit.Set_Width_Chars (Edit_Width);
               if Find_Property (Edit, "max-width-chars") /= null then
                  Set_Property
                  (  Edit,
                     Build ("max-width-chars"),
                     GInt'(Edit_Width)
                  );
               end if;
               Edit.Set_Text (Text);
               Box.Pack_Start (Edit, False, False);
               Shortcuts_Handlers.Connect
               (  Edit,
                  "changed",
                  On_Entry_Changed'Access,
                  Result.all'Unchecked_Access
               );
            end;
         end loop;
      end;
      Rooms.Add_Shortcuts (Result);
      return Result;
   end Gtk_Shortcuts_New;

   procedure On_Changed
             (  Combo  : access Gtk_Cell_Renderer_Combo_Record'Class;
                Params : GValues;
                Data   : MAX_Shortcuts
             )  is
      No         : constant Natural := Data.Combo_To_No (Combo);
      Source_Row : Gtk_Tree_Iter;
      Row        : Gtk_Tree_Iter;
      Mode       : Shortcut_Mode;
      Column     : Natural;
      Key        : Shortcut_Target;
      Action     : Shortcut_Action;
   begin
      Get_Tree_Iter (Nth (Params, 2), Source_Row);
      if Source_Row = Null_Iter then
         return;
      end if;
      Row :=
         Get_Iter_From_String
         (  To_Interface (Data.Store),
            Get_String (Nth (Params, 1))
         );
      if Row = Null_Iter or else No = 0 then
         return;
      end if;
      Column := (No - 1) * 5;
      Key    := Data.Get_Key (Row);
      Mode   := Shortcut_Mode'Val (Get_Int (Data.Modes, Source_Row, 1));
      case Mode is
         when Unchanged_Shortcut =>
            Action := (Mode => Unchanged_Shortcut);
            Data.Store.Set_Extension (Row, Column + 2, True);
            Data.Store.Set_Extension (Row, Column + 4, True);
            Data.Shortcuts (No).Remove (Key);
            if Data.Shortcuts (No).Is_Empty then
               Data.Button (No).Hide;
            end if;
         when Manual_Shortcut =>
            Action := (  Manual_Shortcut,
                         Centigrade
                         (  Data.Store.Get
                            (  Row,
                               Column + 3,
                               First_T,
                               Last_T
                      )  )  );
            if Action.Manual_Temperature = 0.0 then
               Action.Manual_Temperature := Centigrade (Default_T);
            end if;
            Data.Store.Set_Extension (Row, Column + 2, False);
            Data.Store.Set
            (  Row,
               Column + 3,
               GDouble (Action.Manual_Temperature)
            );
            Data.Store.Set_Extension (Row, Column + 4, True);
            Data.Shortcuts (No).Replace (Key, Action);
            if not Data.Shortcuts (No).Is_Empty then
               Data.Button (No).Show;
            end if;
         when Vacation_Shortcut =>
            Action := (  Vacation_Shortcut,
                         Centigrade
                         (  Data.Store.Get
                            (  Row,
                               Column + 3,
                               First_T,
                               Last_T
                         )  ),
                        +Data.Store.Get
                         (  Row,
                            Column + 5,
                            First_Time,
                            Last_Time
                      )  );
            if Action.Vacation_Temperature = 0.0 then
               Action.Vacation_Temperature := Centigrade (Default_T);
            end if;
            Data.Store.Set_Extension (Row, Column + 2, False);
            Data.Store.Set
            (  Row,
               Column + 3,
               GDouble (Action.Vacation_Temperature)
            );
            Data.Store.Set_Extension (Row, Column + 4, False);
            Data.Store.Set (Row, Column + 5, -Action.Vacation_Time);
            Data.Shortcuts (No).Replace (Key, Action);
            if not Data.Shortcuts (No).Is_Empty then
               Data.Button (No).Show;
            end if;
         when Automatic_Shortcut =>
            Action := (Mode => Automatic_Shortcut);
            Data.Store.Set_Extension (Row, Column + 2, True);
            Data.Store.Set_Extension (Row, Column + 4, True);
            Data.Shortcuts (No).Replace (Key, Action);
            if not Data.Shortcuts (No).Is_Empty then
               Data.Button (No).Show;
            end if;
         when Automatic_With_Temperature_Shortcut =>
            Action := (  Automatic_With_Temperature_Shortcut,
                         Centigrade
                         (  Data.Store.Get
                            (  Row,
                               Column + 3,
                               First_T,
                               Last_T
                      )  )  );
            if Action.Automatic_Temperature = 0.0 then
               Action.Automatic_Temperature := Centigrade (Default_T);
            end if;
            Data.Store.Set_Extension (Row, Column + 2, False);
            Data.Store.Set
            (  Row,
               Column + 3,
               GDouble (Action.Automatic_Temperature)
            );
            Data.Store.Set_Extension (Row, Column + 4, True);
            Data.Shortcuts (No).Replace (Key, Action);
            if not Data.Shortcuts (No).Is_Empty then
               Data.Button (No).Show;
            end if;
         when Boost_Shortcut =>
            Action := (Mode => Boost_Shortcut);
            Data.Store.Set_Extension (Row, Column + 2, True);
            Data.Store.Set_Extension (Row, Column + 4, True);
            Data.Shortcuts (No).Replace (Key, Action);
            if not Data.Shortcuts (No).Is_Empty then
               Data.Button (No).Show;
            end if;
         when Eco_Shortcut =>
            Action := (Mode => Eco_Shortcut);
            Data.Store.Set_Extension (Row, Column + 2, True);
            Data.Store.Set_Extension (Row, Column + 4, True);
            Data.Shortcuts (No).Replace (Key, Action);
            if not Data.Shortcuts (No).Is_Empty then
               Data.Button (No).Show;
            end if;
         when Comfort_Shortcut =>
            Action := (Mode => Comfort_Shortcut);
            Data.Store.Set_Extension (Row, Column + 2, True);
            Data.Store.Set_Extension (Row, Column + 4, True);
            Data.Shortcuts (No).Replace (Key, Action);
            if not Data.Shortcuts (No).Is_Empty then
               Data.Button (No).Show;
            end if;
         when Airing_Shortcut =>
            Action := (Mode => Airing_Shortcut);
            Data.Store.Set_Extension (Row, Column + 2, True);
            Data.Store.Set_Extension (Row, Column + 4, True);
            Data.Shortcuts (No).Replace (Key, Action);
            if not Data.Shortcuts (No).Is_Empty then
               Data.Button (No).Show;
            end if;
      end case;
      Data.Store.Set_Extension (Row, Column + 1, To_String (Mode));
      Data.Store_Shortcut (No, Key, Action);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Changed")
         )  );
   end On_Changed;

   procedure On_Entry_Changed
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Data   : MAX_Shortcuts
             )  is
      No : constant Natural := Data.Entry_To_No (Object);
   begin
      if No = 0 then
         return;
      end if;
      declare
         Text : constant String := Data.Entries (No).Get_Text;
      begin
         Store ("Shortcut_Button_Name#" & Image (No), Text);
         if Data.Button (No) /= null then
            Data.Button (No).Set_Label (Text);
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Entry_Changed")
         )  );
   end On_Entry_Changed;

   procedure On_Play
             (  Object : access GObject_Record'Class;
                Data   : MAX_Shortcuts
             )  is
   begin
      Data.Execute (Data.Button_To_No (Object));
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Play")
         )  );
   end On_Play;

   procedure On_Row_Inserted
             (  Reference : access Gtk_Root_Tree_Model_Record'Class;
                Params    : GValues;
                Data      : MAX_Shortcuts
             )  is
      Path : constant Gtk_Tree_Path :=
                      Convert (Get_Address (Nth (Params, 1)));
      Row  : Gtk_Tree_Iter;
   begin
      Row := Data.Store.Get_Iter (Path);
      if Row /= Null_Iter then
         for No in 1..Data.Shorcut_Count loop
            declare
               Column : constant Natural := (No - 1) * 5;
            begin
               Data.Store.Set_Extension (Row, Column + 2, True);
               Data.Store.Set_Extension (Row, Column + 4, True);
            end;
         end loop;
      end if;
      Data.View.Expand_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Row_Inserted")
         )  );
   end On_Row_Inserted;

   function Restore (Prefix : String) return Centigrade is
      Text   : constant String := Restore (Prefix & "temperature", "");
      Result : GDouble := Default_T;
   begin
      if Text'Length > 0 then
         begin
            Result := GDouble (Strings_Edit.Floats.Value (Text));
         exception
            when others =>
               null;
         end;
      end if;
      return Centigrade
             (  GDouble'Max
                (  GDouble'Min (Result, Last_T),
                   First_T
             )  );
   end Restore;

   function Restore (Prefix : String) return Duration is
      Text   : constant String := Restore (Prefix & "duration", "");
      Result : GDouble := Default_Time;
   begin
      if Text'Length > 0 then
         begin
            Result := GDouble (Strings_Edit.Floats.Value (Text));
         exception
            when others =>
               null;
         end;
      end if;
      return +GDouble'Max (GDouble'Min (Result, Last_Time), First_Time);
   end Restore;

   procedure Set
             (  Model  : not null access Shortcut_Store_Record;
                Row    : Gtk_Tree_Iter;
                Column : Positive;
                Value  : GDouble
             )  is
      Item : GValue;
   begin
      Init (Item, GType_Double);
      Set_Double (Item, Value);
      Model.Set_Extension (Row, Column, Item);
      Unset (Item);
   end Set;

   procedure Store_Shortcut
             (  Data   : in out MAX_Shortcuts_Record;
                No     : Positive;
                Key    : Shortcut_Target;
                Action : Shortcut_Action
             )  is
      Prefix : constant String := Get_Prefix (No, Key.Thermostat);
   begin
      case Action.Mode is
         when Unchanged_Shortcut =>
            Delete (Prefix & "mode");
            Delete (Prefix & "duration");
            Delete (Prefix & "temperature");
         when Automatic_With_Temperature_Shortcut =>
            Store (Prefix & "mode", To_String (Action.Mode));
            Store
            (  Prefix & "temperature",
               Image (Action.Automatic_Temperature)
            );
         when Manual_Shortcut =>
            Store (Prefix & "mode", To_String (Action.Mode));
            Store
            (  Prefix & "temperature",
               Image (Action.Manual_Temperature)
            );
         when Vacation_Shortcut =>
            Store (Prefix & "mode", To_String (Action.Mode));
            Store
            (  Prefix & "temperature",
               Image (Action.Vacation_Temperature)
            );
            declare
               Value : constant GDouble := -Action.Vacation_Time;
            begin
               Store
               (  Prefix & "duration",
                  Strings_Edit.Floats.Image (Float (Value))
               );
            end;
         when others =>
            Store (Prefix & "mode", To_String (Action.Mode));
      end case;
   end Store_Shortcut;

   function Temperature_To_No
            (  Data : MAX_Shortcuts_Record;
               Cell : not null access
                      Gtk_Cell_Renderer_Fixed_Record'Class
            )  return Natural is
   begin
      for No in Data.Temperature'Range loop
         if Data.Temperature (No) = Cell then
            return No;
         end if;
      end loop;
      return 0;
   end Temperature_To_No;

   function Timeout_To_No
            (  Data : MAX_Shortcuts_Record;
               Cell : not null access
                      Gtk_Cell_Renderer_Fixed_Record'Class
            )  return Natural is
   begin
      for No in Data.Timeout'Range loop
         if Data.Timeout (No) = Cell then
            return No;
         end if;
      end loop;
      return 0;
   end Timeout_To_No;

end MAX_Shortcuts_Page;
