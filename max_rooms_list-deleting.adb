--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.Deleting                     Luebeck            --
--  Implementation                                 Spring, 2019       --
--                                                                    --
--                                Last revision :  20:42 01 Dec 2020  --
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

with Ada.Exceptions;          use Ada.Exceptions;
with Ada.IO_Exceptions;       use Ada.IO_Exceptions;
with GLib.Messages;           use GLib.Messages;
with GLib.Values;             use GLib.Values;
with Gtk.Cell_Renderer_Text;  use Gtk.Cell_Renderer_Text;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Missed;              use Gtk.Missed;
with Gtk.Scrolled_Window;     use Gtk.Scrolled_Window;
with Gtk.Separator;           use Gtk.Separator;
with Gtk.Stock;               use Gtk.Stock;
with MAX_IO.Pairing;          use MAX_IO.Pairing;
with Strings_Edit;            use Strings_Edit;
with Strings_Edit.Quoted;     use Strings_Edit.Quoted;

with GLib.Object.Checked_Destroy;
with Image_Eco_Switch_XPM.Image;
with Image_Radiator_Thermostat_XPM.Image;
with Image_Shutter_Contact_XPM.Image;
with Image_Wall_Thermostat_XPM.Image;

package body MAX_Rooms_List.Deleting is

   function Where (Name : String) return String is
   begin
      return " in MAX_Rooms_List.Deleting." & Name;
   end Where;

   procedure Canceled (Dialog : in out Delete_Dialog_Record) is
   begin
      null;
   end Canceled;

   procedure Finished (Dialog : in out Delete_Dialog_Record) is
   begin
      null;
   end Finished;

   function Delete_Faulty
            (  Rooms : not null Rooms_List
            )  return Detached_Interface_Ptr is
      Dialog : Faulty_Dialog;

      procedure Insert_Deleted
                (  Address : RF_Address;
                   Data    : Faulty_Device_Data'Class
                )  is
         Row   : Gtk_Tree_Iter;
         Value : GValue;
      begin
         Dialog.Deleted_Store.Append (Row);

         Init (Value, GType_String);
         Set_String (Value, Data.Name);
         Dialog.Deleted_Store.Set_Value (Row, 0, Value);
         Unset (Value);

         Init (Value, GType_String);
         Set_String (Value, Image (Address));
         Dialog.Deleted_Store.Set_Value (Row, 1, Value);
         Unset (Value);

         Init (Value, GType_String);
         Set_String (Value, Data.Guessed);
         Dialog.Deleted_Store.Set_Value (Row, 2, Value);
         Unset (Value);

         Init (Value, GType_String);
         Set_String (Value, Image (Data.Deleted));
         Dialog.Deleted_Store.Set_Value (Row, 3, Value);
         Unset (Value);
         Dialog.Have_Deleted := Dialog.Have_Deleted + 1;
      end Insert_Deleted;

      procedure Insert_Faulty
                (  Cube    : RF_Address;
                   Address : RF_Address;
                   Data    : Faulty_Device_Data'Class
                )  is
         Text    : String (1..200);
         Pointer : Integer := 1;
         Row     : Gtk_Tree_Iter;
         Value   : GValue;
      begin
         Dialog.Faulty_Store.Append (Row);

         Init (Value, GType_String);
         Set_String (Value, Image (Address));
         Dialog.Faulty_Store.Set_Value (Row, 0, Value);
         Unset (Value);

         if Data.Error then
            if Pointer > 1 then
               Put (Text, Pointer, ", ");
            end if;
            Put (Text, Pointer, "erroneous");
         end if;
         if not Data.Initialized then
            if Pointer > 1 then
               Put (Text, Pointer, ", ");
            end if;
            Put (Text, Pointer, "not initialized");
         end if;
         if Data.Orphaned then
            if Pointer > 1 then
               Put (Text, Pointer, ", ");
            end if;
            Put (Text, Pointer, "orphaned");
         end if;

         Init (Value, GType_String);
         Set_String (Value, Text (1..Pointer - 1));
         Dialog.Faulty_Store.Set_Value (Row, 1, Value);
         Unset (Value);

         Init (Value, GType_String);
         Set_String (Value, Data.Guessed);
         Dialog.Faulty_Store.Set_Value (Row, 2, Value);
         Unset (Value);

         Dialog.Faulty_Store.Set (Row, 3, GInt (Cube));
         Dialog.Faulty_Store.Set (Row, 4, GInt (Address));

         Init (Value, GType_String);
         Set_String (Value, Data.Serial_No);
         Dialog.Faulty_Store.Set_Value (Row, 5, Value);
         Unset (Value);

         Dialog.Have_Faulty := Dialog.Have_Faulty + 1;
      end Insert_Faulty;

   begin
      if Rooms.Faulty_List.Is_Empty then
         Rooms.Faulty.Hide;
         return null;
      end if;
      Dialog := new Faulty_Dialog_Record;
      Dialog.List := Rooms;
      Initialize
      (  Dialog,
         Title  => "Faulty devices",
         Parent => Window,
         Flags  => Modal or Destroy_With_Parent
      );
      Dialog.Realize;

      Gtk_New
      (  Dialog.Deleted_Store,
         (  GType_String, -- Name
            GType_String, -- Address
            GType_String, -- Type
            GType_String  -- Time
      )  );
      Gtk_New
      (  Dialog.Faulty_Store,
         (  GType_String, -- Address
            GType_String, -- Status
            GType_String, -- Guessed type
            GType_Int,    -- Cube address
            GType_Int,    -- Device address
            GType_String  -- Serial No
      )  );
      for Cube in reverse 1..Rooms.Faulty_List.Get_Size loop
         declare
            Map : Faulty_Device_Maps.Map renames
                  Rooms.Faulty_List.Get (Cube).Ptr.Map;
         begin
            if Map.Is_Empty then
               Rooms.Faulty_List.Remove (Cube);
            else
               for Device in 1..Map.Get_Size loop
                  declare
                     This : Faulty_Device_Data'Class renames
                            Map.Get (Device).Ptr.all;
                  begin
                     if This.Pending_Delete then
                        Insert_Deleted (Map.Get_Key (Device), This);
                     else
                        Insert_Faulty
                        (  Rooms.Faulty_List.Get_Key (Cube),
                           Map.Get_Key (Device),
                           This
                        );
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;

      if Dialog.Have_Deleted > 0 then
         declare
            Label     : Gtk_Label;
            View      : Gtk_Tree_View;
            Scroll    : Gtk_Scrolled_Window;
            Column_No : Gint;
            Column    : Gtk_Tree_View_Column;
            Text      : Gtk_Cell_Renderer_Text;
         begin
            Gtk_New (Label);
            Label.Set_Markup
            (  "<big><b>Devices being deleted now...</b></big>"
            );
            Dialog.Get_Content_Area.Set_Spacing (3);
            Dialog.Get_Content_Area.Pack_Start (Label, False, False);
            Gtk_New (Scroll);
            Scroll.Set_Policy (Policy_Never, Policy_Automatic);
            Dialog.Get_Content_Area.Pack_Start (Scroll, Padding => 10);
            Gtk_New (View, Dialog.Deleted_Store);

            Gtk_New (Column);
            Column.Set_Title ("Name");
            Gtk_New (Text);
            Column.Pack_Start (Text, True);
            Column.Add_Attribute (Text, "text", 0);
            Column_No := View.Append_Column (Column);

            Gtk_New (Column);
            Column.Set_Title ("Address");
            Gtk_New (Text);
            Column.Pack_Start (Text, True);
            Column.Add_Attribute (Text, "text", 1);
            Column_No := View.Append_Column (Column);

            Gtk_New (Column);
            Column.Set_Title ("Device type");
            Gtk_New (Text);
            Column.Pack_Start (Text, True);
            Column.Add_Attribute (Text, "text", 2);
            Column_No := View.Append_Column (Column);

            Gtk_New (Column);
            Column.Set_Title ("Started");
            Gtk_New (Text);
            Column.Pack_Start (Text, True);
            Column.Add_Attribute (Text, "text", 3);
            Column_No := View.Append_Column (Column);

            Scroll.Add (View);

            Gtk_New (Label);
            Label.Set_Max_Width_Chars (80);
            Label.Set_Line_Wrap (True);
            Label.Set_Markup
            (  "<i>Note</i>, removing devices from the cube may take "
            &  "considerable time, up to several minutes. "
            &  "Until then the devices in the list above remain paired "
            &  "with the cube. "
            &  "Only after the cube finally deletes you will be able "
            &  "to pair them again. "
            &  "Sometimes the time can be reduced by starting "
            &  "pairing the cube with the device being removed. "
            &  "In order to work <b>both</b> the cube and the device "
            &  "must be in the pairing mode. "
            &  "Though cube will not find the device, it might "
            &  "remove it upon leaving the pairing mode."
            );
            Dialog.Get_Content_Area.Pack_Start (Label, False, False);
         end;
      end if;

      if Dialog.Have_Faulty > 0 then
         declare
            Label     : Gtk_Label;
            View      : Gtk_Tree_View renames Dialog.Faulty_View;
            Scroll    : Gtk_Scrolled_Window;
            Column_No : Gint;
            Column    : Gtk_Tree_View_Column;
            Text      : Gtk_Cell_Renderer_Text;
         begin
            Gtk_New (Label);
            Label.Set_Markup
            (  "<big><b>Delete or reattach listed faulty "
            &  "devices?</b></big>"
            );
            Dialog.Get_Content_Area.Pack_Start (Label, False, False);
            Gtk_New (Scroll);
            Scroll.Set_Policy (Policy_Never, Policy_Automatic);
            Dialog.Get_Content_Area.Pack_Start (Scroll, Padding => 10);
            Gtk_New (View, Dialog.Faulty_Store);

            Gtk_New (Column);
            Column.Set_Title ("Address");
            Gtk_New (Text);
            Column.Pack_Start (Text, True);
            Column.Add_Attribute (Text, "text", 0);
            Column_No := View.Append_Column (Column);

            Gtk_New (Column);
            Column.Set_Title ("Device state");
            Gtk_New (Text);
            Column.Pack_Start (Text, True);
            Column.Add_Attribute (Text, "text", 1);
            Column_No := View.Append_Column (Column);

            Gtk_New (Column);
            Column.Set_Title ("Guessed type");
            Gtk_New (Text);
            Column.Pack_Start (Text, True);
            Column.Add_Attribute (Text, "text", 2);
            Column_No := View.Append_Column (Column);

            Gtk_New (Column);
            Column.Set_Title ("Serial No.");
            Gtk_New (Text);
            Column.Pack_Start (Text, True);
            Column.Add_Attribute (Text, "text", 5);
            Column_No := View.Append_Column (Column);

            Scroll.Add (View);
            Delete_Handlers.Connect
            (  View.Get_Selection,
               "changed",
               Delete_Handlers.To_Marshaller (Device_Changed'Access),
               Dialog
            );
         end;
      end if;
      Dialog.Set_Default_Response (Gtk_Response_OK);
      if Dialog.Have_Faulty > 0 then
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Response => Gtk_Response_OK,
            Icon     => Stock_Delete,
            Label    => "_Delete",
            Tip      => "Delete faulty devices"
         );
         Dialog.Reattach :=
            Add_Button_From_Stock
            (  Dialog   => Dialog,
               Response => Gtk_Response_Apply,
               Icon     => Stock_Connect,
               Label    => "_Reattach",
               Tip      => "Try to reattach or delete faulty "   &
                           "devices. "                           &
                           "Sometimes you can make the cube to " &
                           "connect an orphan device. "          &
                           "Press this button to try it"
            );
         Dialog.Reattach.Set_Sensitive (False);
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Response => Gtk_Response_Cancel,
            Icon     => Stock_Cancel,
            Label    => "_Cancel",
            Tip      => "Cancel deleting faulty devices"
         );
      else
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Response => Gtk_Response_OK,
            Icon     => Stock_OK,
            Label    => "_OK",
            Tip      => "Close"
         ) .Set_Can_Default (True);
      end if;
      Dialog.Deleted_Store.Unref;
      Dialog.Faulty_Store.Unref;
      Dialog.Set_Modal (True);
      Dialog.On_Response (On_Faulty_Device_Response'Access);
      Dialog.Show_All;
      return Dialog.all'Unchecked_Access;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Faulty")
         )  );
         return null;
   end Delete_Faulty;

   procedure Delete_Selected
             (  Rooms    : not null Rooms_List;
                Selected : Gtk_Tree_Iter := Null_Iter
             )  is
      Dialog    : Device_Dialog;
      Label     : Gtk_Label;
      Row       : Gtk_Tree_Iter := Selected;
      Separator : Gtk_Separator;
   begin
      if Selected = Null_Iter then
         Row := Rooms.Get_Selected;
         if Row = Null_Iter then
            Rooms.Rename.Hide;
            return;
         end if;
      end if;
      declare
         Cube : constant RF_Address := Rooms.Get_Cube (Row);
      begin
         if Cube = 0 then
            Rooms.Rename.Hide;
            return;
         end if;
         Dialog := new Device_Dialog_Record;
         Dialog.Cube    := Cube;
         Dialog.List    := Rooms;
         Dialog.Kind_Of := Rooms.Get (Row);
      end;
      case Dialog.Kind_Of is
         when Cube =>
            Initialize
            (  Dialog,
               Title  => "Reset cube",
               Parent => Window,
               Flags  => Modal or Destroy_With_Parent
            );
            Dialog.Realize;
            Gtk_New (Label);
            Label.Set_Markup
            (  "<big><b>Reset the cube " & Image (Cube) & "?</b></big>"
            );
            Label.Set_Line_Wrap (True);
            Dialog.Get_Content_Area.Set_Spacing (3);
            Dialog.Get_Content_Area.Pack_Start
            (  Label,
               False,
               False,
               10
            );
            Gtk_New_Hseparator (Separator);
            Dialog.Get_Content_Area.Pack_Start (Separator, False, False);
            Gtk_New
            (  Label,
               (  "All devices and rooms of the cube will be deleted. "
               &  "The removed devices must be paired again with this "
               &  "or other cube"
            )  );
            Label.Set_Max_Width_Chars (50);
            Label.Set_Line_Wrap (True);
            Dialog.Get_Content_Area.Pack_Start (Label, Padding => 10);
            Add_Button_From_Stock
            (  Dialog   => Dialog,
               Response => Gtk_Response_OK,
               Icon     => Stock_OK,
               Label    => "_OK",
               Tip      => "Reset the cube"
            ) .Set_Can_Default (True);
         when Radiator_Thermostat..Eco_Button =>
            Dialog.Address := Rooms.Get (Row);
            Initialize
            (  Dialog,
               Title  => "Delete device",
               Parent => Window,
               Flags  => Modal or Destroy_With_Parent
            );
            Dialog.Realize;
            declare
               Box : Gtk_Box;
            begin
               Gtk_New_HBox (Box);
               Box.Set_Spacing (3);
               Dialog.Get_Content_Area.Pack_Start
               (  Box,
                  False,
                  False,
                  10
               );
               case Dialog.Kind_Of is
                  when Shutter_Contact =>
                     Box.Pack_Start
                     (  Image_Shutter_Contact_XPM.Image,
                        False,
                        False
                     );
                  when Eco_Button =>
                     Box.Pack_Start
                     (  Image_Eco_Switch_XPM.Image,
                        False,
                        False
                     );
                  when Wall_Thermostat =>
                     Box.Pack_Start
                     (  Image_Wall_Thermostat_XPM.Image,
                        False,
                        False
                     );
                  when others =>
                     Box.Pack_Start
                     (  Image_Radiator_Thermostat_XPM.Image,
                        False,
                        False
                     );
               end case;
               Gtk_New (Label);
               Label.Set_Markup
               (  "<big><b>Delete "
               &  Image (Dialog.Kind_Of)
               &  "</b> <i>"
               &  Quote (Rooms.Get (Row, Name_Column), ''')
               &  "</i> <b>"
               &  Image (Dialog.Address)
               &  "? </b></big>"
               );
               Label.Set_Line_Wrap (True);
               Label.Set_Max_Width_Chars (30);
               Box.Pack_Start (Label);
            end;
            Dialog.Room := Rooms.Get (Dialog.Address);
            Dialog.Devices :=
               Get_Thermostats
               (  Dialog.Cube,
                  Dialog.Room,
                  (  Radiator_Thermostat..Eco_Button => True,
                     others => False
               )  );
            if Dialog.Devices.Get_Size = 1 then
               Gtk_New_Hseparator (Separator);
               Dialog.Get_Content_Area.Pack_Start
               (  Separator,
                  False,
                  False
               );
               Gtk_New
               (  Label,
                  (  "Note that this will also remove its room "
                  &  "since it is the last device there"
               )  );
               Label.Set_Max_Width_Chars (50);
               Label.Set_Line_Wrap (True);
               Dialog.Get_Content_Area.Pack_Start (Label, Padding => 10);
            end if;
            Add_Button_From_Stock
            (  Dialog   => Dialog,
               Response => Gtk_Response_OK,
               Icon     => Stock_OK,
               Label    => "_OK",
               Tip      => "Delete the " & Image (Dialog.Kind_Of)
            ) .Set_Can_Default (True);
         when Unknown =>
            Dialog.Room := Rooms.Get (Row);
            Initialize
            (  Dialog,
               Title  => "Delete room",
               Parent => Window,
               Flags  => Modal or Destroy_With_Parent
            );
            Dialog.Realize;
            Gtk_New (Label);
            Label.Set_Markup
            (  "<big><b>Delete room</b> <i>"
            &  Quote (Rooms.Get (Row, Name_Column), ''')
            &  "</i><b>?</b></big>"
            );
            Label.Set_Line_Wrap (True);
            Label.Set_Max_Width_Chars (30);
            Dialog.Get_Content_Area.Pack_Start (Label, False, False, 10);
            Dialog.Devices :=
               Get_Thermostats
               (  Dialog.Cube,
                  Dialog.Room,
                  (  Radiator_Thermostat..Eco_Button => True,
                     others => False
               )  );
            Gtk_New_Hseparator (Separator);
            Dialog.Get_Content_Area.Pack_Start
            (  Separator,
               False,
               False
            );
            Gtk_New
            (  Label,
               (  "Note that this will also remove all devices "
               &  "from this room: "
               &  Image (Dialog.Devices)
            )  );
            Label.Set_Max_Width_Chars (50);
            Label.Set_Line_Wrap (True);
            Dialog.Get_Content_Area.Pack_Start (Label, Padding => 10);
            Add_Button_From_Stock
            (  Dialog   => Dialog,
               Response => Gtk_Response_OK,
               Icon     => Stock_OK,
               Label    => "_OK",
               Tip      => "Delete room"
            ) .Set_Can_Default (True);
      end case;
      Add_Button_From_Stock
      (  Dialog   => Dialog,
         Response => Gtk_Response_Cancel,
         Icon     => Stock_Cancel,
         Label    => "_Cancel",
         Tip      => "Cancel"
      );
      Dialog.Set_Default_Response (Gtk_Response_OK);
      Dialog.Set_Modal (True);
      Dialog.On_Response (On_Delete_Device_Response'Access);
      Dialog.Show_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Delete_Selected")
         )  );
   end Delete_Selected;

   procedure Detached_Device
             (  Dialog    : in out Delete_Dialog_Record;
                Cube      : RF_Address;
                Kind_Of   : Device_Type;
                Device    : RF_Address;
                Serial_No : String
             )  is
      Row : constant Gtk_Tree_Iter := Dialog.Find_Faulty (Device);
   begin
      if Row /= Null_Iter then
         Gtk.Missed.Set
         (  Dialog.Faulty_Store,
            Row,
            2,
            Image (Kind_Of)
         );
         Gtk.Missed.Set
         (  Dialog.Faulty_Store,
            Row,
            5,
            Serial_No
         );
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Detached_Device")
         )  );
   end Detached_Device;

   procedure Device_Changed
             (  Object : access GObject_Record'Class;
                Dialog : Faulty_Dialog
             )  is
      Kind_Of : Device_Type;
      Row     : Gtk_Tree_Iter;
      Model   : Gtk_Tree_Model;
      Store   : Gtk_List_Store renames Dialog.Faulty_Store;
   begin
      if Dialog.Faulty_View.Get_Selection.Count_Selected_Rows = 1 then
         Dialog.Faulty_View.Get_Selection.Get_Selected (Model, Row);
         Kind_Of := Guess_Type (Gtk.Missed.Get (Store, Row, 2));
         if Kind_Of in Radiator_Thermostat..Eco_Button then
            Dialog.Reattach.Set_Sensitive (True);
         else
            Dialog.Reattach.Set_Sensitive (False);
         end if;
      else
         Dialog.Reattach.Set_Sensitive (False);
      end if;
   end Device_Changed;

   function Find_Faulty
            (  Dialog  : not null access Delete_Dialog_Record;
               Address : RF_Address
            )  return Gtk_Tree_Iter is
      Result : Gtk_Tree_Iter;
   begin
      Result := Dialog.Faulty_Store.Children (Null_Iter);
      while Result /= Null_Iter loop
         exit when GInt (Address)
                 = Dialog.Faulty_Store.Get_Int (Result, 4);
         Dialog.Faulty_Store.Next (Result);
      end loop;
      return Result;
   end Find_Faulty;

   function Get_Object (Dialog : not null access Delete_Dialog_Record)
      return GObject is
   begin
      return Dialog.all'Unchecked_Access;
   end Get_Object;

   procedure On_A_Response
             (  Dialog   : in out Delete_Dialog_Record;
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

   procedure On_Faulty_Device_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             )  is
   begin
      declare
         Dialog : Delete_Dialog_Record'Class renames
                  Delete_Dialog_Record'Class (Self.all);
      begin
         if Response = Gtk_Response_OK then
            if Dialog.Have_Faulty > 0 then
               declare
                  Current : RF_Address := 0;
                  Box     : RF_Address := 0;
                  Row     : Gtk_Tree_Iter :=
                            Dialog.Faulty_Store.Children (Null_Iter);
                  Devices : RF_Address_Array (1..Dialog.Have_Faulty);
                  Start   : Integer := Devices'First;
               begin
                  for Index in Devices'Range loop
                     Box := RF_Address
                            (  Dialog.Faulty_Store.Get_Int (Row, 3)
                            );
                     Devices (Index) :=
                            RF_Address
                            (  Dialog.Faulty_Store.Get_Int (Row, 4)
                            );
                     Dialog.Faulty_Store.Next (Row);
                     if Current /= Box then
                        if Start < Index then
                           Delete_Paired_Device
                           (  Box       => Current,
                              Addresses => Devices (Start..Index - 1),
                              Cancel    => False,
                              Handler   => Dialog'Unchecked_Access
                           );
                           Dialog.Start_Deleting
                           (  Cube      => Current,
                              Addresses => Devices (Start..Index - 1)
                           );
                        end if;
                        Current := Box;
                        Start   := Index;
                     end if;
                  end loop;
                  if Start <= Devices'Last then
                     Delete_Paired_Device
                     (  Box       => Current,
                        Addresses => Devices (Start..Devices'Last),
                        Cancel    => False,
                        Handler   => Dialog'Unchecked_Access
                     );
                     Dialog.Start_Deleting
                     (  Cube      => Current,
                        Addresses => Devices (Start..Devices'Last)
                     );
                  end if;
               end;
            end if;
         elsif Response = Gtk_Response_Apply then
            declare
               Kind_Of : Device_Type;
               Row     : Gtk_Tree_Iter;
               Model   : Gtk_Tree_Model;
               View    : Gtk_Tree_View  renames Dialog.Faulty_View;
               Store   : Gtk_List_Store renames Dialog.Faulty_Store;
            begin
               if View.Get_Selection.Count_Selected_Rows = 1 then
                  View.Get_Selection.Get_Selected (Model, Row);
                  Kind_Of := Guess_Type (Get (Store, Row, 2));
                  if Kind_Of in Radiator_Thermostat..Eco_Button then
                     Dialog.List.Pair_Detached
                     (  Cube =>
                           RF_Address (Store.Get_Int (Row, 3)),
                        Device =>
                           RF_Address (Store.Get_Int (Row, 4)),
                        Serial_No =>
                           Get (Store, Row, 5),
                        Kind_Of => Kind_Of
                     );
                  end if;
               end if;
      --           List.Interval := Restore_Pairing_Time;
      --           for Cube in reverse 1..List.Faulty_List.Get_Size loop
      --              declare
      --                 Map : Faulty_Device_Maps.Map renames
      --                       List.Faulty_List.Get (Cube).Ptr.Map;
      --              begin
      --                 if not Map.Is_Empty then
      --                    List.Cube := List.Faulty_List.Get_Key (Cube);
      --                    exit;
      --                 end if;
      --              end;
      --           end loop;
      --           if List.Cube /= 0 then
      --              On_Add_Device (List, List);
      --           end if;
            end;
         end if;
         Dialog.List.Deleting := null;
      exception
         when Error : others =>
            Log
            (  MAX_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("On_Faulty_Device_Response")
            )  );
      end;
      GLib.Object.Checked_Destroy (Self);
   end On_Faulty_Device_Response;

   procedure On_Delete_Device_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             )  is
   begin
      declare
         Dialog : Device_Dialog_Record renames
                  Device_Dialog_Record (Self.all);
      begin
         if Response = Gtk_Response_OK then
            case Dialog.Kind_Of is
               when Cube =>
                  Reset_Cube
                  (  Box     => Dialog.Cube,
                     Handler => Dialog'Unchecked_Access
                  );
               when Radiator_Thermostat..Eco_Button =>
                  Delete_Paired_Device
                  (  Box     => Dialog.Cube,
                     Address => Dialog.Address,
                     Cancel  => False,
                     Handler => Dialog'Unchecked_Access
                  );
                  Dialog.Start_Deleting
                  (  Cube      => Dialog.Cube,
                     Addresses => (1 => Dialog.Address)
                  );
               when Unknown =>
                  Delete_Room
                  (  Box     => Dialog.Cube,
                     Room    => Dialog.Room,
                     Handler => Dialog'Unchecked_Access
                  );
                  Dialog.Start_Deleting
                  (  Cube      => Dialog.Cube,
                     Addresses => To_Array (Dialog.Devices)
                  );
            end case;
         end if;
      exception
         when Error : others =>
            Log
            (  MAX_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("On_Delete_Device_Response")
            )  );
      end;
      GLib.Object.Checked_Destroy (Self);
   end On_Delete_Device_Response;

   procedure On_S_Response
             (  Dialog   : in out Delete_Dialog_Record;
                Cube     : access Cube_Client'Class;
                Error    : Boolean;
                Duty     : Ratio;
                Slots    : Natural;
                Expected : in out Natural
             )  is
   begin
      begin
         Dialog.List.Logger.Update_Status
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
         Expected := 0;
         Say
         (  "Failure deleting devices" & Note (Duty),
            "Device deletion error"
         );
      elsif Expected > 0 then
         Expected := Expected - 1;
      end if;
   exception
      when Status_Error =>
         null;
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_S_Response")
         )  );
   end On_S_Response;

   procedure Start_Deleting
             (  Dialog    : not null access Delete_Dialog_Record;
                Cube      : RF_Address;
                Addresses : RF_Address_Array
             )  is
      Now   : constant Time := Clock;
      Index : Integer;
      Row   : Gtk_Tree_Iter;
      List  : Faulty_Device_Map_Handles.Handle;
      Item  : Faulty_Device_Data_Handles.Handle;
   begin
      Index := Dialog.List.Faulty_List.Find (Cube);
      if Index > 0 then
         List := Dialog.List.Faulty_List.Get (Index);
      else
         List.Set (new Faulty_Device_Map);
         Dialog.List.Faulty_List.Add (Cube, List);
      end if;
      for Device in Addresses'Range loop
         Row := Dialog.List.Find (Addresses (Device));
         if Row = Null_Iter then
            Row := Dialog.Find_Faulty (Addresses (Device));
            if Row = Null_Iter then
               Item.Set (new Faulty_Device_Data (True, 1, 7));
               declare
                  This : Faulty_Device_Data'Class renames Item.Ptr.all;
               begin
                  This.Deleted := Now;
                  This.Poll_No := Poll_No + 1;
                  This.Name    := "?";
                  This.Guessed := "unknown";
               end;
            else
               declare
                  Guessed : constant String :=
                                     Get (Dialog.Faulty_Store, Row, 2);
               begin
                  Item.Set
                  (  new Faulty_Device_Data (True, 1, Guessed'Length)
                  );
                  declare
                     This : Faulty_Device_Data'Class renames Item.Ptr.all;
                  begin
                     This.Deleted := Now;
                     This.Poll_No := Poll_No + 1;
                     This.Name    := "?";
                     This.Guessed := Guessed;
                     This.Kind_Of := Guess_Type (Guessed);
                  end;
               end;
            end if;
         else
            declare
               Kind_Of : constant Device_Type := Dialog.List.Get (Row);
               Guessed : constant String      := Image (Kind_Of);
               Name    : constant String :=
                                  Dialog.List.Get (Row, Name_Column);
            begin
               Item.Set
               (  new Faulty_Device_Data
                      (  True,
                         Name'Length,
                         Guessed'Length
               )      );
               declare
                  This : Faulty_Device_Data'Class renames Item.Ptr.all;
               begin
                  This.Deleted := Now;
                  This.Poll_No := Poll_No + 1;
                  This.Name    := Name;
                  This.Guessed := Guessed;
                  This.Kind_Of := Kind_Of;
               end;
            end;
         end if;
         List.Ptr.Map.Replace (Addresses (Device), Item);
      end loop;
   end Start_Deleting;

end MAX_Rooms_List.Deleting;
