--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.Moving                       Luebeck            --
--  Separate body                                  Spring, 2019       --
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

with Ada.Exceptions;         use Ada.Exceptions;
with GLib.Messages;          use GLib.Messages;
with Gtk.Missed;             use Gtk.Missed;
with Gtk.Stock;              use Gtk.Stock;
with MAX_IO.Moving;          use MAX_IO.Moving;
with MAX_Schedule;           use MAX_Schedule;
with MAX_Settings_Page;      use MAX_Settings_Page;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with GLib.Object.Checked_Destroy;

package body MAX_Rooms_List.Moving is

   function Where (Name : String) return String is
   begin
      return " in MAX_Rooms_List.Moving." & Name;
   end Where;

   procedure On_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             )  is
   begin
      declare
         Dialog : Move_Dialog_Record'Class renames
                  Move_Dialog_Record'Class (Self.all);
      begin
         if Response = Gtk_Response_OK then
--                 if List.History /= null then
--                    List.History.Delete (Box, Address);
--                 end if;
            Close (Pages, Dialog.Cube, Dialog.Address);
            Move
            (  Box     => Dialog.Cube,
               Address => Dialog.Address,
               Kind_Of => Dialog.Kind_Of,
               Room    => Room_ID'Value (Dialog.Combo.Get_Active_ID),
               Name    => Dialog.Targets.Get
                          (  Integer (Dialog.Combo.Get_Active + 1)
                          ) .Ptr.Room
            );
         end if;
      exception
         when Error : others =>
            Log
            (  MAX_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("On_Response")
            )  );
      end;
      GLib.Object.Checked_Destroy (Self);
   end On_Response;

   procedure Move_Selected
             (  Rooms    : Rooms_List;
                Selected : Gtk_Tree_Iter := Null_Iter
             )  is
      Dialog  : Move_Dialog;
      Row     : Gtk_Tree_Iter := Selected;
      Room    : Gtk_Tree_Iter;
      Kind_Of : Device_Type;
      Box     : RF_Address;
   begin
      if Selected = Null_Iter then
         Row := Rooms.Get_Selected;
         if Row = Null_Iter then
            Rooms.Move.Hide;
            return;
         end if;
      end if;
      Box := Rooms.Get_Cube (Row);
      if Box = 0 then
         if Selected = Null_Iter then
            Rooms.Move.Hide;
         end if;
         return;
      end if;
      Kind_Of := Rooms.Get (Row);
      case Kind_Of is
         when Cube | Eco_Button | Unknown =>
            Rooms.Move.Hide;
            return;
         when Radiator_Thermostat..Shutter_Contact =>
            Room := Rooms.List.Parent (Row);
            declare
               Label   : Gtk_Label;
               ID      : constant Room_ID := Rooms.Get (Room);
               Name    : constant String  :=
                                  Rooms.Get (Room, Name_Column);
               Targets : Room_To_Device_List.Map :=
                         Get_Rooms_List (Box, (others => False));
            begin
               Targets.Remove (ID);
               if Targets.Get_Size = 0 then
                  if Selected = Null_Iter then
                     Rooms.Move.Hide;
                  end if;
                  return;
               end if;
               Dialog := new Move_Dialog_Record;
               Initialize
               (  Dialog => Dialog,
                  Title  => "Move device",
                  Parent => Window,
                  Flags  => Modal or Destroy_With_Parent
               );
               Dialog.List    := Rooms;
               Dialog.Cube    := Box;
               Dialog.Kind_Of := Kind_Of;
               Dialog.Address := Rooms.Get (Row);
               Gtk_New (Label);
               Label.Set_Markup
               (  "<big><b> Move "
               &  Image (Kind_Of)
               &  " "
               &  Image (Dialog.Address)
               &  " </b></big>"
               );
               Dialog.Get_Content_Area.Set_Spacing (3);
               Dialog.Get_Content_Area.Pack_Start
               (  Label,
                  False,
                  False,
                  10
               );
               Gtk_New (Dialog.Combo);
               Dialog.Get_Content_Area.Pack_Start (Dialog.Combo);
               for Index in 1..Targets.Get_Size loop
                  Dialog.Combo.Insert
                  (  GInt (Index - 1),
                     Image (Integer (Targets.Get_Key (Index))),
                     Targets.Get (Index).Ptr.Room
                  );
               end loop;
               Dialog.Targets := Targets;
               Dialog.Combo.Set_Active (0);
               Dialog.Combo.Set_Tooltip_Text
               (  "Select a room to move the device into"
               );
               Dialog.Realize;
               Add_Button_From_Stock
               (  Dialog   => Dialog,
                  Response => Gtk_Response_OK,
                  Icon     => Stock_OK,
                  Label    => "_OK",
                  Tip      => "Move"
               ) .Set_Can_Default (True);
               Add_Button_From_Stock
               (  Dialog   => Dialog,
                  Response => Gtk_Response_Cancel,
                  Icon     => Stock_Cancel,
                  Label    => "_Cancel",
                  Tip      => "Cancel moving"
               );
               Dialog.Set_Default_Response (Gtk_Response_OK);
               Dialog.Set_Modal (True);
               Dialog.On_Response (On_Response'Access);
               Dialog.Show_All;
            end;
      end case;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Move_Selected")
         )  );
   end Move_Selected;

end MAX_Rooms_List.Moving;
