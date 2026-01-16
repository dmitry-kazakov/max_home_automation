--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.Renaming                     Luebeck            --
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

with Ada.Exceptions;         use Ada.Exceptions;
with GLib.Messages;          use GLib.Messages;
with Gtk.Missed;             use Gtk.Missed;
with Gtk.Stock;              use Gtk.Stock;
with MAX_IO.Renaming;        use MAX_IO.Renaming;
with MAX_Schedule;           use MAX_Schedule;
with MAX_Settings_Page;      use MAX_Settings_Page;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with GLib.Object.Checked_Destroy;

package body MAX_Rooms_List.Renaming is

   function Where (Name : String) return String is
   begin
      return " in MAX_Rooms_List.Renaming." & Name;
   end Where;

   procedure On_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             )  is
   begin
      declare
         Dialog : Rename_Dialog_Record'Class renames
                  Rename_Dialog_Record'Class (Self.all);
      begin
         if Response = Gtk_Response_OK then
            declare
               Name : constant String := Dialog.Edit.Get_Text;
            begin
               if Dialog.Room = No_Room then
                  Rename (Dialog.Cube, Dialog.Address, Name);
                  Rename (Pages, Dialog.Cube, Dialog.Address, Name);
                  if Dialog.List.History /= null then
                     Dialog.List.History.Rename
                     (  Dialog.Cube,
                        Dialog.Address,
                        Name
                     );
                  end if;
               else
                  Rename (Dialog.Cube, Dialog.Room, Name);
                  if Dialog.List.History /= null then
                     Dialog.List.History.Rename
                     (  Dialog.Cube,
                        Dialog.Room,
                        Name
                     );
                  end if;
               end if;
               Dialog.List.Set (Dialog.Row, Name_Column, Name);
            end;
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

   procedure Rename_Selected
             (  Rooms    : Rooms_List;
                Selected : Gtk_Tree_Iter := Null_Iter
             )  is
      Dialog  : Rename_Dialog;
      Label   : Gtk_Label;
      Box     : RF_Address;
      Kind_Of : Device_Type;
      Row     : Gtk_Tree_Iter := Null_Iter;
   begin
      if Selected = Null_Iter then
         Row := Rooms.Get_Selected;
         if Row = Null_Iter then
            Rooms.Rename.Hide;
            return;
         end if;
         Box := Rooms.Get_Cube (Row);
         if Box = 0 then
            Rooms.Rename.Hide;
            return;
         end if;
      else
         Row := Selected;
         Box := Rooms.Get_Cube (Row);
         if Box = 0 then
            return;
         end if;
      end if;
      Kind_Of := Rooms.Get (Row);
      case Kind_Of is
         when Cube =>
            if Selected = Null_Iter then
               Rooms.Rename.Hide;
            end if;
            return;
         when Radiator_Thermostat..Eco_Button =>
            Dialog := new Rename_Dialog_Record;
            Initialize
            (  Dialog => Dialog,
               Title  => "Rename device",
               Parent => Window,
               Flags  => Modal or Destroy_With_Parent
            );
            Dialog.Realize;
            Dialog.Row     := Row;
            Dialog.List    := Rooms;
            Dialog.Address := Dialog.List.Get (Row);
            Gtk_New (Label);
            Label.Set_Markup
            (  "<big><b> Rename "
            &  Image (Kind_Of)
            &  " "
            &  Image (Dialog.Address)
            &  " </b></big>"
            );
         when Unknown =>
            Dialog := new Rename_Dialog_Record;
            Initialize
            (  Dialog => Dialog,
               Title  => "Rename room",
               Parent => Window,
               Flags  => Modal or Destroy_With_Parent
            );
            Dialog.Realize;
            Dialog.List := Rooms;
            Dialog.Row  := Row;
            Dialog.Room := Dialog.List.Get (Row);
            Gtk_New (Label);
            Label.Set_Markup
            (  "<big><b> Rename room "
            &  Image (Integer (Dialog.Room))
            &  " </b></big>"
            );
      end case;
      Dialog.Cube := Box;
      Dialog.Get_Content_Area.Set_Spacing (3);
      Dialog.Get_Content_Area.Pack_Start (Label, False, False, 10);
      Gtk_New (Dialog.Edit);
      Dialog.Edit.Set_Text (Dialog.List.Get (Row, Name_Column));
      Dialog.Edit.Set_Max_Length (255);
      Dialog.Edit.Set_Alignment (0.0);
      Dialog.Get_Content_Area.Pack_Start
      (  Dialog.Edit,
         False,
         False,
         10
      );
      Add_Button_From_Stock
      (  Dialog   => Dialog,
         Response => Gtk_Response_OK,
         Icon     => Stock_OK,
         Label    => "_OK",
         Tip      => "Rename"
      ) .Set_Can_Default (True);
      Add_Button_From_Stock
      (  Dialog   => Dialog,
         Response => Gtk_Response_Cancel,
         Icon     => Stock_Cancel,
         Label    => "_Cancel",
         Tip      => "Cancel renaming"
      );
      Dialog.Set_Default_Response (Gtk_Response_OK);
      Dialog.Set_Modal (True);
      Dialog.On_Response (On_Response'Access);
      Dialog.Show_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Rename_Selected")
         )  );
   end Rename_Selected;

end MAX_Rooms_List.Renaming;
