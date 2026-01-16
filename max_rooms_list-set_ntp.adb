--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.Set_NTP                      Luebeck            --
--  Implementation                                 Spring, 2019       --
--                                                                    --
--                                Last revision :  13:38 14 Sep 2019  --
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

with Ada.Exceptions;  use Ada.Exceptions;
with GLib.Messages;   use GLib.Messages;
with Gtk.Missed;      use Gtk.Missed;
with Gtk.Separator;   use Gtk.Separator;
with Gtk.Stock;       use Gtk.Stock;
with MAX_User;        use MAX_User;
with Strings_Edit;    use Strings_Edit;

with GLib.Object.Checked_Destroy;

package body MAX_Rooms_List.Set_NTP is

   function Where (Name : String) return String is
   begin
      return " in MAX_Rooms_List.Set_NTP." & Name;
   end Where;

   procedure On_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             )  is
   begin
      declare
         Dialog : NTP_Dialog_Record'Class renames
                  NTP_Dialog_Record'Class (Self.all);
      begin
         if Response = Gtk_Response_OK then
            declare
               Handle : constant Cube_Client_Handle :=
                                 Get_Cube (Dialog.Cube);
            begin
               if Handle.Is_Valid then
                  Handle.Ptr.Set_NTP_Servers (Dialog.Servers.Get_Text);
               end if;
            exception
               when Error : others =>
                  Say
                  (  "Failed to set NTP: "
                  &  Exception_Message (Error)
                  );
                  return;
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

   procedure Set_Selected_NTP (Rooms : Rooms_List) is
      function Image (List : Servers_List.Set) return String is
         Length : Natural := 0;
      begin
         for Index in 1..List.Get_Size loop
            if Index > 1 then
               Length := Length + 2;
            end if;
            Length := Length + List.Get (Index)'Length;
         end loop;
         declare
            Text    : String (1..Length);
            Pointer : Integer := 1;
         begin
            for Index in 1..List.Get_Size loop
               if Index > 1 then
                  Put (Text, Pointer, ", ");
               end if;
               Put (Text, Pointer, List.Get (Index));
            end loop;
            return Text;
         end;
      end Image;

      Cube : RF_Address;
   begin
      Cube := Rooms.Get_Cube (Rooms.Get_Selected);
      if Cube = 0 then
         Rooms.Set_NTP.Hide;
         return;
      end if;
      declare
         Handle : constant Cube_Client_Handle := Get_Cube (Cube);
      begin
         if not Handle.Is_Valid then
            Rooms.Set_NTP.Hide;
            return;
         end if;
         declare
            Client    : Cube_Client'Class renames Handle.Ptr.all;
            Dialog    : NTP_Dialog;
            Separator : Gtk_Separator;
            Label     : Gtk_Label;
         begin
            Dialog := new NTP_Dialog_Record;
            Dialog.List := Rooms;
            Dialog.Cube := Cube;
            Initialize
            (  Dialog,
               Title  => "NTP settings",
               Parent => Window,
               Flags  => Modal or Destroy_With_Parent
            );
            Gtk_New (Label);
            Dialog.Get_Content_Area.Set_Spacing (3);
            Label.Set_Markup
            (  "<big><b> Set NTP servers for the cube "
            &  Image (Cube)
            &  " </b></big>"
            );
            Label.Set_Line_Wrap (True);
            Dialog.Get_Content_Area.Pack_Start (Label, False, False, 10);
            Gtk_New_Hseparator (Separator);
            Dialog.Get_Content_Area.Pack_Start (Separator, False, False);
            Gtk_New (Dialog.Servers);
            Dialog.Get_Content_Area.Pack_Start
            (  Dialog.Servers,
               False,
               False
            );
            Dialog.Servers.Set_Tooltip_Text
            (  "Specify a comma-separated list of names or "
            &  "IP addresses of the NTP servers to use"
            );
            declare
               Index : constant Integer :=
                                Rooms.NTP_Servers.Find (Cube);
            begin
               if Index >= 0 then
                  Dialog.Servers.Set_Text
                  (  Image (Rooms.NTP_Servers.Get (Index))
                  );
               end if;
            end;
            declare
               OS : constant Servers_List.Set := NTP_Servers;
            begin
               if not OS.Is_Empty then
                  Gtk_New (Label);
                  Label.Set_Markup
                  (  "<b>OS NTP servers:</b> "
                  &  Image (OS)
                  );
                  Label.Set_Line_Wrap (True);
                  Label.Set_Halign (Align_Start);
                  Label.Set_Selectable (True);
                  Dialog.Get_Content_Area.Pack_Start (Label, False, False);
               end if;
            end;
            Gtk_New
            (  Label,
               "Cube time skew: " & Image (Client.Get_Clock_Difference)
            );
            Label.Set_Halign (Align_Start);
            Dialog.Get_Content_Area.Pack_Start (Label, False, False);
            Dialog.Realize;
            Add_Button_From_Stock
            (  Dialog   => Dialog,
               Response => Gtk_Response_OK,
               Icon     => Stock_OK,
               Label    => "_OK",
               Tip      => "Set NTP servers"
            ) .Set_Can_Default (True);
            Add_Button_From_Stock
            (  Dialog   => Dialog,
               Response => Gtk_Response_Cancel,
               Icon     => Stock_Cancel,
               Label    => "_Cancel",
               Tip      => "Cancel"
            );
            Dialog.Set_Default_Response (Gtk_Response_OK);
            Dialog.Set_Modal (True);
            Dialog.On_Response (On_Response'Access);
            Dialog.Show_All;
         end;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Set_NTP")
         )  );
   end Set_Selected_NTP;

end MAX_Rooms_List.Set_NTP;
