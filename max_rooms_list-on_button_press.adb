--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.On_Button_Press              Luebeck            --
--  Separate body                                  Autumn, 2020       --
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

with Image_Move_XPM.Image;
with Image_Wake_Up_XPM.Image;

separate (MAX_Rooms_List)
   function On_Button_Press
            (  Object : access GObject_Record'Class;
               Event  : Gdk_Event;
               List   : Rooms_List
            )  return Boolean is
   X, Y : GDouble;
begin
   if Get_Button (Event) = 3 then
      Get_Coords (Event, X, Y);
      declare
         Menu      : Gtk_Menu;
         Mode      : Device_Type;
         Path      : Gtk_Tree_Path;
         Column    : Gtk_Tree_View_Column;
         Cell_X    : GInt;
         Cell_Y    : GInt;
         Row_Found : Boolean;

         procedure Add_Configure (Name : String) is
            Item : Gtk_Image_Menu_Item;
            Icon : Gtk_Image;
         begin
             Gtk_New (Item, Name);
             Gtk_New (Icon, Stock_Execute, Icon_Size_Menu);
             Set_Image (Item, Icon);
             Item.Set_Always_Show_Image (True);
             Append (Menu, Item);
             List_Handlers.Connect
             (  Item,
                "activate",
                On_Configure_Menu'Access,
                List
             );
         end Add_Configure;

         procedure Add_Delete (Name : String) is
            Item : Gtk_Image_Menu_Item;
            Icon : Gtk_Image;
         begin
             Gtk_New (Item, Name);
             Gtk_New (Icon, Stock_Delete, Icon_Size_Menu);
             Set_Image (Item, Icon);
             Item.Set_Always_Show_Image (True);
             Append (Menu, Item);
             List_Handlers.Connect
             (  Item,
                "activate",
                On_Delete_Menu'Access,
                List
             );
         end Add_Delete;

         procedure Add_Device (Name : String) is
            Item : Gtk_Image_Menu_Item;
            Icon : Gtk_Image;
         begin
             Gtk_New (Item, Name);
             Gtk_New (Icon, Add_Device_Icon, Icon_Size_Menu);
             Set_Image (Item, Icon);
             Item.Set_Always_Show_Image (True);
             Append (Menu, Item);
             List_Handlers.Connect
             (  Item,
                "activate",
                On_Add_Device_Menu'Access,
                List
             );
         end Add_Device;

         procedure Add_Move (Name : String) is
            Item : Gtk_Image_Menu_Item;
         begin
             Gtk_New (Item, Name);
             Set_Image (Item, Image_Move_XPM.Image);
             Item.Set_Always_Show_Image (True);
             Append (Menu, Item);
             List_Handlers.Connect
             (  Item,
                "activate",
                On_Move_Menu'Access,
                List
             );
         end Add_Move;

         procedure Add_Rename (Name : String) is
            Item : Gtk_Image_Menu_Item;
            Icon : Gtk_Image;
         begin
             Gtk_New (Item, Name);
             Gtk_New (Icon, Rename_Icon, Icon_Size_Menu);
             Set_Image (Item, Icon);
             Item.Set_Always_Show_Image (True);
             Append (Menu, Item);
             List_Handlers.Connect
             (  Item,
                "activate",
                On_Rename_Menu'Access,
                List
             );
         end Add_Rename;

         procedure Add_Select_Thermostats (Name : String) is
            Item : Gtk_Image_Menu_Item;
         begin
             Gtk_New (Item, Name);
             Set_Image (Item, Image_Radiator_XPM.Image);
             Item.Set_Always_Show_Image (True);
             Append (Menu, Item);
             List_Handlers.Connect
             (  Item,
                "activate",
                On_Select_Thermostats'Access,
                List
             );
         end Add_Select_Thermostats;

         procedure Add_Wake_Up (Name : String) is
            Item : Gtk_Image_Menu_Item;
         begin
             Gtk_New (Item, Name);
             Set_Image (Item, Image_Wake_Up_XPM.Image);
             Item.Set_Always_Show_Image (True);
             Append (Menu, Item);
             List_Handlers.Connect
             (  Item,
                "activate",
                On_Wake_Up_Menu'Access,
                List
             );
         end Add_Wake_Up;
      begin
         List.View.Get_Path_At_Pos
         (  GInt (X),
            GInt (Y),
            Path,
            Column,
            Cell_X,
            Cell_Y,
            Row_Found
         );
         if Row_Found then
            List.Right_Click := List.List.Get_Iter (Path);
            Path_Free (Path);
            if List.Right_Click /= Null_Iter then
               Mode := List.Get (List.Right_Click);
               Gtk_New (Menu);
               case Mode is
                  when Eco_Button | Shutter_Contact =>
                     Add_Move ("Move to another room");
                     Add_Rename ("Rename");
                     Add_Delete ("Delete");
                     Add_Wake_Up ("Wake up");
                  when Cube =>
                     if List.Has_Thermostats (List.Right_Click) then
                        Add_Select_Thermostats ("Select thermostats");
                     end if;
                     Add_Device ("Add new device");
                  when Radiator_Thermostat      |
                       Radiator_Thermostat_Plus |
                       Wall_Thermostat          =>
                     Add_Configure ("Configure");
                     Add_Move ("Move to another room");
                     Add_Rename ("Rename");
                     Add_Delete ("Delete");
                     Add_Wake_Up ("Wake up");
                  when others =>
                     if List.Has_Thermostats (List.Right_Click) then
                        Add_Select_Thermostats ("Select thermostats");
                     end if;
                     Add_Rename ("Rename");
                     Add_Delete ("Delete");
               end case;
               Show_All (Menu);
               Popup
               (  Menu,
                  Button        => Get_Button (Event),
                  Activate_Time => Get_Time (Event)
               );
            end if;
         end if;
      end;
   end if;
   return False;
exception
   when Error : others =>
      Log
      (  MAX_Domain,
         Log_Level_Critical,
         (  "Fault: "
         &  Exception_Information (Error)
         &  Where ("On_Button_Press")
      )  );
      return False;
end On_Button_Press;
