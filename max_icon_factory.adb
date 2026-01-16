--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Icon_Factory                            Luebeck            --
--  Implementation                                 Summer, 2015       --
--                                                                    --
--                                 Last revision : 12:32 10 Jun 2003  --
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

with Gdk.Pixbuf;        use Gdk.Pixbuf;
with Gtk.Icon_Factory;  use Gtk.Icon_Factory;
with Gtk.Icon_Set;      use Gtk.Icon_Set;
with Gtk.Icon_Source;   use Gtk.Icon_Source;
with Gtk.Stock;         use Gtk.Stock;

with Image_Add_Device_XPM;
with Image_Automatic_XPM;
with Image_Battery_High_XPM;
with Image_Battery_Low_XPM;
with Image_Boost_XPM;
with Image_Cancel_XPM;
with Image_Calendar_XPM;
with Image_Copy_From_XPM;
with Image_Cube_XPM;
with Image_Cube_Crossed_XPM;
with Image_Disconnect_XPM;
with Image_Eco_Button_XPM;
with Image_EMail_XPM;
with Image_Error_Link_XPM;
with Image_Error_No_Link_XPM;
with Image_Link_XPM;
with Image_Manual_XPM;
with Image_MAX_MAX_XPM;
with Image_Move_XPM;
with Image_Network_Scan_XPM;
with Image_No_Link_XPM;
with Image_NTP_XPM;
with Image_Operating_Mode_XPM;
with Image_Radiator_XPM;
with Image_Reconnect_XPM;
with Image_Rename_XPM;
with Image_Room_XPM;
with Image_Set_Thermostat_XPM;
with Image_Thermometer_XPM;
with Image_Vacation_XPM;
with Image_Wake_Up_XPM;
with Image_Window_XPM;
with Image_Window_Closed_XPM;

package body Max_Icon_Factory is

   Icons      : Gtk.Icon_Factory.Gtk_Icon_Factory;
   Panel_Size : Gtk_Icon_Size := 0;

   type Pixbuf_Array is array (Positive range <>) of Gdk_Pixbuf;

   procedure Add_Stock
             (  Pictures : Pixbuf_Array;
                Name     : String;
                Label    : String
             )  is
      Set    : Gtk_Icon_Set;
      Source : Gtk_Icon_Source;
   begin
      Gtk_New (Set);
      for Index in Pictures'Range loop
         case Get_Width (Pictures (Index)) is
             when 16 =>
                Gtk_New (Source);
                Source.Set_Pixbuf (Pictures (Index));
                Source.Set_Size (Icon_Size_Menu);
                Source.Set_Size_Wildcarded (False);
                Set.Add_Source (Source);
                Free (Source);
             when 18 =>
                Gtk_New (Source);
                Source.Set_Pixbuf (Pictures (Index));
                Source.Set_Size (Icon_Size_Small_Toolbar);
                Source.Set_Size_Wildcarded (False);
                Set.Add_Source (Source);
                Free (Source);
             when 64 =>
                Gtk_New (Source);
                Source.Set_Pixbuf (Pictures (Index));
                Source.Set_Size (Icon_Size_Panel);
                Source.Set_Size_Wildcarded (False);
                Set.Add_Source (Source);
                Free (Source);
             when others =>
                null;
         end case;
      end loop;
      Gtk_New (Source);
      Set_Pixbuf (Source, Pictures (Pictures'First));
      Set.Add_Source (Source);
      Free (Source);
      for Index in Pictures'Range loop
         Unref (Pictures (Index));
      end loop;
      Add (Icons, Name, Set);
      Unref (Set);
      declare
         Item : Gtk_Stock_Item;
      begin
         Gtk_New
         (  Item     => Item,
            Stock_ID => Name,
            Label    => Label,
            Modifier => 0,
            KeyVal   => 0,
            Translation_Domain => "home-automation"
         );
         Add_Static ((1 => Item));
      end;
   end Add_Stock;

   function Escape_Name (Name : UTF8_String) return UTF8_String is
      Count : Natural := 0;
   begin
      for Index in Name'Range loop
         if Name (Index) = '_' then
            Count := Count + 1;
         end if;
      end loop;
      if Count = 0 then
         return Name;
      end if;
      declare
         Result : String (1..Name'Length + Count);
      begin
         Count := Result'First;
         for Index in Name'Range loop
            if Name (Index) = '_' then
               Result (Count) := '_';
               Count := Count + 1;
            end if;
            Result (Count) := Name (Index);
            Count := Count + 1;
         end loop;
         return Result;
      end;
   end Escape_Name;

   function Icon_Size_Panel return Gtk_Icon_Size is
   begin
      if Panel_Size = 0 then
         Panel_Size := Icon_Size_Register ("Panel", 64, 64);
      end if;
      return Panel_Size;
   end Icon_Size_Panel;

   procedure Init is
      Images : Pixbuf_Array (1..1);
   begin
      if Icons = null then
         Gtk_New (Icons);
         Add_Default (Icons);

         Images (1) := Image_Add_Device_XPM.Get_Pixbuf;
         Add_Stock (Images, Add_Device_Icon, "_Add_Device");
         Images (1).Unref;

         Images (1) := Image_Automatic_XPM.Get_Pixbuf;
         Add_Stock (Images, Automatic_Icon, "_Automatic");
         Images (1).Unref;

         Images (1) := Image_Battery_High_XPM.Get_Pixbuf;
         Add_Stock (Images, Battery_High_Icon, "_Battery_High");
         Images (1).Unref;

         Images (1) := Image_Battery_Low_XPM.Get_Pixbuf;
         Add_Stock (Images, Battery_Low_Icon, "_Battery_Low");
         Images (1).Unref;

         Images (1) := Image_Boost_XPM.Get_Pixbuf;
         Add_Stock (Images, Boost_Icon, "_Boost");
         Images (1).Unref;

         Images (1) := Image_Calendar_XPM.Get_Pixbuf;
         Add_Stock (Images, Calendar_Icon, "_Calendar");
         Images (1).Unref;

         Images (1) := Image_Cancel_XPM.Get_Pixbuf;
         Add_Stock (Images, Cancel_Icon, "_Cancel");
         Images (1).Unref;

         Images (1) := Image_Copy_From_XPM.Get_Pixbuf;
         Add_Stock (Images, Copy_From_Icon, "_Copy_From");
         Images (1).Unref;

         Images (1) := Image_Cube_XPM.Get_Pixbuf;
         Add_Stock (Images, Cube_Icon, "_Cube");
         Images (1).Unref;

         Images (1) := Image_Cube_Crossed_XPM.Get_Pixbuf;
         Add_Stock (Images, Cube_Crossed_Icon, "_Cube_Crossed");
         Images (1).Unref;

         Images (1) := Image_Disconnect_XPM.Get_Pixbuf;
         Add_Stock (Images, Disconnect_Icon, "_Disconnect");
         Images (1).Unref;

         Images (1) := Image_Eco_Button_XPM.Get_Pixbuf;
         Add_Stock (Images, Eco_Button_Icon, "_Eco_Button");
         Images (1).Unref;

         Images (1) := Image_EMail_XPM.Get_Pixbuf;
         Add_Stock (Images, EMail_Icon, "_EMail");
         Images (1).Unref;

         Images (1) := Image_Error_Link_XPM.Get_Pixbuf;
         Add_Stock (Images, Error_Link_Icon, "_Error_Link");
         Images (1).Unref;

         Images (1) := Image_Error_No_Link_XPM.Get_Pixbuf;
         Add_Stock (Images, Error_No_Link_Icon, "_Error_No_Link");
         Images (1).Unref;

         Images (1) := Image_Link_XPM.Get_Pixbuf;
         Add_Stock (Images, Link_Icon, "_Link");
         Images (1).Unref;

         Images (1) := Image_Manual_XPM.Get_Pixbuf;
         Add_Stock (Images, Manual_Icon, "_Manual");
         Images (1).Unref;

         Images (1) := Image_MAX_MAX_XPM.Get_Pixbuf;
         Add_Stock (Images, MAX_Icon, "_MAX");
         Images (1).Unref;

         Images (1) := Image_Move_XPM.Get_Pixbuf;
         Add_Stock (Images, Move_Icon, "_Move");
         Images (1).Unref;

         Images (1) := Image_Network_Scan_XPM.Get_Pixbuf;
         Add_Stock (Images, Network_Scan_Icon, "_Network_Scan");
         Images (1).Unref;

         Images (1) := Image_No_Link_XPM.Get_Pixbuf;
         Add_Stock (Images, No_Link_Icon, "_No_Link");
         Images (1).Unref;

         Images (1) := Image_NTP_XPM.Get_Pixbuf;
         Add_Stock (Images, NTP_Icon, "_NTP");
         Images (1).Unref;

         Images (1) := Image_Operating_Mode_XPM.Get_Pixbuf;
         Add_Stock (Images, Operating_Mode_Icon, "_Operating_Mode");
         Images (1).Unref;

         Images (1) := Image_Reconnect_XPM.Get_Pixbuf;
         Add_Stock (Images, Reconnect_Icon, "_Reconnect");
         Images (1).Unref;

         Images (1) := Image_Rename_XPM.Get_Pixbuf;
         Add_Stock (Images, Rename_Icon, "_Rename");
         Images (1).Unref;

         Images (1) := Image_Radiator_XPM.Get_Pixbuf;
         Add_Stock
         (  Images,
            Radiator_Thermostat_Icon,
            "Radiator_Thermostat"
         );
         Images (1).Unref;

         Images (1) := Image_Room_XPM.Get_Pixbuf;
         Add_Stock (Images, Room_Icon, "_Room");
         Images (1).Unref;

         Images (1) := Image_Set_Thermostat_XPM.Get_Pixbuf;
         Add_Stock (Images, Set_Thermostat_Icon, "_Set_Thermostat");
         Images (1).Unref;

         Images (1) := Image_Vacation_XPM.Get_Pixbuf;
         Add_Stock (Images, Vacation_Icon, "_Vacation");
         Images (1).Unref;

         Images (1) := Image_Wake_Up_XPM.Get_Pixbuf;
         Add_Stock (Images, Wake_Up_Icon, "_Wake_Up");
         Images (1).Unref;

         Images (1) := Image_Thermometer_XPM.Get_Pixbuf;
         Add_Stock (Images, Wall_Thermostat_Icon, "_Wall_Thermostat");
         Images (1).Unref;

         Images (1) := Image_Window_XPM.Get_Pixbuf;
         Add_Stock (Images, Window_Open_Icon, "_Window_Open");
         Images (1).Unref;

         Images (1) := Image_Window_Closed_XPM.Get_Pixbuf;
         Add_Stock (Images, Window_Closed_Icon, "_Window_Closed");
         Images (1).Unref;

      end if;
   end Init;

end Max_Icon_Factory;
