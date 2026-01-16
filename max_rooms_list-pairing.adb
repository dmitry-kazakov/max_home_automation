--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List.Pairing                      Luebeck            --
--  Implementation                                 Spring, 2019       --
--                                                                    --
--                                Last revision :  14:21 11 May 2019  --
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
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with GLib;                   use GLib;
with GLib.Messages;          use GLib.Messages;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Image;              use Gtk.Image;
with Gtk.Missed;             use Gtk.Missed;
with Gtk.Stock;              use Gtk.Stock;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Widget;             use Gtk.Widget;
with MAX_Icon_Factory;       use MAX_Icon_Factory;
with MAX_IO.Pairing;         use MAX_IO.Pairing;
with MAX_Trace;              use MAX_Trace;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with GLib.Object.Checked_Destroy;
with Image_Eco_Switch_XPM.Image;
with Image_Radiator_Thermostat_XPM.Image;
with Image_Shutter_Contact_XPM.Image;
with Image_Wall_Thermostat_XPM.Image;

package body MAX_Rooms_List.Pairing is

   Pairing_Height : constant := 4;

   function Where (Name : String) return String is
   begin
      return " in MAX_Rooms_List.Pairing." & Name;
   end Where;

   procedure Canceled (Pairing : in out Pairing_Dialog_Record) is
   begin
      null;
   end Canceled;

   procedure Finished (Pairing : in out Pairing_Dialog_Record) is
   begin
      null;
   end Finished;

   function Get_Address
            (  Pairing : Pairing_Dialog_Record;
               Device  : Positive
            )  return RF_Address is
   begin
      return Value
             (  Gtk_Label_Record'Class
                (  Pairing.Grid.Get_Child_At
                   (  1,
                      GInt (Device - 1) * Pairing_Height + 1
                   ) .all
                ) .Get_Text
             );
   end Get_Address;

   function Get_Kind_Of
            (  Pairing : Pairing_Dialog_Record;
               Device  : Positive
            )  return Device_Type is
      Kind_Of : constant String :=
                         Gtk_Label_Record'Class
                         (  Pairing.Grid.Get_Child_At
                            (  1,
                               GInt (Device - 1) * Pairing_Height
                            ) .all
                         ) .Get_Text;
   begin
      case Kind_Of (Kind_Of'First) is
         when 'c' =>
            return Cube;
         when 'r' =>
            if Kind_Of = "radiator thermostat" then
               return Radiator_Thermostat;
            else
               return Radiator_Thermostat_Plus;
            end if;
         when 'w' =>
            return Wall_Thermostat;
         when 's' =>
            return Shutter_Contact;
         when 'e' =>
            return Eco_Button;
         when others =>
            return Unknown;
      end case;
   end Get_Kind_Of;

   function Get_Name
            (  Pairing : Pairing_Dialog_Record;
               Device  : Positive
            )  return String is
   begin
      return Gtk_Entry_Record'Class
             (  Pairing.Grid.Get_Child_At
                (  1,
                   GInt (Device - 1) * Pairing_Height + 3
                ) .all
             ) .Get_Text;
   end Get_Name;

   function Get_Object (Pairing : not null access Pairing_Dialog_Record)
      return GObject is
   begin
      return Pairing.all'Unchecked_Access;
   end Get_Object;

   function Get_Room (Pairing : Pairing_Dialog_Record) return Room_ID is
   begin
      if Pairing.Combo = null then
         return No_Room;
      elsif Pairing.Combo.Get_Active < 0 then
         return No_Room;
      else
         return Room_ID (Integer'(Value (Pairing.Combo.Get_Active_ID)));
      end if;
   exception
      when others =>
         return No_Room;
   end Get_Room;

   function Get_Room (Pairing : Pairing_Dialog_Record) return String is
   begin
      if Pairing.Combo = null then
         return "";
      else
         return Gtk_Entry_Record'Class
                (  Pairing.Combo.Get_Child.all
                ) .Get_Text;
      end if;
   end Get_Room;

   function Get_Serial_No
            (  Pairing : Pairing_Dialog_Record;
               Device  : Positive
            )  return String is
   begin
      return Gtk_Label_Record'Class
             (  Pairing.Grid.Get_Child_At
                (  1,
                   GInt (Device - 1) * Pairing_Height + 2
                ) .all
             ) .Get_Text;
   end Get_Serial_No;

   procedure On_A_Response
             (  Pairing  : in out Pairing_Dialog_Record;
                Address  : RF_Address;
                Devices  : Devices_Maps.Map;
                List     : Rooms_Maps.Map;
                Expected : in out Natural
             )  is
   begin
      if Expected > 0 then
         Expected := Expected - 1;
      end if;
      if Expected = 0 then
         Pairing.On_Response (1);
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_A_Response")
         )  );
   end On_A_Response;

   procedure On_Response
             (  Pairing   : in out Pairing_Dialog_Record;
                Completed : Natural
             )  is
   begin
      Pairing.Pending := Integer'Max (0, Pairing.Pending - Completed);
      if Pairing.Pending = 0 then
         case Pairing.Response is
            when Gtk_Response_Apply | Gtk_Response_Yes =>
               null;
            when others =>
               GLib.Object.Checked_Destroy (Pairing'Unchecked_Access);
         end case;
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
   end On_Response;

   procedure On_Destroy
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
   begin
      if List.Pairing /= null then
         declare
            Pairing : Pairing_Dialog_Record'Class renames
                      Pairing_Dialog_Record'Class (List.Pairing.all);
         begin
            Pairing.List.Pairing := null;
            if (  Pairing.Handler /= null
               and then
                  not Replace_Handler
                      (  Pairing.No,
                         Pairing.Handler.all'Unchecked_Access
               )      )  then
               Pairing.Handler.Completed (Pairing.Successful);
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Destroy")
         )  );
   end On_Destroy;

   procedure On_Response_Pairing
             (  Dialog   : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             )  is
   begin
      declare
         Room    : Room_Data_Handles.Handle;
         Pairing : Pairing_Dialog_Record'Class renames
                   Pairing_Dialog_Record'Class (Dialog.all);
         Count   : constant Natural :=
                            Pairing.Grid_Height / Pairing_Height;
      begin
         Pairing.Response := Response;
         case Response is
            when Gtk_Response_Delete_Event | Gtk_Response_Close =>
               if Count > 0 then
                  Stop_Pairing
                  (  Box       => Pairing.Cube,
                     Handler   => Pairing'Unchecked_Access
                  );
                  declare
                     List : RF_Address_Array (1..Count);
                  begin
                     for Index in List'Range loop
                        List (Index) := Pairing.Get_Address (Index);
                     end loop;
                     Pairing.Pending := Pairing.Pending + 1;
                     Delete_Paired_Device
                     (  Box       => Pairing.Cube,
                        Addresses => List,
                        Cancel    => True,
                        Handler   => Pairing'Unchecked_Access
                     );
                  end;
               end if;
            when Gtk_Response_OK => -- Accept device
               Pairing.Default.Hide;
               Pairing.Ignore.Hide;
               Pairing.Retry.Hide;
               Pairing.Action := Add_Paired_Device;
               Pairing.Successful := True;
               for Index in 1..Count loop
                  declare
                     Found : constant Device_Type :=
                                      Pairing.Get_Kind_Of (Index);
                  begin
                     if Found /= Eco_Button and then Index = 1 then
                        Room := Create
                                (  Pairing.Get_Room,
                                   Pairing.Get_Room
                                );
                     end if;
                     Pairing.Pending := Pairing.Pending + 1;
                     Add_Paired_Device
                     (  Box       => Pairing.Cube,
                        Kind_Of   => Found,
                        Serial_No => Pairing.Get_Serial_No (Index),
                        Address   => Pairing.Get_Address   (Index),
                        Name      => Pairing.Get_Name      (Index),
                        Room      => Room,
                        Handler   => Pairing'Unchecked_Access
                     );
                  end;
               end loop;
            when Gtk_Response_Apply => -- Retry
               Pairing.Title.Set_Markup
               (  "<big><b>" & Pairing.Head & "</b></big>"
               );
               Pairing.Progress.Show;
               Pairing.Progress.Set_Fraction (0.0);
               Pairing.Cancel.Show;
               Pairing.Stop.Show;
               Pairing.Default.Hide;
               Pairing.Ignore.Hide;
               Pairing.Retry.Hide;
               Pairing.Interval := Manual_Pairing_Time;
               Pairing.Pending  := Pairing.Pending + 1;
               Pair
               (  Box     => Pairing.Cube,
                  Time    => Pairing.Interval,
                  Handler => Pairing'Unchecked_Access
               );
               Pairing.Started := Clock;
            when Gtk_Response_Yes => -- Stop pairing
               Stop_Pairing
               (  Box       => Pairing.Cube,
                  Handler   => Pairing'Unchecked_Access
               );
               Pairing.Interval := Manual_Pairing_Time;
            when Gtk_Response_Cancel => -- Delete device
               if Count > 0 then
                  Pairing.Action := Delete_Paired_Device;
                  declare
                     List : RF_Address_Array (1..Count);
                  begin
                     for Index in List'Range loop
                        List (Index) := Pairing.Get_Address (Index);
                     end loop;
                     Pairing.Pending := Pairing.Pending + 1;
                     Delete_Paired_Device
                     (  Box       => Pairing.Cube,
                        Addresses => List,
                        Cancel    => False,
                        Handler   => Pairing'Unchecked_Access
                     );
                  end;
               end if;
            when others =>
               null;
         end case;
         Pairing.On_Response (0);
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Response_Pairing")
         )  );
   end On_Response_Pairing;

   procedure On_S_Response
             (  Pairing  : in out Pairing_Dialog_Record;
                Cube     : access Cube_Client'Class;
                Error    : Boolean;
                Duty     : Ratio;
                Slots    : Natural;
                Expected : in out Natural
             )  is
      Device : RF_Address;
   begin
      begin
         Pairing.List.Get_Logger.Update_Status
         (  Cube.Get_RF_Address,
            Error,
            Duty,
            Slots
         );
      exception
         when Status_Error =>
            null;
      end;
      case Pairing.Action is
         when Add_Paired_Device =>
            Device := Pairing.Get_Address (1);
            if Error then
               if Expected > 0 then
                  Expected := Expected - 1;
               end if;
               Pairing.Successful := False;
               if Pairing.To_Report then
                  Pairing.To_Report := False;
                  Say
                  (  (  "Error reported when adding paired device "
                     &  Image (Device)
                     &  Note (Duty)
                     ),
                     "Pairing error"
                  );
               end if;
            elsif Expected > 0 then
               Expected := Expected - 1;
            else
               declare
                  No_IO      : IO_Blocker;
                  Parent     : Gtk_Tree_Iter;
                  Parameters : constant Device_Parameters :=
                               Cube.Get_Device_Parameters (Device);
               begin
                  Parent := Pairing.List.Find (Cube.Get_RF_Address);
                  if Parent = Null_Iter then
                     Pairing.Successful := False;
                     Say
                     (  (  "No cube found when adding paired device "
                        &  Image (Device)
                        &  Note (Duty)
                        ),
                        "Pairing error"
                     );
                  end if;
               end;
            end if;
         when Delete_Paired_Device =>
            if Error then
               if Expected > 0 then
                  Expected := Expected - 1;
               end if;
               Pairing.Successful := False;
               if Pairing.To_Report then
                  Pairing.To_Report := False;
                  Say
                  (  "Failure deleting the paired device" & Note (Duty),
                     "Pairing error"
                  );
               end if;
            elsif Expected > 0 then
               Expected := Expected - 1;
            end if;
      end case;
      if Expected = 0 then
         Pairing.On_Response (1);
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

   procedure Pair_Device
             (  Rooms     : not null Rooms_List;
                Head      : String;
                Cube      : RF_Address;
                Room      : Room_ID            := No_Room;
                Kind_Of   : Device_Type        := Unknown;
                Device    : RF_Address         := 0;
                Serial_No : String             := "?         ";
                Interval  : Duration           := Manual_Pairing_Time;
                Handler   : Pairing_Issuer_Ptr := null
             )  is
      Pairing  : Pairing_Dialog;
      Search   : constant Boolean := Kind_Of = Unknown;
   begin
      if Rooms.Pairing /= null then
         return;
      end if;
      Pairing := new Pairing_Dialog_Record (Head'Length);
      Initialize
      (  Dialog => Pairing,
         Title  => "Pair with a new device",
         Parent => Window,
         Flags  => Modal or Destroy_With_Parent
      );
      Pairing.List     := Rooms.all'Unchecked_Access;
      Pairing.Head     := Head;
      Pairing.Cube     := Cube;
      Pairing.Room     := Room;
      Pairing.Handler  := Handler;
      Pairing.Interval := Interval;
      Pairing.Realize;

      Pairing.Get_Content_Area.Set_Spacing (3);
      Gtk_New (Pairing.Title);
      Pairing.Get_Content_Area.Pack_Start (Pairing.Title, False, False);
      Gtk_New (Pairing.Progress);
      Pairing.Get_Content_Area.Pack_End (Pairing.Progress, False, False);

      Pairing.Default :=
         Add_Button_From_Stock
         (  Dialog   => Pairing,
            Response => Gtk_Response_OK,
            Icon     => Stock_OK,
            Label    => "_OK",
            Tip      => "Accept this device"
         );
      Pairing.Default.Set_Can_Default (True);
      Pairing.Stop :=
         Add_Button_From_Stock
         (  Dialog   => Pairing,
            Response => Gtk_Response_Yes,
            Icon     => Stock_Media_Next,
            Label    => "_Stop",
            Tip      => "End pairing mode"
         );
      Pairing.Retry :=
         Add_Button_From_Stock
         (  Dialog   => Pairing,
            Response => Gtk_Response_Apply,
            Icon     => Stock_Refresh,
            Label    => "_Retry",
            Tip      => "Try again"
         );
      Pairing.Ignore :=
         Add_Button_From_Stock
         (  Dialog   => Pairing,
            Response => Gtk_Response_Cancel,
            Icon     => Stock_Cancel,
            Label    => "_Cancel",
            Tip      => "Delete newly paired device(s)"
         );
      Pairing.Cancel :=
         Add_Button_From_Stock
         (  Dialog   => Pairing,
            Response => Gtk_Response_Close,
            Icon     => Stock_Cancel,
            Label    => "_Cancel",
            Tip      => "Cancel pairing"
         );
      Pairing.Set_Default_Response (Gtk_Response_OK);
      Pairing.Show_All;

      Pairing.Default.Hide;
      Pairing.Ignore.Hide;
      Pairing.Retry.Hide;
      Rooms.Pairing := Pairing.all'Unchecked_Access;
      Pairing.On_Response (On_Response_Pairing'Access);
      List_Handlers.Connect
      (  Pairing,
         "destroy",
         On_Destroy'Access,
         Rooms
      );
      if Search then
         Pairing.Title.Set_Markup ("<big><b>" & Head & "</b></big>");
         Pairing.Progress.Show;
         Pairing.Progress.Set_Fraction (0.0);
         Pairing.Cancel.Show;
         Pairing.Stop.Show;
         Pairing.Default.Hide;
         Pairing.Ignore.Hide;
         Pairing.Retry.Hide;
         Pair
         (  Box     => Pairing.Cube,
            Time    => Interval,
            Handler => Pairing.all'Unchecked_Access
         );
         Pairing.Started := Clock;
            -- Now wait
      else
         Pairing.List.Paired_Device
         (  Cube      => Cube,
            Device    => Kind_Of,
            Address   => Device,
            Serial_No => Serial_No
         );
         Pairing.List.Pairing_Ended;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Pair_Device")
         )  );
         Pairing.List := null;
   end Pair_Device;

   procedure Paired_Device
             (  Pairing   : in out Pairing_Dialog_Record;
                Box       : RF_Address;
                Device    : Device_Type;
                Address   : RF_Address;
                Serial_No : String
             )  is
      function Get_Name return String is
      begin
         if Pairing.Handler /= null then
            declare
               Name : constant String :=
                               Pairing.Handler.Get_Name (Address);
            begin
               if Name'Length > 0 then
                  return Name;
               end if;
            end;
         end if;
         return "New " & Image (Device);
      end Get_Name;

      function Get_Room (Default_To_Empty : Boolean) return String is
      begin
         if Pairing.Handler /= null then
            declare
               Name : constant String :=
                               Pairing.Handler.Get_Room (Address);
            begin
               if Name'Length > 0 then
                  return Name;
               end if;
            end;
         end if;
         if Default_To_Empty then
            return "";
         else
            return "New room";
         end if;
      end Get_Room;

      Label       : Gtk_Label;
      Picture     : Gtk_Image;
      Device_Name : Gtk_GEntry;
   begin
      if Device not in Radiator_Thermostat..Eco_Button then
         return;
      end if;
      case Device is
         when Radiator_Thermostat..Radiator_Thermostat_Plus =>
            Picture := Image_Radiator_Thermostat_XPM.Image;
         when Wall_Thermostat =>
            Picture := Image_Wall_Thermostat_XPM.Image;
         when Shutter_Contact =>
            Picture := Image_Shutter_Contact_XPM.Image;
         when others =>
            Picture := Image_Eco_Switch_XPM.Image;
      end case;

      if Pairing.Grid = null then
         Pairing.Grid_Height := 0;
         Gtk_New (Pairing.Grid);
         Pairing.Grid.Set_Row_Homogeneous (False);
         Pairing.Grid.Set_Column_Homogeneous (False);
         Pairing.Grid.Set_Border_Width (5);
         Pairing.Grid.Set_Row_Spacing (3);
         Pairing.Grid.Set_Column_Spacing (5);
         Gtk_New (Pairing.Scroll);
         Pairing.Scroll.Add (Pairing.Grid);
         Pairing.Scroll.Set_Policy (Policy_Never, Policy_Never);
         Pairing.Get_Content_Area.Pack_Start
         (  Child  => Pairing.Scroll,
            Expand => False
         );
         Pairing.Scroll.Show_All;
      end if;

      Pairing.Title.Set_Markup ("<big><b>A new device found</b></big>");

      for Index in 1..Pairing_Height loop
         Pairing.Grid.Insert_Row (GInt (Pairing.Grid_Height));
      end loop;

      -- Column 1 ------------------------------------------------------
      Gtk_New (Label, "Type");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Pairing.Grid.Attach (Label, 0, GInt (Pairing.Grid_Height + 0));

      Gtk_New (Label, "Address");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Pairing.Grid.Attach (Label, 0, GInt (Pairing.Grid_Height + 1));

      Gtk_New (Label, "Serial No.");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Pairing.Grid.Attach (Label, 0, GInt (Pairing.Grid_Height + 2));

      Gtk_New (Label, "Name");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Pairing.Grid.Attach (Label, 0, GInt (Pairing.Grid_Height + 3));

      if Device /= Eco_Button and then Pairing.Combo = null then
         Gtk_New (Label, "Room");
         Label.Set_Halign (Align_End);
         Label.Set_Valign (Align_Center);
         Pairing.Grid.Attach (Label, 0, GInt (Pairing.Grid_Height + 4));
      end if;

      -- Column 2 ------------------------------------------------------
      Gtk_New (Label, Image (Device));
      Label.Set_Halign (Align_Start);
      Label.Set_Valign (Align_Center);
      Pairing.Grid.Attach (Label, 1, GInt (Pairing.Grid_Height + 0));

      Gtk_New (Label, Image (Address));
      Label.Set_Halign (Align_Start);
      Label.Set_Valign (Align_Center);
      Pairing.Grid.Attach (Label, 1, GInt (Pairing.Grid_Height + 1));

      Gtk_New (Label, Serial_No);
      Label.Set_Halign (Align_Start);
      Label.Set_Valign (Align_Center);
      Pairing.Grid.Attach (Label, 1, GInt (Pairing.Grid_Height + 2));

      Gtk_New (Device_Name);
      Device_Name.Set_Tooltip_Text
      (  "Enter the name of the new device"
      );
      Device_Name.Set_Max_Length (255);
      Device_Name.Set_Alignment (0.0);
      Device_Name.Set_Text (Get_Name);
      Pairing.Grid.Attach
      (  Device_Name,
         1,
         GInt (Pairing.Grid_Height + 3)
      );

      if Device /= Eco_Button and then Pairing.Combo = null then
         Gtk_New_With_Entry (Pairing.Combo);
         Pairing.Grid.Attach
         (  Pairing.Combo,
            1,
            GInt (Pairing.Grid_Height + 4)
         );
         Gtk_Entry_Record'Class
         (  Pairing.Combo.Get_Child.all
         ) .Set_Max_Length (255);
         declare
            Selected : Boolean := False;
            List     : constant Room_To_Device_List.Map :=
                                Get_Rooms_List (Box, (others => False));
         begin
            for Index in 1..List.Get_Size loop
               declare
                  Name : constant String := List.Get (Index).Ptr.Room;
               begin
                  Pairing.Combo.Insert
                  (  GInt (Index - 1),
                     Image (Integer (List.Get_Key (Index))),
                     Name
                  );
                  if Pairing.Room = List.Get_Key (Index) then
                     Pairing.Combo.Set_Active (GInt (Index - 1));
                     Selected := True;
                  elsif (  Pairing.Handler /= null
                        and then
                           Pairing.Handler.Get_Room (Address) = Name
                        )  then
                     Pairing.Combo.Set_Active (GInt (Index - 1));
                     Selected := True;
                  end if;
               end;
            end loop;
            if List.Get_Size > 0 then
               if not Selected then
                  declare
                     Name : constant String := Get_Room (True);
                  begin
                     if Name'Length = 0 then -- No room name, select
                        Pairing.Combo.Set_Active (0);
                     else
                        Gtk_Entry_Record'Class
                        (  Pairing.Combo.Get_Child.all
                        ) .Set_Text (Name);
                     end if;
                  end;
               end if;
               Pairing.Combo.Set_Tooltip_Text
               (  "Select a room to put the device in or else "
               &  "enter a name for a new room to create"
               );
            else
               Pairing.Combo.Set_Tooltip_Text
               (  "Enter the new room name"
               );
               Gtk_Entry_Record'Class
               (  Pairing.Combo.Get_Child.all
               ) .Set_Text (Get_Room (False));
            end if;
         end;
      end if;
      -- Column 3 ------------------------------------------------------
      Pairing.Grid.Attach
      (  Picture,
         2,
         GInt (Pairing.Grid_Height + 0),
         1,
         4
      );
      if Pairing.Grid_Height > 0 then
         Pairing.Scroll.Set_Policy (Policy_Never, Policy_Never);
      end if;
      Pairing.Grid_Height := Pairing.Grid_Height + Pairing_Height;
      if Pairing.Grid_Height > Pairing_Height then
         Pairing.Title.Set_Markup
         (  "<big><b>New devices found</b></big>"
         );
      end if;
      Pairing.Grid.Show_All;
   end Paired_Device;

   procedure Pairing_Ended (Pairing : in out Pairing_Dialog_Record) is
   begin
      Pairing.Progress.Hide;
      Pairing.Cancel.Hide;
      Pairing.Stop.Hide;
      Pairing.Retry.Show;
      if Pairing.Grid_Height > 0 then
         if Pairing.Grid_Height > Pairing_Height then
            Pairing.Title.Set_Markup
            (  "<big><b>New devices found</b></big>"
            );
         else
            Pairing.Title.Set_Markup
            (  "<big><b>A new device found</b></big>"
            );
         end if;
         Pairing.Default.Show;
      else
         Pairing.Title.Set_Markup
         (  "<big><b>No new devices found</b></big>"
         );
      end if;
      Pairing.Ignore.Show;
   end Pairing_Ended;

   procedure Ping_Progress (Pairing : in out Pairing_Dialog_Record) is
   begin
      Pairing.Progress.Set_Fraction
      (  Gdouble'Min
         (  1.0,
            Gdouble'Max
            (  0.0,
               (  Gdouble (Clock - Pairing.Started)
               /  Gdouble (Pairing.Interval)
      )  )  )  );
   end Ping_Progress;

   procedure Registered
             (  Handler : in out Pairing_Dialog_Record;
                No      : Settings_Sequence_No
             )  is
   begin
      Handler.No := No;
   end Registered;

end MAX_Rooms_List.Pairing;
