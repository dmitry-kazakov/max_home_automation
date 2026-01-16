--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List                              Luebeck            --
--  Implementation                                 Summer, 2015       --
--                                                                    --
--                                Last revision :  09:11 20 Feb 2021  --
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

with Ada.Exceptions;              use Ada.Exceptions;
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with Ada.Streams;                 use Ada.Streams;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with MAX_Schedule;                use MAX_Schedule;
with GLib.Main;                   use GLib.Main;
with GLib.Messages;               use GLib.Messages;
with GLib.Properties;             use GLib.Properties;
with Gtk.Frame;                   use Gtk.Frame;
with Gtk.Cell_Renderer_Pixbuf;    use Gtk.Cell_Renderer_Pixbuf;
with Gtk.File_Chooser;            use Gtk.File_Chooser;
with Gtk.File_Filter;             use Gtk.File_Filter;
with Gtk.Image_Menu_Item;         use Gtk.Image_Menu_Item;
with Gtk.Menu;                    use Gtk.Menu;
with Gtk.Missed;                  use Gtk.Missed;
with Gtk.Recent_Manager_Keys;     use Gtk.Recent_Manager_Keys;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;
with Gtk.Separator;               use Gtk.Separator;
with Gtk.Stock;                   use Gtk.Stock;
with Gtk.Text_Iter;               use Gtk.Text_Iter;
with Gtk.Tree_Selection;          use Gtk.Tree_Selection;
with MAX_IO.Set_Mode;             use MAX_IO.Set_Mode;
with MAX_Settings_Page;           use MAX_Settings_Page;
with Storage_Streams;             use Storage_Streams;
with Strings_Edit.Floats;         use Strings_Edit.Floats;
with Strings_Edit.Integers;       use Strings_Edit.Integers;
with Strings_Edit.Quoted;         use Strings_Edit.Quoted;

with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GLib.Object.Checked_Destroy;
with Image_Automatic_XPM;
with Image_Battery_High_XPM;
with Image_Battery_Low_XPM;
with Image_Battery_XPM.Image;
with Image_Boost_XPM;
with Image_Cube_XPM;
with Image_Cube_Crossed_XPM;
with Image_Eco_Button_XPM;
with Image_Error_Link_XPM;
with Image_Error_No_Link_XPM;
with Image_Link_XPM.Image;
with Image_Manual_XPM;
with Image_No_Link_XPM;
with Image_Operating_Mode_XPM.Image;
with Image_Ping_0_XPM.Image;
with Image_Ping_1_XPM.Image;
with Image_Ping_2_XPM.Image;
with Image_Ping_3_XPM.Image;
with Image_Ping_4_XPM.Image;
with Image_Ping_5_XPM.Image;
with Image_Radiator_XPM.Image;
with Image_Room_XPM;
with Image_Thermometer_XPM;
with Image_Vacation_XPM;
with Image_Window_XPM;
with Image_Window_Closed_XPM;
with MAX_Cube_Configuration;
with MAX_Home_Automation_Version;
with MAX_IO.Set_Display;
with MAX_Rooms_List.Deleting;
with MAX_Rooms_List.Moving;
with MAX_Rooms_List.Pairing;
with MAX_Rooms_List.Renaming;
with MAX_Rooms_List.Set_NTP;
with MAX_User;
with MAX_Version_Check;
--with System.Storage_Elements;

package body MAX_Rooms_List is
   use Gtk.Enums;
   use Gtk.Image;
   use Max_Icon_Factory;

   CRLF : constant String := (Character'Val (13), Character'Val (10));

   LF        : constant Character    := Character'Val (13);
   Copyright : constant UTF8_String  := Character'Val (16#C2#) &
                                        Character'Val (16#A9#);
   Averaged  : constant UTF8_String  := "%" &
                                        Character'Val (16#CE#) &
                                        Character'Val (16#A3#);
   Ping_Interval     : constant Duration := 0.150;
   Delete_Delay      : constant Duration := 13.0;
   Last_To_Go_Update : Time := Clock;

   View    : Trace_Box;
   Reports : Device_To_Cache_Map.Map;

   About_Text : constant String :=
             "<big><b>MAX! Home Automation</b> v"                      &
             MAX_Home_Automation_Version.Value & "</big>"         & LF &
             "Copyright " & Copyright & " 2015-2020 "                  &
             "Dmitry A. Kazakov"                                  & LF &
             "<a href=""http://www.dmitry-kazakov.de/"                 &
             "ada/max_home_automation.htm"" "                          &
             "title=""Home page"">"                                    &
             "www.dmitry-kazakov.de/"                                  &
             "ada/max_home_automation.htm</a>."                   & LF &
                                                                    LF &
             "This program is free software: you can redistribute it " &
             "and/or modify"                                      & LF &
             "it under the terms of the GNU General Public License "   &
             "as published by"                                    & LF &
             "the Free Software Foundation, either version 2 of "      &
             "the License, or"                                    & LF &
             "(at your option) any later version."                & LF &
                                                                    LF &
             "This program is distributed in the hope that it will "   &
             "be useful,"                                         & LF &
             "but WITHOUT ANY WARRANTY; without even the implied "     &
             "warranty of"                                        & LF &
             "MERCHANTABILITY or FITNESS FOR A PARTICULAR "            &
             "PURPOSE.  See the"                                  & LF &
             "GNU General Public License for more details."       & LF &
                                                                    LF &
             "You should have received a copy of the GNU General "     &
             "Public License"                                     & LF &
             "along with this program.  If not, see "                  &
             "<a href=""http://www.gnu.org/licenses/"" "               &
             "title=""GNU site"">www.gnu.org</a>.";

   type Root_Stream_Ptr is access all Root_Stream_Type'Class;
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Root_Stream_Type'Class,
             Root_Stream_Ptr
          );

   package Timers is new Generic_Sources (Rooms_List);

   function Where (Name : String) return String is
   begin
      return " in MAX_Rooms_List." & Name;
   end Where;

   function "=" (Left, Right : Room_Reference'Class) return Boolean is
   begin
      if Left.Kind_Of /= Right.Kind_Of then
         return False;
      else
         case Left.Kind_Of is
            when Cube |
                 Radiator_Thermostat..Wall_Thermostat |
                 Shutter_Contact |
                 Eco_Button =>
               return Left.Parameters.Address =
                      Right.Parameters.Address;
            when Unknown =>
               return Left.Room = Right.Room;
         end case;
      end if;
   end "=";

   function "=" (Left, Right : Room_Reference_Handles.Handle)
      return Boolean is
   begin
      if Left.Is_Valid and then Right.Is_Valid then
         return Left.Ptr.all = Right.Ptr.all;
      else
         return not (Left.Is_Valid xor Right.Is_Valid);
      end if;
   end "=";

   function "<" (Left, Right : Restore_Key) return Boolean is
   begin
      return
      (  Left.Cube < Right.Cube
      or else
         (  Left.Cube = Right.Cube
         and then
            Left.Room < Right.Room
      )  );
   end "<";

   procedure Activated
             (  Object : access GObject_Record'Class;
                Params : GValues;
                List   : Rooms_List
             )  is
      Path   : constant Gtk_Tree_Path :=
                        Convert (Get_Address (Nth (Params, 1)));
      Box     : RF_Address;
      Device  : RF_Address;
      Kind_Of : Device_Type;
      Row     : Gtk_Tree_Iter;
   begin
      if Path = Null_Gtk_Tree_Path then
         return;
      end if;
      Row := List.List.Get_Iter (Path);
      if Row /= Null_Iter then
         Kind_Of := Device_Type'(List.Get (Row));
         case Kind_Of is
            when Radiator_Thermostat..Wall_Thermostat =>
               Box    := List.Get_Cube (Row);
               Device := List.Get (Row);
               declare
                  Data : constant Device_Parameters_Data_Handles.
                         Handle := Get_Parameters (Box, Device);
               begin
                  if Data.Is_Valid then
                     Configure (Pages, List, Box, Data);
                  end if;
               end;
            when Cube | Shutter_Contact | Eco_Button | Unknown =>
               null;
         end case;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Activated")
         )  );
   end Activated;

   function Add
            (  Rooms     : not null access Rooms_List_Record;
               Address   : RF_Address;
               Serial_No : String;
               Source    : GNAT.Sockets.Sock_Addr_Type
            )  return Gtk_Tree_Iter is
      use GNAT.Sockets;
      This : Gtk_Tree_Iter := Null_Iter;
      Icon : Gdk_Pixbuf;
   begin
      declare
         Saved : constant RF_Address_Array := Restore;
         That  : Gtk_Tree_Iter := Rooms.List.Get_Iter_First;
      begin
         if That /= Null_Iter and then Saved'Length > 0 then
            for Index in Saved'Range loop
               exit when Saved (Index) = Address
                 or else Saved (Index) /= Rooms.Get_Cube (That);
               Rooms.List.Next (That);
            end loop;
            if That = Null_Iter then
               Rooms.List.Append (This, Null_Iter);
            else
               Rooms.List.Insert_Before (This, Null_Iter, That);
            end if;
         else
            Rooms.List.Insert (This, Null_Iter, -1);
         end if;
      end;
      Icon := Image_Cube_XPM.Get_Pixbuf;
      Rooms.Set (This, Type_Column, Icon);
      Icon.Unref;
      Rooms.Set (This, Address_Column, Image (Address));
      Rooms.Set (This, Serial_No_Column, Serial_No);
      Rooms.Set (This, IP_Column, Image (Source.Addr));
      Rooms.Set (This, Drop_Is_Temperature_Column,  True);
      Rooms.Set (This, Drop_Set_Temperature_Column, True);
      Rooms.Set (This, Name_Column, "at " & Image (Source.Addr));
      Rooms.Set (This, Duty_Column, 0);
--    Rooms.Set (This, Mode_Column, Cube.Get_Version);
      Rooms.Set (This, Room_ID_Column, GInt (No_Room));
      Rooms.Set
      (  This,
         Device_Column,
         GInt (Device_Type'Pos (Cube))
      );
      Rooms.Set (This, Operating_Mode_Column, GInt'(0));
      Rooms.Set
      (  This,
         Time_Stamp_Column,
         "connected " & Image (Clock, True)
      );
      Icon := Image_Link_XPM.Get_Pixbuf;
      Rooms.Set (This, Link_Column, Icon);
      Icon.Unref;
      declare
         Cube : constant Cube_Client_Handle := Get_Cube (Address);
      begin
         if Cube.Is_Valid then
            Rooms.Set
            (  This,
               Duty_Column,
               GInt (Float (Cube.Ptr.Get_Duty) * 100.0)
            );
         end if;
      end;
      return This;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add (cube)")
         )  );
         return Null_Iter;
   end Add;

   procedure Add
             (  Rooms   : not null access Rooms_List_Record;
                Room    : Gtk_Tree_Iter;
                Device  : Device_Parameters;
                Order   : RF_Address_Array;
                Address : RF_Address;
                Added   : out Boolean
             )  is
      This  : Gtk_Tree_Iter;
      That  : Gtk_Tree_Iter;
      Index : Integer := Order'First;
   begin
      This := Rooms.List.Children (Room);
      while This /= Null_Iter loop
         if Rooms.Get (This) = Device.Address then
            exit;
         end if;
         Rooms.List.Next (This);
      end loop;
      if This = Null_Iter then
         Added := True;
         That  := Rooms.List.Children (Room);
         if That = Null_Iter then
            Rooms.List.Insert (This, Room, -1);
         else
            loop
               if Index > Order'Last then
                  Rooms.List.Insert (This, Room, -1);
                  exit;
               elsif Device.Address = Order (Index) then
                  Rooms.List.Insert_After (This, Room, This);
                  exit;
               elsif Rooms.Get (That) = Order (Index) then
                  This := That;
                  Rooms.List.Next (That);
                  if That = Null_Iter then
                     Rooms.List.Insert (This, Room, -1);
                     exit;
                  end if;
               end if;
               Index := Index + 1;
            end loop;
         end if;
         Rooms.Set (This, Address_Column, Image (Device.Address));
         Rooms.Set (This, Serial_No_Column, Device.Serial_No);
         Rooms.Set (This, Name_Column, Device.Name);
         Rooms.Set (This, Duty_Column, 0);
         declare
            Image : Gdk_Pixbuf;
         begin
            case Device.Kind_Of is
               when Cube =>
                  Image := Image_Cube_XPM.Get_Pixbuf;
                  Rooms.Set (This, Type_Column, Image);
                  Rooms.Set (This, Drop_Is_Temperature_Column,  True);
                  Rooms.Set (This, Drop_Set_Temperature_Column, True);
               when Radiator_Thermostat | Radiator_Thermostat_Plus =>
                  Image := Image_Radiator_XPM.Get_Pixbuf;
                  Rooms.Set (This, Type_Column, Image);
                  Rooms.Set (This, Drop_Is_Temperature_Column,  True);
                  Rooms.Set (This, Drop_Set_Temperature_Column, False);
               when Wall_Thermostat =>
                  Image := Image_Thermometer_XPM.Get_Pixbuf;
                  Rooms.Set (This, Type_Column, Image);
                  Rooms.Set (This, Drop_Is_Temperature_Column,  False);
                  Rooms.Set (This, Drop_Set_Temperature_Column, False);
               when Shutter_Contact =>
                  Image := Image_Window_Closed_XPM.Get_Pixbuf;
                  Rooms.Set (This, Type_Column, Image);
                  Rooms.Set (This, Drop_Is_Temperature_Column,  True);
                  Rooms.Set (This, Drop_Set_Temperature_Column, True);
               when Eco_Button =>
                  Image := Image_Eco_Button_XPM.Get_Pixbuf;
                  Rooms.Set (This, Type_Column, Image);
                  Rooms.Set (This, Drop_Is_Temperature_Column,  True);
                  Rooms.Set (This, Drop_Set_Temperature_Column, True);
               when others =>
                  Image := Image_Room_XPM.Get_Pixbuf;
                  Rooms.Set (This, Type_Column, Image);
                  Rooms.Set (This, Drop_Is_Temperature_Column,  True);
                  Rooms.Set (This, Drop_Set_Temperature_Column, True);
            end case;
            Image.Unref;
         end;
         Rooms.Set (This, Room_ID_Column, GInt (Device.Room));
         Rooms.Set
         (  This,
            Device_Column,
            GInt (Device_Type'Pos (Device.Kind_Of))
         );
         Rooms.Set (This, Operating_Mode_Column, GInt'(0));
         if Rooms.Shortcuts /= null then
            Rooms.Shortcuts.Device_Added (Address, Device);
         end if;
      else
         Rooms.Set (This, Name_Column, Device.Name);
         Added := False;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add (device)")
         )  );
   end Add;

   procedure Add_Shortcuts
             (  Rooms   : not null access Rooms_List_Record;
                Handler : not null access
                          Device_List_Monior_Interface'Class
             )  is
   begin
      Rooms.Shortcuts := Handler.all'Unchecked_Access;
   end Add_Shortcuts;

   procedure Canceled (Rooms : in out Rooms_List_Record) is
   begin
      null;
   end Canceled;

   procedure Check_Move_Buttons
             (  Rooms : not null access Rooms_List_Record;
                Row   : Gtk_Tree_Iter
             )  is
      Sibling : Gtk_Tree_Iter;
   begin
      Sibling := Row;
      Next (Rooms.List, Sibling);
      Rooms.Move_Down.Set_Sensitive (Sibling /= Null_Iter);
      Sibling := Row;
      Previous (Rooms.List, Sibling);
      Rooms.Move_Up.Set_Sensitive (Sibling /= Null_Iter);
   end Check_Move_Buttons;

   procedure Delete
             (  Rooms   : not null access Rooms_List_Record;
                Room    : Gtk_Tree_Iter;
                Device  : Device_Parameters;
                Deleted : out Boolean
             )  is
      This : Gtk_Tree_Iter;
   begin
      Deleted := False;
      This    := Rooms.List.Children (Room);
      while This /= Null_Iter loop
         if Rooms.Get (This) = Device.Address then
            Rooms.List.Remove (This);
            Deleted := True;
            exit;
         end if;
         Rooms.List.Next (This);
      end loop;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Delete (device)")
         )  );
   end Delete;

   procedure Detached_Device
             (  Rooms     : in out Rooms_List_Record;
                Cube      : RF_Address;
                Kind_Of   : Device_Type;
                Device    : RF_Address;
                Serial_No : String
             )  is
      Index : Integer;
      List  : Faulty_Device_Map_Handles.Handle;
   begin
      if (  Device = 0
         or else
            Cube = 0
         or else
            Kind_Of not in Radiator_Thermostat..Eco_Button
         or else
            Rooms.Pairing /= null
         )  then
         return;
      end if;
      Index := Rooms.Faulty_List.Find (Cube);
      if Index > 0 then
         List := Rooms.Faulty_List.Get (Index);
      else
         List.Set (new Faulty_Device_Map);
         Rooms.Faulty_List.Add (Cube, List);
      end if;
      declare
         Item : Faulty_Device_Data_Handles.Handle;
      begin
         Index := List.Ptr.Map.Find (Device);
         if Index <= 0 then
            declare
               Guessed : constant String := Image (Kind_Of);
            begin
               Item.Set
               (  new Faulty_Device_Data (False, 0, Guessed'Length)
               );
               declare
                  This : Faulty_Device_Data'Class renames Item.Ptr.all;
               begin
                  This.Kind_Of   := Kind_Of;
                  This.Guessed   := Guessed;
                  This.Serial_No := Serial_No;
               end;
               List.Ptr.Map.Add (Device, Item);
            end;
         else
            Item := List.Ptr.Map.Get (Index);
         end if;
         declare
            This : Faulty_Device_Data'Class renames Item.Ptr.all;
         begin
            This.Kind_Of := Kind_Of;
            This.Poll_No := Poll_No;
         end;
      end;
      if Rooms.Deleting /= null then
         Rooms.Deleting.Detached_Device
         (  Cube      => Cube,
            Kind_Of   => Kind_Of,
            Device    => Device,
            Serial_No => Serial_No
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
                List   : Rooms_List
             )  is separate;

   procedure Display_Changed
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      Box     : RF_Address;
      Device  : RF_Address;
      Kind_Of : Device_Type;
      Mode    : Display_Mode;

      package Walker is new Selected_Foreach_User_Data (Boolean);

      procedure Enumerate
                (  Model : Gtk_Tree_Model;
                   Path  : Gtk_Tree_Path;
                   Iter  : Gtk_Tree_Iter;
                   Data  : Boolean
                )  is
         No_IO  : IO_Blocker;
         Box    : RF_Address;
         Device : RF_Address;
      begin
         if (  No_IO.Is_Free
            and then
               List.Get (Iter) in Radiator_Thermostat..Wall_Thermostat
            )
         then
            Device := List.Get (Iter);
            Box    := List.Get_Cube (Iter);
            MAX_IO.Set_Display.Set_Display_Mode
            (  Box         => Box,
               Device      => Device,
               Temperature => Mode
            );
            List.Thermostats.Add (Device);
         end if;
      exception
         when Data_Error =>
            null;
         when Error : others =>
            Log
            (  MAX_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Display_Changed (Enumerate)")
            )  );
      end Enumerate;
   begin
      List.Get_Selected (Box, Device, Kind_Of);
      if Kind_Of = Wall_Thermostat then
         case List.Display.Get_Active is
            when 1 =>
               Mode := Display_Is_Temperature;
            when 2 =>
               Mode := Display_Set_Temperature;
           when others =>
               return;
          end case;
      end if;
      List.Thermostats.Erase;
      Walker.Selected_Foreach
      (  List.View.Get_Selection,
         Enumerate'Access,
         True
      );
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Display_Changed")
         )  );
   end Display_Changed;

   procedure Do_About_Dialog_Destroy
             (  Rooms    : access GObject_Record'Class;
                Response : Gtk_Response_Type
             )  is
   begin
      Rooms_List_Record'Class (Rooms.all).Version_Label := null;
   end Do_About_Dialog_Destroy;

   function Do_Ping (Rooms : Rooms_List) return Boolean is separate;

   procedure Faulty_Device
             (  Rooms       : in out Rooms_List_Record;
                Cube        : RF_Address;
                Device      : RF_Address;
                Length      : Natural;
                Error       : Boolean;
                Initialized : Boolean;
                Orphaned    : Boolean
             )  is
      function Guess_Kind_Of return String is
      begin
         case Length is
            when 6 =>
               return "shutter contact or eco button";
            when 11 =>
               return "radiator thermostat";
            when 12 =>
               return "wall-mounted thermostat";
            when others =>
               return "unknown";
         end case;
      end Guess_Kind_Of;

      Index : Integer;
      Query : Boolean := False;
      List  : Faulty_Device_Map_Handles.Handle;
   begin
      if Device = 0 or else Cube = 0 or else Rooms.Pairing /= null then
         return;
      end if;
      Index := Rooms.Faulty_List.Find (Cube);
      if Index > 0 then
         List := Rooms.Faulty_List.Get (Index);
      else
         List.Set (new Faulty_Device_Map);
         Rooms.Faulty_List.Add (Cube, List);
      end if;
      declare
         Item : Faulty_Device_Data_Handles.Handle;
      begin
         Index := List.Ptr.Map.Find (Device);
         if Index <= 0 then
            declare
               Guessed : constant String := Guess_Kind_Of;
            begin
               Item.Set
               (  new Faulty_Device_Data (False, 0, Guessed'Length)
               );
               Item.Ptr.Guessed := Guessed;
               Item.Ptr.Queried := False;
               List.Ptr.Map.Add (Device, Item);
            end;
         else
            Item := List.Ptr.Map.Get (Index);
         end if;
         declare
            This : Faulty_Device_Data'Class renames Item.Ptr.all;
         begin
            if not This.Pending_Delete then
               This.Length      := Length;
               This.Error       := Error;
               This.Initialized := Initialized;
               This.Orphaned    := Orphaned;
            end if;
            This.Poll_No := Poll_No;
            if not This.Queried then
               This.Queried := True;
               Query := True;
            end if;
         end;
      end;
      if Query then
         declare
            Box : constant Cube_Client_Handle := Get_Cube (Cube);
         begin
            if Box.Is_Valid then
               Box.Ptr.Query_Device_Configuration (Cube);
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
            &  Where ("Faulty_Device")
         )  );
   end Faulty_Device;

   function Find
            (  Rooms   : not null access Rooms_List_Record;
               Address : RF_Address
            )  return Gtk_Tree_Iter is
      This   : constant String := Image (Address);
      Result : Gtk_Tree_Iter   := Null_Iter;

      type Local is not null access function
           (  Model : Gtk_Tree_Model;
              Path  : Gtk_Tree_Path;
              Iter  : Gtk_Tree_Iter
           )  return Boolean;
      function "+" is
         new Ada.Unchecked_Conversion
             (  Local,
                Gtk.Tree_Store.Gtk_Tree_Model_Foreach_Func
             );
      function Look_Up
               (  Model : Gtk_Tree_Model;
                  Path  : Gtk_Tree_Path;
                  Iter  : Gtk_Tree_Iter
               )  return Boolean is
      begin
         if This = Rooms.Get (Iter, Address_Column) then
            Result := Iter;
            return True;
         else
            return False;
         end if;
      end Look_Up;
   begin
      Rooms.List.Foreach (+Look_Up'Access);
      return Result;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Find (by address)")
         )  );
         return Null_Iter;
   end Find;

   function Find
            (  Rooms : not null access Rooms_List_Record;
               ID    : Cube_Descriptor
            )  return Gtk_Tree_Iter is
      This : constant String := GNAT.Sockets.Image (ID.Address);
      Row  : Gtk_Tree_Iter   := Rooms.List.Children (Null_Iter);
   begin
      while Row /= Null_Iter loop
         exit when ID.Serial_No = Rooms.Get (Row, Serial_No_Column)
          and then This = Rooms.Get (Row, IP_Column);
         Rooms.List.Next (Row);
      end loop;
      return Row;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Find (cube)")
         )  );
         return Null_Iter;
   end Find;

   function Find
            (  Rooms : not null access Rooms_List_Record;
               Cube  : Gtk_Tree_Iter;
               Room  : Room_ID
            )  return Gtk_Tree_Iter is
      This : constant GInt := GInt (Room);
      Row  : Gtk_Tree_Iter := Cube;
   begin
      Row := Rooms.List.Children (Cube);
      while Row /= Null_Iter loop
         exit when Unknown = Rooms.Get (Row) and then
                   This = Rooms.List.Get_Int (Row, Room_ID_Column);
         Rooms.List.Next (Row);
      end loop;
      return Row;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Find (room)")
         )  );
         return Null_Iter;
   end Find;

   function Find_Cube
            (  Rooms : not null access Rooms_List_Record;
               Cube  : RF_Address
            )  return Gtk_Tree_Iter is
      Row : Gtk_Tree_Iter;
   begin
      Row := Rooms.List.Children (Null_Iter);
      while Row /= Null_Iter loop
         exit when Cube = Rooms.Get (Row);
         Rooms.List.Next (Row);
      end loop;
      return Row;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Find_Cube")
         )  );
         return Null_Iter;
   end Find_Cube;

   procedure Finished (Rooms : in out Rooms_List_Record) is
   begin
      null;
   end Finished;

   function Get
            (  Rooms  : not null access Rooms_List_Record;
               Row    : Gtk_Tree_Iter;
               Column : GInt
            )  return String is
   begin
      return Get (Rooms.List, Row, Column);
   end Get;

   procedure Get
             (  Rooms : not null access Rooms_List_Record;
                Row   : Gtk_Tree_Iter;
                Mode  : in out Operating_Mode;
                Valid : out Boolean
             )  is
   begin
      case Rooms.List.Get_Int (Row, Operating_Mode_Column) is
         when 1 =>
            Mode  := Automatic;
            Valid := True;
         when 2 =>
            Mode  := Manual;
            Valid := True;
         when 3 =>
            Mode := Boost;
            Valid := True;
         when 4 =>
            Mode  := Vacation;
            Valid := True;
         when others =>
            Valid := False;
      end case;
   end Get;

   function Get
            (  Rooms : not null access Rooms_List_Record;
               Row   : Gtk_Tree_Iter
            )  return RF_Address is
   begin
      return Value (Rooms.Get (Row, Address_Column));
   end Get;

   function Get
            (  Rooms  : not null access Rooms_List_Record;
               Row    : Gtk_Tree_Iter
            )  return Room_ID is
      Room : GInt;
   begin
      Room := Rooms.List.Get_Int (Row, Room_ID_Column);
      return Room_ID (Room);
   exception
      when others =>
         return No_Room;
   end Get;

   function Get
            (  Rooms  : not null access Rooms_List_Record;
               Device : RF_Address
            )  return Room_ID is
      Row : Gtk_Tree_Iter;
   begin
      Row := Rooms.Find (Device);
      if Row = Null_Iter then
         return No_Room;
      else
         return Rooms.Get (Row);
      end if;
   exception
      when others =>
         return No_Room;
   end Get;

   function Get
            (  Rooms : not null access Rooms_List_Record;
               Row   : Gtk_Tree_Iter
            )  return Device_Type is
      Result : GInt;
   begin
      Result := Rooms.List.Get_Int (Row, Device_Column);
      return Device_Type'Val (Result);
   exception
      when others =>
         return Unknown;
   end Get;

   function Get
            (  Rooms  : not null access Rooms_List_Record;
               Cube   : RF_Address;
               Room   : String
            )  return Room_ID is
      Row : Gtk_Tree_Iter := Rooms.Find_Cube (Cube);
   begin
      if Row = Null_Iter then
         return No_Room;
      end if;
      Row := Rooms.List.Children (Row);
      while Row /= Null_Iter loop
         if (  Unknown = Rooms.Get (Row)
            and then
               Rooms.Get (Row, Name_Column) = Room
            )  then
            return Rooms.Get (Row);
         end if;
         Rooms.List.Next (Row);
      end loop;
      return No_Room;
   end Get;

   function Get_Cube
            (  Rooms : not null access Rooms_List_Record;
               Row   : Gtk_Tree_Iter
            )  return RF_Address is
      This : Gtk_Tree_Iter := Row;
   begin
      loop
         declare
            Predecessor : constant Gtk_Tree_Iter :=
                                   Parent (Rooms.List, This);
         begin
            exit when Predecessor = Null_Iter;
            This := Predecessor;
         end;
      end loop;
      declare
         Text : constant String := Rooms.Get (This, Address_Column);
      begin
         return
            RF_Address (Strings_Edit.Integers.Value (Text, Base => 16));
      end;
   exception
      when others =>
         return 0;
   end Get_Cube;

   function Get_Is
            (  Rooms : not null access Rooms_List_Record;
               Row   : Gtk_Tree_Iter
            )  return Centigrade is
      Value  : GValue;
      Result : GDouble;
   begin
      Rooms.List.Get_Value (Row, Is_Temperature_Column, Value);
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
            &  Where ("Get_Is")
         )  );
         return 18.0;
   end Get_Is;

   function Get_List
            (  Rooms : not null access Rooms_List_Record
            )  return Gtk_Tree_Store is
   begin
      return Rooms.List;
   end Get_List;

   function Get_Logger
            (  Rooms : not null access Rooms_List_Record
            )  return MAX_Database is
   begin
      return Rooms.Logger;
   end Get_Logger;

   function Get_Object (Rooms : not null access Rooms_List_Record)
      return GObject is
   begin
      return Rooms.all'Unchecked_Access;
   end Get_Object;

   function Get_Play_Button
            (  Rooms : not null access Rooms_List_Record;
               Index : Positive
            )  return Gtk_Button is
   begin
      return Rooms.Play (Index);
   end Get_Play_Button;

   function Get_Room_Name
             (  Kind_Of : Device_Type;
                Room    : String
             )  return String is
   begin
      if Kind_Of = Eco_Button or else Room'Length = 0 then
         return "";
      else
         return " in " & Room;
      end if;
   end Get_Room_Name;

   function Get_Room_Position
            (  Rooms : not null access Rooms_List_Record;
               Cube  : RF_Address;
               Room  : Room_ID
            )  return Positive is
      Result   : Natural := 0;
      Got_Cube : Boolean := False;
      package Walker is
         new Gtk.Tree_Store.Foreach_User_Data (RF_Address);
      function Visit
               (  Model : Gtk_Tree_Model;
                  Path  : Gtk_Tree_Path;
                  Iter  : Gtk_Tree_Iter;
                  Data  : RF_Address
               )  return Boolean is
         Kind_Of : constant Device_Type := Rooms.Get (Iter);
      begin
         case Kind_Of is
            when Radiator_Thermostat..Eco_Button =>
               null;
            when Unknown =>
               Result := Result + 1;
               if Got_Cube and then Rooms.Get (Iter) = Room then
                  return True;
               end if;
            when others =>
               Got_Cube := Got_Cube or else Cube = Rooms.Get (Iter);
         end case;
         return False;
      exception
         when Error : others =>
            Log
            (  MAX_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Get_Room_Position.Visit")
            )  );
            return False;
      end Visit;
   begin
      Walker.Foreach (Rooms.List, Visit'Access, Cube);
      return Integer'Max (1, Result);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Thermostats")
         )  );
         return 1;
   end Get_Room_Position;

   function Get_Selected
            (  Rooms : not null access Rooms_List_Record
            )  return Gtk_Tree_Iter is
      package Walker is new Selected_Foreach_User_Data (Boolean);
      Row : Gtk_Tree_Iter;
      procedure Enumerate
                (  Model : Gtk_Tree_Model;
                   Path  : Gtk_Tree_Path;
                   Iter  : Gtk_Tree_Iter;
                   Data  : Boolean
                )  is
      begin
         Row := Iter;
      end Enumerate;
   begin
      if Rooms.View.Get_Selection.Count_Selected_Rows = 1 then
         Walker.Selected_Foreach
         (  Rooms.View.Get_Selection,
            Enumerate'Access,
            True
         );
         return Row;
      else
         return Null_Iter;
      end if;
   end Get_Selected;

   function Get_Set
            (  Rooms : not null access Rooms_List_Record;
               Row   : Gtk_Tree_Iter
            )  return Centigrade is
      Value  : GValue;
      Result : GDouble;
   begin
      Rooms.List.Get_Value (Row, Set_Temperature_Column, Value);
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
            &  Where ("Get_Set")
         )  );
         return 18.0;
   end Get_Set;

   function Get_Rooms_Count
            (  Rooms  : not null access Rooms_List_Record;
               Row    : Gtk_Tree_Iter
            )  return Natural is
   begin
      if Row = Null_Iter then
         return 0;
      end if;
      declare
         Parent : constant Gtk_Tree_Iter := Rooms.List.Parent (Row);
      begin
         if Parent = Null_Iter then -- Must be a cube
            return 0;
         end if;
         declare
            Grandparent : constant Gtk_Tree_Iter :=
                                   Rooms.List.Parent (Parent);
         begin
            if Grandparent = Null_Iter then -- Parent was the cube
               return Natural (Rooms.List.N_Children (Parent));
            else
               return Natural (Rooms.List.N_Children (Grandparent));
            end if;
         end;
      end;
   end Get_Rooms_Count;

   function Get_Room_Name
            (  Rooms : not null access Rooms_List_Record;
               Cube  : RF_Address;
               Room  : Room_ID
            )  return String is
      Row : Gtk_Tree_Iter;
   begin
      Row := Rooms.Find (Cube);
      if Row /= Null_Iter then
         Row := Rooms.Find (Row, Room);
         if Row /= Null_Iter then
            return Rooms.Get (Row, Name_Column);
         end if;
      end if;
      return "No name";
   exception
      when others =>
         return "No name";
   end Get_Room_Name;

   function Get_Room_Name
            (  Rooms : not null access Rooms_List_Record;
               Row   : Gtk_Tree_Iter
            )  return String is
      Grandparent : Gtk_Tree_Iter;
      Parent      : Gtk_Tree_Iter;
   begin
      Parent := Rooms.List.Parent (Row);
      if Parent = Null_Iter then
         return "";
      end if;
      Grandparent := Rooms.List.Parent (Parent);
      if Grandparent = Null_Iter then
         return "";
      end if;
      return Rooms.Get (Parent, Name_Column);
   exception
      when others =>
         return "No name";
   end Get_Room_Name;

   procedure Get_Selected
             (  Rooms   : not null access Rooms_List_Record;
                Box     : out RF_Address;
                Device  : out RF_Address;
                Kind_Of : out Device_Type
             )  is
      Model : constant Gtk_Tree_Model := +Rooms.List;
      Row   : Gtk_Tree_Iter;
   begin
      Kind_Of := Unknown;
      Box     := 0;
      Device  := 0;
      Row     := Rooms.Get_Selected;
      if Row /= Null_Iter then
         Kind_Of := Rooms.Get (Row);
         Device  := Rooms.Get (Row);
         loop
            Row := Parent (Model, Row);
            exit when Row = Null_Iter;
            if Rooms.Get (Row) = Cube then
               Box := Rooms.Get (Row);
               return;
            end if;
         end loop;
      end if;
      Kind_Of := Unknown;
      Box     := 0;
      Device  := 0;
   end Get_Selected;

   procedure Get_Selected_Cube
             (  Rooms : not null access Rooms_List_Record;
                Box   : out RF_Address
             )  is
      Row : constant Gtk_Tree_Iter := Rooms.Get_Selected;
   begin
      if Row = Null_Iter then
         Box := 0;
      else
         Box := Rooms.Get (Row);
      end if;
   end Get_Selected_Cube;

   procedure Get_Selected_Cube
             (  Rooms   : not null access Rooms_List_Record;
                Address : out GNAT.Sockets.Inet_Addr_Type
             )  is
      use GNAT.Sockets;
      Row : constant Gtk_Tree_Iter := Rooms.Get_Selected;
   begin
      if Row = Null_Iter then
         Address := No_Inet_Addr;
      else
         Address := Inet_Addr (Rooms.Get (Row, IP_Column));
      end if;
   exception
      when others =>
         Address := No_Inet_Addr;
   end Get_Selected_Cube;

   function Gtk_Rooms_List_New
            (  History  : not null access Graphs_Record'Class;
               Overview : not null access Graphs_Overview_Record'Class;
               Mails    : not null access MAX_Mail_Record'Class;
               Logger   : not null access MAX_Database_Record'Class;
               Control  : not null access MAX_Control_Record'Class
            )  return Rooms_List is separate;

   procedure Get_Thermostats
             (  Rooms   : in out Rooms_List_Record;
                Store   : not null access Gtk_List_Store_Record'Class;
                Exclude : RF_Address
             )  is
      package Walker is
         new Gtk.Tree_Store.Foreach_User_Data (RF_Address);
      function Visit
               (  Model : Gtk_Tree_Model;
                  Path  : Gtk_Tree_Path;
                  Iter  : Gtk_Tree_Iter;
                  Data  : RF_Address
               )  return Boolean is
      begin
         if (  Rooms.Get (Iter) in Radiator_Thermostat..Wall_Thermostat
            and then
               Rooms.Get (Iter) /= Exclude
            )
         then
            declare
               Row  : Gtk_Tree_Iter;
               Room : Gtk_Tree_Iter;
            begin
               Store.Append (Row);
               Gtk.Missed.Set
               (  Store,
                  Row,
                  0,
                  Rooms.Get (Iter, Name_Column)
               );
               Gtk.Missed.Set
               (  Store,
                  Row,
                  1,
                  Image (RF_Address'(Rooms.Get (Iter)))
               );
               Room := Rooms.List.Parent (Iter);
               if Room /= Null_Iter then
                  if Rooms.Get (Room) = Unknown then
                     Gtk.Missed.Set
                     (  Store,
                        Row,
                        2,
                        Rooms.Get (Room, Name_Column)
                     );
                     Room := Rooms.List.Parent (Room);
                  end if;
                  if Rooms.Get (Room) = Cube then
                     Store.Set
                     (  Row,
                        3,
                        GInt (RF_Address'(Rooms.Get (Room)))
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
               &  Where ("Get_Thermostats.Visit")
            )  );
            return False;
      end Visit;
   begin
      Walker.Foreach (Rooms.List, Visit'Access, Exclude);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Thermostats")
         )  );
   end Get_Thermostats;

   procedure Get_Thermostats
             (  Rooms : not null access Rooms_List_Record;
                Row   : Gtk_Tree_Iter;
                List  : in out RF_Address_Sets.Set
             )  is
   begin
      if Row = Null_Iter then
         return;
      end if;
      case Device_Type'(Rooms.Get (Row)) is
         when Radiator_Thermostat      |
              Radiator_Thermostat_Plus |
              Wall_Thermostat          =>
            List.Add (Rooms.Get (Row));
         when Shutter_Contact | Eco_Button =>
            null;
         when others =>
            declare
               This : Gtk_Tree_Iter := Rooms.List.Children (Row);
            begin
               while This /= Null_Iter loop
                  Rooms.Get_Thermostats (This, List);
                  Rooms.List.Next (This);
               end loop;
            end;
      end case;
   end Get_Thermostats;

   function Guess_Type (Text : String) return Device_Type is
   begin
      if Text = "cube" then
         return Cube;
      elsif Text = "radiator thermostat" then
         return Radiator_Thermostat;
      elsif Text = "radiator thermostat plus" then
         return Radiator_Thermostat_Plus;
      elsif Text = "wall thermostat" then
         return Wall_Thermostat;
      elsif Text = "shutter contact" then
         return Shutter_Contact;
      elsif Text = "eco button" then
         return Eco_Button;
      else
         return Unknown;
      end if;
   end Guess_Type;

   function Has_Thermostats
            (  Rooms : not null access Rooms_List_Record;
               Row   : Gtk_Tree_Iter
            )  return Boolean is
   begin
      if Row = Null_Iter then
         return False;
      end if;
      case Device_Type'(Rooms.Get (Row)) is
         when Radiator_Thermostat      |
              Radiator_Thermostat_Plus |
              Wall_Thermostat          =>
            return True;
         when Shutter_Contact | Eco_Button =>
            return False;
         when others =>
            declare
               This : Gtk_Tree_Iter := Rooms.List.Children (Row);
            begin
               while This /= Null_Iter loop
                  if Rooms.Has_Thermostats (This) then
                     return True;
                  end if;
                  Rooms.List.Next (This);
               end loop;
               return False;
            end;
      end case;
   end Has_Thermostats;

   procedure Hide_Mode (Rooms : in out Rooms_List_Record) is
   begin
      Rooms.Add_Device.Hide;
      Rooms.Date.Hide;
      Rooms.Degree_Label.Hide;
      Rooms.Delete_Device.Hide;
      Rooms.Disconnect.Hide;
      Rooms.Display.Hide;
      Rooms.Calendar.Hide;
      Rooms.Export_To_MAX.Hide;
      Rooms.Faulty.Hide;
      Rooms.Mode_Label.Hide;
      Rooms.Mode.Hide;
      Rooms.Move.Hide;
      Rooms.Reboot.Hide;
      Rooms.Reconnect.Hide;
      Rooms.Rename.Hide;
      Rooms.Reset.Hide;
      Rooms.Restore.Hide;
      Rooms.Save.Hide;
      Rooms.Set_NTP.Hide;
      Rooms.Store.Hide;
      Rooms.Temperature.Hide;
      Rooms.Wake_Up.Hide;
      for Index in 0..4 loop
         Rooms.Ping_Icons (Index).Hide;
      end loop;
      for Index in Rooms.Play'Range loop
         Rooms.Play (Index).Hide;
      end loop;
   end Hide_Mode;

   function Keep_All (Room : Room_ID) return Boolean is
   begin
      return True;
   end Keep_All;

   function Keep_All (Device : RF_Address) return Boolean is
   begin
      return True;
   end Keep_All;

   procedure Link_Cell_Data
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Row    : Gtk_Tree_Iter;
                Rooms  : Rooms_List
             )  is
   begin
      Cell.Set_Visible (Cube = Rooms.Get (Row));
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Link_Cell_Data")
         )  );
   end Link_Cell_Data;

   procedure Mode_Cell_Data
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Row    : Gtk_Tree_Iter;
                Rooms  : Rooms_List
             )  is
      Mode  : Operating_Mode := Automatic;
      Image : Gdk_Pixbuf;
      Valid : Boolean;
   begin
      Rooms.Get (Row, Mode, Valid);
      if Valid then
         case Mode is
            when Automatic =>
               Image := Image_Automatic_XPM.Get_Pixbuf;
            when Manual =>
               Image := Image_Manual_XPM.Get_Pixbuf;
            when Vacation =>
               Image := Image_Vacation_XPM.Get_Pixbuf;
            when Boost =>
               Image := Image_Boost_XPM.Get_Pixbuf;
         end case;
         Set_Property
         (  Cell,
            Gtk.Cell_Renderer_Pixbuf.Pixbuf_Property,
            Image
         );
         Image.Unref;
      else
         Set_Property
         (  Cell,
            Gtk.Cell_Renderer_Pixbuf.Pixbuf_Property,
            null
         );
      end if;
   end Mode_Cell_Data;

   procedure Mode_Changed
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      Box     : RF_Address;
      Device  : RF_Address;
      Kind_Of : Device_Type;
   begin
      List.Get_Selected (Box, Device, Kind_Of);
      if Kind_Of in Radiator_Thermostat..Wall_Thermostat then
         case List.Mode.Get_Active is
            when 1 => -- Manual
               List.Temperature.Show;
               List.Degree_Label.Show;
               List.Calendar.Hide;
               List.Date.Hide;
            when 2 => -- Vacation
               List.Temperature.Show;
               List.Degree_Label.Show;
               List.Calendar.Show;
               List.Date.Show;
            when 3 => -- Boost
               List.Temperature.Hide;
               List.Degree_Label.Hide;
               List.Calendar.Hide;
               List.Date.Hide;
           when others => -- Automatic
--                 List.Temperature.Hide;
--                 List.Degree_Label.Hide;
               List.Temperature.Show;
               List.Degree_Label.Show;
               List.Calendar.Hide;
               List.Date.Hide;
          end case;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Mode_Changed")
         )  );
   end Mode_Changed;

   procedure On_A_Response
             (  Rooms    : in out Rooms_List_Record;
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

   procedure On_Add_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             )  is
   begin
      declare
         Dialog : Add_Dialog_Record'Class renames
                  Add_Dialog_Record'Class (Self.all);
      begin
         if Response = Gtk_Response_OK then
            Add_Manually (Dialog.Edit.Get_Text);
         end if;
      exception
         when Error : others =>
            Log
            (  MAX_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("On_Add_Response")
            )  );
      end;
      GLib.Object.Checked_Destroy (Self);
   end On_Add_Response;

   procedure On_Add
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      Box    : Gtk_Box;
      Dialog : Add_Dialog;
   begin
      Dialog := new Add_Dialog_Record;
      Initialize
      (  Dialog => Dialog,
         Title  => "Enter address of a MAX! cube",
         Parent => Window,
         Flags  => Modal or Destroy_With_Parent
      );
      Dialog.Realize;
      Gtk_New_Hbox (Box);
      Dialog.Get_Content_Area.Set_Spacing (3);
      Dialog.Get_Content_Area.Pack_Start (Box);
      Gtk_New (Dialog.Edit);
      Dialog.Edit.Set_Alignment (0.0);
      Dialog.Edit.Set_Max_Width_Chars (40);
      Box.Pack_Start (Dialog.Edit);
      Add_Button_From_Stock
      (  Dialog   => Dialog,
         Response => Gtk_Response_OK,
         Icon     => Stock_OK,
         Label    => "_OK",
         Tip      => "Connect to the cube"
      ) .Set_Can_Default (True);
      Add_Button_From_Stock
      (  Dialog   => Dialog,
         Response => Gtk_Response_Cancel,
         Icon     => Stock_Cancel,
         Label    => "_Cancel",
         Tip      => "Cancel address input"
      );
      Dialog.Set_Default_Response (Gtk_Response_OK);
      Dialog.On_Response (On_Add_Response'Access);
      Dialog.Show_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Add")
         )  );
   end On_Add;

   procedure On_Add_Device
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      Cube : RF_Address;
      Row  : constant Gtk_Tree_Iter := List.Get_Selected;
   begin
      if Row = Null_Iter then
         List.Add_Device.Hide;
         return;
      end if;
      Cube := List.Get_Cube (Row);
      if Cube = 0 then
         List.Add_Device.Hide;
         return;
      end if;
      Pairing.Pair_Device
      (  Rooms => List,
         Head  => "Looking for devices ...",
         Cube  => Cube,
         Room  => List.Get (Row)
      );
   end On_Add_Device;

   procedure On_Add_Device_Menu
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      Cube : RF_Address;
   begin
      if List.Right_Click = Null_Iter then
         return;
      end if;
      Cube := List.Get_Cube (List.Right_Click);
      if Cube = 0 then
         return;
      end if;
      Pairing.Pair_Device
      (  Rooms => List,
         Head  => "Looking for devices ...",
         Cube  => Cube,
         Room  => List.Get (List.Right_Click)
      );
   end On_Add_Device_Menu;

   procedure On_About
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      Dialog : Gtk_Dialog;
      Label  : Gtk_Label;
      Box    : Gtk_Box;
   begin
      Gtk_New
      (  Dialog,
         Title  => "About MAX! home automation",
         Parent => Window,
         Flags  => Modal or Destroy_With_Parent
      );
      Dialog.Realize;
      Gtk_New_HBox (Box);
      Dialog.Get_Content_Area.Pack_Start (Box, Padding => 10);
      Gtk_New (Label);
      Label.Set_Markup (About_Text);
      Label.Set_Selectable (True);
      Box.Pack_Start (Label, Padding => 10);
      Dialog.Get_Content_Area.Pack_Start (Gtk_HSeparator_New);
      Gtk_New (List.Version_Label);
      List.Version_Label.Set_Text ("Checking for updates ...");
      Dialog.Get_Content_Area.Pack_Start (List.Version_Label);
      Dialog.On_Response (Do_About_Dialog_Destroy'Access, List);
      Dialog.On_Response (Do_Destroy'Access);
      Dialog.Show_All;
      MAX_Version_Check.Get_Version;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_About")
         )  );
   end On_About;

   function On_Button_Press
            (  Object : access GObject_Record'Class;
               Event  : Gdk_Event;
               List   : Rooms_List
            )  return Boolean is separate;

   procedure On_Calendar_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             )  is
   begin
      declare
         Dialog : Calendar_Dialog_Record'Class renames
                  Calendar_Dialog_Record'Class (Self.all);
      begin
         if Gtk_Response_Accept = Response then
            declare
               Year   : GUInt;
               Month  : GUInt;
               Day    : GUInt;
               Hour   : GInt;
               Minute : GInt;
               Date   : Time;
            begin
               Dialog.Calendar.Get_Date (Year, Month, Day);
               Hour   := Dialog.End_Hour.Get_Active;
               Minute := Dialog.End_Minute.Get_Active * 30;
               Date   := Time_Of
                         (  Year    => Year_Number (Year),
                            Month   => Month_Number (Month + 1),
                            Day     => Day_Number (Day),
                            Seconds => Day_Duration
                                       (  Hour   * 3600
                                       +  Minute * 60
                         )             );
               Dialog.List.Date.Set_Text (Image (Date));
            exception
               when others =>
                  null;
            end;
         end if;
      exception
         when Error : others =>
            Log
            (  MAX_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("On_Calendar_Response")
            )  );
      end;
      GLib.Object.Checked_Destroy (Self);
   end On_Calendar_Response;

   procedure On_Calendar
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      Dialog    : Calendar_Dialog;
      Label     : Gtk_Label;
      Separator : Gtk_Separator;
      Box       : Gtk_HBox;
   begin
      Dialog := new Calendar_Dialog_Record;
      Initialize
      (  Dialog,
         "Select end of vacation time",
         Window,
         Modal or Destroy_With_Parent
      );
      Dialog.List := List;
      Dialog.Get_Content_Area.Set_Spacing (3);
      Gtk_New (Dialog.Calendar);
      Dialog.Get_Content_Area.Pack_Start (Dialog.Calendar);
      Gtk_New (Box, Orientation_Horizontal, 3);
      Dialog.Get_Content_Area.Pack_Start (Box);

      Gtk_New (Label, "End time");
      Box.Pack_Start (Label, False, False);

      declare
         End_Hour : Gtk_Combo_Box_Text renames Dialog.End_Hour;
      begin
         Gtk_New (End_Hour);
         Box.Pack_Start (End_Hour, False, False);
         End_Hour.Append_Text ("00");  End_Hour.Append_Text ("01");
         End_Hour.Append_Text ("02");  End_Hour.Append_Text ("03");
         End_Hour.Append_Text ("04");  End_Hour.Append_Text ("05");
         End_Hour.Append_Text ("06");  End_Hour.Append_Text ("07");
         End_Hour.Append_Text ("08");  End_Hour.Append_Text ("09");
         End_Hour.Append_Text ("10");  End_Hour.Append_Text ("11");
         End_Hour.Append_Text ("12");  End_Hour.Append_Text ("13");
         End_Hour.Append_Text ("14");  End_Hour.Append_Text ("15");
         End_Hour.Append_Text ("16");  End_Hour.Append_Text ("17");
         End_Hour.Append_Text ("18");  End_Hour.Append_Text ("19");
         End_Hour.Append_Text ("20");  End_Hour.Append_Text ("21");
         End_Hour.Append_Text ("22");  End_Hour.Append_Text ("23");
         End_Hour.Set_Active (0);
      end;
      Gtk_New (Label, ":");
      Box.Pack_Start (Label, False, False);
      declare
         End_Minute : Gtk_Combo_Box_Text renames Dialog.End_Minute;
      begin
         Gtk_New (End_Minute);
         Box.Pack_Start (End_Minute, False, False);
         End_Minute.Append_Text ("00");
         End_Minute.Append_Text ("30");
         End_Minute.Set_Active (0);
      end;
      Gtk_New_HSeparator (Separator);
      Dialog.Get_Content_Area.Pack_Start (Separator);
      Add_Button_From_Stock
      (  Dialog   => Dialog,
         Response => Gtk_Response_Accept,
         Icon     => Stock_OK,
         Label    => "_OK",
         Tip      => "Commit selected date"
      ) .Set_Can_Default (True);
      Add_Button_From_Stock
      (  Dialog   => Dialog,
         Response => Gtk_Response_Cancel,
         Icon     => Stock_Cancel,
         Label    => "_Cancel",
         Tip      => "Cancel date selection"
      );
      Dialog.On_Response (On_Calendar_Response'Access);
      Dialog.Show_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Calendar")
         )  );
   end On_Calendar;

   procedure On_Configure
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      No_IO   : IO_Blocker;
      Box     : RF_Address;
      Device  : RF_Address;
      Kind_Of : Device_Type;
   begin
      List.Get_Selected (Box, Device, Kind_Of);
      if Kind_Of in Radiator_Thermostat..Wall_Thermostat then
         declare
            Data : constant Device_Parameters_Data_Handles.Handle :=
                            Get_Parameters (Box, Device);
         begin
            if Data.Is_Valid then
               Configure (Pages, List, Box, Data);
            end if;
         end;
      end if;
      List.Configure.Set_Sensitive (False);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Configure")
         )  );
   end On_Configure;

   procedure On_Configure_Menu
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      No_IO   : IO_Blocker;
      Box     : RF_Address;
      Device  : RF_Address;
      Kind_Of : Device_Type;
   begin
      if List.Right_Click = Null_Iter then
         return;
      end if;
      Box     := List.Get_Cube (List.Right_Click);
      Kind_Of := List.Get (List.Right_Click);
      Device  := List.Get (List.Right_Click);
      if Kind_Of in Radiator_Thermostat..Wall_Thermostat then
         declare
            Data : constant Device_Parameters_Data_Handles.Handle :=
                            Get_Parameters (Box, Device);
         begin
            if Data.Is_Valid then
               Configure (Pages, List, Box, Data);
            end if;
         end;
      end if;
      List.Configure.Set_Sensitive (False);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Configure_Menu")
         )  );
   end On_Configure_Menu;

   procedure On_Delete
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      use MAX_Rooms_List.Deleting;
   begin
      Delete_Selected (List);
   end On_Delete;

   procedure On_Delete_Menu
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      use MAX_Rooms_List.Deleting;
   begin
      if List.Right_Click /= Null_Iter then
         Delete_Selected (List, List.Right_Click);
      end if;
   end On_Delete_Menu;

   procedure On_Destroy
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      use GLib.Main;
   begin
      if List.Timer /= No_Source_ID then
         Remove (List.Timer);
         List.Timer := No_Source_ID;
      end if;
      if List.History /= null then
         List.History.Unref;
      end if;
      if List.Overview /= null then
         List.Overview.Unref;
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

   procedure On_Disconnect
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      No_IO : IO_Blocker;
   begin
      List.Update_Selected_Cube (Do_Disconnect => True);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Disconnect")
         )  );
   end On_Disconnect;

   procedure On_Export_To_MAX
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is separate;

   procedure On_Faulty
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      use MAX_Rooms_List.Deleting;
   begin
      List.Deleting := Delete_Faulty (List);
   end On_Faulty;

   procedure On_Move
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      use MAX_Rooms_List.Moving;
   begin
      Move_Selected (List);
   end On_Move;

   procedure On_Move_Menu
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      use MAX_Rooms_List.Moving;
   begin
      if List.Right_Click /= Null_Iter then
         Move_Selected (List, List.Right_Click);
      end if;
   end On_Move_Menu;

   procedure On_Move_Down
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
   begin
      if List.View.Get_Selection.Count_Selected_Rows = 1 then
         declare
            This    : Gtk_Tree_Iter;
            That    : Gtk_Tree_Iter;
            Kind_Of : Device_Type;
         begin
            This    := List.Get_Selected;
            Kind_Of := List.Get (This);
            case Kind_Of is
               when Radiator_Thermostat..Eco_Button | Unknown =>
                  That := This;
                  Next (List.List, That);
                  if That /= Null_Iter then
                     List.List.Move_After (This, That);
                     List.Check_Move_Buttons (This);
                     if Kind_Of = Unknown and then List.History /= null
                     then
                        declare
                           Cube : constant RF_Address :=
                                           List.Get_Cube (This);
                           Key  : constant Room_ID := List.Get (This);
                        begin
                           List.History.Reorder
                           (  Cube,
                              Key,
                              GLib.Gint
                              (  List.Get_Room_Position (Cube, Key) - 1
                           )  );
                        end;
                     end if;
                     List.Store_Order (This);
                     return;
                  end if;
               when others =>
                  null;
            end case;
         end;
      end if;
      List.Move_Down.Set_Sensitive (False);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Move_Down")
         )  );
   end On_Move_Down;

   procedure On_Move_Up
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
   begin
      if List.View.Get_Selection.Count_Selected_Rows = 1 then
         declare
            This    : Gtk_Tree_Iter;
            That    : Gtk_Tree_Iter;
            Kind_Of : Device_Type;
         begin
            This    := List.Get_Selected;
            Kind_Of := List.Get (This);
            case Kind_Of is
               when Radiator_Thermostat..Eco_Button | Unknown =>
                  That := This;
                  Previous (List.List, That);
                  if That /= Null_Iter then
                     List.List.Move_Before (This, That);
                     List.Check_Move_Buttons (This);
                     if Kind_Of = Unknown and then List.History /= null
                     then
                        declare
                           Cube : constant RF_Address :=
                                           List.Get_Cube (This);
                           Key  : constant Room_ID := List.Get (This);
                        begin
                           List.History.Reorder
                           (  Cube,
                              Key,
                              GLib.Gint
                              (  List.Get_Room_Position (Cube, Key) - 1
                           )  );
                        end;
                     end if;
                     List.Store_Order (This);
                     return;
                  end if;
               when Cube =>
                  That := This;
                  Previous (List.List, That);
                  if That /= Null_Iter then
                     List.List.Move_Before (This, That);
                     List.Check_Move_Buttons (This);
                     List.Store_Order (This);
                     return;
                  end if;
            end case;
         end;
      end if;
      List.Move_Up.Set_Sensitive (False);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Move_Up")
         )  );
   end On_Move_Up;

   procedure On_Page_Switch
             (  Object : access GObject_Record'Class;
                Params : GLib.Values.GValues;
                List   : Rooms_List
             )  is
   begin
      for Index in List.Ping_Icons'Range loop
         if Index = List.Ping_State then
            List.Ping_Icons (Index).Show;
         else
            List.Ping_Icons (Index).Hide;
         end if;
      end loop;
   end On_Page_Switch;

   procedure On_Reboot
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      No_IO : IO_Blocker;
   begin
      List.Update_Selected_Cube (Do_Reboot => True);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Reboot")
         )  );
   end On_Reboot;

   procedure On_Reconnect
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      No_IO : IO_Blocker;
   begin
      List.Update_Selected_Cube (Do_Reconnect => True);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Reconnect")
         )  );
   end On_Reconnect;

   procedure On_Rename
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      use MAX_Rooms_List.Renaming;
   begin
      Rename_Selected (List);
   end On_Rename;

   procedure On_Rename_Menu
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      use MAX_Rooms_List.Renaming;
   begin
      if List.Right_Click /= Null_Iter then
         Rename_Selected (List, List.Right_Click);
      end if;
   end On_Rename_Menu;

   procedure On_Response_Restore
             (  Dialog   : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             )  is
      use MAX_Cube_Configuration;
      Cube     : RF_Address := 0;
      Topology : Topology_Handles.Handle;
      Data     : Device_Parameters_Data_Sets.Set;
      List     : Rooms_List;
   begin
      declare
         Preview : Preview_Dialog_Record'Class renames
                   Preview_Dialog_Record'Class (Dialog.all);
      begin
      if Response = Gtk_Response_Accept then
         declare
            use Ada.Streams.Stream_IO;
            File  : File_Type;
            Name  : UTF8_String := Preview.Get_Filename;
            Depth : Integer := 1;
         begin
            begin
               Open (File, In_File, Preview.Get_Filename);
            exception
               when Error : others =>
                  Say
                  (  (  "File "
                     &  Preview.Get_Filename
                     &  ": "
                     &  Exception_Message (Error)
                     ),
                     "Open file error"
                  );
                  GLib.Object.Checked_Destroy (Dialog);
                  return;
            end;
            begin
               Read (Stream (File), Data, Topology);
            exception
               when Error : others =>
                  Say
                  (  (  "File "
                     &  Preview.Get_Filename
                     &  ": "
                     &  Exception_Message (Error)
                     ),
                     "File read error"
                  );
                  Close (File);
                  GLib.Object.Checked_Destroy (Dialog);
                  return;
            end;
            Close (File);
            List := Preview.List;
            Cube := Preview.Cube;
         end;
      end if;
      GLib.Object.Checked_Destroy (Dialog);
      if Cube > 0 then
         Restore (Pages, List, Cube, Data, Topology);
      end if;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Response_Restore")
         )  );
   end On_Response_Restore;

   procedure On_Restore
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      Scroll : Gtk_Scrolled_Window;
      Dialog : Preview_Dialog;
      Cube   : RF_Address;
      Filter : Gtk_File_Filter;
   begin
      List.Get_Selected_Cube (Cube);
      if Cube = 0 then
         return;
      end if;
      Dialog := new Preview_Dialog_Record;
      Initialize
      (  Dialog,
         "Select MAX! cube configuration file",
         Window,
         Action_Open
      );
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name (Max_Filter_Name);
      Filter.Add_Pattern ("*" & Max_Extension);
      Dialog.Add_Filter (Filter);
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name ("Any file");
      Filter.Add_Pattern ("*");
      Dialog.Add_Filter (Filter);
      Dialog.Realize;
      Dialog.Set_Do_Overwrite_Confirmation (True);
      Dialog.Cube    := Cube;
      Dialog.List    := List;
      Dialog.Buffer  := Gtk_Text_Buffer_New;
      Dialog.Preview := Gtk_Text_View_New_With_Buffer (Dialog.Buffer);
      Dialog.Buffer.Unref;
      Gtk_New (Scroll);
      Scroll.Add (Dialog.Preview);
      Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Scroll.Set_Size_Request (250, -1);
      Scroll.Show_All;
      Dialog.Set_Preview_Widget (Scroll);
      Add_Button_From_Stock
      (  Dialog   => Dialog,
         Response => Gtk_Response_Cancel,
         Icon     => Stock_Cancel,
         Label    => "_Cancel",
         Tip      => "Close the file selection dialog"
      );
      Add_Button_From_Stock
      (  Dialog   => Dialog,
         Response => Gtk_Response_Accept,
         Icon     => Stock_OK,
         Label    => "_OK",
         Tip      => "Open the selected file"
      ) .Set_Can_Default (True);
      Chooser_Handlers.Connect
      (  Dialog,
         "update-preview",
         On_Update_Preview'Access,
         Dialog
      );
      Dialog.Set_Modal (True);
      Dialog.On_Response (On_Response_Restore'Access);
      Dialog.Show_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Restore")
         )  );
   end On_Restore;

   procedure On_S_Response
             (  Rooms    : in out Rooms_List_Record;
                Cube     : access Cube_Client'Class;
                Error    : Boolean;
                Duty     : Ratio;
                Slots    : Natural;
                Expected : in out Natural
             )  is
   begin
      begin
         Rooms.Logger.Update_Status
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
         if Rooms.Thermostats.Is_Empty then
            Say
            (  "Failure changing mode of a thermostat",
               "Set mode error"
            );
         else
            Say
            (  (  "Failure changing mode of the thermostat "
               &  Image (Rooms.Thermostats.Get (1))
               ),
               "Set mode error"
            );
         end if;
      else
         if not Rooms.Thermostats.Is_Empty then
            Rooms.Thermostats.Remove (Positive'(1));
         end if;
         if Expected > 0 then
            Expected := Expected - 1;
         end if;
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
            &  Where ("On_S_Response (set mode)")
         )  );
   end On_S_Response;

   procedure On_Save_Configuration (File : String; Cube : RF_Address) is
   begin
      Save_To_File (File, Cube);
   end On_Save_Configuration;

   procedure On_Save
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      Box    : RF_Address;
      Dialog : File_Dialog;
      Filter : Gtk_File_Filter;
   begin
      List.Get_Selected_Cube (Box);
      if Box = 0 then
         return;
      end if;
      Dialog :=
         Gtk_New
         (  "Select or create a MAX! cube configuration file",
            Action_Save,
            On_Save_Configuration'Access,
            Box,
            "Store configuration into the selected file"
         );
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name (Max_Filter_Name);
      Filter.Add_Pattern ("*" & Max_Extension);
      Dialog.Add_Filter (Filter);
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name ("Any file");
      Filter.Add_Pattern ("*");
      Dialog.Add_Filter (Filter);
      Dialog.Set_Do_Overwrite_Confirmation (True);
      Dialog.Show_All;
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

   procedure On_Scan_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             )  is
   begin
      declare
         Dialog : Scan_Dialog_Record'Class renames
                  Scan_Dialog_Record'Class (Self.all);
      begin
         if Response = Gtk_Response_OK then
            Host := To_Unbounded_String (Dialog.Edit.Get_Text);
            GLib.Object.Checked_Destroy (Self);
            MAX_Settings_Page.Update_Host;
            declare
               No_IO : IO_Blocker;
            begin
               Scan (To_String (Host));
            end;
         else
            GLib.Object.Checked_Destroy (Self);
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Scan_Response")
         )  );
   end On_Scan_Response;

   procedure On_Scan
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      Dialog : Scan_Dialog;
      Box    : Gtk_Box;
   begin
      Dialog := new Scan_Dialog_Record;
      Initialize
      (  Dialog => Dialog,
         Title  => "Enter address of the host",
         Parent => Window,
         Flags  => Modal or Destroy_With_Parent
      );
      Dialog.Realize;
      Dialog.Get_Content_Area.Set_Spacing (3);
      Gtk_New_Hbox (Box);
      Dialog.Get_Content_Area.Pack_Start (Box);
      Gtk_New (Dialog.Edit);
      Dialog.Edit.Set_Alignment (0.0);
      Dialog.Edit.Set_Max_Width_Chars (40);
      declare
         use GNAT.Sockets;
         Host : constant Host_Entry_Type :=
                         Get_Host_By_Name (Host_Name);
      begin
         if Addresses_Length (Host) > 0 then
            Dialog.Edit.Set_Text (Image (Addresses (Host, 1)));
         end if;
      end;
      Dialog.Edit.Set_Tooltip_Text
      (  "The address must belong to this machine. "
      &  "When the machine has several configured network adapters "
      &  "it has an address for each of them. "
      &  "Specify the one corresponding to the LAN segment "
      &  "you want to scan for cubes."
      );
      Box.Pack_Start (Dialog.Edit);
      Add_Button_From_Stock
      (  Dialog   => Dialog,
         Response => Gtk_Response_OK,
         Icon     => Stock_OK,
         Label    => "_OK",
         Tip      => "Scan LAN for the cube"
      ) .Set_Can_Default (True);
      Add_Button_From_Stock
      (  Dialog   => Dialog,
         Response => Gtk_Response_Cancel,
         Icon     => Stock_Cancel,
         Label    => "_Cancel",
         Tip      => "Cancel host name or address input"
      );
      Dialog.Set_Default_Response (Gtk_Response_OK);
      Dialog.On_Response (On_Scan_Response'Access);
      Dialog.Show_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Scan")
         )  );
   end On_Scan;

   procedure On_Select_All_Thermostats
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      Row : Gtk_Tree_Iter := List.List.Get_Iter_First;
   begin
      while Row /= Null_Iter loop
         List.Select_Thermostats (Row, True);
         List.List.Next (Row);
      end loop;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Select_All_Thermostats")
         )  );
   end On_Select_All_Thermostats;

   procedure On_Select_Thermostats
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
   begin
      List.Select_Thermostats (List.Right_Click, True);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Select_Thermostats")
         )  );
   end On_Select_Thermostats;

   procedure On_Set_NTP
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      use MAX_Rooms_List.Set_NTP;
   begin
      Set_Selected_NTP (List);
   end On_Set_NTP;

   procedure On_Store
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is separate;

   procedure On_Update_Preview
             (  Object : access GObject_Record'Class;
                Dialog : Preview_Dialog
             )  is separate;

   procedure On_Wake_Up
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
   begin
      if List.View.Get_Selection.Count_Selected_Rows = 1 then
         declare
            No_IO : IO_Blocker;
            This  : Gtk_Tree_Iter;
         begin
            This := List.Get_Selected;
            case Device_Type'(List.Get (This)) is
               when Cube =>
                  Wake_Up (List.Get (This));
               when Radiator_Thermostat..Eco_Button =>
                  declare
                     Address : constant RF_Address := List.Get (This);
                  begin
                     Wake_Up (List.Get_Cube (This), (1 => Address));
                  end;
               when others =>
                  declare
                     Room : constant Room_ID := List.Get (This);
                  begin
                     Wake_Up (List.Get_Cube (This), Room);
                  end;
            end case;
         end;
      else
         List.Wake_Up.Hide;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Wake_Up")
         )  );
   end On_Wake_Up;

   procedure On_Wake_Up_Menu
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             )  is
      No_IO : IO_Blocker;
   begin
      case Device_Type'(List.Get (List.Right_Click)) is
         when Cube =>
            Wake_Up (List.Get (List.Right_Click));
         when Radiator_Thermostat..Eco_Button =>
            declare
               Address : constant RF_Address :=
                                  List.Get (List.Right_Click);
            begin
               Wake_Up
               (  List.Get_Cube (List.Right_Click), (1 => Address)
               );
            end;
         when others =>
            declare
               Room : constant Room_ID := List.Get (List.Right_Click);
            begin
               Wake_Up (List.Get_Cube (List.Right_Click), Room);
            end;
      end case;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Wake_Up_Menu")
         )  );
   end On_Wake_Up_Menu;

   procedure Pair_Detached
             (  Rooms     : not null access Rooms_List_Record;
                Cube      : RF_Address;
                Device    : RF_Address;
                Kind_Of   : Device_Type;
                Serial_No : String;
                Room      : String := "";
                Handler   : Settings_Handler_Ptr := null
             )  is
      function Get_Room_ID return Room_ID is
      begin
         if Kind_Of = Eco_Button or else Room'Length = 0 then
            return No_Room;
         else
            return Rooms.Get (Cube, Room);
         end if;
      end Get_Room_ID;
   begin
      Pairing.Pair_Device
      (  Rooms     => Rooms.all'Unchecked_Access,
         Head      => (  "Pair orphaned "
                      &  Image (Kind_Of)
                      &  " "
                      &  Serial_No
                      &  Get_Room_Name (Kind_Of, Room)
                      ),
         Cube      => Cube,
         Kind_Of   => Kind_Of,
         Device    => Device,
         Room      => Get_Room_ID,
         Serial_No => Serial_No
      );
   end Pair_Detached;

   procedure Pair_Missing
             (  Rooms     : not null access Rooms_List_Record;
                Cube      : RF_Address;
                Room      : String;
                Device    : RF_Address;
                Kind_Of   : Device_Type;
                Serial_No : String;
                Handler   : Pairing_Issuer_Ptr := null
             )  is
      function Get_Room_ID return Room_ID is
      begin
         if Kind_Of = Eco_Button or else Room'Length = 0 then
            return No_Room;
         else
            return Rooms.Get (Cube, Room);
         end if;
      end Get_Room_ID;
   begin
      Pairing.Pair_Device
      (  Rooms     => Rooms.all'Unchecked_Access,
         Head      => (  "Pair missing "
                      &  Image (Kind_Of)
                      &  " "
                      &  Serial_No
                      &  Get_Room_Name (Kind_Of, Room)
                      ),
         Cube      => Cube,
         Device    => Device,
         Room      => Get_Room_ID,
         Serial_No => Serial_No,
         Handler   => Handler
      );
   end Pair_Missing;

   procedure Paired_Device
             (  Rooms     : in out Rooms_List_Record;
                Cube      : RF_Address;
                Device    : Device_Type;
                Address   : RF_Address;
                Serial_No : String
             )  is
   begin
      if Rooms.Pairing /= null then
         Rooms.Pairing.Paired_Device
         (  Cube      => Cube,
            Device    => Device,
            Address   => Address,
            Serial_No => Serial_No
         );
      end if;
   end Paired_Device;

   procedure Pairing_Ended (Rooms : in out Rooms_List_Record) is
   begin
      if Rooms.Pairing /= null then
         Rooms.Pairing.Pairing_Ended;
      end if;
   end Pairing_Ended;

   procedure Purge
             (  Rooms       : not null access Rooms_List_Record;
                Address     : RF_Address;
                Updated     : in out Room_Flags;
                Keep_Room   : not null access
                              function (Room : Room_ID)
                                 return Boolean := Keep_All'Access;
                Keep_Device : not null access
                              function (Device : RF_Address)
                                 return Boolean := Keep_All'Access
             )  is
      List : Gtk_Tree_Store renames Rooms.List;
      Root : Gtk_Tree_Iter;
      Room : Gtk_Tree_Iter;
   begin
      Root := Rooms.Find (Address);
      if Root = Null_Iter then
         return;
      end if;
      Room := List.Children (Root);
      while Room /= Null_Iter loop
         case Device_Type'(Rooms.Get (Room)) is
            when Cube =>
               List.Remove (Room);
            when Radiator_Thermostat..Shutter_Contact =>
               Close (Pages, Address, Rooms.Get (Room));
               List.Remove (Room);
            when Eco_Button =>
               if Keep_Device (Rooms.Get (Room)) then
                  List.Next (Room);
               else
                  List.Remove (Room);
               end if;
            when Unknown =>
               declare
                  ID      : constant Room_ID := Rooms.Get (Room);
                  Device  : Gtk_Tree_Iter    := List.Children (Room);
                  Empty   : Boolean          := True;
                  Changed : Boolean          := False;
                  This    : RF_Address;
               begin
                  if Keep_Room (ID) then
                     while Device /= Null_Iter loop
                        case Device_Type'(Rooms.Get (Device)) is
                           when Cube | Unknown =>
                              List.Remove (Device);
                           when Radiator_Thermostat..Eco_Button =>
                              This := Rooms.Get (Device);
                              if Keep_Device (This) then
                                 Empty := False;
                                 List.Next (Device);
                              else
                                 Changed := False;
                                 List.Remove (Device);
                                 Close (Pages, Address, This);
                              end if;
                        end case;
                     end loop;
                     if Empty then
                        if Rooms.History /= null then
                           Rooms.History.Delete (Address, ID);
                        end if;
                        List.Remove (Room);
                     elsif Changed then
                        Updated (ID) := True;
                        List.Next (Room);
                     else
                        List.Next (Room);
                     end if;
                  else
                     while Device /= Null_Iter loop
                        case Device_Type'(Rooms.Get (Device)) is
                           when Cube | Shutter_Contact..Unknown =>
                              List.Remove (Device);
                           when Radiator_Thermostat..Wall_Thermostat =>
                              This := Rooms.Get (Device);
                              List.Remove (Device);
                              Close (Pages, Address, This);
                        end case;
                     end loop;
                     if Rooms.History /= null then
                        Rooms.History.Delete (Address, ID);
                     end if;
                     List.Remove (Room);
                  end if;
               end;
         end case;
      end loop;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Purge")
         )  );
   end Purge;

   procedure Reorder_Graphs
             (  Rooms : not null access Rooms_List_Record
             )  is
      use GLib;
      Position : GInt       := 0;
      Cube     : RF_Address := 0;
      package Walker is
         new Gtk.Tree_Store.Foreach_User_Data (RF_Address);
      function Visit
               (  Model : Gtk_Tree_Model;
                  Path  : Gtk_Tree_Path;
                  Iter  : Gtk_Tree_Iter;
                  Data  : RF_Address
               )  return Boolean is
         Kind_Of : constant Device_Type := Rooms.Get (Iter);
      begin
         case Kind_Of is
            when Radiator_Thermostat..Eco_Button =>
               null;
            when Unknown =>
               Rooms.History.Reorder (Cube, Rooms.Get (Iter), Position);
               Position := Position + 1;
            when others =>
               Cube := Rooms.Get (Iter);
         end case;
         return False;
      exception
         when Error : others =>
            Log
            (  MAX_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Reorder_.Visit")
            )  );
            return False;
      end Visit;
   begin
      if Rooms.History /= null then
         Walker.Foreach (Rooms.List, Visit'Access, 0);
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Reorder_Graphs")
         )  );
   end Reorder_Graphs;

   Null_List : constant Restore_Item_Handles.Handle :=
                        Restore_Item_Handles.Ref
                        (  new Restore_Item'
                               (  Object.Entity
                               with
                                  Size => 0,
                                  List => (1..0 => 0)
                        )      );
   function Restore
            (  Rooms : not null access Rooms_List_Record;
               Cube  : RF_Address;
               Room  : Room_ID
            )  return Restore_Item_Handles.Handle is
      Offset : Integer;
   begin
      if Room = No_Room then
         return Null_List;
      end if;
      Offset := Rooms.Orders.Find ((Cube, Room));
      if Offset > 0 then
         return Rooms.Orders.Get (Offset);
      end if;
      declare
         Key     : constant String := "Room_" & Image (Cube) &
                                      '.' & Image (Integer (Room));
         List    : constant String := Restore (Key, "");
         Pointer : Integer := List'First;
         Index   : Natural := 0;
         Address : RF_Address;
         Cached  : Restore_Item_Handles.Handle;
      begin
          while Pointer <= List'Last loop
             Get
             (  Source  => List,
                Pointer => Pointer,
                Value   => Integer (Address),
                Base    => 16
             );
             Pointer := Pointer + 1;
             Index   := Index   + 1;
          end loop;
          Cached.Set (new Restore_Item (Index));
          Rooms.Orders.Add ((Cube, Room), Cached);
          declare
             Result : RF_Address_Array renames Cached.Ptr.List;
          begin
             Pointer := List'First;
             Index   := 0;
             while Pointer <= List'Last loop
                Index := Index + 1;
                Get
                (  Source  => List,
                   Pointer => Pointer,
                   Value   => Integer (Result (Index)),
                   Base    => 16
                );
                Pointer := Pointer + 1;
             end loop;
             return Cached;
          end;
      end;
   exception
      when others =>
         return Null_List;
   end Restore;

   function Restore return RF_Address_Array is
   begin
      declare
         Size    : Positive := 1;
         Order   : constant String := Restore ("cubes_order", "");
         Pointer : Integer         := Order'First;
      begin
         if Order'Length = 0 then
            return (1..0 => 0);
         end if;
         for Index in Order'Range loop
            case Order (Index) is
               when ',' =>
                  Size := Size + 1;
               when others =>
                  null;
            end case;
         end loop;
         declare
            use Strings_Edit;
            List : RF_Address_Array (1..Size);
         begin
            Get (Order, Pointer);
            for Index in List'Range loop
               Get
               (  Source  => Order,
                  Pointer => Pointer,
                  Value   => Integer (List (Index)),
                  Base    => 16,
                  First   => 0,
                  Last    => 16#FFFFFF#
               );
               Get (Order, Pointer);
               if Pointer <= Order'Last and then Order (Pointer) = ','
               then
                  Pointer := Pointer + 1;
               end if;
            end loop;
            return List;
         end;
      end;
   exception
      when others =>
         return (1..0 => 0);
   end Restore;

   function Restore (Cube : RF_Address) return Room_Reference_Array is
   begin
      declare
         Size    : Natural := 0;
         Order   : constant String :=
                      Restore ("Rooms_order_of_" & Image (Cube), "");
         Pointer : Integer := Order'First;
      begin
         for Index in Order'Range loop
            case Order (Index) is
               when '@' | '!' =>
                  Size := Size + 1;
               when others =>
                  null;
            end case;
         end loop;
         if Size = 0 then
            return (1..0 => Room_Reference_Handles.Null_Handle);
         end if;
         declare
            List  : Room_Reference_Array (1..Size);
            Index : Positive := List'First;
         begin
            while Pointer < Order'Last loop
               case Order (Pointer) is
                  when '@' =>
                     Pointer := Pointer + 1;
                     List (Index).Set (new Room_Reference (Unknown, 0));
                     Get
                     (  Source  => Order,
                        Pointer => Pointer,
                        Value   => Integer (List (Index).Ptr.Room)
                     );
                  when '!' =>
                     Pointer := Pointer + 1;
                     List (Index).Set
                     (  new Room_Reference (Eco_Button, 0)
                     );
                     Get
                     (  Source  => Order,
                        Pointer => Pointer,
                        Base    => 16,
                        Value   => Integer
                                   (  List (Index).Ptr.Parameters.
                                      Address
                     )             );
                  when others =>
                     return
                        (1..0 => Room_Reference_Handles.Null_Handle);
               end case;
               Index := Index + 1;
            end loop;
            return List;
         end;
      end;
   exception
      when others =>
         return (1..0 => Room_Reference_Handles.Null_Handle);
   end Restore;

   procedure Save_To_File (Name : UTF8_String; Box : RF_Address) is
      use MAX_Cube_Configuration;
      use Ada.Streams.Stream_IO;
      File        : File_Type;
      Thermostats : Device_Parameters_Data_Sets.Set;
   begin
      begin
         Create (File, Out_File, Name);
      exception
         when Error : others =>
            Say
            (  "File " & Name & ": " & Exception_Message (Error),
               "Output file open error"
            );
            return;
      end;
      declare
         List : constant RF_Address_Maps.Map :=
                         Get_Thermostats (Box, No_Room);
      begin
         for Index in 1..List.Get_Size loop
            if List.Get (Index) in Radiator_Thermostat
                                .. Wall_Thermostat then
               Thermostats.Add
               (  Get_Parameters
                  (  Box,
                     List.Get_Key (Index)
               )  );
            end if;
         end loop;
         Write (Stream (File).all, Thermostats, Get_Topology (Box));
      exception
         when Layout_Error =>
            Say
            (  "Too many devices or rooms to store into file " & Name
            );
            begin
               Close (File);
            exception
               when others =>
                  null;
            end;
            return;
         when Error : others =>
            Say
            (  "File " & Name & ": " & Exception_Message (Error),
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
            (  "File " & Name & ": " & Exception_Message (Error),
               "File close error"
            );
            return;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Save_To_File")
         )  );
   end Save_To_File;

   procedure Select_Thermostats
             (  Rooms        : not null access Rooms_List_Record;
                Row          : Gtk_Tree_Iter;
                Deselect_Row : Boolean
             )  is
   begin
      if Row = Null_Iter then
         return;
      end if;
      case Device_Type'(Rooms.Get (Row)) is
         when Radiator_Thermostat      |
              Radiator_Thermostat_Plus |
              Wall_Thermostat          =>
            if Deselect_Row then
               Rooms.View.Get_Selection.Unselect_Iter (Row);
            else
               Rooms.View.Get_Selection.Select_Iter (Row);
            end if;
         when Shutter_Contact | Eco_Button =>
            if Deselect_Row then
               Rooms.View.Get_Selection.Unselect_Iter (Row);
            end if;
         when others =>
            if Deselect_Row then
               Rooms.View.Get_Selection.Unselect_Iter (Row);
            end if;
            declare
               This : Gtk_Tree_Iter := Rooms.List.Children (Row);
            begin
               while This /= Null_Iter loop
                  Rooms.Select_Thermostats (This, False);
                  Rooms.List.Next (This);
               end loop;
            end;
      end case;
   end Select_Thermostats;

   procedure Set
             (  Rooms : not null access Rooms_List_Record;
                Row   : Gtk_Tree_Iter;
                Mode  : Operating_Mode
             )  is
   begin
      case Mode is
         when Automatic =>
            Rooms.Set (Row, Operating_Mode_Column, 1);
         when Manual =>
            Rooms.Set (Row, Operating_Mode_Column, 2);
         when Boost =>
            Rooms.Set (Row, Operating_Mode_Column, 3);
         when Vacation =>
            Rooms.Set (Row, Operating_Mode_Column, 4);
      end case;
   end Set;

   procedure Set
             (  Rooms  : not null access Rooms_List_Record;
                Row    : Gtk_Tree_Iter;
                Column : GInt;
                Icon   : Gdk_Pixbuf
             )  is
   begin
      Rooms.List.Set (Row, Column, Icon.all'Unchecked_Access);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set (icon)")
         )  );
   end Set;

   procedure Set
             (  Rooms  : not null access Rooms_List_Record;
                Row    : Gtk_Tree_Iter;
                Column : GInt;
                Text   : String
             )  is
   begin
      Gtk.Missed.Set (Rooms.List, Row, Column, Text);
   end Set;

   procedure Set
             (  Rooms  : not null access Rooms_List_Record;
                Row    : Gtk_Tree_Iter;
                Column : GInt;
                Data   : GInt
             )  is
      Value : GValue;
   begin
      Init (Value, GType_Int);
      Set_Int (Value, Data);
      Rooms.List.Set_Value (Row, Column, Value);
      Unset (Value);
   end Set;

   procedure Set
             (  Rooms  : not null access Rooms_List_Record;
                Row    : Gtk_Tree_Iter;
                Column : GInt;
                Flag   : Boolean
             )  is
      Value : GValue;
   begin
      Init (Value, GType_Boolean);
      Set_Boolean (Value, Flag);
      Rooms.List.Set_Value (Row, Column, Value);
      Unset (Value);
   end Set;

   procedure Set_Auto_Scale (Rooms : in out Rooms_List_Record) is
   begin
      if Rooms.History /= null then
         Rooms.History.Set_Auto_Scale;
      end if;
   end Set_Auto_Scale;

   procedure Set_Fixed_Scale
             (  Rooms : in out Rooms_List_Record;
                Low   : Centigrade;
                High  : Centigrade
             )  is
   begin
      if Rooms.History /= null then
         Rooms.History.Set_Fixed_Scale (Low, High);
      end if;
   end Set_Fixed_Scale;

   procedure Set_Is
             (  Rooms       : not null access Rooms_List_Record;
                Device      : Gtk_Tree_Iter;
                Temperature : Centigrade
             )  is
      Value : GValue;
   begin
      Init (Value, GType_Double);
      Set_Double (Value, GDouble (Temperature));
      Rooms.List.Set_Value (Device, Is_Temperature_Column, Value);
      Unset (Value);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Is")
         )  );
   end Set_Is;

   procedure Set_Trace_Box (Box : Trace_Box) is
   begin
      View := Box;
   end Set_Trace_Box;

   procedure Set_NTP_List
             (  Rooms : in out Rooms_List_Record;
                Cube  : RF_Address;
                List  : Servers_List.Set
             )  is
   begin
      Rooms.NTP_Servers.Replace (Cube, List);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_NTP_List")
         )  );
   end Set_NTP_List;

   procedure Set_Pending_Restore
             (  Rooms   : in out Rooms_List_Record;
                Pending : Boolean
             )  is
   begin
      Rooms.Restoring := True;
      Rooms.Restore.Set_Sensitive (not Pending);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Pending_Restore")
         )  );
   end Set_Pending_Restore;

   procedure Set_Set
             (  Rooms       : not null access Rooms_List_Record;
                Device      : Gtk_Tree_Iter;
                Temperature : Centigrade
             )  is
      Value : GValue;
   begin
      Init (Value, GType_Double);
      Set_Double (Value, GDouble (Temperature));
      Rooms.List.Set_Value (Device, Set_Temperature_Column, Value);
      Unset (Value);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Set")
         )  );
   end Set_Set;

   procedure Store
             (  Address     : RF_Address;
                Battery_Low : Boolean;
                Stamp       : String
             )  is
   begin
      if Battery_Low then
         Store (Image (Address), "l@" & Stamp);
      else
         Store (Image (Address), "h@" & Stamp);
      end if;
   end Store;

   procedure Store_Order
             (  Rooms : not null access Rooms_List_Record;
                Row   : Gtk_Tree_Iter
             )  is
      This : Gtk_Tree_Iter;
   begin
      case Device_Type'(Rooms.Get (Row)) is
         when Radiator_Thermostat..Shutter_Contact =>
            This := Parent (Rooms.List, Row);
            if This = Null_Iter then
               return;
            end if;
            declare
               Cube : constant RF_Address := Rooms.Get_Cube (Row);
               Room : constant Room_ID    := Rooms.Get (Row);
               List : Unbounded_String;
            begin
               if Room /= No_Room then
                  This := Rooms.List.Children (This);
                  while This /= Null_Iter loop
                     if Length (List) > 0 then
                        Append (List, ",");
                     end if;
                     Append (List, Rooms.Get (This, Address_Column));
                     Rooms.List.Next (This);
                  end loop;
                  Store
                  (  (  "Room_"
                     &  Image (Cube)
                     &  '.'
                     &  Image (Integer (Room))
                     ),
                     To_String (List)
                  );
               end if;
            end;
         when Cube =>
            declare
               Value : Unbounded_String;
            begin
               This := Rooms.List.Get_Iter_First;
               while This /= Null_Iter loop
                  case Device_Type'(Rooms.Get (This)) is
                     when Cube =>
                        if Length (Value) > 0 then
                           Append (Value, ",");
                        end if;
                        Append (Value, Image (Rooms.Get_Cube (This)));
                     when others =>
                        null;
                  end case;
                  Rooms.List.Next (This);
               end loop;
               Store ("cubes_order", To_String (Value));
            end;
         when others =>
            This := Parent (Rooms.List, Row);
            if This = Null_Iter then
               return;
            end if;
            declare
               Value   : Unbounded_String;
               Room_ID : Integer;
               Cube    : constant RF_Address := Rooms.Get_Cube (Row);
            begin
               This := Rooms.List.Children (This);
               while This /= Null_Iter loop
                  case Device_Type'(Rooms.Get (This)) is
                     when Eco_Button =>
                        Append
                        (  Value,
                           "!" & Rooms.Get (This, Address_Column)
                        );
                     when Unknown =>
                        Room_ID := Integer
                                   (  Rooms.List.Get_Int
                                      (  This,
                                         Room_ID_Column
                                   )  );
                        Append (Value, "@" & Image (Room_ID));
                     when others =>
                        return;
                  end case;
                  Rooms.List.Next (This);
               end loop;
               Store
               (  "Rooms_order_of_" & Image (Cube),
                  To_String (Value)
               );
            end;
      end case;
   end Store_Order;

   procedure Update_Connected
             (  Rooms : in out Rooms_List_Record;
                ID    : Cube_Descriptor
             )  is
      No_IO : IO_Blocker;
   begin
      if View /= null then
         View.Trace
         (  GNAT.Sockets.Image (ID.Address)
         &  ' '
         &  ID.Serial_No
         &  ' '
         &  ID.Name
         &  " Connected"
         );
      end if;
      Rooms.Update_Selected_Cube;
      declare
         Cube : constant Gtk_Tree_Iter := Rooms.Find (ID);
         Icon : Gdk_Pixbuf;
      begin
         if Cube /= Null_Iter then
            Icon := Image_Cube_XPM.Get_Pixbuf;
            Rooms.Set (Cube, Type_Column, Icon);
            Icon.Unref;
            Rooms.Set
            (  Cube,
               Time_Stamp_Column,
               "connected " & Image (Clock, True)
            );
            Icon := Image_Link_XPM.Get_Pixbuf;
            Rooms.Set (Cube, Link_Column, Icon);
            Icon.Unref;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Update_Connected")
         )  );
   end Update_Connected;

   procedure Update_Cube
             (  Rooms     : in out Rooms_List_Record;
                Address   : RF_Address;
                Serial_No : String;
                Source    : GNAT.Sockets.Sock_Addr_Type
             )  is
      No_IO : IO_Blocker;
      This  : Gtk_Tree_Iter;
   begin
      This := Rooms.Find (Address);
      if This = Null_Iter then
         This := Rooms.Add (Address, Serial_No, Source);
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
            &  Where ("Update_Cube")
         )  );
   end Update_Cube;

   procedure Update_Cube_Statistics
             (  Rooms   : in out Rooms_List_Record;
                Address : RF_Address;
                Average : Natural
             )  is
      Row : Gtk_Tree_Iter;
   begin
      Row := Rooms.Find (Address);
      if Row = Null_Iter then
         Trace
         (  "Cannot find tree view row for " & Image (Address),
            Error_Text
         );
      else
         Rooms.Set (Row, Valve_Column, Image (Average) & Averaged);
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Update_Cube_Statistics")
         )  );
   end Update_Cube_Statistics;

   procedure Update_Data
             (  Rooms  : in out Rooms_List_Record;
                Cube   : RF_Address;
                Data   : Device_Data;
                Offset : Centigrade
             )  is
      Icon  : Gdk_Pixbuf;
      No_IO : IO_Blocker;
      Row   : Gtk_Tree_Iter;
      Now   : constant Time := Clock;
   begin
      Update_Reports (Data);
      Rooms.Logger.Update_Data (Data, Offset);
      Row := Rooms.Find (Data.Address);
      if Rooms.Ping_State /= 0 then
         if Pages.Get_Current_Page = 0 then
            Rooms.Ping_Icons (Rooms.Ping_State).Hide;
            Rooms.Ping_State := 0;
            Rooms.Ping_Icons (Rooms.Ping_State).Show;
         else
            Rooms.Ping_State := 0;
         end if;
      end if;
      if Row = Null_Iter then
         Trace
         (  "Cannot find tree view row for " & Image (Data.Address),
            Error_Text
         );
      else
         case Data.Kind_Of is
            when Shutter_Contact =>
               if Data.Open then
                  Icon := Image_Window_XPM.Get_Pixbuf;
                  Rooms.Set (Row, Type_Column, Icon);
                  Icon.Unref;
                  Rooms.Set (Row, Text_Column, "open");
               else
                  Icon := Image_Window_Closed_XPM.Get_Pixbuf;
                  Rooms.Set (Row, Type_Column, Icon);
                  Icon.Unref;
                  Rooms.Set (Row, Text_Column, "closed");
               end if;
            when Radiator_Thermostat | Radiator_Thermostat_Plus =>
               Rooms.Set (Row, Text_Column, "set");
               Rooms.Set (Row, Data.Mode);
               Rooms.Set
               (  Row,
                  Valve_Column,
                  (  Image
                     (  Integer (Float (Data.Valve_Position) * 100.0)
                     )
                  &  "%"
               )  );
               Rooms.Set_Set (Row, Data.Set_Temperature);
               if Rooms.History /= null then
                  Rooms.History.Feed
                  (  Data.Address,
                     Data.Valve_Position,
                     Now
                  );
                  Rooms.History.Feed
                  (  Data.Address,
                     Data.Set_Temperature,
                     Offset,
                     Now
                  );
               end if;
               if Data.Temperature /= Centigrade'First then
                  Rooms.Set_Is (Row, Data.Temperature);
                  Rooms.Set (Row, Drop_Is_Temperature_Column, False);
                  Rooms.Set
                  (  Row,
                     Time_Stamp_Column,
                     Image (Data.Received_At, True)
                  );
                  if Rooms.History /= null then
                     Rooms.History.Feed
                     (  Data.Address,
                        Data.Temperature,
                        Data.Received_At
                     );
                  end if;
                  Rooms.Set_Set (Row, Data.Set_Temperature);
               end if;
--             Device_Changed (Rooms.View, Rooms'Unchecked_Access);
            when Wall_Thermostat =>
               Rooms.Set (Row, Data.Mode);
               Rooms.Set_Is  (Row, Data.Temperature);
               Rooms.Set_Set (Row, Data.Set_Temperature);
               Rooms.Set
               (  Row,
                  Time_Stamp_Column,
                  Image (Now, True)
               );
               if Rooms.History /= null then
                  Rooms.History.Feed
                  (  Data.Address,
                     Data.Temperature,
                     Now
                  );
                  Rooms.History.Feed
                  (  Data.Address,
                     Data.Set_Temperature,
                     0.0,
                     Now
                  );
               end if;
            when others =>
               null;
         end case;
         if Data.Kind_Of in Radiator_Thermostat..Eco_Button then
            if Data.Battery_Low then
               Icon := Image_Battery_Low_XPM.Get_Pixbuf;
               Rooms.Set (Row, Battery_Column, Icon);
               Icon.Unref;
            else
               Icon := Image_Battery_High_XPM.Get_Pixbuf;
               Rooms.Set (Row, Battery_Column, Icon);
               Icon.Unref;
            end if;
            if Data.Link_Error then
               if Data.Error then
                  Icon := Image_Error_No_Link_XPM.Get_Pixbuf;
               else
                  Icon := Image_No_Link_XPM.Get_Pixbuf;
               end if;
            else
               if Data.Error then
                  Icon := Image_Error_Link_XPM.Get_Pixbuf;
               else
                  Icon := Image_Link_XPM.Get_Pixbuf;
               end if;
            end if;
            Rooms.Set (Row, Link_Column, Icon);
            Icon.Unref;
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Update_Data")
         )  );
   end Update_Data;

   procedure Update_Devices
             (  Rooms     : in out Rooms_List_Record;
                Handler   : Settings_Handler_Ptr;
                Address   : RF_Address;
                Serial_No : String;
                Source    : GNAT.Sockets.Sock_Addr_Type;
                Devices   : Devices_Maps.Map;
                List      : Rooms_Maps.Map;
                Expected  : in out Natural
             )  is
      No_IO   : IO_Blocker;
      Parent  : Gtk_Tree_Iter;
      Updated : Room_Flags := (others => False);

      procedure Add_New_Room (Key : Room_ID; Name : String) is
         This    : Gtk_Tree_Iter := Rooms.Find (Parent, Key);
         Picture : Gdk_Pixbuf;
         Order   : RF_Address_Array renames
                   Restore (Rooms'Access, Address, Key).Ptr.List;
      begin
         if This = Null_Iter then
            Rooms.List.Insert (This, Parent, -1);
            Rooms.Set (This, Name_Column, Name);
            Rooms.Set (This, Room_ID_Column, GInt (Key));
            Picture := Image_Room_XPM.Get_Pixbuf;
            Rooms.Set (This, Type_Column, Picture);
            Picture.Unref;
            Rooms.Set (This, Drop_Is_Temperature_Column,  True);
            Rooms.Set (This, Drop_Set_Temperature_Column, True);
            Rooms.Set (This, Duty_Column, 0);
            Rooms.Set
            (  This,
               Device_Column,
               Device_Type'Pos (Unknown)
            );
            Rooms.Set (This, Operating_Mode_Column, GInt'(0));
         end if;
         for Index in 1..Devices.Get_Size loop
            declare
               Parameters : constant Device_Parameters :=
                                     Devices.Get (Index);
            begin
               if Parameters.Room = Key then
                  Rooms.Add
                  (  This,
                     Parameters,
                     Order,
                     Address,
                     Updated (Key)
                  );
               else
                  Rooms.Delete (This, Parameters, Updated (Key));
               end if;
            end;
         end loop;
      end Add_New_Room;

      function Keep (Room : Room_ID) return Boolean is
      begin
         return List.Is_In (Room);
      end Keep;

      function Keep (Device : RF_Address) return Boolean is
      begin
         return Devices.Is_In (Device);
      end Keep;

      Saved : constant Room_Reference_Array := Restore (Address);
      Items : Room_Reference_Array
              (  1
              .. List.Get_Size + Devices.Get_Size
              );
      Count : Natural := 0;
   begin
      Parent := Rooms.Find (Address);
      if Parent = Null_Iter then
         Parent := Rooms.Add (Address, Serial_No, Source);
      end if;
         -- Setting up rooms
      for Room in 1..List.Get_Size loop
         Count := Count + 1;
         declare
            ID   : constant Room_ID := List.Get_Key (Room);
            Name : constant String  := List.Get (Room);
            This : Room_Reference_Handles.Handle renames Items (Count);
         begin
            This.Set
            (  new Room_Reference
                   (  Kind_Of     => Unknown,
                      Name_Length => Name'Length
            )      );
            This.Ptr.Room := ID;
            This.Ptr.Name := Name;
         end;
      end loop;
         -- Setting up roomless devices
      for Index in 1..Devices.Get_Size loop
         declare
            Parameters : constant Device_Parameters :=
                                  Devices.Get (Index);
         begin
            if Parameters.Room = No_Room then
               Count := Count + 1;
               declare
                  This : Room_Reference_Handles.Handle renames
                         Items (Count);
               begin
                  This.Set
                  (  new Room_Reference
                         (  Kind_Of     => Parameters.Kind_Of,
                            Name_Length => Parameters.Name_Length
                  )      );
                  This.Ptr.Parameters := Parameters;
               end;
            end if;
         end;
      end loop;
         -- Inserting items
      if Saved'Length > 0 then
         declare
            Unsorted : Room_Reference_Array := Items;
            To       : Positive := Items'First;
         begin
            for Index in Saved'Range loop
               for From in Unsorted'Range loop
                  if Saved (Index) = Unsorted (From) then
                     Items (To) := Unsorted (From);
                     To := To + 1;
                     Unsorted (From).Invalidate;
                     exit;
                  end if;
               end loop;
            end loop;
            for From in Unsorted'Range loop
               if Unsorted (From).Is_Valid then
                  Items (To) := Unsorted (From);
                  To := To + 1;
               end if;
            end loop;
         end;
      end if;
      for Index in 1..Count loop
         declare
            Added : Boolean;
            This  : Room_Reference'Class renames Items (Index).Ptr.all;
         begin
            case This.Kind_Of is
               when Cube |
                  Radiator_Thermostat..Wall_Thermostat |
                  Shutter_Contact |
                  Eco_Button =>
                  Rooms.Add
                  (  Parent,
                     This.Parameters,
                     (1..0 => 0),
                     Address,
                     Added
                  );
               when Unknown =>
                  Add_New_Room (This.Room, This.Name);
            end case;
         end;
      end loop;
         -- Deleting non-existing devices
      Rooms.Purge (Address, Updated, Keep'Access, Keep'Access);
      for Index in 1..List.Get_Size loop
         declare
            Key : constant Room_ID := List.Get_Key (Index);
         begin
            if Updated (Key) then
               if Rooms.History /= null then
                  Rooms.History.Delete (Address, Key);
                  Rooms.History.Add (Address, Key, List.Get (Index));
                  Rooms.History.Reorder
                  (  Address,
                     Key,
                     GLib.Gint
                     (  Rooms.Get_Room_Position (Address, Key) - 1
                  )  );
               end if;
            end if;
         end;
      end loop;
      Rooms.View.Expand_All;
      if Handler = null then
         if Expected > 0 then
            Expected := Expected - 1;
         end if;
      else
         Handler.On_A_Response (Address, Devices, List, Expected);
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Update_Devices")
         )  );
   end Update_Devices;

   procedure Update_Disconnected
             (  Rooms : in out Rooms_List_Record;
                ID    : Cube_Descriptor
             )  is
      No_IO : IO_Blocker;
   begin
      if View /= null then
         View.Trace
         (  GNAT.Sockets.Image (ID.Address)
         &  ' '
         &  ID.Serial_No
         &  ' '
         &  ID.Name
         &  " disconnected"
         );
      end if;
      Rooms.Update_Selected_Cube;
      declare
         Cube : constant Gtk_Tree_Iter := Rooms.Find (ID);
         Icon : Gdk_Pixbuf;
      begin
         if Cube /= Null_Iter then
            Icon := Image_Cube_Crossed_XPM.Get_Pixbuf;
            Rooms.Set (Cube, Type_Column, Icon);
            Icon.Unref;
            Rooms.Set
            (  Cube,
               Time_Stamp_Column,
               "disconnected " & Image (Clock, True)
            );
            Icon := Image_Error_No_Link_XPM.Get_Pixbuf;
            Rooms.Set (Cube, Link_Column, Icon);
            Icon.Unref;
         end if;
      end;

--        This := Rooms.Find (ID); -- Find the cube
--        if This = Null_Iter then
--           return;
--        end if;
--        Cube := Rooms.Get (This);
--        This := Rooms.List.Children (This); -- Get first room or device
--        while This /= Null_Iter loop
--           if (  Rooms.History /= null
--              and then
--                 Unknown = Rooms.Get (This) -- This is a room
--              )  then
--              Rooms.History.Delete (Cube, Rooms.Get (This));
--           end if;
--           Rooms.List.Remove (This);
--        end loop;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Update_Disconnected")
         )  );
   end Update_Disconnected;

   procedure Update_Parameters
             (  Rooms      : in out Rooms_List_Record;
                Cube       : RF_Address;
                Parameters : Device_Parameters
             )  is
      Added  : Boolean;
      No_IO  : IO_Blocker;
      Parent : Gtk_Tree_Iter;
   begin
      Parent := Rooms.Find (Cube);
      if Parent /= Null_Iter then -- Have the cube
         if Parameters.Room = No_Room then
            Rooms.Add (Parent, Parameters, (1..0 => 0), Cube, Added);
         else
            Parent := Rooms.Find (Parent, Parameters.Room);
            if Parent /= Null_Iter then
               declare
                   Name : constant String :=
                                   Rooms.Get (Parent, Name_Column);
               begin
                  Rooms.Add
                  (  Parent,
                     Parameters,
                     Restore
                     (  Rooms'Access,
                        Cube,
                        Parameters.Room
                     ) .Ptr.List,
                     Cube,
                     Added
                  );
                  if Rooms.History /= null then
                     Rooms.History.Delete (Cube, Parameters.Room);
                     Rooms.History.Add (Cube, Parameters.Room, Name);
                     Rooms.Reorder_Graphs;
                  end if;
               end;
            end if;
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Update_Parameters")
         )  );
   end Update_Parameters;

   procedure Update_Reports (Data : Device_Data) is
      function Is_Low return Boolean is
      begin
--           if Data.Address = 16#0B7E19# then
--              if Clock - Start > 30.0 then
--                 return False;
--              elsif Clock - Start > 10.0 then
--                 return True;
--              end if;
--           elsif Data.Address = 16#0C9809# then
--              if Clock - Start > 20.0 then
--                 return True;
--              else
--                 return False;
--              end if;
--           end if;
         return Data.Battery_Low;
      end Is_Low;

      function Get_Stamp (Text : String) return String is
      begin
         for Index in Text'Range loop
            if Text (Index) = '@' then
               exit when Text'Last - Index /= 16;
               return Text (Index + 1..Text'Last);
            end if;
         end loop;
         return Image (Clock);
      end Get_Stamp;

      Index : Integer;
   begin
      Index := Reports.Find (Data.Address);
      if Index <= 0 then -- Not cached yet
         declare
            Report : constant String :=
                              Restore (Image (Data.Address), "");
         begin
            if Report = "" then -- Not saved
               Reports.Add
               (  Data.Address,
                  (  Reported    => False,
                     Battery_Low => Is_Low,
                     Stamp       => Image (Clock)
               )  );
            else -- Saved
               Reports.Add
               (  Data.Address,
                  (  Reported    => True,
                     Battery_Low => Report (Report'First) = 'l',
                     Stamp       => Get_Stamp (Report)
               )  );
            end if;
            Index := Reports.Find (Data.Address);
         end;
      end if;
      if Is_Low /= Reports.Get (Index).Battery_Low then
         Reports.Replace
         (  Index,
            (  Reported    => False,
               Battery_Low => Is_Low,
               Stamp       => Image (Clock)
         )  );
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Update_Reports")
         )  );
   end Update_Reports;

   procedure Update_Selected_Cube
             (  List          : access Rooms_List_Record;
                Do_Reconnect  : Boolean := False;
                Do_Disconnect : Boolean := False;
                Do_Reboot     : Boolean := False
             )  is
      use GNAT.Sockets;
      Address : Inet_Addr_Type;
   begin
      List.Get_Selected_Cube (Address);
      if Address = No_Inet_Addr then
         List.Disconnect.Hide;
         List.Reboot.Hide;
         List.Reconnect.Hide;
      else
         declare
            Cube : constant Cube_Client_Handle := Get_Cube (Address);
         begin
            if Cube.Is_Valid then
               if Cube.Ptr.Is_Connected then
                  List.Disconnect.Show;
                  List.Reconnect.Hide;
                  if Do_Disconnect then
                     Cube.Ptr.Reconnect (False, False);
                  end if;
               else
                  List.Disconnect.Hide;
                  List.Reconnect.Show;
                  if Do_Reconnect then
                     Cube.Ptr.Reconnect (True, False);
                  end if;
               end if;
               List.Reboot.Show;
               if Do_Reboot then
                  Trace
                  (  "Rebooting cube " & Cube.Ptr.Get_Serial_No,
                     Message_Text
                  );
                  Reboot (Cube.Ptr.Get_Serial_No);
               end if;
            else
               List.Disconnect.Hide;
               List.Reboot.Hide;
               List.Reconnect.Hide;
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
            &  Where ("Update_Selected_Cube")
         )  );
   end Update_Selected_Cube;

   procedure Update_Status
             (  Rooms    : in out Rooms_List_Record;
                Handler  : Settings_Handler_Ptr;
                Cube     : in out Cube_Client'Class;
                Error    : Boolean;
                Duty     : Ratio;
                Slots    : Natural;
                Expected : in out Natural
             )  is
      No_IO : IO_Blocker;
      This  : Gtk_Tree_Iter;
   begin
      if Slots /= Natural'Last then
         This := Rooms.Find (Cube.Get_RF_Address);
         Rooms.Set
         (  This,
            Duty_Column,
            GInt (Float (Duty) * 100.0)
         );
      end if;
      if Handler = null then
         if Expected > 0 then
            Expected := Expected - 1;
         end if;
      else
         Handler.On_S_Response
         (  Cube'Unchecked_Access,
            Error,
            Duty,
            Slots,
            Expected
         );
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
            &  Where ("Update_Status (generic)")
         )  );
   end Update_Status;

   procedure Update_Version
             (  Rooms   : in out Rooms_List_Record;
                Major   : Natural;
                Minor   : Natural;
                Version : Version_Status
             )  is
   begin
      if Rooms.Version_Label = null then
         return;
      end if;
      case Version is
         when Outdated =>
            Rooms.Version_Label.Set_Text
            (  "A newer version "
            &  Image (Major)
            &  '.'
            &  Image (Minor)
            &  " is available"
            );
         when Uptodate =>
            Rooms.Version_Label.Set_Text
            (  "The latest version is installed"
            );
         when Unknown =>
            Rooms.Version_Label.Set_Text
            (  "Version check failed, try later"
            );
      end case;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Update_Version")
         )  );
   end Update_Version;

end MAX_Rooms_List;
