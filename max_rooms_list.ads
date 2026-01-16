--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Rooms_List                              Luebeck            --
--  Interface                                      Summer, 2015       --
--                                                                    --
--                                Last revision :  15:58 12 Jan 2021  --
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

with Ada.Calendar;             use Ada.Calendar;
with GLib;                     use GLib;
with GLib.Object;              use GLib.Object;
with GLib.Values;              use GLib.Values;
with Gdk.Event;                use Gdk.Event;
with Gdk.Pixbuf;               use Gdk.Pixbuf;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Calendar;             use Gtk.Calendar;
with Gtk.Cell_Renderer;        use Gtk.Cell_Renderer;
with Gtk.Combo_Box_Text;       use Gtk.Combo_Box_Text;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.File_Chooser_Dialog;  use Gtk.File_Chooser_Dialog;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Image;                use Gtk.Image;
with Gtk.Label;                use Gtk.Label;
with Gtk.List_Store;           use Gtk.List_Store;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with MAX_Control_Page;         use MAX_Control_Page;
with MAX_Database_Page;        use MAX_Database_Page;
with MAX_Graphs;               use MAX_Graphs;
with MAX_Icon_Factory;         use MAX_Icon_Factory;
with MAX_IO;                   use MAX_IO;
with MAX_Mail_Page;            use MAX_Mail_Page;
with MAX_Trace;                use MAX_Trace;

with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
use  GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;

with Generic_Map;
with GLib.Main;
with GLib.Values;
with Gtk.Handlers;
with Gtk.Missed;
with Object.Handle;

package MAX_Rooms_List is

   Max_Duty                    : constant Ratio := 0.9;

   Type_Column                 : constant := 0;
   Address_Column              : constant := 1;
   Is_Temperature_Column       : constant := 2;
   Set_Temperature_Column      : constant := 3;
   Text_Column                 : constant := 4;
   Serial_No_Column            : constant := 5;
   Valve_Column                : constant := 6;
   Duty_Column                 : constant := 7;
   Battery_Column              : constant := 8;
   Link_Column                 : constant := 9;
   Name_Column                 : constant := 10;
   IP_Column                   : constant := 11;
   Room_ID_Column              : constant := 12;
   Drop_Is_Temperature_Column  : constant := 13;
   Drop_Set_Temperature_Column : constant := 14;
   Device_Column               : constant := 15;
   Operating_Mode_Column       : constant := 16;
   Time_Stamp_Column           : constant := 17;

   type Rooms_List_Record is
      new Gtk_Widget_Record
      and GUI_Interface
      and Detached_Interface
      and Settings_Handler_Interface with private;
   type Rooms_List is access all Rooms_List_Record'Class;

   procedure Add_Shortcuts
             (  Rooms   : not null access Rooms_List_Record;
                Handler : not null access
                          Device_List_Monior_Interface'Class
             );
   function Find
            (  Rooms   : not null access Rooms_List_Record;
               Address : RF_Address
            )  return Gtk_Tree_Iter;
   function Find
            (  Rooms : not null access Rooms_List_Record;
               Cube  : Gtk_Tree_Iter;
               Room  : Room_ID
            )  return Gtk_Tree_Iter;
   function Find_Cube
            (  Rooms : not null access Rooms_List_Record;
               Cube  : RF_Address
            )  return Gtk_Tree_Iter;
   function Get
            (  Rooms  : not null access Rooms_List_Record;
               Row    : Gtk_Tree_Iter
            )  return RF_Address;
   function Get
            (  Rooms  : not null access Rooms_List_Record;
               Row    : Gtk_Tree_Iter
            )  return Room_ID;
   function Get
            (  Rooms  : not null access Rooms_List_Record;
               Device : RF_Address
            )  return Room_ID;
   function Get
            (  Rooms  : not null access Rooms_List_Record;
               Row    : Gtk_Tree_Iter
            )  return Device_Type;
   procedure Get
             (  Rooms : not null access Rooms_List_Record;
                Row   : Gtk_Tree_Iter;
                Mode  : in out Operating_Mode;
                Valid : out Boolean
             );
   function Get
            (  Rooms  : not null access Rooms_List_Record;
               Cube   : RF_Address;
               Room   : String
            )  return Room_ID;
   function Get
            (  Rooms  : not null access Rooms_List_Record;
               Row    : Gtk_Tree_Iter;
               Column : GInt
            )  return String;
   function Get_Cube
            (  Rooms : not null access Rooms_List_Record;
               Row   : Gtk_Tree_Iter
            )  return RF_Address;
   function Get_List
            (  Rooms : not null access Rooms_List_Record
            )  return Gtk_Tree_Store;
   function Get_Logger
            (  Rooms : not null access Rooms_List_Record
            )  return MAX_Database;
   function Get_Play_Button
            (  Rooms : not null access Rooms_List_Record;
               Index : Positive
            )  return Gtk_Button;
   function Get_Rooms_Count
            (  Rooms  : not null access Rooms_List_Record;
               Row    : Gtk_Tree_Iter
            )  return Natural;
   function Get_Room_Name
            (  Rooms : not null access Rooms_List_Record;
               Row   : Gtk_Tree_Iter
            )  return String;
   function Get_Room_Name
            (  Rooms : not null access Rooms_List_Record;
               Cube  : RF_Address;
               Room  : Room_ID
            )  return String;
   procedure Get_Thermostats
             (  Rooms   : in out Rooms_List_Record;
                Store   : not null access Gtk_List_Store_Record'Class;
                Exclude : RF_Address
             );
   function Get_Room_Position
            (  Rooms : not null access Rooms_List_Record;
               Cube  : RF_Address;
               Room  : Room_ID
            )  return Positive;
   function Gtk_Rooms_List_New
            (  History  : not null access Graphs_Record'Class;
               Overview : not null access Graphs_Overview_Record'Class;
               Mails    : not null access MAX_Mail_Record'Class;
               Logger   : not null access MAX_Database_Record'Class;
               Control  : not null access MAX_Control_Record'Class
            )  return Rooms_List;
   function Get_Selected
            (  Rooms : not null access Rooms_List_Record
            )  return Gtk_Tree_Iter;
   procedure Hide_Mode (Rooms : in out Rooms_List_Record);
   procedure Pair_Detached
             (  Rooms     : not null access Rooms_List_Record;
                Cube      : RF_Address;
                Device    : RF_Address;
                Kind_Of   : Device_Type;
                Serial_No : String;
                Room      : String := "";
                Handler   : Settings_Handler_Ptr := null
             );
   procedure Pair_Missing
             (  Rooms     : not null access Rooms_List_Record;
                Cube      : RF_Address;
                Room      : String;
                Device    : RF_Address;
                Kind_Of   : Device_Type;
                Serial_No : String;
                Handler   : Pairing_Issuer_Ptr := null
             );
   procedure Reorder_Graphs (Rooms : not null access Rooms_List_Record);
   procedure Set_Pending_Restore
             (  Rooms   : in out Rooms_List_Record;
                Pending : Boolean
             );

   overriding
      procedure Canceled (Rooms : in out Rooms_List_Record);
   overriding
      procedure Detached_Device
                (  Rooms     : in out Rooms_List_Record;
                   Cube      : RF_Address;
                   Kind_Of   : Device_Type;
                   Device    : RF_Address;
                   Serial_No : String
                );
   overriding
      procedure Faulty_Device
                (  Rooms       : in out Rooms_List_Record;
                   Cube        : RF_Address;
                   Device      : RF_Address;
                   Length      : Natural;
                   Error       : Boolean;
                   Initialized : Boolean;
                   Orphaned    : Boolean
                );
   overriding
      procedure Finished (Rooms : in out Rooms_List_Record);
   overriding
      function Get_Object (Rooms : not null access Rooms_List_Record)
         return GObject;
   overriding
      procedure Paired_Device
                (  Rooms     : in out Rooms_List_Record;
                   Cube      : RF_Address;
                   Device    : Device_Type;
                   Address   : RF_Address;
                   Serial_No : String
                );
   overriding
      procedure Pairing_Ended (Rooms : in out Rooms_List_Record);
   overriding
      procedure Set_NTP_List
                (  Rooms : in out Rooms_List_Record;
                   Cube  : RF_Address;
                   List  : Servers_List.Set
                );
   overriding
      procedure Update_Connected
                (  Rooms : in out Rooms_List_Record;
                   ID    : Cube_Descriptor
                );
   overriding
      procedure Update_Cube
                (  Rooms     : in out Rooms_List_Record;
                   Address   : RF_Address;
                   Serial_No : String;
                   Source    : GNAT.Sockets.Sock_Addr_Type
                );
   overriding
      procedure Update_Cube_Statistics
                (  Rooms   : in out Rooms_List_Record;
                   Address : RF_Address;
                   Average : Natural
                );
   overriding
      procedure Update_Data
                (  Rooms   : in out Rooms_List_Record;
                   Cube    : RF_Address;
                   Data    : Device_Data;
                   Offset  : Centigrade
                );
   overriding
      procedure Update_Devices
                (  Rooms     : in out Rooms_List_Record;
                   Handler   : Settings_Handler_Ptr;
                   Address   : RF_Address;
                   Serial_No : String;
                   Source    : GNAT.Sockets.Sock_Addr_Type;
                   Devices   : Devices_Maps.Map;
                   List      : Rooms_Maps.Map;
                   Expected  : in out Natural
                );
   overriding
      procedure On_A_Response
                (  Rooms    : in out Rooms_List_Record;
                   Address  : RF_Address;
                   Devices  : Devices_Maps.Map;
                   List     : Rooms_Maps.Map;
                   Expected : in out Natural
                );
   overriding
      procedure On_S_Response
                (  Rooms    : in out Rooms_List_Record;
                   Cube     : access Cube_Client'Class;
                   Error    : Boolean;
                   Duty     : Ratio;
                   Slots    : Natural;
                   Expected : in out Natural
                );
   overriding
      procedure Set_Auto_Scale (Rooms : in out Rooms_List_Record);
   overriding
      procedure Set_Fixed_Scale
                (  Rooms : in out Rooms_List_Record;
                   Low   : Centigrade;
                   High  : Centigrade
                );
   overriding
      procedure Update_Disconnected
                (  Rooms : in out Rooms_List_Record;
                   ID    : Cube_Descriptor
                );
   overriding
      procedure Update_Parameters
                (  Rooms      : in out Rooms_List_Record;
                   Cube       : RF_Address;
                   Parameters : Device_Parameters
                );
   overriding
      procedure Update_Status
                (  Rooms    : in out Rooms_List_Record;
                   Handler  : Settings_Handler_Ptr;
                   Cube     : in out Cube_Client'Class;
                   Error    : Boolean;
                   Duty     : Ratio;
                   Slots    : Natural;
                   Expected : in out Natural
                );
   overriding
      procedure Update_Version
                (  Rooms   : in out Rooms_List_Record;
                   Major   : Natural;
                   Minor   : Natural;
                   Version : Version_Status
                );

   procedure Save_To_File (Name : UTF8_String; Box : RF_Address);
   procedure Set_Trace_Box (Box : Trace_Box);
   procedure Store
             (  Address     : RF_Address;
                Battery_Low : Boolean;
                Stamp       : String
             );
private
   function Guess_Type (Text : String) return Device_Type;

   type Device_Status_Report is record
      Reported    : Boolean := True;
      Battery_Low : Boolean := False;
      Stamp       : String (1..16);
   end record;
   package Device_To_Cache_Map is
      new Generic_Map (RF_Address, Device_Status_Report);

   type Room_Reference
        (  Kind_Of     : Device_Type;
           Name_Length : Natural
        )  is new Object.Entity with
   record
      case Kind_Of is
         when Radiator_Thermostat..Wall_Thermostat |
              Shutter_Contact                      |
              Eco_Button                           =>
            Parameters : Device_Parameters (Kind_Of, Name_Length);
         when Cube =>
            Address : RF_Address;
         when Unknown =>
            Room : Room_ID;
            Name : String (1..Name_Length);
      end case;
   end record;
   type Room_Reference_Ptr is access Room_Reference'Class;
   function "=" (Left, Right : Room_Reference'Class) return Boolean;
   package Room_Reference_Handles is
      new Object.Handle (Room_Reference, Room_Reference_Ptr);
   function "=" (Left, Right : Room_Reference_Handles.Handle)
      return Boolean;
   type Room_Reference_Array is
      array (Positive range <>) of Room_Reference_Handles.Handle;
   function Restore return RF_Address_Array;
   function Restore (Cube : RF_Address) return Room_Reference_Array;

   type Restore_Item (Size : Natural) is new Object.Entity with record
      List : aliased RF_Address_Array (1..Size);
   end record;
   type Restore_Item_Ptr is access Restore_Item'Class;
   package Restore_Item_Handles is
      new Object.Handle (Restore_Item, Restore_Item_Ptr);
   type Restore_Key is record
      Cube : RF_Address;
      Room : Room_ID;
   end record;
   function "<" (Left, Right : Restore_Key) return Boolean;
   package Restore_Item_Maps is
      new Generic_Map (Restore_Key, Restore_Item_Handles.Handle);

   type Ping_Icon_List is array (0..5) of Gtk_Image;

   type Faulty_Device_Data
        (  Pending_Delete : Boolean;
           Name_Length    : Natural;
           Kind_Of_Length : Natural
        )  is new Object.Entity with
   record
      Kind_Of   : Device_Type    := Unknown;
      Poll_No   : Poll_Count     := 0;
      Queried   : Boolean        := True;
      Serial_No : String (1..10) := "?         ";
      Guessed   : String (1..Kind_Of_Length);
      case Pending_Delete is
         when True =>
            Deleted : Time;
            Name    : String (1..Name_Length);
         when False =>
            Length      : Natural := 0;
            Error       : Boolean := False;
            Initialized : Boolean := True;
            Orphaned    : Boolean := True;
      end case;
   end record;
   type Faulty_Device_Data_Ptr is access Faulty_Device_Data'Class;
   package Faulty_Device_Data_Handles is
      new Object.Handle (Faulty_Device_Data, Faulty_Device_Data_Ptr);

   package Faulty_Device_Maps is
      new Generic_Map (RF_Address, Faulty_Device_Data_Handles.Handle);
   type Faulty_Device_Map is new Object.Entity with record
      Map : Faulty_Device_Maps.Map;
   end record;
   type Faulty_Device_Map_Ptr is access Faulty_Device_Map'Class;
   package Faulty_Device_Map_Handles is
      new Object.Handle (Faulty_Device_Map, Faulty_Device_Map_Ptr);
   package Faulty_Cube_Maps is
      new Generic_Map (RF_Address, Faulty_Device_Map_Handles.Handle);

   package NTP_Servers_Maps is
      new Generic_Map (RF_Address, Servers_List.Set);

   function Keep_All (Room : Room_ID     ) return Boolean;
   function Keep_All (Device : RF_Address) return Boolean;

   type Play_Button_Array is
      array (1..Number_Of_Shortcuts) of Gtk_Button;
   type Room_Flags is array (Room_ID) of Boolean;
   type Rooms_List_Record is
      new Gtk_VBox_Record
      and GUI_Interface
      and Settings_Handler_Interface with
   record
      Buttons         : Gtk_HBox;
      Mode_Label      : Gtk_Label;
      Mode            : Gtk_Combo_Box_Text;
      Display         : Gtk_Combo_Box_Text;
      Temperature     : Gtk_GEntry;
      Date            : Gtk_GEntry;
      Degree_Label    : Gtk_Label;
      Version_Label   : Gtk_Label;
      View            : Gtk_Tree_View;
      List            : Gtk_Tree_Store;
      Right_Click     : Gtk_Tree_Iter;   -- Volatile
      Restoring       : Boolean := False;

      Thermostats     : RF_Address_Sets.Set;
      Mails           : MAX_Mail;
      Logger          : MAX_Database;
      History         : Graphs;
      Overview        : Graphs_Overview;
      Timer           : GLib.Main.G_Source_ID := GLib.Main.No_Source_ID;
         -- Ping icons
      Ping_State      : Natural := 0;
      Ping_Icons      : Ping_Icon_List;
      Next_Poll       : Gtk_Label;
         -- Buttons
      About           : About_Buttons.Gtk_Style_Button;
      Add_Cube        : Add_Buttons.Gtk_Style_Button;
      Add_Device      : Add_Device_Buttons.Gtk_Style_Button;
      All_Thermostats : Select_Thermostats_Buttons.Gtk_Style_Button;
      Calendar        : Calendar_Buttons.Gtk_Style_Button;
      Configure       : Configure_Buttons.Gtk_Style_Button;
      Delete_Device   : Delete_Buttons.Gtk_Style_Button;
      Export_To_MAX   : MAX_Buttons.Gtk_Style_Button;
      Faulty          : Faulty_Devices_Buttons.Gtk_Style_Button;
      Move            : Move_Buttons.Gtk_Style_Button;
      Move_Down       : Down_Buttons.Gtk_Style_Button;
      Move_Up         : Up_Buttons.Gtk_Style_Button;
      Set_NTP         : NTP_Buttons.Gtk_Style_Button;
      Wake_Up         : Wake_Up_Buttons.Gtk_Style_Button;
      Disconnect      : Disconnect_Buttons.Gtk_Style_Button;
      Play            : Play_Button_Array;
      Reboot          : Reboot_Buttons.Gtk_Style_Button;
      Reconnect       : Reconnect_Buttons.Gtk_Style_Button;
      Rename          : Rename_Buttons.Gtk_Style_Button;
      Reset           : Reset_Buttons.Gtk_Style_Button;
      Restore         : Restore_Buttons.Gtk_Style_Button;
      Save            : Save_As_Buttons.Gtk_Style_Button;
      Scan            : Network_Scan_Buttons.Gtk_Style_Button;
      Store           : Set_Thermostat_Buttons.Gtk_Style_Button;

      Orders          : Restore_Item_Maps.Map;
         -- Pairing
      Pairing         : Pairing_Interface_Ptr;
         -- Faulty devices
      Deleting        : Detached_Interface_Ptr;
      Faulty_List     : Faulty_Cube_Maps.Map;
      Poll_No         : Poll_Count := 0;
          -- Shortcuts
      Shortcuts       : Device_List_Monior_Ptr;

      NTP_Servers     : NTP_Servers_Maps.Map;
   end record;

   function Add
            (  Rooms     : not null access Rooms_List_Record;
               Address   : RF_Address;
               Serial_No : String;
               Source    : GNAT.Sockets.Sock_Addr_Type
            )  return Gtk_Tree_Iter;
   procedure Add
             (  Rooms   : not null access Rooms_List_Record;
                Room    : Gtk_Tree_Iter;
                Device  : Device_Parameters;
                Order   : RF_Address_Array;
                Address : RF_Address;
                Added   : out Boolean
             );
   procedure Check_Move_Buttons
             (  Rooms : not null access Rooms_List_Record;
                Row   : Gtk_Tree_Iter
             );
   procedure Delete
             (  Rooms   : not null access Rooms_List_Record;
                Room    : Gtk_Tree_Iter;
                Device  : Device_Parameters;
                Deleted : out Boolean
             );
   function Find
            (  Rooms : not null access Rooms_List_Record;
               ID    : Cube_Descriptor
            )  return Gtk_Tree_Iter;
   function Get_Is
            (  Rooms : not null access Rooms_List_Record;
               Row   : Gtk_Tree_Iter
            )  return Centigrade;
   procedure Get_Selected
             (  Rooms   : not null access Rooms_List_Record;
                Box     : out RF_Address;
                Device  : out RF_Address;
                Kind_Of : out Device_Type
             );
   procedure Get_Selected_Cube
             (  Rooms : not null access Rooms_List_Record;
                Box   : out RF_Address
             );
   procedure Get_Selected_Cube
             (  Rooms   : not null access Rooms_List_Record;
                Address : out GNAT.Sockets.Inet_Addr_Type
             );
   function Get_Set
            (  Rooms : not null access Rooms_List_Record;
               Row   : Gtk_Tree_Iter
            )  return Centigrade;
   procedure Get_Thermostats
             (  Rooms : not null access Rooms_List_Record;
                Row   : Gtk_Tree_Iter;
                List  : in out RF_Address_Sets.Set
             );
   function Has_Thermostats
            (  Rooms : not null access Rooms_List_Record;
               Row   : Gtk_Tree_Iter
            )  return Boolean;
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
             );
   function Restore
            (  Rooms : not null access Rooms_List_Record;
               Cube  : RF_Address;
               Room  : Room_ID
            )  return Restore_Item_Handles.Handle;
   procedure Select_Thermostats
             (  Rooms        : not null access Rooms_List_Record;
                Row          : Gtk_Tree_Iter;
                Deselect_Row : Boolean
             );
   procedure Set
             (  Rooms : not null access Rooms_List_Record;
                Row   : Gtk_Tree_Iter;
                Mode  : Operating_Mode
             );
   procedure Set
             (  Rooms  : not null access Rooms_List_Record;
                Row    : Gtk_Tree_Iter;
                Column : GInt;
                Icon   : Gdk_Pixbuf
             );
   procedure Set
             (  Rooms  : not null access Rooms_List_Record;
                Row    : Gtk_Tree_Iter;
                Column : GInt;
                Text   : String
             );
   procedure Set
             (  Rooms  : not null access Rooms_List_Record;
                Row    : Gtk_Tree_Iter;
                Column : GInt;
                Data   : GInt
             );
   procedure Set
             (  Rooms  : not null access Rooms_List_Record;
                Row    : Gtk_Tree_Iter;
                Column : GInt;
                Flag   : Boolean
             );
   procedure Set_Is
             (  Rooms       : not null access Rooms_List_Record;
                Device      : Gtk_Tree_Iter;
                Temperature : Centigrade
             );
   procedure Set_Set
             (  Rooms       : not null access Rooms_List_Record;
                Device      : Gtk_Tree_Iter;
                Temperature : Centigrade
             );
   procedure Store_Order
             (  Rooms : not null access Rooms_List_Record;
                Row   : Gtk_Tree_Iter
             );

   package Event_Handlers is
      new Gtk.Handlers.User_Return_Callback
          (  GObject_Record,
             Boolean,
             Rooms_List
          );
   package List_Handlers is new Gtk.Handlers.User_Callback
           (  GObject_Record,
              Rooms_List
           );
   procedure Activated
             (  Object : access GObject_Record'Class;
                Params : GValues;
                List   : Rooms_List
             );
   procedure Device_Changed
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure Display_Changed
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure Mode_Changed
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Add
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Add_Device
             (  Object   : access GObject_Record'Class;
                List     : Rooms_List
             );
   procedure On_Add_Device_Menu
             (  Object   : access GObject_Record'Class;
                List     : Rooms_List
             );
   procedure On_About
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   function On_Button_Press
            (  Object : access GObject_Record'Class;
               Event  : Gdk_Event;
               List   : Rooms_List
            )  return Boolean;
   procedure On_Calendar
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Configure
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Configure_Menu
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Delete
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Delete_Menu
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Destroy
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Disconnect
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Export_To_MAX
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Faulty
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Move
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Move_Menu
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Move_Down
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Move_Up
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Page_Switch
             (  Object : access GObject_Record'Class;
                Params : GLib.Values.GValues;
                List   : Rooms_List
             );
   procedure On_Reboot
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Reconnect
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Rename
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Rename_Menu
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Restore
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Save
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Scan
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Select_All_Thermostats
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Select_Thermostats
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Set_NTP
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Store
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Wake_Up
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure On_Wake_Up_Menu
             (  Object : access GObject_Record'Class;
                List   : Rooms_List
             );
   procedure Update_Selected_Cube
             (  List          : access Rooms_List_Record;
                Do_Reconnect  : Boolean := False;
                Do_Disconnect : Boolean := False;
                Do_Reboot     : Boolean := False
             );

   type Preview_Dialog_Record is
      new Gtk_File_Chooser_Dialog_Record with
   record
      Cube    : RF_Address;
      List    : Rooms_List;
      Preview : Gtk_Text_View;
      Buffer  : Gtk_Text_Buffer;
   end record;
   type Preview_Dialog is access all Preview_Dialog_Record;

   package Chooser_Handlers is new Gtk.Handlers.User_Callback
           (  GObject_Record,
              Preview_Dialog
           );
   procedure On_Update_Preview
             (  Object : access GObject_Record'Class;
                Dialog : Preview_Dialog
             );

   package Set_Mode_Cell_Data is
      new Gtk.Missed.Set_Column_Cell_Data (Rooms_List);

   procedure Link_Cell_Data
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Row    : Gtk_Tree_Iter;
                Rooms  : Rooms_List
             );
   procedure Mode_Cell_Data
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Row    : Gtk_Tree_Iter;
                Rooms  : Rooms_List
             );
   procedure Update_Reports (Data : Device_Data);

   type Add_Dialog_Record is new Gtk_Dialog_Record with record
      Edit : Gtk_GEntry;
   end record;
   type Add_Dialog is access all Add_Dialog_Record'Class;
   procedure On_Add_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             );

   type Calendar_Dialog_Record is new Gtk_Dialog_Record with record
      List       : Rooms_List;
      Calendar   : Gtk_Calendar;
      End_Hour   : Gtk_Combo_Box_Text;
      End_Minute : Gtk_Combo_Box_Text;
   end record;
   type Calendar_Dialog is access all Calendar_Dialog_Record'Class;
   procedure On_Calendar_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             );

   type Scan_Dialog_Record is new Gtk_Dialog_Record with record
      Edit : Gtk_GEntry;
   end record;
   type Scan_Dialog is access all Scan_Dialog_Record'Class;
   procedure On_Scan_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             );

end MAX_Rooms_List;
