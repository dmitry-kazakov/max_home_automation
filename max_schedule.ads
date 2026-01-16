--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Schedule                                Luebeck            --
--  Interface                                      Summer, 2015       --
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

with Ada.Calendar;             use Ada.Calendar;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with MAX_IO;                   use MAX_IO;
with MAX_Rooms_List;           use MAX_Rooms_List;
with Gdk.RGBA;                 use Gdk.RGBA;
with GLib;                     use GLib;
with GLib.Object;              use GLib.Object;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Cell_Renderer_Fixed;  use Gtk.Cell_Renderer_Fixed;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Combo_Box_Text;       use Gtk.Combo_Box_Text;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Event_Box;            use Gtk.Event_Box;
with Gtk.Image;                use Gtk.Image;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Label;                use Gtk.Label;
with Gtk.List_Store;           use Gtk.List_Store;
with Gtk.Notebook;             use Gtk.Notebook;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Widget;               use Gtk.Widget;
with Max_Icon_Factory;         use Max_Icon_Factory;

with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.
     Stream_IO;

with Generic_Map;
with Generic_Segmented_Stack;
with Gtk.Handlers;
with Gtk.Main.Router;

package MAX_Schedule is
   use GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
   use GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.
       Stream_IO;

   type Schedule_Record
        (  Kind_Of     : Device_Type;
           Name_Length : Natural
        )  is new Gtk_Widget_Record
              and Settings_Handler_Interface with private;
   type Schedule is access all Schedule_Record'Class;

   overriding
      procedure Canceled (Page : in out Schedule_Record);
   overriding
      procedure Finished (Page : in out Schedule_Record);
   overriding
      function Get_Object
               (  Page : not null access Schedule_Record
               )  return GObject;
   overriding
      procedure On_A_Response
                (  Page     : in out Schedule_Record;
                   Address  : RF_Address;
                   Devices  : Devices_Maps.Map;
                   List     : Rooms_Maps.Map;
                   Expected : in out Natural
                );
   overriding
      procedure On_S_Response
                (  Page     : in out Schedule_Record;
                   Cube     : access Cube_Client'Class;
                   Error    : Boolean;
                   Duty     : Ratio;
                   Slots    : Natural;
                   Expected : in out Natural
                );
   function Gtk_Schedule_New
            (  Pages      : not null access Gtk_Notebook_Record'Class;
               Rooms      : not null access Rooms_List_Record'Class;
               Parameters : Device_Parameters
            )  return Schedule;
   procedure Close
             (  Pages  : not null access Gtk_Notebook_Record'Class;
                Cube   : RF_Address;
                Device : RF_Address
             );
   procedure Configure
             (  Pages : not null access Gtk_Notebook_Record'Class;
                Rooms : not null access Rooms_List_Record'Class;
                Cube  : RF_Address;
                Data  : Device_Parameters_Data_Handles.Handle
             );
   procedure Rename
             (  Pages  : not null access Gtk_Notebook_Record'Class;
                Cube   : RF_Address;
                Device : RF_Address;
                Name   : String
             );
private
   type Day_View is record
      Day      : Week_Day;
      Parent   : Schedule;
      Box      : Gtk_Event_Box;
      View     : Gtk_Tree_View;
      All_Room : Gtk_Check_Button;
      List     : Gtk_List_Store;
   end record;
   type Day_View_Ptr is access all Day_View;
   type Week_View is array (Week_Day) of aliased Day_View;

   type Key_Type is record
      Box    : RF_Address;
      Device : RF_Address;
   end record;
   function "<" (Left, Right : Key_Type) return Boolean;

   type Action_Type is
       (  Begin_Item,
          End_Item,
          Delete_Item,
          Erase_Item,
          Insert_Item,
          Replace_Item
       );
   type Action_Item (To_Do : Action_Type := Begin_Item) is record
      Day : Week_Day;
      case To_Do is
         when Delete_Item =>
            Deleted  : Set_Point_Packed;
         when Insert_Item =>
            Inserted : Set_Point_Packed;
         when Replace_Item =>
            Replaced : Set_Point_Packed;
         when Begin_Item | End_Item | Erase_Item =>
            null;
      end case;
   end record;
   package Action_Stack is
      new Generic_Segmented_Stack
          (  Positive,
             Action_Item,
             (Begin_Item, Mo)
          );

   type Edit_Data is limited record
      Redo : Action_Stack.Segmented_Stack.Stack;
      Undo : Action_Stack.Segmented_Stack.Stack;
   end record;
   type Edit_Data_Ptr is access Edit_Data;

   type Schedule_Record
        (  Kind_Of     : Device_Type;
           Name_Length : Natural
        )  is new Gtk_VBox_Record
              and Settings_Handler_Interface with
   record
      Data           : Edit_Data_Ptr := new Edit_Data;
      Current        : Week_Day      := Mo;
      Edited         : Boolean       := False;
      Changed        : Boolean       := False;
      Valve          : Boolean       := False;
      Name           : Gtk_Label;
      Rooms          : Rooms_List;
      Parent         : Gtk_Notebook;
      Key            : Key_Type;
      Close          : Gtk_Event_Box;
      All_Room       : Gtk_Check_Button;
      Intact         : Gtk_Image;
      Modified       : Gtk_Image;
      Buttons        : Gtk_HBox;
      Header         : Gtk_HBox;
      Schedule       : Gtk_HBox;
      View           : Week_View;
      Selected       : Gdk_RGBA;
      Unselected     : Gdk_RGBA;
      File           : Unbounded_String;
         -- Buttons
      Delete         : Delete_Buttons.Gtk_Style_Button;
      Erase          : Erase_Buttons.Gtk_Style_Button;
      Copy           : Copy_Buttons.Gtk_Style_Button;
      Copy_From      : Copy_From_Buttons.Gtk_Style_Button;
      Cut            : Cut_Buttons.Gtk_Style_Button;
      Open           : Open_Buttons.Gtk_Style_Button;
      Paste          : Paste_Buttons.Gtk_Style_Button;
      Undo           : Undo_Buttons.Gtk_Style_Button;
      Redo           : Redo_Buttons.Gtk_Style_Button;
      Save           : Save_Buttons.Gtk_Style_Button;
      Save_As        : Save_As_Buttons.Gtk_Style_Button;
      Select_All     : Select_All_Buttons.Gtk_Style_Button;
      Store          : Set_Thermostat_Buttons.Gtk_Style_Button;
         -- Parameters
      Comfort        : Gtk_GEntry;
      Eco            : Gtk_GEntry;
      Max            : Gtk_GEntry;
      Min            : Gtk_GEntry;
      Offset         : Gtk_GEntry;
      Window_Open    : Gtk_GEntry;
      Window_Time    : Gtk_GEntry;
      Boost_Time     : Gtk_GEntry;
      Boost_Valve    : Gtk_GEntry;
      Max_Valve      : Gtk_GEntry;
      Valve_Offset   : Gtk_GEntry;
      Decalc_Time    : Gtk_Combo_Box_Text;
      Decalc_Day     : Gtk_Combo_Box_Text;
         -- Progress
      Thermostats    : RF_Address_Sets.Set;
      Status         : Store_Mode := Store_Start;
      Reported       : Boolean    := False;
      Parameters     : Device_Parameters (Kind_Of, Name_Length);
   end record;

   function Can_Close
            (  This : not null access Schedule_Record
            )  return Boolean;
   function Find (This : not null access Schedule_Record) return GInt;
   function Get
            (  Widget : not null access Schedule_Record
            )  return Week_Schedule;
   procedure Execute
             (  Widget : not null access Schedule_Record;
                Action : Action_Item;
                Depth  : in out Integer;
                Undo   : in out Action_Stack.Segmented_Stack.Stack
             );
   procedure Execute
             (  Widget : not null access Schedule_Record;
                Redo   : in out Action_Stack.Segmented_Stack.Stack;
                Undo   : in out Action_Stack.Segmented_Stack.Stack
             );
   procedure Replace
             (  Widget   : not null access Schedule_Record;
                Schedule : Week_Schedule
             );
   procedure Save_To_File
             (  Widget : not null access Schedule_Record;
                Name   : UTF8_String
             );
   procedure Set
             (  Widget   : not null access Schedule_Record;
                Day      : Week_Day;
                Schedule : Day_Schedule
             );
   procedure Set_Action_Buttons
             (  Widget : not null access Schedule_Record
             );
   function To_Duration
            (  Text     : String;
               Truncate : Boolean
            )  return Day_Duration;

   procedure Changed_Page
             (  Widget : access Gtk_Notebook_Record'Class;
                Page   : not null access Gtk_Widget_Record'Class;
                No     : GUInt
             );
   function On_Close
            (  Object : access GObject_Record'Class;
               Page   : Schedule
            )  return Boolean;
   procedure On_Copy
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );
   procedure On_Copy_From
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );
   procedure On_Cut
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );
   procedure On_Delete
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );
   procedure On_Destroy
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );
   procedure On_Erase
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );
   procedure On_Open
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );
   procedure On_Parameter_Changed
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );
   procedure On_Paste
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );
   procedure On_Redo
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );
   procedure On_Save
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );
   procedure On_Save_As
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );
   procedure On_Selection
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                Page      : Day_View_Ptr
             );
   function On_Selection
            (  Object : access GObject_Record'Class;
               Page   : Schedule
            )  return Boolean;
   procedure On_Select_All
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );
   procedure On_Store
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );
   procedure On_Thermostat
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                Button    : Gtk_Button
             );
   procedure On_Undo
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );
   procedure On_Valve_Changed
             (  Object : access GObject_Record'Class;
                Page   : Schedule
             );

   package Schedule_Handlers is
      new Gtk.Handlers.User_Callback (GObject_Record, Schedule);
   package Event_Handlers is
      new Gtk.Handlers.User_Return_Callback
          (  GObject_Record,
             Boolean,
             Schedule
          );
   package Temperature_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Cell_Renderer_Fixed_Record,
             Day_View_Ptr
          );
   package Edited_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Cell_Renderer_Text_Record,
             Day_View_Ptr
          );
   package Selection_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Tree_Selection_Record,
             Day_View_Ptr
          );
   package Chooser_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Tree_Selection_Record,
             Gtk_Button
          );
   package Scheduled_Devices_Maps is
      new Generic_Map (Key_Type, Schedule);

   function Delete
            (  Store : not null access Gtk_List_Store_Record'Class;
               Row   : Gtk_Tree_Iter
            )  return Set_Point;
   function Erase
            (  Store : not null access Gtk_List_Store_Record'Class
            )  return Points_List;
   procedure Find
             (  Store : not null access Gtk_List_Store_Record'Class;
                Time  : Day_Duration;
                Row   : out Gtk_Tree_Iter;
                Equal : out Boolean
             );
   function Get
            (  Store : not null access Gtk_List_Store_Record'Class;
               Row   : Gtk_Tree_Iter
            )  return Boolean;
   function Get
            (  Store : not null access Gtk_List_Store_Record'Class;
               Row   : Gtk_Tree_Iter
            )  return Centigrade;
   function Get
            (  Store : not null access Gtk_List_Store_Record'Class;
               Row   : Gtk_Tree_Iter
            )  return Day_Duration;
   function Get
            (  Store : not null access Gtk_List_Store_Record'Class
            )  return Points_List;
   function Get_Last
            (  Store : not null access Gtk_List_Store_Record'Class
            )  return Gtk_Tree_Iter;
   procedure Insert
             (  Store     : not null access Gtk_List_Store_Record'Class;
                Row       : Gtk_Tree_Iter;
                New_Point : Set_Point;
                Old_Point : out Set_Point;
                Full      : out Boolean
             );
   procedure Replace
             (  Store : not null access Gtk_List_Store_Record'Class;
                Row   : Gtk_Tree_Iter;
                Point : in out Set_Point
             );
   function Selected
            (  Store : not null access Gtk_List_Store_Record'Class;
               From  : not null access Gtk_Tree_Selection_Record'Class
            )  return Points_List;
   procedure Set
             (  Store : not null access Gtk_List_Store_Record'Class;
                Row      : Gtk_Tree_Iter;
                Until_Up : Day_Duration
             );
   procedure Set
             (  Store : not null access Gtk_List_Store_Record'Class;
                Row   : Gtk_Tree_Iter;
                Temperature : Centigrade
             );
   procedure Set
             (  Store : not null access Gtk_List_Store_Record'Class;
                Row   : Gtk_Tree_Iter;
                Flag  : Boolean
             );

   type Focus_Data is record
      Page  : Day_View_Ptr;
      Point : Set_Point;
   end record;

   procedure On_Focus_Temperature (Data : in out Focus_Data);
   procedure On_Focus_Until (Data : in out Focus_Data);

   package Focus_Messages is
      new Gtk.Main.Router.Generic_Message (Focus_Data);

   type Copy_From_Dialog_Record is new Gtk_Dialog_Record with record
      List : Gtk_List_Store;
      View : Gtk_Tree_View;
      Page : Schedule;
   end record;
   type Copy_From_Dialog is access all Copy_From_Dialog_Record'Class;
   procedure On_Copy_From_Response
             (  Self     : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             );

end MAX_Schedule;
