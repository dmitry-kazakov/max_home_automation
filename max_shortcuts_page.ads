--                                                                    --
--  package MAX_Shortcuts_Page      Copyright (c)  Dmitry A. Kazakov  --
--     Shortcuts page                              Luebeck            --
--  Interface                                      Autumn, 2020       --
--                                                                    --
--                                Last revision :  14:41 02 Dec 2020  --
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

with GLib;                     use GLib;
with GLib.Object;              use GLib.Object;
with GLib.Values;              use GLib.Values;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Cell_Renderer;        use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Combo;  use Gtk.Cell_Renderer_Combo;
with Gtk.Cell_Renderer_Fixed;  use Gtk.Cell_Renderer_Fixed;
with Gtk.List_Store;           use Gtk.List_Store;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Missed;               use Gtk.Missed;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with MAX_IO;                   use MAX_IO;
with MAX_Rooms_List;           use MAX_Rooms_List;

with Gtk.Handlers;
with Gtk.Tree_Model.Extension_Store;

with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
use  GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;

package MAX_Shortcuts_Page is
   use Gtk.Tree_Model.Extension_Store;

   type MAX_Shortcuts_Record
        (  Shorcut_Count : Positive
        )  is new Gtk_Widget_Record
              and Device_List_Monior_Interface with private;
   type MAX_Shortcuts is access all MAX_Shortcuts_Record'Class;
   function Gtk_Shortcuts_New
            (  Rooms         : not null access Rooms_List_Record'Class;
               Shorcut_Count : Positive
            )  return MAX_Shortcuts;
--
-- Add_Button -- A thermostat settings shortcut button
--
--    Data   - The shortcuts pane
--    No     - The shorcut number
--    Button - The button to execute the shorcut
--
   procedure Add_Button
             (  Data   : not null access MAX_Shortcuts_Record'Class;
                No     : Positive;
                Button : not null access Gtk_Button_Record'Class
             );
   overriding
      procedure Device_Added
                (  Data   : in out MAX_Shortcuts_Record;
                   Cube   : RF_Address;
                   Device : Device_Parameters
                );
--
-- Execute -- Thermostat settings shortcut
--
--    Data - The shortcuts pane
--    No   - The shorcut number
--
   procedure Execute
             (  Data : not null access MAX_Shortcuts_Record;
                No   : Positive
             );

private
   function Get_Prefix
            (  No      : Positive;
               Address : RF_Address
            )  return String;

   type Shortcut_Store_Record is
      new Gtk_Extension_Store_Record with null record;
   type Shortcut_Store is access all Shortcut_Store_Record'Class;
   overriding
      procedure Changed
                (  Model : not null access Shortcut_Store_Record;
                   Path  : Gtk_Tree_Path;
                   Iter  : Gtk_Tree_Iter
                );
   function Get
            (  Model  : not null access Shortcut_Store_Record;
               Row    : Gtk_Tree_Iter;
               Column : Positive;
               First  : GDouble;
               Last   : GDouble
            )  return GDouble;
   procedure Set
             (  Model  : not null access Shortcut_Store_Record;
                Row    : Gtk_Tree_Iter;
                Column : Positive;
                Value  : GDouble
             );

   type Button_Array   is array (Positive range <>) of Gtk_Button;
   type Entry_Array    is array (Positive range <>) of Gtk_GEntry;
   type Shortcut_Array is array (Positive range <>) of Shortcut;
   type Combo_Cells is
      array (Positive range <>) of Gtk_Cell_Renderer_Combo;
   type Value_Cells is
      array (Positive range <>) of Gtk_Cell_Renderer_Fixed;

   type MAX_Shortcuts_Record
        (  Shorcut_Count : Positive
        )  is new Gtk_VBox_Record
              and Device_List_Monior_Interface with
   record
      Rooms       : Rooms_List;
      Store       : Shortcut_Store;
      View        : Gtk_Tree_View;
      Modes       : Gtk_List_Store;
      Button      : Button_Array   (1..Shorcut_Count);
      Combo       : Combo_Cells    (1..Shorcut_Count);
      Temperature : Value_Cells    (1..Shorcut_Count);
      Timeout     : Value_Cells    (1..Shorcut_Count);
      Shortcuts   : Shortcut_Array (1..Shorcut_Count);
      Entries     : Entry_Array    (1..Shorcut_Count);
   end record;
   function Button_To_No
            (  Data   : MAX_Shortcuts_Record;
               Button : not null access GObject_Record'Class
            )  return Natural;
   function Combo_To_No
            (  Data : MAX_Shortcuts_Record;
               Cell : not null access Gtk_Cell_Renderer_Record'Class
            )  return Natural;
   function Entry_To_No
            (  Data  : MAX_Shortcuts_Record;
               Edit : not null access GObject_Record'Class
            )  return Natural;
   function Get_Key
            (  Data : MAX_Shortcuts_Record;
               Row  : Gtk_Tree_Iter
            )  return Shortcut_Target;
   function Get_Mode
            (  Data : MAX_Shortcuts_Record;
               Row  : Gtk_Tree_Iter;
               No   : Positive
            )  return Shortcut_Mode;
   procedure Store_Shortcut
             (  Data   : in out MAX_Shortcuts_Record;
                No     : Positive;
                Key    : Shortcut_Target;
                Action : Shortcut_Action
             );
   function Temperature_To_No
            (  Data : MAX_Shortcuts_Record;
               Cell : not null access
                      Gtk_Cell_Renderer_Fixed_Record'Class
            )  return Natural;
   function Timeout_To_No
            (  Data : MAX_Shortcuts_Record;
               Cell : not null access
                      Gtk_Cell_Renderer_Fixed_Record'Class
            )  return Natural;

   package Combo_Handlers is new Gtk.Handlers.User_Callback
           (  Gtk_Cell_Renderer_Combo_Record,
              MAX_Shortcuts
           );
   package Shortcuts_Handlers is new Gtk.Handlers.User_Callback
           (  GObject_Record,
              MAX_Shortcuts
           );
   package Set_Mode_Cell_Data is
      new Gtk.Missed.Set_Column_Cell_Data (MAX_Shortcuts);

   package Value_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Cell_Renderer_Fixed_Record,
             MAX_Shortcuts
          );

   procedure Commit_Temperature
             (  Cell : access Gtk_Cell_Renderer_Fixed_Record'Class;
                Data : MAX_Shortcuts
             );
   procedure Commit_Timeout
             (  Cell : access Gtk_Cell_Renderer_Fixed_Record'Class;
                Data : MAX_Shortcuts
             );

   procedure On_Play
             (  Object : access GObject_Record'Class;
                Data   : MAX_Shortcuts
             );

   package Tree_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Root_Tree_Model_Record,
             MAX_Shortcuts
          );

   procedure Combo_Cell_Data
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Combo  : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Row    : Gtk_Tree_Iter;
                Data   : MAX_Shortcuts
             );
   procedure On_Changed
             (  Combo  : access Gtk_Cell_Renderer_Combo_Record'Class;
                Params : GValues;
                Data   : MAX_Shortcuts
             );
   procedure On_Entry_Changed
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Data   : MAX_Shortcuts
             );
   procedure On_Row_Inserted
             (  Reference : access Gtk_Root_Tree_Model_Record'Class;
                Params    : GValues;
                Data      : MAX_Shortcuts
             );

   function Restore (Prefix : String) return Centigrade;
   function Restore (Prefix : String) return Duration;

end MAX_Shortcuts_Page;
