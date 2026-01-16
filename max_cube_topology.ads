--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Cube_Topology                           Luebeck            --
--  Interface                                      Spring, 2019       --
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

with GLib.Object;               use GLib.Object;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Cell_Renderer;         use Gtk.Cell_Renderer;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Notebook;              use Gtk.Notebook;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget;                use Gtk.Widget;
with MAX_Icon_Factory;          use MAX_Icon_Factory;
with MAX_IO;                    use MAX_IO;
with MAX_Rooms_List;            use MAX_Rooms_List;

with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
use  GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;

with Gtk.Handlers;

package MAX_Cube_Topology is

   Success : constant String := "gtk-apply";

   type Restore_Topology_Record is
      new Gtk_VBox_Record
      and Pairing_Issuer with
   record
      Parent           : Gtk_Notebook;
      Immediate_Parent : Gtk_Notebook;
      Rooms            : Rooms_List;
      Cube             : RF_Address;

      Cancel           : Cancel_Topology_Buttons.Gtk_Style_Button;
      OK               : Start_Topology_Buttons.Gtk_Style_Button;

      Warning          : Gtk_Image;
      Error            : Gtk_Image;
      Scroll           : Gtk_Scrolled_Window;
      View             : Gtk_Tree_View;
      List             : Gtk_Tree_Store;
      Action           : Gtk_Label;
      Current          : Gtk_Tree_Iter;
      Start            : Gtk_Tree_Iter;
      Repeat           : Boolean := False;
      Active           : Boolean := False;
   end record;

   type Restore_Topology is
      access all Restore_Topology_Record'Class;
   overriding
      procedure Canceled
                (  Restore : in out Restore_Topology_Record
                );
   overriding
      procedure Completed
                (  Restore    : in out Restore_Topology_Record;
                   Successful : Boolean
                );
   overriding
      procedure Finished
                (  Restore : in out Restore_Topology_Record
                );
   overriding
      function Get_Name
               (  Restore : Restore_Topology_Record;
                  Device  : RF_Address
               )  return String;
   overriding
      function Get_Object
               (  Restore : not null access Restore_Topology_Record
               )  return GObject;
   overriding
      function Get_Room
               (  Restore : Restore_Topology_Record;
                  Device  : RF_Address
               )  return String;
   overriding
      procedure On_A_Response
                (  Restore  : in out Restore_Topology_Record;
                   Address  : RF_Address;
                   Devices  : Devices_Maps.Map;
                   List     : Rooms_Maps.Map;
                   Expected : in out Natural
                );
   overriding
      procedure On_S_Response
                (  Restore  : in out Restore_Topology_Record;
                   Cube     : access Cube_Client'Class;
                   Error    : Boolean;
                   Duty     : Ratio;
                   Slots    : Natural;
                   Expected : in out Natural
                );
   function Restore_Topology_New
            (  Pages        : not null access Gtk_Notebook_Record'Class;
               Immediate    : not null access Gtk_Notebook_Record'Class;
               Rooms        : not null access Rooms_List_Record'Class;
               Cube_Address : RF_Address;
               Topology     : Topology_Handles.Handle
            )  return Restore_Topology;
private
   use GLib;

   type Op_Type is
        (  No_Op,
           Conflicting_Op,
           Ignore_Op,
           Rename_Device_Op,
           Move_Device_Op,
           Delete_Device_Op,
           Pair_Device_Op
        );

   procedure Clean_Errors (Restore : in out Restore_Topology_Record);
   procedure Do_Cancel (Restore : in out Restore_Topology_Record);
   procedure Do_Restore
             (  Restore        : in out Restore_Topology_Record;
                After_Selected : Boolean
             );
   function Find
            (  Restore        : not null access Restore_Topology_Record;
               After_Selected : Boolean
            )  return Boolean;
   function Find_Device
            (  Restore : Restore_Topology_Record;
               Address : RF_Address
            )  return Gtk_Tree_Iter;
   function Find_Room
            (  Restore : Restore_Topology_Record;
               Room    : Room_ID
            )  return Gtk_Tree_Iter;
   function Find_Page
            (  Restore : Restore_Topology_Record
            )  return GInt;
   function Get
            (  Restore : Restore_Topology_Record;
               Row     : Gtk_Tree_Iter
            )  return RF_Address;
   function Get
            (  Restore : Restore_Topology_Record;
               Row     : Gtk_Tree_Iter
            )  return Room_ID;
   function Get
            (  Restore : Restore_Topology_Record;
               Row     : Gtk_Tree_Iter
            )  return Device_Type;
   function Get
            (  Restore : Restore_Topology_Record;
               Row     : Gtk_Tree_Iter;
               Column  : GInt
            )  return String;
   function Get
            (  Restore : Restore_Topology_Record;
               Row     : Gtk_Tree_Iter
            )  return Op_Type;
   function Get_Name
            (  Restore : Restore_Topology_Record;
               Room    : Room_ID
            )  return String;
   procedure Set
             (  Restore : not null access Restore_Topology_Record;
                Row     : Gtk_Tree_Iter;
                Column  : GInt;
                Text    : String
             );
   procedure Set
             (  Restore : not null access Restore_Topology_Record;
                Row     : Gtk_Tree_Iter;
                Op      : Op_Type
             );
   procedure Update (Restore : in out Restore_Topology_Record);

   package Restore_Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Restore_Topology
          );

   package Walker is
      new Gtk.Tree_Store.Foreach_User_Data
          (  Restore_Topology
          );

   package Set_Mode_Cell_Data is
      new Gtk.Missed.Set_Column_Cell_Data (Restore_Topology);

   procedure Apply_Cell_Data
             (  Column  : not null access
                             Gtk_Tree_View_Column_Record'Class;
                Cell    : not null access
                             Gtk_Cell_Renderer_Record'Class;
                Model   : Gtk_Tree_Model;
                Row     : Gtk_Tree_Iter;
                Restore : Restore_Topology
             );
   procedure Cube_Cell_Data
             (  Column  : not null access
                             Gtk_Tree_View_Column_Record'Class;
                Cell    : not null access
                             Gtk_Cell_Renderer_Record'Class;
                Model   : Gtk_Tree_Model;
                Row     : Gtk_Tree_Iter;
                Restore : Restore_Topology
             );

end MAX_Cube_Topology;
