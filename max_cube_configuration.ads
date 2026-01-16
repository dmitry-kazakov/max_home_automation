--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Cube_Configuration                      Luebeck            --
--  Interface                                      Autumn, 2016       --
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

with Ada.Streams;           use Ada.Streams;
with GLib.Object;           use GLib.Object;
with Gtk.Box;               use Gtk.Box;
with Gtk.Cell_Renderer;     use Gtk.Cell_Renderer;
with Gtk.Check_Button;      use Gtk.Check_Button;
with Gtk.Image;             use Gtk.Image;
with Gtk.Label;             use Gtk.Label;
with Gtk.List_Store;        use Gtk.List_Store;
with Gtk.Notebook;          use Gtk.Notebook;
with Gtk.Scrolled_Window;   use Gtk.Scrolled_Window;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_Store;        use Gtk.Tree_Store;
with Gtk.Tree_View;         use Gtk.Tree_View;
with Gtk.Tree_View_Column;  use Gtk.Tree_View_Column;
with Gtk.Widget;            use Gtk.Widget;
with MAX_Icon_Factory;      use MAX_Icon_Factory;
with MAX_IO;                use MAX_IO;
with MAX_Rooms_List;        use MAX_Rooms_List;

with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
use  GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;

with Gtk.Handlers;
with Gtk.Missed;
with Gtk.Tree_Model.Extension_Store;

package MAX_Cube_Configuration is

   type Restore_Configuration_Record is
      new Gtk_Widget_Record
      and Settings_Handler_Interface with private;
   type Restore_Configuration is
      access all Restore_Configuration_Record'Class;
   overriding
      procedure Canceled
                (  Restore : in out Restore_Configuration_Record
                );
   overriding
      procedure Finished
                (  Restore : in out Restore_Configuration_Record
                );
   overriding
      function Get_Object
               (  Restore : not null access Restore_Configuration_Record
               )  return GObject;
   overriding
      procedure On_A_Response
                (  Restore  : in out Restore_Configuration_Record;
                   Address  : RF_Address;
                   Devices  : Devices_Maps.Map;
                   List     : Rooms_Maps.Map;
                   Expected : in out Natural
                );
   overriding
      procedure On_S_Response
                (  Restore  : in out Restore_Configuration_Record;
                   Cube     : access Cube_Client'Class;
                   Error    : Boolean;
                   Duty     : Ratio;
                   Slots    : Natural;
                   Expected : in out Natural
                );
   function Restore_Configuration_New
            (  Pages      : not null access Gtk_Notebook_Record'Class;
               Immediate  : not null access Gtk_Notebook_Record'Class;
               Rooms      : not null access Rooms_List_Record'Class;
               Cube       : RF_Address;
               Parameters : Device_Parameters_Data_Sets.Set
            )  return Restore_Configuration;

   procedure Restore
             (  Pages      : not null access Gtk_Notebook_Record'Class;
                Rooms      : not null access Rooms_List_Record'Class;
                Cube       : RF_Address;
                Parameters : Device_Parameters_Data_Sets.Set;
                Topology   : Topology_Handles.Handle
             );
   procedure Read
             (  Stream      : not null access Root_Stream_Type'Class;
                Thermostats : out Device_Parameters_Data_Sets.Set;
                Topology    : in out Topology_Handles.Handle
             );
   procedure Write
             (  Stream      : in out Root_Stream_Type'Class;
                Thermostats : Device_Parameters_Data_Sets.Set;
                Topology    : Topology_Handles.Handle
             );
private
   use GLib;
   use Gtk.Tree_Model.Extension_Store;

   type Restore_Configuration_Record is
      new Gtk_VBox_Record
      and Settings_Handler_Interface with
   record
      Parent             : Gtk_Notebook;
      Immediate_Parent   : Gtk_Notebook;
      Rooms              : Rooms_List;
      Parameters         : Device_Parameters_Data_Sets.Set;
      Cube               : RF_Address;

      Cancel             : Cancel_Restore_Buttons.Gtk_Style_Button;
      OK                 : Start_Restore_Buttons.Gtk_Style_Button;
      Retry              : Retry_Restore_Buttons.Gtk_Style_Button;

      Warning            : Gtk_Image;
      Error              : Gtk_Image;
      Scroll             : Gtk_Scrolled_Window;
      View               : Gtk_Tree_View;
      List               : Gtk_Extension_Store;
      Sources            : Gtk_List_Store;
      Restore_Schedule   : Gtk_Check_Button;
      Restore_Parameters : Gtk_Check_Button;
      Restore_Valve      : Gtk_Check_Button;
      Action             : Gtk_Label;
      Current            : Gtk_Tree_Iter;
      Selected           : Natural := 0;
      Repeat             : Boolean := False;
      Rejected           : Boolean := False;
         -- Progress
      Status             : Store_Mode := Store_Stop;
      Source             : Natural    := 0;
      From               : RF_Address;   -- Current thermostats
      To                 : RF_Address;
   end record;

   procedure Do_Cancel
             (  Restore : in out Restore_Configuration_Record
             );
   procedure Do_Restore
             (  Restore : in out Restore_Configuration_Record
             );
   function Find_Page
            (  Restore : Restore_Configuration_Record
            )  return GInt;

   function Clean_Errors
            (  Model : Gtk_Tree_Model;
               Path  : Gtk_Tree_Path;
               Row   : Gtk_Tree_Iter;
               Data  : Restore_Configuration
            )  return Boolean;

   function Find_Next
            (  Model : Gtk_Tree_Model;
               Path  : Gtk_Tree_Path;
               Row   : Gtk_Tree_Iter;
               Data  : Restore_Configuration
            )  return Boolean;

   function Visit
            (  Model : Gtk_Tree_Model;
               Path  : Gtk_Tree_Path;
               Row   : Gtk_Tree_Iter;
               Data  : Restore_Configuration
            )  return Boolean;

   package Restore_Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Restore_Configuration
          );

   package Walker is
      new Gtk.Tree_Store.Foreach_User_Data
          (  Restore_Configuration
         );

   package Set_Mode_Cell_Data is
      new Gtk.Missed.Set_Column_Cell_Data (Restore_Configuration);

   procedure Link_Cell_Data
             (  Column  : not null access
                             Gtk_Tree_View_Column_Record'Class;
                Cell    : not null access
                             Gtk_Cell_Renderer_Record'Class;
                Model   : Gtk_Tree_Model;
                Row     : Gtk_Tree_Iter;
                Restore : Restore_Configuration
             );

end MAX_Cube_Configuration;
