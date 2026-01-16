--                                                                    --
--  package MAX_Control_Page        Copyright (c)  Dmitry A. Kazakov  --
--     Control page                                Luebeck            --
--  Interface                                      Winter, 2018       --
--                                                                    --
--                                Last revision :  21:22 23 Oct 2021  --
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

with Gtk.Box;             use Gtk.Box;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Combo_Box_Text;  use Gtk.Combo_Box_Text;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Grid;            use Gtk.Grid;
with Gtk.Label;           use Gtk.Label;
with Gtk.Widget;          use Gtk.Widget;
with MAX_Icon_Factory;    use MAX_Icon_Factory;

with Gtk.Handlers;

package MAX_Control_Page is
   type MAX_Control_Record is new Gtk_Widget_Record with private;
   type MAX_Control is access all MAX_Control_Record'Class;
   function Is_Enabled (Widget : not null access MAX_Control_Record)
      return Boolean;
   function Gtk_Control_New return MAX_Control;

private
   Julia_Stop_Timeout  : constant Duration := 2.0;
   Python_Stop_Timeout : constant Duration := 2.0;
   Controller          : constant String   := "controller";

   task type Python_Worker
             (  Widget : not null access MAX_Control_Record'Class
             )  is
      entry Start
            (  Python_Library   : String;
               Script_Directory : String;
               Script_File      : String;
               Start_Delay      : Duration;
               Each             : Duration;
               Error            : in out String;
               Pointer          : in out Integer
            );
      entry Stop;
   end Python_Worker;
   type Python_Worker_Ptr is access Python_Worker;

   type String_Ptr is access String;

   type Python_Data is record
      Worker    : Python_Worker_Ptr;
      Path      : String_Ptr;
      Method    : String_Ptr;
      Folder    : Integer := 0;
      Extension : Integer := 0;
      Executing : Boolean := False;
      pragma Atomic (Executing);
   end record;

   task type Julia_Worker
             (  Widget : not null access MAX_Control_Record'Class
             )  is
      entry Start
            (  Script_File : String;
               Start_Delay : Duration;
               Each        : Duration;
               Error       : in out String;
               Pointer     : in out Integer
            );
      entry Stop;
   end Julia_Worker;
   type Julia_Worker_Ptr is access Julia_Worker;

   type Julia_Data is record
      Worker    : Julia_Worker_Ptr;
      Path      : String_Ptr;
      Method    : String_Ptr;
      Executing : Boolean := False;
      pragma Atomic (Executing);
   end record;

   type MAX_Control_Record is new Gtk_VBox_Record with record
      Grid           : Gtk_Grid;
      Enable         : Gtk_Check_Button;
      Method         : Gtk_Combo_Box_Text;
      File_Name      : Gtk_GEntry;
      Library_Name   : Gtk_GEntry;
      Library_Label  : Gtk_Label;
      Sample_Label   : Gtk_Label;
      Period         : Gtk_GEntry;
      Start_Delay    : Gtk_GEntry;
      Browse_File    : Script_File_Buttons.Gtk_Style_Button;
      Browse_Library : Script_Library_Buttons.Gtk_Style_Button;
      Python         : Python_Data; -- Python data
      Julia          : Julia_Data;  -- Julia data
   end record;
   function Get_Delay (Widget : MAX_Control_Record) return Duration;
   function Get_Directory   (Widget : MAX_Control_Record) return String;
   function Get_Julia_File  (Widget : MAX_Control_Record) return String;
   function Get_Each (Widget : MAX_Control_Record) return Duration;
   function Get_Path        (Widget : MAX_Control_Record) return String;
   function Get_Python      (Widget : MAX_Control_Record) return String;
   function Get_Python_File (Widget : MAX_Control_Record) return String;

   procedure On_Browse_File
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Control
             );
   procedure On_Browse_Library
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Control
             );
   procedure On_Destroy
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Control
             );
   procedure On_Method_Changed
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Control
             );
--     procedure On_Revert
--               (  Object : access Gtk_Widget_Record'Class;
--                  Widget : MAX_Control
--               );
   procedure On_Toggled
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Control
             );
   procedure Set_Enabled (Widget : not null access MAX_Control_Record);
   procedure Set_Julia_Path
             (  Widget : in out MAX_Control_Record;
                Path   : String
             );
   procedure Set_Python_Path
             (  Widget : in out MAX_Control_Record;
                Path   : String
             );
   procedure Store (Widget : not null access MAX_Control_Record);
   procedure Update_Method
             (  Widget : not null access MAX_Control_Record
             );

   package Control_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             MAX_Control
          );
   use Control_Handlers;

   function Start_Julia
            (  Widget      : not null access Max_Control_Record;
               Interactive : Boolean;
               Start_Delay : Duration;
               Each        : Duration
            )  return Boolean;
   procedure Stop_Julia (Widget : in out Max_Control_Record);
   function Start_Python
            (  Widget      : not null access Max_Control_Record;
               Interactive : Boolean;
               Start_Delay : Duration;
               Each        : Duration
            )  return Boolean;
   procedure Stop_Python (Widget : in out Max_Control_Record);

   procedure Tell (Error : String; Interactive : Boolean);

end MAX_Control_Page;
