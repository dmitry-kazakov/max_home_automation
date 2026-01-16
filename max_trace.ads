--                                                                    --
--  MAX! Home Automation            Copyright (c)  Dmitry A. Kazakov  --
--     MAX_Trace                                   Luebeck            --
--  Interface                                      Summer, 2015       --
--                                                                    --
--                                Last revision :  10:14 10 Dec 2023  --
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
with Ada.Exceptions;           use Ada.Exceptions;
with GLib;                     use GLib;
with Gdk.Pixbuf;               use Gdk.Pixbuf;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.File_Chooser;         use Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog;  use Gtk.File_Chooser_Dialog;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Label;                use Gtk.Label;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Max_Icon_Factory;         use Max_Icon_Factory;

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Gtk.Main.Router;
with Gtk.Handlers;

with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
use  GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;

package MAX_Trace is

   Key          : constant String := "MAX! home automation";
   Decoded_Size : constant := 1024 * 10;
      -- Main window
   Window       : Gtk_Window;
   Exiting      : Boolean := False;
   pragma Atomic (Exiting);
      -- Trace file
   Trace_Open   : Boolean := False;
   Trace_File   : Ada.Text_IO.File_Type;

   Max_Filter_Name   : constant String := "MAX! configuration file";
   Max_Extension     : constant String := ".max";

   CSS_Filter_Name   : constant String := "CSS file";
   CSS_Extension     : constant String := ".css";

   Jl_Filter_Name    : constant String := "Juila script";
   Jl_Extension      : constant String := ".jl";

   HTML_Filter_Name  : constant String := "HTTP file";
   HTML_Extension    : constant String := ".html";

   Log_Filter_Name   : constant String := "Trace file";
   Log_Extension     : constant String := ".log";

   Py_Filter_Name    : constant String := "Python script";
   Py_Extension      : constant String := ".py";

   Txt_Filter_Name   : constant String := "Settings file";
   Txt_Extension     : constant String := ".txt";

   Sch_Filter_Name   : constant String := "Schedule file";
   Sch_Extension     : constant String := ".sch";

   type Trace_Box_Record is new Gtk_Widget_Record with private;
   type Trace_Box is access all Trace_Box_Record'Class;
   procedure Open_Trace (File : UTF8_String; Box : Trace_Box := null);

   procedure Do_Destroy
             (  Dialog   : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             );
   function Gtk_Trace_Box_New return Trace_Box;

   function Image
            (  Date     : Time;
               Fraction : Boolean := False
            )  return String;
   function Image (Interval : Duration) return String;

   procedure Say
             (  Text  : String;
                Title : String  := "Error";
                Mode  : String  := "gtk-dialog-error";
                Modal : Boolean := False
             );
   function Stack_Traceback return String;

   type Trace_Type is
        (  Message_Text,
           Received_Text,
           Sent_Text,
           Error_Text,
           Mode_Text
        );
   procedure Trace
             (  Box     : not null access Trace_Box_Record;
                Message : UTF8_String;
                Mode    : Trace_Type  := Message_Text;
                Prefix  : UTF8_String := ""
             );
   procedure Trace
             (  Box     : not null access Trace_Box_Record;
                Message : UTF8_String;
                Error   : Exception_Occurrence
             );
   procedure Trace_Exceptions (Enable : Boolean);
   procedure Trace_Location (Message : UTF8_String);
   procedure Trace_To_File
             (  Message : UTF8_String;
                Mode    : Trace_Type  := Message_Text;
                Prefix  : UTF8_String := ""
             );
   procedure Trace_To_File
             (  Message : UTF8_String;
                Error   : Exception_Occurrence
             );
------------------------------------------------------------------------
   type Action_Mode is (Entry_Mode, Cube_Mode, Widget_Mode);
   type File_Dialog_Record (Mode : Action_Mode) is
      new Gtk_File_Chooser_Dialog_Record with private;
   type File_Dialog is access all File_Dialog_Record;

   function Gtk_New
            (  Title      : String;
               Action     : Gtk_File_Chooser_Action;
               File       : not null Gtk_GEntry;
               Accept_Tip : String := "Use the selected file";
               Close_Tip  : String := "Close the file selection dialog"
            )  return not null File_Dialog;

   type Cube_Select_Handler is
      access procedure (File : String; Cube : RF_Address);
   function Gtk_New
            (  Title      : String;
               Action     : Gtk_File_Chooser_Action;
               Handler    : not null Cube_Select_Handler;
               Cube       : RF_Address := 0;
               Accept_Tip : String := "Use the selected file";
               Close_Tip  : String := "Close the file selection dialog"
            )  return not null File_Dialog;

   type Widget_Select_Handler is access procedure
        (  File   : String;
           Widget : in out Gtk_Widget_Record'Class
        );
   function Gtk_New
            (  Title      : String;
               Action     : Gtk_File_Chooser_Action;
               Handler    : not null Widget_Select_Handler;
               Widget     : not null access Gtk_Widget_Record'Class;
               Accept_Tip : String := "Use the selected file";
               Close_Tip  : String := "Close the file selection dialog"
            )  return not null File_Dialog;
------------------------------------------------------------------------
   type Gtk_Style_Label_Kind is
        (  Topology_Label,
           Configuration_Label,
           Trace_Label,
           Monitor_Label,
           Graphs_Label,
           Settings_Label,
           General_Label,
           E_Mail_Label,
           Database_Label,
           Control_Label,
           Presets_Label,
           Overview_Label,
           Restore_Label,
           No_Topology_Label
        );
   type Gtk_Style_Label_Record is new Gtk_Label_Record with record
      Kind_Of : Gtk_Style_Label_Kind;
      Suffix  : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Gtk_Style_Label is access all Gtk_Style_Label_Record'Class;
   procedure Gtk_New
             (  Label   : out Gtk_Style_Label;
                Kind_Of : Gtk_Style_Label_Kind;
                Suffix  : UTF8_String := ""
             );
   function Gtk_Style_Label_New
            (  Kind_Of : Gtk_Style_Label_Kind;
               Suffix  : UTF8_String := ""
            )
      return access Gtk_Label_Record'Class;
   procedure Initialize
             (  Label : not null access Gtk_Style_Label_Record'Class
             );
------------------------------------------------------------------------
--     procedure Memory_Trace
--               (  Text         : String  := "";
--                  Request_Info : Boolean := False
--               );
private
   use Ada.Strings.Unbounded;

   pragma Atomic (Trace_Open);

   procedure Style_Updated
             (  Label : access Gtk_Style_Label_Record'Class
             );

   type Trace_Box_Record is new Gtk_VBox_Record with record
      Buttons     : Gtk_HBox;
      Clear       : Clear_Buttons.Gtk_Style_Button;
      Hold        : Hold_Buttons.Gtk_Style_Button;
      Continue    : Continue_Buttons.Gtk_Style_Button;
      Enable_File : Gtk_Check_Button;
      Append_File : Gtk_Check_Button;
      Browse      : Trace_File_Buttons.Gtk_Style_Button;
      File_Name   : Gtk_Entry;
      Frozen      : Boolean := False;
      View        : Gtk_Text_View;
      Buffer      : Gtk_Text_Buffer;
   end record;

   procedure Trim
             (  Buffer : not null access Gtk_Text_Buffer_Record'Class
             );

   package Trace_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Trace_Box
          );

   procedure Clear_Trace
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Trace_Box
             );
   procedure Hold_Trace
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Trace_Box
             );
   procedure Continue_Trace
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Trace_Box
             );
   procedure On_Browse_File
             (  Object : access Gtk_Widget_Record'Class;
                Box    : Trace_Box
             );
   procedure On_Trace_To_File
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Trace_Box
             );

   function To_Pixbuf
            (  Box  : not null access Trace_Box_Record;
               Mode : Trace_Type
            )  return Gdk_Pixbuf;

   type Say_Data is record
      Text  : Unbounded_String;
      Title : Unbounded_String;
      Mode  : Unbounded_String;
      Modal : Boolean := True;
   end record;
   package Say_Messages is
      new Gtk.Main.Router.Generic_Message (Say_Data);
   procedure Service (Message : in out Say_Data);

   type File_Dialog_Record (Mode : Action_Mode) is
      new Gtk_File_Chooser_Dialog_Record with
   record
      case Mode is
         when Entry_Mode =>
            File           : Gtk_GEntry;
         when Cube_Mode =>
            Cube           : RF_Address := 0;
            Cube_Handler   : Cube_Select_Handler;
         when Widget_Mode =>
            Widget         : Gtk_Widget;
            Widget_Handler : Widget_Select_Handler;
      end case;
   end record;
   subtype Entry_Dialog_Record  is File_Dialog_Record (Entry_Mode);
   subtype Cube_Dialog_Record   is File_Dialog_Record (Cube_Mode);
   subtype Widget_Dialog_Record is File_Dialog_Record (Widget_Mode);

   procedure On_Response
             (  Dialog   : access Gtk_Dialog_Record'Class;
                Response : Gtk_Response_Type
             );
end MAX_Trace;
