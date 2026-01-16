--                                                                    --
--  package MAX_Mail_Page           Copyright (c)  Dmitry A. Kazakov  --
--  E-Mail notifications page                      Luebeck            --
--  Interface                                      Winter, 2017       --
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

with Ada.Streams;               use Ada.Streams;
with GNAT.Sockets.SMTP;         use GNAT.Sockets.SMTP;
with GNAT.Sockets.SMTP.Client;  use GNAT.Sockets.SMTP.Client;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Combo_Box_Text;        use Gtk.Combo_Box_Text;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Grid;                  use Gtk.Grid;
with Gtk.Widget;                use Gtk.Widget;
with MAX_Icon_Factory;          use MAX_Icon_Factory;

with Gtk.Handlers;

package MAX_Mail_Page is
   type MAX_Mail_Record is new Gtk_Widget_Record with private;
   type MAX_Mail is access all MAX_Mail_Record'Class;
   function Is_Enabled (Widget : not null access MAX_Mail_Record)
      return Boolean;
   procedure Send
             (  Widget  : not null access MAX_Mail_Record;
                Testing : Boolean;
                Message : access Root_Stream_Type'Class
             );
   procedure Send
             (  Widget  : not null access MAX_Mail_Record;
                Testing : Boolean;
                Message : Mail
             );
   function Gtk_Mail_New return MAX_Mail;
private
   type MAX_Mail_Record is new Gtk_VBox_Record with record
      Grid     : Gtk_Grid;

      Enable   : Gtk_Check_Button;
      From     : Gtk_GEntry;
      Subject  : Gtk_GEntry;
      To       : Gtk_GEntry;
      Server   : Gtk_GEntry;
      Port     : Gtk_GEntry;
      Security : Gtk_Combo_Box_Text;
      Method   : Gtk_Combo_Box_Text;
      User     : Gtk_GEntry;
      Password : Gtk_GEntry;
      Visible  : Gtk_Check_Button;
      Test     : Email_Buttons.Gtk_Style_Button;
      Revert   : Revert_Buttons.Gtk_Style_Button;
      Save     : Store_Buttons.Gtk_Style_Button;
   end record;
   procedure Set_Enabled (Widget : not null access MAX_Mail_Record);
   procedure Store
             (  Widget        : not null access MAX_Mail_Record;
                Settings_Only : Boolean
             );
   procedure On_Method_Changed
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Mail
             );
   procedure On_Revert
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Mail
             );
   procedure On_Save
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Mail
             );
   procedure On_Test
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Mail
             );
   procedure On_Toggled
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Mail
             );
   procedure On_Visible
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Mail
             );

   package Mail_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             MAX_Mail
          );
   use Mail_Handlers;

   type MAX_SMTP_Client is new SMTP_Client with record
      Opportunistic : Boolean := True;
      Testing       : Boolean := False;
   end record;
   overriding
      procedure Disconnected (Client : in out MAX_SMTP_Client);
   overriding
      function Is_Opportunistic (Client : MAX_SMTP_Client)
         return Boolean;
   overriding
      procedure Send_Abandoned
                (  Client   : in out MAX_SMTP_Client;
                   Messages : Mail_Array
                );
   overriding
      procedure Send_Error
                (  Client  : in out MAX_SMTP_Client;
                   Code    : Error_Code;
                   Context : SMTP_Command;
                   Reply   : String
                );
   overriding
      procedure Send_Error
                (  Client  : in out MAX_SMTP_Client;
                   Code    : Error_Code;
                   Context : SMTP_Command;
                   Reply   : String;
                   Message : Mail
                );
   overriding
      procedure Send_Success
                (  Client  : in out MAX_SMTP_Client;
                   Message : Mail
                );

end MAX_Mail_Page;
