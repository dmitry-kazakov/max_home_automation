--                                                                    --
--  package MAX_Mail_Page           Copyright (c)  Dmitry A. Kazakov  --
--  E-Mail notifications page                      Luebeck            --
--  Implementation                                 Winter, 2017       --
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

with Ada.Exceptions;           use Ada.Exceptions;
with GLib;                     use GLib;
with GLib.Messages;            use GLib.Messages;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Label;                use Gtk.Label;
with Gtk.Recent_Manager_Keys;  use Gtk.Recent_Manager_Keys;
with MAX_IO;                   use MAX_IO;
with MAX_Trace;                use MAX_Trace;
with Strings_Edit;             use Strings_Edit;
with Strings_Edit.Integers;    use Strings_Edit.Integers;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;

with Ada.IO_Exceptions;
with GNAT.Sockets.Server.Handles;
with Strings_Edit.Symmetric_Serialization;

package body MAX_Mail_Page is
   use GNAT.Sockets.Server;
   use Strings_Edit.Symmetric_Serialization;

   function Where (Name : String) return String is
   begin
      return " in MAX_Mail_Page." & Name;
   end Where;

   function Create
            (  Listener : not null access Connections_Server'Class
            )  return not null access SMTP_Client'Class is
   begin
      return new MAX_SMTP_Client
                 (  Listener     => Listener.all'Unchecked_Access,
                    Input_Size   => 1024,
                    Output_Size  => 1024,
                    Reply_Length => 1024
                 );
   end Create;

   procedure Disconnected (Client : in out MAX_SMTP_Client) is
   begin
      SMTP_Client (Client).Disconnected;
      Client.Shutdown;
   end Disconnected;

   function Gtk_Mail_New return MAX_Mail is
      Result : constant MAX_Mail := new MAX_Mail_Record;
      Label  : Gtk_Label;
   begin
      Gtk.Box.Initialize (Result, Orientation_Vertical, 3);
      Result.Set_Border_Width (5);
      Gtk_New (Result.Grid);
      Result.Grid.Set_Row_Spacing (3);
      Result.Grid.Set_Column_Spacing (3);
      Result.Pack_Start (Result.Grid);
      -- Row 1 ---------------------------------------------------------
      Gtk_New (Result.Enable, "Enable");
      Result.Grid.Attach (Result.Enable, 0, 0);
      Result.Enable.Set_Tooltip_Text ("Enable E-Mail notifications");
      Result.Enable.Set_Active
      (  Restore ("enable-e-mail", "off") = "on"
      );
      -- Row 2 ---------------------------------------------------------
      Gtk_New (Label, "From");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To (Label, Result.Enable, Pos_Bottom);
      Gtk_New (Result.From);
      Result.Grid.Attach_Next_To (Result.From, Label, Pos_Right);
      Result.From.Set_Tooltip_Text
      (  "The sender's E-Mail address of the E-Mail notifications"
      );
      Result.From.Set_Hexpand (True);
      -- Row 3 ---------------------------------------------------------
      Gtk_New (Result.Subject);
      Result.Grid.Attach_Next_To
      (  Result.Subject,
         Result.From,
         Pos_Bottom
      );
      Gtk_New (Label, "Subject");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To (Label, Result.Subject, Pos_Left);
      Result.Subject.Set_Tooltip_Text
      (  "The subject to use in the E-Mail notifications"
      );
      -- Row 4 ---------------------------------------------------------
      Gtk_New (Result.To);
      Result.Grid.Attach_Next_To
      (  Result.To,
         Result.Subject,
         Pos_Bottom
      );
      Gtk_New (Label, "To");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center); --Label.Set_Alignment (1.0, 0.5);
      Result.Grid.Attach_Next_To (Label, Result.To, Pos_Left);
      Result.To.Set_Tooltip_Text
      (  "The notifications recepient's E-Mail address"
      );
      -- Row 5 ---------------------------------------------------------
      Gtk_New (Result.Server);
      Result.Grid.Attach_Next_To (Result.Server, Result.To, Pos_Bottom);
      Result.Server.Set_Tooltip_Text
      (  "The outgoing mail server (SMTP), used to send E-Mails"
      );
      Gtk_New (Label, "Server");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.Server, Pos_Left);
      -- Row 6 ---------------------------------------------------------
      Gtk_New (Result.Port);
      Result.Grid.Attach_Next_To
      (  Result.Port,
         Result.Server,
         Pos_Bottom
      );
      Result.Port.Set_Tooltip_Text
      (  "The port listened by the SMPT server"
      );
      Gtk_New (Label, "Port");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.Port, Pos_Left);
      -- Row 7 ---------------------------------------------------------
      Gtk_New (Result.Security);
      Result.Security.Append_Text ("None");
      Result.Security.Append_Text ("STARTTLS");
      Result.Security.Append_Text ("SSL/TLS");
      Result.Security.Set_Tooltip_Text
      (  "The connection security. The connection is not encrypted " &
         "if None is selected. STARTTLS begins plain and then " &
         "switches to SSL/TLS when the server supports it. " &
         "SSL/TLS is encrypted from the beginning. " &
         "Most of SMTP servers support SSL/TLS which then would be " &
         "the safest choice"
      );
      Result.Grid.Attach_Next_To
      (  Result.Security,
         Result.Port,
         Pos_Bottom
      );
      Gtk_New (Label, "Connection security");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.Security, Pos_Left);
      -- Row 8 ---------------------------------------------------------
      Gtk_New (Result.Method);
      Result.Method.Append_Text ("Anonymous");
      Result.Method.Append_Text ("Plain");
      Result.Method.Append_Text ("Login");
      Result.Method.Append_Text ("CRAM MD5");
      Result.Method.Append_Text ("DIGEST-MD5");
      Result.Method.Set_Tooltip_Text
      (  "The authentication method to use. The choices are "
      &  "listed in the order of strength. When the server offers "
      &  "multiple methods the one of the highest strength is "
      &  "used up to the method selected in the list. So the safest "
      &  "choice would be to select the last method from the list"
      );
      Result.Grid.Attach_Next_To
      (  Result.Method,
         Result.Security,
         Pos_Bottom
      );
      Gtk_New (Label, "Authentication method");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.Method, Pos_Left);
      -- Row 9 ---------------------------------------------------------
      Gtk_New (Result.User);
      Result.Grid.Attach_Next_To
      (  Result.User,
         Result.Method,
         Pos_Bottom
      );
      Result.User.Set_Tooltip_Text
      (  "The user name to authenticate by the server"
      );
      Gtk_New (Label, "User name");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.User, Pos_Left);
      -- Row 10 --------------------------------------------------------
      declare
         Box     : constant Gtk_HBox := Gtk_HBox_New;
         Buttons : constant Gtk_HBox := Gtk_HBox_New;
      begin
         Box.Set_Spacing (3);
         Buttons.Set_Spacing (3);
         Result.Grid.Attach_Next_To
         (  Box,
            Result.User,
            Pos_Bottom
         );
         Gtk_New (Result.Password);
         Box.Pack_Start (Result.Password);
         Result.Password.Set_Visibility (False);
         Gtk_New (Result.Visible);
         Result.Visible.Set_Tooltip_Text ("Show the password");
         Box.Pack_Start (Result.Visible, False, False);
         Gtk_New (Label, "Password");
         Label.Set_Halign (Align_End);
         Label.Set_Valign (Align_Center);
         Result.Grid.Attach_Next_To (Label, Box, Pos_Left);
         Result.Password.Set_Tooltip_Text
         (  "The password to authenticate by the server"
         );
      -- Row 11 --------------------------------------------------------
         Result.Grid.Attach_Next_To
         (  Buttons,
            Box,
            Pos_Bottom
         );
         Buttons.Set_Halign (Align_Start);
         Buttons.Set_Hexpand (False);

         Email_Buttons.Gtk_New (Result.Test);
         Buttons.Pack_Start (Result.Test);
         Revert_Buttons.Gtk_New (Result.Revert);
         Buttons.Pack_Start (Result.Revert);
         Store_Buttons.Gtk_New (Result.Save);
         Buttons.Pack_Start (Result.Save);
      end;

      On_Revert (Result, Result);

      Connect
      (  Result.Save,
         "clicked",
         On_Save'Access,
         Result
      );
      Connect
      (  Result.Test,
         "clicked",
         On_Test'Access,
         Result
      );
      Connect
      (  Result.Revert,
         "clicked",
         On_Revert'Access,
         Result
      );
      Connect
      (  Result.Enable,
         "toggled",
         On_Toggled'Access,
         Result
      );
      Connect
      (  Result.Visible,
         "toggled",
         On_Visible'Access,
         Result
      );
      Connect
      (  Result.Method,
         "changed",
         On_Method_Changed'Access,
         Result
      );
      return Result;
   end Gtk_Mail_New;

   function Is_Enabled (Widget : not null access MAX_Mail_Record)
      return Boolean is
   begin
      return Widget.Enable.Get_Active;
   end Is_Enabled;

   function Is_Opportunistic (Client : MAX_SMTP_Client)
      return Boolean is
   begin
      return Client.Opportunistic;
   end Is_Opportunistic;

   procedure On_Method_Changed
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Mail
             )  is
   begin
      Widget.Set_Enabled;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Method_Changed")
         )  );
   end On_Method_Changed;

   procedure On_Revert
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Mail
             )  is
   begin
      Widget.From.Set_Text (Restore ("from-address", ""));
      Widget.Subject.Set_Text
      (  Restore
         (  "subject-text",
            "MAX! Home automation notification"
      )  );
      Widget.To.Set_Text (Restore ("to-address", ""));
      Widget.Server.Set_Text (Restore ("smtp-server", ""));
      Widget.Port.Set_Text (Image (Restore ("smtp-port", 465)));
      begin
         declare
            Security : constant String :=
                                Restore ("smtp-security", "SSL/TLS");
         begin
            if (  Security'Length = 0
               or else
                  not Widget.Security.Set_Active_ID (Security)
               )  then
               Widget.Security.Set_Active (2);
            end if;
         end;
      exception
         when others =>
            Widget.Security.Set_Active (2);
      end;
      begin
         Widget.Method.Set_Active
         (  GInt'Value (Restore ("smtp-authentication", "4"))
         );
      exception
         when Constraint_Error =>
            Widget.Method.Set_Active (4);
      end;
      declare
         Encoded : constant String := Restore ("smtp-credentials", "");
      begin
         if Encoded'Length > 0 then
            declare
               type State is (Have_User, Have_Password, Tail);
               Data     : constant String := Decode (Encoded, Key);
               Part     : State   := Have_User;
               User     : Integer := Data'First - 1;
               Password : Integer := Data'First - 1;
            begin
               for Index in Data'Range loop
                  if Data (Index) = Character'Val (0) then
                     case Part is
                        when Have_User =>
                           User     := Index - 1;
                           Password := User;
                           Part     := Have_Password;
                        when Have_Password =>
                           Password := Index - 1;
                           Part     := Tail;
                           exit;
                        when Tail =>
                           exit;
                     end case;
                  end if;
               end loop;
               case Part is
                  when Have_User => -- Nothing found
                     Widget.User.Set_Text ("");
                     Widget.Password.Set_Text ("");
                  when Have_Password => -- No password
                     Widget.User.Set_Text (Data (Data'First..User));
                     Widget.Password.Set_Text ("");
                  when Tail => -- Everything is here
                     Widget.User.Set_Text (Data (Data'First..User));
                     Widget.Password.Set_Text
                     (  Data (User + 2..Password)
                     );
               end case;
            end;
         else
            Widget.User.Set_Text ("");   -- Ignore decode error
            Widget.Password.Set_Text ("");
         end if;
      exception
         when Constraint_Error | Ada.IO_Exceptions.Data_Error =>
            Widget.User.Set_Text ("");   -- Ignore decode error
            Widget.Password.Set_Text ("");
      end;
      Widget.Set_Enabled;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Revert")
         )  );
   end On_Revert;

   procedure On_Save
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Mail
             )  is
   begin
      Widget.Store (False);
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

   procedure On_Test
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Mail
             )  is
   begin
      Widget.Send
      (  True,
         Create
         (  From     => Widget.From.Get_Text,
            Subject  => Widget.Subject.Get_Text,
            To       => Widget.To.Get_Text,
            Contents => "This is a test mail"
      )  );
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Test")
         )  );
   end On_Test;

   procedure On_Toggled
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Mail
             )  is
   begin
      Widget.Set_Enabled;
      Widget.Store (False);
   exception
      when Error : GNAT.Sockets.Host_Error =>
         Say
         (  "Invalid SMTP server address:" & Exception_Message (Error),
            "SMTP settings error"
         );
      when Error : GNAT.Sockets.Socket_Error =>
         Say
         (  "Communication error" & Exception_Message (Error),
            "SMTP settings error"
         );
      when Error : Ada.IO_Exceptions.Data_Error =>
         Say
         (  "Invalid SMTP port number",
            "SMTP settings error"
         );
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Toggled")
         )  );
   end On_Toggled;

   procedure On_Visible
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Mail
             )  is
   begin
      Widget.Password.Set_Visibility (Widget.Visible.Get_Active);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Visible")
         )  );
   end On_Visible;

   procedure Send
             (  Widget  : not null access MAX_Mail_Record;
                Testing : Boolean;
                Message : access Root_Stream_Type'Class
             )  is
      This : Mail;
   begin
      This := Create
              (  From    => Widget.From.Get_Text,
                 Subject => Widget.Subject.Get_Text,
                 To      => Widget.To.Get_Text
              );
      Attach_Stream (This, Message);
      Widget.Send (False, This);
   end Send;

   procedure Send
             (  Widget  : not null access MAX_Mail_Record;
                Testing : Boolean;
                Message : Mail
             )  is
      Port : GNAT.Sockets.Port_Type;
   begin
      begin
         Port := GNAT.Sockets.Port_Type
                 (  Strings_Edit.Integers.Value
                     (  Widget.Port.Get_Text
                 )   );
      exception
         when others =>
            raise Ada.IO_Exceptions.Data_Error with
                  "Invalid port number";
      end;
      if SMTP_Server.Is_Valid then
         if (  SMTP_Server.Ptr.Security /= Widget.Security.Get_Active
            or else
               SMTP_Server.Ptr.Method /= Widget.Method.Get_Active
            )  then
            SMTP_Server.Invalidate;
         end if;
      end if;
      if not SMTP_Server.Is_Valid then
         SMTP_Server.Set
         (  new Server_Data
                (  Security => Widget.Security.Get_Active,
                   Method   => Widget.Method.Get_Active
         )      );
         case SMTP_Server.Ptr.Security is
            when 0 =>
               SMTP_Server.Ptr.Factory := new ELV_Factory;
               declare
                  Factory : ELV_Factory renames
                            ELV_Factory (SMTP_Server.Ptr.Factory.all);
               begin
                  Factory.Trace_On
                  (  Received => GNAT.Sockets.Server.Trace_Decoded,
                     Sent     => GNAT.Sockets.Server.Trace_Decoded
                  );
               end;
            when others =>
               SMTP_Server.Ptr.Factory :=
                  new ELV_Secure_Factory (Decoded_Size);
               declare
                  Factory : ELV_Secure_Factory renames
                            ELV_Secure_Factory
                            (  SMTP_Server.Ptr.Factory.all
                            );
               begin
                  Factory.Trace_On
                  (  Received => GNAT.Sockets.Server.Trace_Decoded,
                     Sent     => GNAT.Sockets.Server.Trace_Decoded
                  );
                  Factory.Add_System_Trust;
                  Factory.Generate_Diffie_Hellman_Parameters;
                  Factory.Set_TLS_Tracing
                  (  Session => True,
                     Decoded => True
                  );
               end;
         end case;
         SMTP_Server.Ptr.Server :=
            new Connections_Server (SMTP_Server.Ptr.Factory, 0);
      end if;
      declare
         Reference : constant GNAT.Sockets.Server.Handles.Handle :=
                     GNAT.Sockets.Server.Handles.Ref
                     (  new MAX_SMTP_Client
                            (  Listener     => SMTP_Server.Ptr.Server,
                               Reply_Length => 1024,
                               Input_Size   => 80,
                               Output_Size  => 1024
                     )      );
         Client    : MAX_SMTP_Client'Class renames
                     MAX_SMTP_Client'Class (Reference.Ptr.all);
      begin
         Client.Testing := Testing;
         case SMTP_Server.Ptr.Method is
            when 1 =>
               Client.Set_Credentials
               (  Widget.User.Get_Text,
                  Widget.Password.Get_Text,
                  SMTP_PLAIN
               );
            when 2 =>
               Client.Set_Enhanced (True);
               Client.Set_Credentials
               (  Widget.User.Get_Text,
                  Widget.Password.Get_Text,
                  SMTP_PLAIN or SMTP_LOGIN
               );
            when 3 =>
               Client.Set_Enhanced (True);
               Client.Set_Credentials
               (  Widget.User.Get_Text,
                  Widget.Password.Get_Text,
                  SMTP_PLAIN or SMTP_LOGIN or SMTP_CRAM_MD5
               );
            when 4 =>
               Client.Set_Enhanced (True);
               Client.Set_Credentials
               (  Widget.User.Get_Text,
                  Widget.Password.Get_Text,
                  (  SMTP_PLAIN
                  or SMTP_LOGIN
                  or SMTP_CRAM_MD5
                  or SMTP_DIGEST_MD5
               )  );
            when others =>
               null;
         end case;
         case SMTP_Server.Ptr.Security is
            when 0 =>
               Client.Opportunistic := False;
            when 1 =>
               Client.Opportunistic := True;
               Client.Set_TLS (True, True);
            when others =>
               Client.Opportunistic := False;
         end case;
         Client.Send (Message);
         SMTP_Server.Ptr.Server.Connect
         (  Client         => Reference.Ptr,
            Host           => Widget.Server.Get_Text,
            Port           => Port,
            Max_Connect_No => 1
         );
      end;
   exception
      when GNAT.Sockets.Host_Error |
           GNAT.Sockets.Socket_Error |
           Ada.IO_Exceptions.Data_Error =>
         raise;
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Send")
         )  );
   end Send;

   procedure Send_Abandoned
             (  Client   : in out MAX_SMTP_Client;
                Messages : Mail_Array
             )  is
   begin
      SMTP_Client (Client).Send_Abandoned (Messages);
   end Send_Abandoned;

   procedure Send_Error
             (  Client  : in out MAX_SMTP_Client;
                Code    : Error_Code;
                Context : SMTP_Command;
                Reply   : String
             )  is
   begin
      if Client.Testing then
         Say
         (  Image (Code) & ". " & Reply,
            "Test E-Mail failure"
         );
      else
         SMTP_Client (Client).Send_Error (Code, Context, Reply);
      end if;
   end Send_Error;

   procedure Send_Error
             (  Client  : in out MAX_SMTP_Client;
                Code    : Error_Code;
                Context : SMTP_Command;
                Reply   : String;
                Message : Mail
             )  is
   begin
      if Client.Testing then
         Say
         (  (  Image (Code)
            &  ". "
            &  Reply
            &  ". While sending mail "
            &  Quote (Get (Message, Mail_Subject))
            &  " to: "
            &  GNAT.Sockets.SMTP.Client.Image (Get (Message, Mail_To))
            ),
            "Test E-Mail failure"
         );
      else
         SMTP_Client (Client).Send_Error
         (  Code,
            Context,
            Reply,
            Message
         );
      end if;
   end Send_Error;

   procedure Send_Success
             (  Client  : in out MAX_SMTP_Client;
                Message : Mail
             )  is
   begin
      if Client.Testing then
         Say
         (  "Successfully sent",
            "Test E-Mail success",
            "gtk-dialog-info"
         );
      else
         SMTP_Client (Client).Send_Success (Message);
      end if;
   end Send_Success;

   procedure Set_Enabled (Widget : not null access MAX_Mail_Record) is
      Enabled : constant Boolean := Widget.Enable.Get_Active;
      Login   : constant Boolean := Widget.Method.Get_Active /= 0;
   begin
      Widget.From.Set_Sensitive     (not Enabled);
      Widget.Subject.Set_Sensitive  (not Enabled);
      Widget.To.Set_Sensitive       (not Enabled);
      Widget.Server.Set_Sensitive   (not Enabled);
      Widget.Port.Set_Sensitive     (not Enabled);
      Widget.Security.Set_Sensitive (not Enabled);
      Widget.Method.Set_Sensitive   (not Enabled);
      Widget.User.Set_Sensitive     (Login and not Enabled);
      Widget.Password.Set_Sensitive (Login and not Enabled);
      Widget.Visible.Set_Sensitive  (Login and not Enabled);
      Widget.Test.Set_Sensitive     (not Enabled);
      Widget.Save.Set_Sensitive     (not Enabled);
      Widget.Revert.Set_Sensitive   (not Enabled);
   end Set_Enabled;

   procedure Store
             (  Widget        : not null access MAX_Mail_Record;
                Settings_Only : Boolean
             )  is
   begin
      if not Settings_Only then
         if Widget.Enable.Get_Active then
            Store ("enable-e-mail", "on");
         else
            Store ("enable-e-mail", "off");
         end if;
      end if;
      if Settings_Only or else not Widget.Enable.Get_Active then
         Store ("from-address", Widget.From.Get_Text);
         Store ("subject-text", Widget.Subject.Get_Text);
         Store ("to-address",   Widget.To.Get_Text);
         Store ("smtp-server",  Widget.Server.Get_Text);
         declare
            Port : Integer;
         begin
            Port := Value (Widget.Port.Get_Text);
            Store ("smtp-port", Image (Port));
         exception
            when others =>
               null;
         end;
         Store
         (  "smtp-security",
            Image (Integer (Widget.Security.Get_Active))
         );
         Store
         (  "smtp-authentication",
            Image (Integer (Widget.Method.Get_Active))
         );
         Store
         (  "smtp-credentials",
            Encode
            (  (  Widget.User.Get_Text
               &  Character'Val (0)
               &  Widget.Password.Get_Text
               &  Character'Val (0)
               ),
               Key
         )  );
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Store")
         )  );
   end Store;

end MAX_Mail_Page;
