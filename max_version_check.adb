--                                                                    --
--  package MAX_Version_Check       Copyright (c)  Dmitry A. Kazakov  --
--  Version update checking                        Luebeck            --
--  Implementation                                 Winter, 2020       --
--                                                                    --
--                                Last revision :  15:03 29 Feb 2020  --
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

with GNAT.Sockets.Server;    use GNAT.Sockets.Server;
with MAX_IO;                 use MAX_IO;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with MAX_Home_Automation_Version;

package body MAX_Version_Check is

   Home    : constant String := "www.dmitry-kazakov.de";
   Page    : constant String := "/ada/max_home_automation.htm";
   Prefix  : constant String := "<EM>version ";
   Suffix  : constant String := "/EM><BR>";
   Port    : constant := 80;

   procedure Commit (Destination : in out Content) is
      Major   : Natural := 0;
      Minor   : Natural := 0;
      Pointer : Integer := MAX_Home_Automation_Version.Value'First;
   begin
      Get (MAX_Home_Automation_Version.Value, Pointer, Major);
      Pointer := Pointer + 1;
      Get (MAX_Home_Automation_Version.Value, Pointer, Minor);
      if Destination.Major = 0 then
         Version_Unknown;
      elsif (  Major < Destination.Major
         or else
            (  Major = Destination.Major
            and then
               Minor < Destination.Minor
         )  )  then
         Version_Update (Destination.Major, Destination.Minor);
      else
         Version_Uptodate (Destination.Major, Destination.Minor);
      end if;
      Destination.Client.Shutdown;
   exception
      when others =>
         null;
   end Commit;

   procedure Activated
             (  Client : in out MAX_HTTP_Client
             )  is
   begin
      HTTP_Session (Client).Connected;
      Client.Set_Request_Header (Accept_Header, "text/plain");
      Client.Get
      (  "http://" & Home & Page,
         Client.Page'Unchecked_Access
      );
   end Activated;

   procedure Connect_Error
             (  Client : in out MAX_HTTP_Client;
                Error  : GNAT.Sockets.Error_Type
             )  is
   begin
      Version_Unknown;
   end Connect_Error;

   procedure Get_Version is
   begin
      if not Hosting_Server.Is_Valid then
         Hosting_Server.Set (new Server_Data (0, 0));
         Hosting_Server.Ptr.Factory := new Connections_Factory;
--           Hosting_Server.Ptr.Factory.Trace_On
--           (  Received => GNAT.Sockets.Server.Trace_Decoded,
--              Sent     => GNAT.Sockets.Server.Trace_Decoded
--           );
         Hosting_Server.Ptr.Server :=
            new GNAT.Sockets.Server.Connections_Server
                (  Hosting_Server.Ptr.Factory.all'Unchecked_Access,
                   0 -- Only clients
                );
      end if;
      Hosting_Server.Ptr.Server.Connect
      (  Client         => new MAX_HTTP_Client
                               (  Hosting_Server.Ptr.Server,
                                  80,
                                  80,
                                  80
                               ),
         Host           => Home,
         Port           => Port,
         Max_Connect_No => 3
      );
   exception
      when others =>
         null;
   end Get_Version;

   procedure Put
             (  Destination : in out Content;
                Data        : String
             )  is
   begin
      for Index in Data'Range loop
         case Destination.State is
            when 1..Prefix'Length =>
               if Data (Index) = Prefix (Destination.State) then
                  Destination.State := Destination.State + 1;
                  Destination.Major := 0;
               else
                  Destination.State := 1;
               end if;
            when Prefix'Length + 1 =>
               case Data (Index) is
                  when '0'..'9' =>
                     begin
                        Destination.Major :=
                           (  Destination.Major * 10
                           +  Character'Pos (Data (Index))
                           -  Character'Pos ('0')
                           );
                     exception
                        when others =>
                           Destination.State := 1;
                     end;
                  when '.' =>
                     Destination.State := Destination.State + 1;
                     Destination.Minor := 0;
                  when others =>
                     Destination.State := 1;
               end case;
            when Prefix'Length + 2 =>
               case Data (Index) is
                  when '0'..'9' =>
                     begin
                        Destination.Minor :=
                           (  Destination.Minor * 10
                           +  Character'Pos (Data (Index))
                           -  Character'Pos ('0')
                           );
                     exception
                        when others =>
                           Destination.State := 1;
                     end;
                  when '<' =>
                     Destination.State := Destination.State + 1;
                  when others =>
                     Destination.State := 1;
               end case;
            when Prefix'Length + 3..Prefix'Length + Suffix'Length + 2 =>
               if (  Data (Index)
                  =  Suffix (Destination.State - Prefix'Length - 2)
                  )  then
                  Destination.State := Destination.State + 1;
               else
                  Destination.State := 1;
               end if;
            when Prefix'Length + Suffix'Length + 3 =>
               null;
            when others =>
               Destination.State := 1;
         end case;
      end loop;
   end Put;

   procedure Write
             (  Stream : not null access Root_Stream_Type'Class;
                Item   : Content
             )  is
   begin
      null;
   end Write;

end MAX_Version_Check;
