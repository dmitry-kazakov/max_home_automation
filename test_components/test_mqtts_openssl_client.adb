--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_MQTTS_OpenSSL_Client                   Luebeck            --
--  Test                                           Winter, 2025       --
--                                                                    --
--                                Last revision :  22:23 22 Jun 2025  --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Text_IO;                  use Ada.Text_IO;
with GNAT.Sockets.MQTT;            use GNAT.Sockets.MQTT;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;
with Test_MQTT_Servers_OpenSSL;    use Test_MQTT_Servers_OpenSSL;

with GNAT.Exception_Traces;

procedure Test_MQTTS_OpenSSL_Client is
   Server_Address : constant String := "test.mosquitto.org";
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   declare
      Factory   : aliased MQTTS_OpenSSL_Factory
                          (  Max_Subscribe_Topics => 10,
                             Input_Size           => 80,
                             Output_Size          => 80,
                             Decoded_Size         => 256,
                             Max_Connections      => 10
                          );
      Server    : aliased Connections_Server (Factory'Access, 0);
      Reference : Handle;
   begin
      Trace_On
      (  Factory  => Factory,
         Received => GNAT.Sockets.Server.Trace_Decoded,
         Sent     => GNAT.Sockets.Server.Trace_Decoded
      );
      Set
      (  Reference,
         new Test_Client
             (  Listener             => Server'Unchecked_Access,
                Input_Size           => 80,
                Output_Size          => 10, -- Deliberately small
                Max_Subscribe_Topics => 20
      )      );
      declare
         Client : Test_Client renames Test_Client (Ptr (Reference).all);

         procedure Test_1 is
         begin
            Set_Overlapped_Size (Client, 4); -- One response packet
            Connect
            (  Server,
               Client'Unchecked_Access,
               Server_Address,
               8885 -- Encrypted autheticated
            );
            while not Is_Connected (Client) loop -- Busy waiting
               delay 0.1;
            end loop;
            Put_Line ("MQTT client connected to " & Server_Address);
            Send_Connect
            (  Peer      => Client,
               Client    => "TestMQTTclient",
               User_Name => "ro",
               Password  => "readonly"
            );
            delay 1.0;
            Send_Ping (Client);
            delay 1.0;
            Send_Subscribe
            (  Client,
               12,
               "$SYS/broker/uptime" / "$SYS/broker/load/#",
               (At_Least_Once, Exactly_Once)
            );
            delay 1.0;
            Send_Ping (Client);
            delay 5.0;
            Send_Unsubscribe (Client, 13, "$SYS/broker/uptime" / "??");
            delay 1.0;
            Send_Disconnect (Client);
            delay 1.0;
         end Test_1;

      begin
         Put_Line ("MQTT client test 1 started");
         Test_1;
      end;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_MQTTS_OpenSSL_Client;
