--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_MQTT_Servers_OpenSSL                   Luebeck            --
--  Test server OpenSSL factory                    Winter, 2025       --
--  Interface                                                         --
--                                Last revision :  14:35 11 Mar 2025  --
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

with Ada.Streams;          use Ada.Streams;
with GNAT.Sockets.MQTT;    use GNAT.Sockets.MQTT;
with GNAT.Sockets.Server;  use GNAT.Sockets.Server;
with OpenSSL;              use OpenSSL;

with GNAT.Sockets.Server.OpenSSL;
with Synchronization.Events;

package Test_MQTT_Servers_OpenSSL is
   use GNAT.Sockets;
   use GNAT.Sockets.Server.OpenSSL;

   type Test_Client is new MQTT_Peer with private;
   procedure Finalize (Client : in out Test_Client);
   function Get_Name (Client : Test_Client) return String;
   procedure On_Connect_Accepted
             (  Peer            : in out Test_Client;
                Session_Present : Boolean
             );
   procedure On_Connect_Rejected
             (  Peer     : in out Test_Client;
                Response : Connect_Response
             );
   procedure On_Ping_Response (Peer : in out Test_Client);
   procedure On_Publish
             (  Peer      : in out Test_Client;
                Topic     : String;
                Message   : Stream_Element_Array;
                Packet    : Packet_Identification;
                Duplicate : Boolean;
                Retain    : Boolean
             );
   procedure On_Subscribe_Acknowledgement
             (  Peer   : in out Test_Client;
                Packet : Packet_Identifier;
                Codes  : Return_Code_List
             );
   procedure Set_Name (Peer : in out Test_Client; Name : String);
   procedure Reset_Event (Peer : in out Test_Client);
   procedure Wait_For_Event (Peer : in out Test_Client);

   type MQTTS_OpenSSL_Factory
        (  Max_Subscribe_Topics : Positive;
           Input_Size           : Buffer_Length;
           Output_Size          : Buffer_Length;
           Decoded_Size         : Buffer_Length;
           Max_Connections      : Positive
        )  is new Abstract_OpenSSL_Factory with private;

   function Create
            (  Factory  : access MQTTS_OpenSSL_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr;
   procedure Handshake_Completed
             (  Factory : in out MQTTS_OpenSSL_Factory;
                Client  : in out Connection'Class;
                Session : SSL
             );
   procedure Prepare
             (  Factory : in out MQTTS_OpenSSL_Factory;
                Client  : in out Connection'Class;
                Session : SSL
             );
private
   use Synchronization.Events;

   type String_Ptr is access String;
   type Test_Client is new MQTT_Peer with record
      Name   : String_Ptr;
      Action : Event; -- Connected/subscribed
   end record;

   type MQTTS_OpenSSL_Factory
        (  Max_Subscribe_Topics : Positive;
           Input_Size           : Buffer_Length;
           Output_Size          : Buffer_Length;
           Decoded_Size         : Buffer_Length;
           Max_Connections      : Positive
        )  is new Abstract_OpenSSL_Factory
                  (  Decoded_Size => Decoded_Size
                  )  with null record;
--     procedure Trace_Service_Loop
--               (  Factory : in out MQTTS_OpenSSL_Factory;
--                  Stage   : Service_Loop_Stage;
--                  Server  : in out Connections_Server'Class
--               );

end Test_MQTT_Servers_OpenSSL;
