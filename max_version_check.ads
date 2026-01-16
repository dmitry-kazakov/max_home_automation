--                                                                    --
--  package MAX_Version_Check       Copyright (c)  Dmitry A. Kazakov  --
--  Version update checking                        Luebeck            --
--  Interface                                      Winter, 2020       --
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

with Ada.Streams;  use Ada.Streams;

with GNAT.Sockets.Connection_State_Machine.HTTP_Client;
with GNAT.Sockets.Connection_State_Machine.HTTP_Server;

package MAX_Version_Check is

   procedure Get_Version;
private
   use GNAT.Sockets.Connection_State_Machine.HTTP_Client;
   use GNAT.Sockets.Connection_State_Machine.HTTP_Server;

   type MAX_HTTP_Client;
   type Content
        (  Client : not null access MAX_HTTP_Client'Class
        )  is new Content_Destination with
   record
      State : Natural := 1;
      Major : Natural := 0;
      Minor : Natural := 0;
   end record;
   overriding
      procedure Commit (Destination : in out Content);
   overriding
      procedure Put
                (  Destination : in out Content;
                   Data        : String
                );
   procedure Write
             (  Stream : not null access Root_Stream_Type'Class;
                Item   : Content
             );
   for Content'Write use Write;

   type MAX_HTTP_Client is new HTTP_Session with record
      Page : aliased Content (MAX_HTTP_Client'Unchecked_Access);
   end record;
   overriding
      procedure Activated
                (  Client : in out MAX_HTTP_Client
                );
   overriding
      procedure Connect_Error
                (  Client : in out MAX_HTTP_Client;
                   Error  : GNAT.Sockets.Error_Type
                );

end MAX_Version_Check;
