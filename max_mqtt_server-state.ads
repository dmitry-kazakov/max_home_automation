--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_MQTT_Server.State                       Luebeck            --
--  Interface                                      Spring, 2016       --
--                                                                    --
--                                Last revision :  14:21 11 May 2019  --
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

with Ada.Streams;        use Ada.Streams;
with GNAT.Sockets.MQTT;  use GNAT.Sockets.MQTT;

with Generic_Indefinite_Set;
with Object.Handle;

package MAX_MQTT_Server.State is

   package Topic_Sets is new Generic_Indefinite_Set (String);

   type MAX_Server_State is new MQTT_Server with private;
   procedure Disable_Publishing (Server : in out MAX_Server_State);
   procedure Enable_Publishing
             (  Server : in out MAX_Server_State;
                List   : Topic_Sets.Set
             );
private
   type Topics_List is new Object.Entity with record
      Enabled : Boolean := False;
      List    : Topic_Sets.Set;
   end record;
   type Topics_List_Ptr is access Topics_List'Class;
   package Topic_List_Handles is
      new Object.Handle (Topics_List, Topics_List_Ptr);

   type MAX_Server_State is new MQTT_Server with record
      List : Topic_List_Handles.Handle;
   end record;
   overriding
      procedure Received
                (  Server  : in out MAX_Server_State;
                   Client  : in out MQTT_Connection'Class;
                   Topic   : String;
                   Message : Stream_Element_Array;
                   QoS     : QoS_Level;
                   Policy  : in out Message_Type
                );
end MAX_MQTT_Server.State;
