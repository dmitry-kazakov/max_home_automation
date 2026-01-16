--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_MQTT_Server                             Luebeck            --
--  Interface                                      Spring, 2016       --
--                                                                    --
--                                Last revision :  18:37 02 Jun 2023  --
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

with GNAT.Sockets.MQTT.Server;  use GNAT.Sockets.MQTT.Server;

with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
use  GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;

package MAX_MQTT_Server is

--
-- Encode -- Encode device data in JSON format
--
--    Data      - The device data
--    Serial_No - The device serial number
--    Name      - The device name
--    Offset    - The device offset
--
   function Encode
            (  Data      : Device_Data;
               Serial_No : String;
               Name      : String;
               Offset    : Centigrade
            )  return String;

   function Image (Mode : Operating_Mode) return String;
   procedure Publish
             (  Client    : in out ELV_MAX_Cube_Client'Class;
                Server    : in out MQTT_Server'Class;
                Cube      : RF_Address;
                Connected : Boolean;
                Policy    : Message_Type
             );
   procedure Publish
             (  Client    : in out ELV_MAX_Cube_Client'Class;
                Server    : in out MQTT_Server'Class;
                Data      : Device_Data;
                Serial_No : String;
                Name      : String;
                Offset    : Centigrade;
                Policy    : Message_Type
             );
   procedure Publish
             (  Client  : in out ELV_MAX_Cube_Client'Class;
                Server  : in out MQTT_Server'Class;
                Cube    : RF_Address;
                Average : Integer;
                Min     : Integer;
                Max     : Integer;
                Policy  : Message_Type
             );
   procedure Publish
             (  Client  : in out ELV_MAX_Cube_Client'Class;
                Server  : in out MQTT_Server'Class;
                Cube    : RF_Address;
                Duty    : Ratio;
                Policy  : Message_Type
             );

end MAX_MQTT_Server;
