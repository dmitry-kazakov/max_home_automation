--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_IO.Storing_Parameters                   Luebeck            --
--  Interface                                      Summer, 2015       --
--                                                                    --
--                                Last revision :  14:58 31 Oct 2019  --
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

package MAX_IO.Storing_Parameters is
--
-- Store_Configuration -- Thermostat and topology parameters
--
--    Box        - The cube address
--    Device     - The thermostat address
--    Parameters - The temperature schedule
--    Action     - What to store
--    Handler    - The issuer of the request
--
   procedure Store_Configuration
             (  Box        : RF_Address;
                Device     : RF_Address;
                Parameters : Device_Parameters;
                Action     : Store_Action;
                Handler    : Settings_Handler_Ptr := null
             );
--
-- Store_Topology -- Devices and rooms topology
--
--    Box      - The cube address
--    Topology - The topology to store
--    Handler  - The issuer of the request
--
   procedure Store_Topology
             (  Box      : RF_Address;
                Topology : String;
                Handler  : Settings_Handler_Ptr := null
             );
private
   type Store_Request
        (  Kind_Of     : Device_Type;
           Name_Length : Natural
        )  is new Settings_Request with
   record
      Wake_Up    : Boolean      := True;
      Day        : Week_Day     := Week_Day'First;
      Mode       : Setting_Mode := S_Command;
      Thermostat : RF_Address;
      Action     : Store_Action;
      Parameters : Device_Parameters (Kind_Of, Name_Length);
   end record;
   overriding
      function Image (Request : Store_Request) return String;
   overriding
      procedure Start
                (  Request : in out Store_Request;
                   Client  : in out Cube_Client'Class;
                   List    : Cube_List.Map
                );
   overriding
      procedure Continue
                (  Request : in out Store_Request;
                   Client  : in out Cube_Client'Class
                );

   type Topology_Request (Length : Natural) is
      new Settings_Request with
   record
      Wake_Up  : Boolean := False;
      Topology : String (1..Length);
   end record;
   overriding
      function Image (Request : Topology_Request) return String;
   overriding
      procedure Start
                (  Request : in out Topology_Request;
                   Client  : in out Cube_Client'Class;
                   List    : Cube_List.Map
                );
   overriding
      procedure Continue
                (  Request : in out Topology_Request;
                   Client  : in out Cube_Client'Class
                );

end MAX_IO.Storing_Parameters;
