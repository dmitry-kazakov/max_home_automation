--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_IO.Set_Schedule                         Luebeck            --
--  Interface                                      Summer, 2015       --
--                                                                    --
--                                Last revision :  19:00 04 Feb 2021  --
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

package MAX_IO.Set_Schedule is
--
-- Set_Thermostat_Schedule -- Thermostat schedule
--
--    Box      - The cube address
--    Device   - The thermostat address
--    Schedule - The thermostat schedule
--    Silent   - Do not pop up error box
--    Handler  - The issuer of the request
--
   procedure Set_Thermostat_Schedule
             (  Box      : RF_Address;
                Device   : RF_Address;
                Schedule : Week_Schedule;
                Silent   : Boolean              := False;
                Handler  : Settings_Handler_Ptr := null
             );
private
   type Processing_Status is (Starting, Working, Finished);
   type Set_Schedule_Request is new Settings_Request with record
      Thermostat : RF_Address;
      Schedule   : Week_Schedule;
      Day        : Week_Day;
      Status     : Processing_Status;
   end record;
   overriding
      function Image (Request : Set_Schedule_Request) return String;
   overriding
      procedure Start
                (  Request : in out Set_Schedule_Request;
                   Client  : in out Cube_Client'Class;
                   List    : Cube_List.Map
                );
   overriding
      procedure Continue
                (  Request : in out Set_Schedule_Request;
                   Client  : in out Cube_Client'Class
                );
end MAX_IO.Set_Schedule;
