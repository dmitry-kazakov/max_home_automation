--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_IO.Renaming                             Luebeck            --
--  Interface                                      Winter, 2019       --
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

package MAX_IO.Renaming is
--
-- Rename -- Rename device
--
--    Box     - The cube
--    Address - The device address
--    Name    - The new name
--    Silent  - Do not pop up error box
--    Handler - The issuer of the request
--
   procedure Rename
             (  Box     : RF_Address;
                Address : RF_Address;
                Name    : String;
                Silent  : Boolean              := False;
                Handler : Settings_Handler_Ptr := null
             );
--
-- Rename -- Rename room
--
--    Box     - The cube
--    Room    - The room ID
--    Name    - The new name
--    Silent  - Do not pop up error box
--    Handler - The issuer of the request
--
   procedure Rename
             (  Box     : RF_Address;
                Room    : Room_ID;
                Name    : String;
                Silent  : Boolean              := False;
                Handler : Settings_Handler_Ptr := null
             );
private
   type Rename_Request (Name_Length : Natural) is
      new Settings_Request with
   record
      Address : RF_Address := 0;
      Room    : Room_ID    := No_Room;
      Name    : String (1..Name_Length);
   end record;
   overriding
      function Image (Request : Rename_Request) return String;
   overriding
      procedure Start
                (  Request : in out Rename_Request;
                   Client  : in out Cube_Client'Class;
                   List    : Cube_List.Map
                );
   overriding
      procedure Continue
                (  Request : in out Rename_Request;
                   Client  : in out Cube_Client'Class
                );
end MAX_IO.Renaming;
