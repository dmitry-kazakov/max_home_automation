--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_IO.Pairing                              Luebeck            --
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

package MAX_IO.Pairing is
--
-- Room_Data -- Shared room data
--
   type Room_Data (Length : Natural) is new Object.Entity with record
      Room : Room_ID := No_Room;
      Name : String (1..Length);
   end record;
   type Room_Data_Ptr is access Room_Data'Class;
   package Room_Data_Handles is
      new Object.Handle (Room_Data, Room_Data_Ptr);
   function Create
            (  Name : String;
               Room : Room_ID := No_Room
            )  return Room_Data_Handles.Handle;
--
-- Add_Paired_Device -- Add paired device
--
--    Box       - The cube
--    Kind_Of   - The device type
--    Address   - The device address
--    Serial_No - The serial number
--    Name      - The device name
--    Room      - The room
--    Silent    - Do not pop up error box
--    Handler   - The issuer of the request
--
   procedure Add_Paired_Device
             (  Box       : RF_Address;
                Kind_Of   : Device_Type;
                Address   : RF_Address;
                Serial_No : String;
                Name      : String;
                Room      : Room_Data_Handles.Handle;
                Silent    : Boolean := False;
                Handler   : Settings_Handler_Ptr := null
             );
--
-- Delete_Paired_Device -- Delete paired device
--
--    Box     - The cube
--    Address - The device address or addresses
--    Cancel  - Cancel pairing
--    Silent  - Do not pop up error box
--    Handler - The issuer of the request
--
   procedure Delete_Paired_Device
             (  Box       : RF_Address;
                Address   : RF_Address;
                Cancel    : Boolean;
                Silent    : Boolean              := False;
                Handler   : Settings_Handler_Ptr := null
             );
   procedure Delete_Paired_Device
             (  Box       : RF_Address;
                Addresses : RF_Address_Array;
                Cancel    : Boolean;
                Silent    : Boolean              := False;
                Handler   : Settings_Handler_Ptr := null
             );
--
-- Delete_Room -- Delete room
--
--    Box     - The cube
--    Room    - The room to delete
--    Silent  - Do not pop up error box
--    Handler - The issuer of the request
--
   procedure Delete_Room
             (  Box     : RF_Address;
                Room    : Room_ID;
                Silent  : Boolean              := False;
                Handler : Settings_Handler_Ptr := null
             );
--
-- Pair -- Start pairing
--
--    Box     - The cube
--    Time    - The pairing time
--    Silent  - Do not pop up error box
--    Handler - The issuer of the request
--
   procedure Pair
             (  Box     : RF_Address;
                Time    : Duration;
                Silent  : Boolean              := False;
                Handler : Settings_Handler_Ptr := null
             );
--
-- Reset_Cube -- Reset cube
--
--    Box     - The cube
--    Silent  - Do not pop up error box
--    Handler - The issuer of the request
--
   procedure Reset_Cube
             (  Box     : RF_Address;
                Silent  : Boolean              := False;
                Handler : Settings_Handler_Ptr := null
             );
--
-- Stop_Pairing -- Stop pairing
--
--    Box     - The cube
--    Silent  - Do not pop up error box
--    Handler - The issuer of the request
--
   procedure Stop_Pairing
             (  Box     : RF_Address;
                Silent  : Boolean              := False;
                Handler : Settings_Handler_Ptr := null
             );
private
   type Add_Request (Length : Natural) is
      new Settings_Request with
   record
      Kind_Of    : Device_Type;
      New_Room   : Boolean := False;
      Completing : Boolean := False;
      Room       : Room_Data_Handles.Handle;
      Address    : RF_Address;
      Serial_No  : String (1..10);
      Name       : String (1..Length);
   end record;
   overriding
      function Image (Request : Add_Request) return String;
   overriding
      procedure Start
                (  Request : in out Add_Request;
                   Client  : in out Cube_Client'Class;
                   List    : Cube_List.Map
                );
   overriding
      procedure Continue
                (  Request : in out Add_Request;
                   Client  : in out Cube_Client'Class
                );

   type Delete_Request (Length : Natural) is
      new Settings_Request with
   record
      Cancel     : Boolean;
      Completing : Boolean := False;
      Devices    : RF_Address_Array (1..Length);
   end record;
   overriding
      function Image (Request : Delete_Request) return String;
   overriding
      procedure Start
                (  Request : in out Delete_Request;
                   Client  : in out Cube_Client'Class;
                   List    : Cube_List.Map
                );
   overriding
      procedure Continue
                (  Request : in out Delete_Request;
                   Client  : in out Cube_Client'Class
                );

   type Delete_Room_Request is new Settings_Request with record
      Room       : Room_ID;
      Completing : Boolean := False;
   end record;
   overriding
      function Image (Request : Delete_Room_Request) return String;
   overriding
      procedure Start
                (  Request : in out Delete_Room_Request;
                   Client  : in out Cube_Client'Class;
                   List    : Cube_List.Map
                );
   overriding
      procedure Continue
                (  Request : in out Delete_Room_Request;
                   Client  : in out Cube_Client'Class
                );

   type Pair_Request is new Settings_Request with record
      Time : Duration;
   end record;
   overriding
      function Image (Request : Pair_Request) return String;
   overriding
      procedure Start
                (  Request : in out Pair_Request;
                   Client  : in out Cube_Client'Class;
                   List    : Cube_List.Map
                );
   overriding
      procedure Continue
                (  Request : in out Pair_Request;
                   Client  : in out Cube_Client'Class
                );

   type Reset_Cube_Request is new Settings_Request with null record;
   overriding
      function Image (Request : Reset_Cube_Request) return String;
   overriding
      procedure Start
                (  Request : in out Reset_Cube_Request;
                   Client  : in out Cube_Client'Class;
                   List    : Cube_List.Map
                );
   overriding
      procedure Continue
                (  Request : in out Reset_Cube_Request;
                   Client  : in out Cube_Client'Class
                );

   type Stop_Pair_Request is new Settings_Request with null record;
   overriding
      function Image (Request : Stop_Pair_Request) return String;
   overriding
      procedure Start
                (  Request : in out Stop_Pair_Request;
                   Client  : in out Cube_Client'Class;
                   List    : Cube_List.Map
                );
   overriding
      procedure Continue
                (  Request : in out Stop_Pair_Request;
                   Client  : in out Cube_Client'Class
                );

end MAX_IO.Pairing;
