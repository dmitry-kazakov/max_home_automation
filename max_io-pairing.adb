--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_IO.Pairing                              Luebeck            --
--  Implementation                                 Winter, 2019       --
--                                                                    --
--                                Last revision :  11:07 11 Apr 2021  --
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

with Strings_Edit.Floats;    use Strings_Edit.Floats;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

package body MAX_IO.Pairing is
   use Settings_Request_List.Doubly_Linked;

   procedure Add_Paired_Device
             (  Box       : RF_Address;
                Kind_Of   : Device_Type;
                Address   : RF_Address;
                Serial_No : String;
                Name      : String;
                Room      : Room_Data_Handles.Handle;
                Silent    : Boolean := False;
                Handler   : Settings_Handler_Ptr := null
             )  is
      Request : constant Item := new Add_Request (Name'Length);
      This    : Add_Request'Class renames
                Add_Request'Class (Request.all);
   begin
      This.Cube        := Box;
      This.Kind_Of     := Kind_Of;
      This.Address     := Address;
      This.Serial_No   := Serial_No;
      This.Name        := Name;
      This.Room        := Room;
      This.Silent_Mode := Silent;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Add_Paired_Device;

   function Create
            (  Name : String;
               Room : Room_ID := No_Room
            )  return Room_Data_Handles.Handle is
      Result : Room_Data_Handles.Handle;
   begin
      Result.Set (new Room_Data (Name'Length));
      Result.Ptr.Room := Room;
      Result.Ptr.Name := Name;
      return Result;
   end Create;

   procedure Continue
             (  Request : in out Add_Request;
                Client  : in out Cube_Client'Class
             )  is
      Expected : Natural;
   begin
      if Request.Completing then
         Expected := 0;
      else
         Request.Completing := True;
         if Request.Kind_Of = Eco_Button then -- No room
            Client.Add_New_Device
            (  Serial_No   => Request.Serial_No,
               Address     => Request.Address,
               Device_Name => Request.Name,
               S_Commands  => Expected,
               Mode        => S_Response
            );
         else
            declare
               Data : Room_Data'Class renames Request.Room.Ptr.all;
            begin
               if Request.New_Room then -- New room
                  Client.Add_New_Device
                  (  Room_Name   => Data.Name,
                     Kind_Of     => Request.Kind_Of,
                     Serial_No   => Request.Serial_No,
                     Address     => Request.Address,
                     Device_Name => Request.Name,
                     ID          => Data.Room,
                     S_Commands  => Expected,
                     Mode        => S_Response
                  );
               else -- Existing room
                  Client.Add_New_Device
                  (  ID          => Data.Room,
                     Kind_Of     => Request.Kind_Of,
                     Serial_No   => Request.Serial_No,
                     Address     => Request.Address,
                     Device_Name => Request.Name,
                     S_Commands  => Expected,
                     Mode        => S_Response
                  );
               end if;
            end;
         end if;
         Expected := Expected + 1; -- A-response awaited
         Trace_To_File
         (  "Device "
         &  Image (Request.Kind_Of)
         &  " "
         &  Image (Request.Address)
         &  " added. New topology: "
         &  Image_Metadata (Client.Get_Metadata)
         );
      end if;
      Request.Expected := Request.Expected + Expected;
      if Request.Expected = 0 then
         Client.Configuration_Updated ((Kind_Of => Topology_Update));
      end if;
   end Continue;

   procedure Continue
             (  Request : in out Delete_Request;
                Client  : in out Cube_Client'Class
             )  is
      Expected : Natural;
   begin
      if Request.Completing then
         Expected := 0;
      else
         Request.Completing := True;
         Client.Delete
         (  List       => Request.Devices,
            S_Commands => Expected,
            Mode       => S_Response
         );
         Expected := Expected + 1; -- A-response awaited
         Trace_To_File
         (  "Device(s) "
         &  Image (Request.Devices)
         &  " deleted. New topology: "
         &  Image_Metadata (Client.Get_Metadata)
         );
      end if;
      Request.Expected := Request.Expected + Expected;
      if Request.Expected = 0 then
         Client.Configuration_Updated ((Kind_Of => Topology_Update));
      end if;
   end Continue;

   procedure Continue
             (  Request : in out Delete_Room_Request;
                Client  : in out Cube_Client'Class
             )  is
      Expected : Natural;
   begin
      if Request.Completing then
         Expected := 0;
      else
         Request.Completing := True;
         Client.Delete_Room
         (  ID         => Request.Room,
            S_Commands => Expected,
            Mode       => S_Response
         );
         Trace_To_File
         (  "Room "
         &  Image (Integer (Request.Room))
         &  " deleted. New topology: "
         &  Image_Metadata (Client.Get_Metadata)
         );
      end if;
      Request.Expected := Request.Expected + Expected;
      if Request.Expected = 0 then
         Client.Configuration_Updated ((Kind_Of => Topology_Update));
      end if;
   end Continue;

   procedure Continue
             (  Request : in out Pair_Request;
                Client  : in out Cube_Client'Class
             )  is
   begin
      null;
   end Continue;

   procedure Continue
             (  Request : in out Reset_Cube_Request;
                Client  : in out Cube_Client'Class
             )  is
   begin
      null;
   end Continue;

   procedure Continue
             (  Request : in out Stop_Pair_Request;
                Client  : in out Cube_Client'Class
             )  is
   begin
      null;
   end Continue;

   procedure Delete_Paired_Device
             (  Box     : RF_Address;
                Address : RF_Address;
                Cancel   : Boolean;
                Silent  : Boolean              := False;
                Handler : Settings_Handler_Ptr := null
             )  is
   begin
      Delete_Paired_Device
      (  Box        => Box,
         Addresses  => (1 => Address),
         Cancel     => Cancel,
         Silent     => Silent,
         Handler    => Handler
      );
   end Delete_Paired_Device;

   procedure Delete_Paired_Device
             (  Box       : RF_Address;
                Addresses : RF_Address_Array;
                Cancel    : Boolean;
                Silent    : Boolean              := False;
                Handler   : Settings_Handler_Ptr := null
             )  is
   begin
      if Addresses'Length = 0 then
         return;
      end if;
      declare
         Request : constant Item :=
                      new Delete_Request (Addresses'Length);
         This    : Delete_Request'Class renames
                   Delete_Request'Class (Request.all);
      begin
         This.Cube        := Box;
         This.Devices     := Addresses;
         This.Cancel      := Cancel;
         This.Silent_Mode := Silent;
         Register (Handler, Request);
         Worker.Enqueue (Request);
      end;
   end Delete_Paired_Device;

   procedure Delete_Room
             (  Box     : RF_Address;
                Room    : Room_ID;
                Silent  : Boolean              := False;
                Handler : Settings_Handler_Ptr := null
             )  is
      Request : constant Item := new Delete_Room_Request;
      This    : Delete_Room_Request'Class renames
                Delete_Room_Request'Class (Request.all);
   begin
      This.Cube        := Box;
      This.Room        := Room;
      This.Silent_Mode := Silent;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Delete_Room;

   function Image (Request : Add_Request) return String is
   begin
      return "add " & Image (Request.Kind_Of) &
                " " & Image (Request.Address) &
             " to " & Image (Request.Cube);
   end Image;

   function Image (Request : Delete_Request) return String is
   begin
      if Request.Cancel then
         if Request.Devices'Length > 0 then
            return "cancel pairing " & Image (Request.Cube) &
                      " and delete " & Image (Request.Devices);
         else
            return "cancel pairing " & Image (Request.Cube);
         end if;
      else
         return "delete " & Image (Request.Devices) &
                " from "  & Image (Request.Cube);
      end if;
   end Image;

   function Image (Request : Delete_Room_Request) return String is
   begin
     return "delete room " & Image (Integer (Request.Room)) &
            " from "       & Image (Request.Cube);
   end Image;

   function Image (Request : Pair_Request) return String is
   begin
      return "pair " & Image (Request.Cube);
   end Image;

   function Image (Request : Reset_Cube_Request) return String is
   begin
      return "reset cube " & Image (Request.Cube);
   end Image;

   function Image (Request : Stop_Pair_Request) return String is
   begin
      return "stop pairing " & Image (Request.Cube);
   end Image;

   procedure Pair
             (  Box     : RF_Address;
                Time    : Duration;
                Silent  : Boolean              := False;
                Handler : Settings_Handler_Ptr := null
             )  is
      Request : constant Item := new Pair_Request;
      This    : Pair_Request'Class renames
                Pair_Request'Class (Request.all);
   begin
      This.Cube        := Box;
      This.Time        := Time;
      This.Silent_Mode := Silent;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Pair;

   procedure Reset_Cube
             (  Box     : RF_Address;
                Silent  : Boolean              := False;
                Handler : Settings_Handler_Ptr := null
             )  is
      Request : constant Item := new Reset_Cube_Request;
      This    : Reset_Cube_Request'Class renames
                Reset_Cube_Request'Class (Request.all);
   begin
      This.Cube        := Box;
      This.Silent_Mode := Silent;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Reset_Cube;

   procedure Start
             (  Request : in out Add_Request;
                Client  : in out Cube_Client'Class;
                List    : Cube_List.Map
             )  is
   begin
      if Request.Kind_Of = Eco_Button then -- No room
         Trace
         (  (  "Adding "
            &  Image (Request.Kind_Of)
            &  " "
            &  Image (Request.Address)
            &  " to the cube "
            &  Image (Request.Cube)
            ),
            Message_Text
         );
         Client.Add_New_Device
         (  Serial_No   => Request.Serial_No,
            Address     => Request.Address,
            Device_Name => Request.Name,
            S_Commands  => Request.Expected,
            Mode        => S_Command
         );
      else
         declare
            Data : Room_Data'Class renames Request.Room.Ptr.all;
         begin
            if Data.Room = No_Room then -- New room
               Request.New_Room := True;
               Trace
               (  (  "Adding "
                  &  Image (Request.Kind_Of)
                  &  " "
                  &  Image (Request.Address)
                  &  " to the new room "
                  &  Quote (Data.Name)
                  &  " of the cube "
                  &  Image (Request.Cube)
                  ),
                  Message_Text
               );
               Client.Add_New_Device
               (  Room_Name   => Data.Name,
                  Kind_Of     => Request.Kind_Of,
                  Serial_No   => Request.Serial_No,
                  Address     => Request.Address,
                  Device_Name => Request.Name,
                  ID          => Data.Room,
                  S_Commands  => Request.Expected,
                  Mode        => S_Command
               );
            else -- Existing room
               Trace
               (  (  "Adding "
                  &  Image (Request.Kind_Of)
                  &  " "
                  &  Image (Request.Address)
                  &  " to the room "
                  &  Quote (Data.Name)
                  &  " of the cube "
                  &  Image (Request.Cube)
                  ),
                  Message_Text
               );
               Client.Add_New_Device
               (  ID          => Data.Room,
                  Kind_Of     => Request.Kind_Of,
                  Serial_No   => Request.Serial_No,
                  Address     => Request.Address,
                  Device_Name => Request.Name,
                  S_Commands  => Request.Expected,
                  Mode        => S_Command
               );
            end if;
         end;
      end if;
   end Start;

   procedure Start
             (  Request : in out Delete_Request;
                Client  : in out Cube_Client'Class;
                List    : Cube_List.Map
             )  is
   begin
      if Request.Cancel then
         Trace
         (  "Cancel pairing " & Image (Request.Cube),
            Message_Text
         );
         Client.Cancel_Pairing;
      end if;
      Trace
      (  (  "Deleting devices "
         &  Image (Request.Devices)
         &  " from the cube "
         &  Image (Request.Cube)
         ),
         Message_Text
      );
      Client.Delete
      (  List       => Request.Devices,
         S_Commands => Request.Expected,
         Mode       => S_Command
      );
      Request.Expected := Request.Expected + 1; -- A-response expected
   end Start;

   procedure Start
             (  Request : in out Delete_Room_Request;
                Client  : in out Cube_Client'Class;
                List    : Cube_List.Map
             )  is
   begin
      Trace
      (  (  "Deleting room "
         &  Image (Integer (Request.Room))
         &  " from the cube "
         &  Image (Request.Cube)
         ),
         Message_Text
      );
      Client.Delete_Room
      (  ID         => Request.Room,
         S_Commands => Request.Expected,
         Mode       => S_Command
      );
      Request.Expected := Request.Expected + 1; -- A-response expected
   end Start;

   procedure Start
             (  Request : in out Pair_Request;
                Client  : in out Cube_Client'Class;
                List    : Cube_List.Map
             )  is
   begin
      Trace
       (  (  "Initiate pairing to the cube "
          &  Image (Request.Cube)
          &  " for "
          &  Image (Float (Request.Time), AbsSmall => 0)
          &  "s"
          ),
          Message_Text
      );
      Client.Pair (Request.Time);
      Request.Expected := 0;
   end Start;

   procedure Start
             (  Request : in out Reset_Cube_Request;
                Client  : in out Cube_Client'Class;
                List    : Cube_List.Map
             )  is
   begin
      Trace ("Reset the cube " & Image (Request.Cube), Message_Text);
      Client.Reset_Devices;
      Request.Expected := 2; -- Two A-responses
   end Start;

   procedure Start
             (  Request : in out Stop_Pair_Request;
                Client  : in out Cube_Client'Class;
                List    : Cube_List.Map
             )  is
   begin
      Trace
       (  "Stop pairing to the cube " & Image (Request.Cube),
          Message_Text
      );
      Client.Cancel_Pairing;
      Request.Expected := 0;
   end Start;

   procedure Stop_Pairing
             (  Box     : RF_Address;
                Silent  : Boolean              := False;
                Handler : Settings_Handler_Ptr := null
             )  is
      Request : constant Item := new Stop_Pair_Request;
      This    : Stop_Pair_Request'Class renames
                Stop_Pair_Request'Class (Request.all);
   begin
      This.Cube        := Box;
      This.Silent_Mode := Silent;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Stop_Pairing;

end MAX_IO.Pairing;
