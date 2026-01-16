--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_IO.Set_Mode                             Luebeck            --
--  Implementation                                 Summer, 2015       --
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

with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with MAX_MQTT_Server;        use MAX_MQTT_Server;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body MAX_IO.Set_Mode is
   use Settings_Request_List.Doubly_Linked;

   procedure Continue
             (  Request : in out Set_Mode_Request;
                Client  : in out Cube_Client'Class
             )  is
   begin
      null;
   end Continue;

   procedure Continue
             (  Request : in out Wake_Up_Request;
                Client  : in out Cube_Client'Class
             )  is
   begin
      null;
   end Continue;

   function Image (Request : Set_Mode_Request) return String is
   begin
      return "set mode " & Image (Request.Thermostat);
   end Image;

   function Image (Request : Wake_Up_Request) return String is
   begin
      if Request.Room /= No_Room then
         return "waking up room "                &
                Image (Integer (Request.Room)) &
                " of "                         &
                Image (Request.Cube);
      elsif Request.Length > 0 then
         return "waking up device(s) "    &
                Image (Request.Devices) &
                " of "                  &
                Image (Request.Cube);
      else
         return "waking up cube " & Image (Request.Cube);
      end if;
   end Image;

   procedure Set_Thermostat_Mode
             (  Box         : RF_Address;
                Device      : RF_Address;
                Mode        : Operating_Mode;
                Temperature : Centigrade           := Centigrade'First;
                Up_Until    : Time                 := Clock;
                Disposition : Temperature_Mode     := Absolute;
                Silent      : Boolean              := False;
                Handler     : Settings_Handler_Ptr := null
             )  is
      Request : constant Item := new Set_Mode_Request;
      This    : Set_Mode_Request'Class renames
                Set_Mode_Request'Class (Request.all);
   begin
      This.Cube            := Box;
      This.Thermostat      := Device;
      This.New_Mode        := Mode;
      This.New_Temperature := Temperature;
      This.Up_Until        := Up_Until;
      This.New_Disposition := Disposition;
      This.Silent_Mode     := Silent;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Set_Thermostat_Mode;

   procedure Start
             (  Request : in out Set_Mode_Request;
                Client  : in out Cube_Client'Class;
                List    : Cube_List.Map
             )  is
      procedure Change_Mode
                (  Client : in out Cube_Client'Class;
                   Data   : Device_Data
                )  is
         Thermostat : RF_Address renames Data.Address;
      begin
         case Request.New_Disposition is
            when Airing =>
               Request.New_Temperature :=
                  Client.Get_Device_Parameters (Thermostat).Window_Open;
            when Eco =>
               Request.New_Temperature :=
                  Client.Get_Device_Parameters (Thermostat).Eco;
            when Comfort =>
               Request.New_Temperature :=
                  Client.Get_Device_Parameters (Thermostat).Comfort;
            when Absolute =>
               null;
            when Increment =>
               Request.New_Temperature :=
                  Data.New_Temperature + Request.New_Temperature;
            when Decrement =>
               Request.New_Temperature :=
                  Data.New_Temperature - Request.New_Temperature;
         end case;
         loop
            begin
               case Request.New_Mode is
                  when Automatic =>
                     if Request.New_Temperature = Centigrade'First then
                        Trace
                        (  (  "Setting thermostat "
                           &  Image (Thermostat)
                           &  " to "
                           &  Image (Request.New_Mode)
                           &  " mode"
                           ),
                           Message_Text
                        );
                     else
                        Trace
                        (  (  "Setting thermostat "
                           &  Image (Thermostat)
                           &  " to "
                           &  Image (Request.New_Mode)
                           &  " mode "
                           &  Image (Request.New_Temperature)
                           ),
                           Message_Text
                        );
                     end if;
                     Client.Set_Thermostat_Automatic
                     (  Thermostat,
                        Request.New_Temperature
                     );
                  when Manual =>
                     Trace
                     (  (  "Setting thermostat "
                        &  Image (Thermostat)
                        &  " to "
                        &  Image (Request.New_Mode)
                        &  " mode "
                        &  Image (Request.New_Temperature)
                        ),
                        Message_Text
                     );
                     Client.Set_Thermostat_Temperature
                     (  Thermostat,
                        Request.New_Temperature
                     );
                  when Vacation =>
                     Trace
                     (  (  "Setting thermostat "
                        &  Image (Thermostat)
                        &  " to "
                        &  Image (Request.New_Mode)
                        &  " mode "
                        &  Image (Request.New_Temperature)
                        &  " until "
                        &  Image (Request.Up_Until)
                        ),
                        Message_Text
                     );
                     Client.Set_Thermostat_Temperature
                     (  Thermostat,
                        Request.New_Temperature,
                        Request.Up_Until
                     );
                  when Boost =>
                     Trace
                     (  (  "Setting thermostat "
                        &  Image (Thermostat)
                        &  " to "
                        &  Image (Request.New_Mode)
                        &  " mode"
                        ),
                        Message_Text
                     );
                     Client.Set_Thermostat_Boost (Request.Thermostat);
               end case;
               exit;
            exception
               when Error : Use_Error =>
                  if Clock - Request.Started > 2.0 then
                     if not Request.Silent_Mode then
                        Say
                        (  (  "Setting mode fault: "
                           &  Exception_Message (Error)
                           ),
                           "Communication error"
                        );
                     end if;
                     raise End_Error;
                  end if;
                  delay 0.1; -- Try later
            end;
         end loop;
      exception
         when End_Error => -- No such thermostat
            null;
      end Change_Mode;
   begin
      Request.Expected := 0;
      begin
         if Request.Thermostat = 0 then
            for Position in 1..Client.Get_Number_Of_Devices loop
               declare
                  Data : constant Device_Data :=
                         Client.Get_Device_Data (Position);
               begin
                  case Data.Kind_Of is
                     when Radiator_Thermostat |
                          Radiator_Thermostat_Plus =>
                        Trace
                        (  (  "Waking up thermostat "
                           &  Image (Data.Address)
                           ),
                           Message_Text
                        );
                        Client.Wake_Up (Data.Address);
                        Change_Mode (Client, Data);
                        Request.Expected := Request.Expected + 1;
                     when others =>
                        null;
                  end case;
               end;
            end loop;
         else
            Trace
            (  (  "Waking up thermostat "
               &  Image (Request.Thermostat)
               ),
               Message_Text
            );
            Client.Wake_Up (Request.Thermostat);
            Change_Mode
            (  Client,
               Client.Get_Device_Data (Request.Thermostat)
            );
            Request.Expected := Request.Expected + 1;
         end if;
      exception
         when Status_Error =>
            Trace
            (  (  "Setting thermostat "
               &  Image (Request.Thermostat)
               &  " error, the thermostat has not yet responded"
               ),
               Error_Text
            );
            raise;
         when Error : others =>
            Trace
            (  (  "Setting thermostat "
               &  Image (Request.Thermostat)
               &  " error: "
               &  Exception_Information (Error)
               ),
               Error_Text
            );
            raise;
      end;
   end Start;

   procedure Start
             (  Request : in out Wake_Up_Request;
                Client  : in out Cube_Client'Class;
                List    : Cube_List.Map
             )  is
   begin
      Request.Expected := 0;
      if Request.Room /= No_Room then
         Client.Wake_Up (Request.Room);
         Request.Expected := Request.Expected + 1;
      elsif Request.Length > 0 then
         for Index in Request.Devices'Range loop
            Client.Wake_Up (Request.Devices (Index));
            Request.Expected := Request.Expected + 1;
         end loop;
      else
         Client.Wake_Up;
         Request.Expected := Request.Expected + 1;
      end if;
   exception
      when Status_Error =>
         Trace
         (  "Waking up, cube not yet responded",
            Error_Text
         );
         raise;
      when Error : others =>
         Trace
         (  "Waking up error: " & Exception_Information (Error),
            Error_Text
         );
         raise;
   end Start;

   procedure Wake_Up
             (  Box         : RF_Address;
                Silent      : Boolean              := False;
                Handler     : Settings_Handler_Ptr := null
             )  is
      Request : constant Item := new Wake_Up_Request (0);
      This    : Wake_Up_Request'Class renames
                Wake_Up_Request'Class (Request.all);
   begin
      This.Cube        := Box;
      This.Silent_Mode := Silent;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Wake_Up;

   procedure Wake_Up
             (  Box         : RF_Address;
                Devices     : RF_Address_Array;
                Silent      : Boolean              := False;
                Handler     : Settings_Handler_Ptr := null
             )  is
      Request : constant Item := new Wake_Up_Request (Devices'Length);
      This    : Wake_Up_Request'Class renames
                Wake_Up_Request'Class (Request.all);
   begin
      This.Cube        := Box;
      This.Devices     := Devices;
      This.Silent_Mode := Silent;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Wake_Up;

   procedure Wake_Up
             (  Box         : RF_Address;
                Room        : Room_ID;
                Silent      : Boolean              := False;
                Handler     : Settings_Handler_Ptr := null
             )  is
      Request : constant Item := new Wake_Up_Request (0);
      This    : Wake_Up_Request'Class renames
                Wake_Up_Request'Class (Request.all);
   begin
      This.Cube        := Box;
      This.Room        := Room;
      This.Silent_Mode := Silent;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Wake_Up;

end MAX_IO.Set_Mode;
