--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_IO.Set_Display                          Luebeck            --
--  Implementation                                 Summer, 2015       --
--                                                                    --
--                                Last revision :  16:05 08 Jun 2019  --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body MAX_IO.Set_Display is
   use Settings_Request_List.Doubly_Linked;

   function Image (Request : Set_Display_Request) return String is
   begin
      return "set display " & Image (Request.Thermostat);
   end Image;

   procedure Set_Display_Mode
             (  Box         : RF_Address;
                Device      : RF_Address;
                Temperature : Display_Mode;
                Silent      : Boolean              := False;
                Handler     : Settings_Handler_Ptr := null
             )  is
      Request : constant Item := new Set_Display_Request;
      This    : Set_Display_Request'Class renames
                Set_Display_Request'Class (Request.all);
   begin
      This.Cube        := Box;
      This.Thermostat  := Device;
      This.Temperature := Temperature;
      This.Silent_Mode := Silent;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Set_Display_Mode;

   procedure Start
             (  Request : in out Set_Display_Request;
                Client  : in out Cube_Client'Class;
                List    : Cube_List.Map
             )  is
      function Get_Display_Mode return String is
      begin
         case Request.Temperature is
            when Display_Is_Temperature =>
               return "measured termperature";
            when Display_Set_Temperature =>
               return "set termperature";
         end case;
      end Get_Display_Mode;

      Mode : constant String := Get_Display_Mode;

      procedure Change_Mode (Client : in out Cube_Client'Class) is
      begin
         loop
            begin
               Trace
               (  (  "Setting wall thermostat display "
                  &  Image (Request.Thermostat)
                  &  " to "
                  &  Mode
                  ),
                  Message_Text
               );
               Client.Set_Thermostat_Display
               (  Request.Thermostat,
                  Request.Temperature
               );
               exit;
            exception
               when Error : Use_Error =>
                  if Clock - Request.Started > 2.0 then
                     Request.Report
                     (  "Setting wall thermostat display fault: "
                     &  Exception_Message (Error)
                     );
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
      Change_Mode (Client);
      Request.Expected := Request.Expected + 1;
   exception
      when Status_Error =>
         Trace
         (  (  "Setting wall thermostat display"
            &  Image (Request.Thermostat)
            &  " error, the thermostat has not yet responded"
            ),
            Error_Text
         );
      when Error : others =>
         Trace
         (  (  "Setting wall thermostat display "
            &  Image (Request.Thermostat)
            &  " error: "
            &  Exception_Information (Error)
            ),
            Error_Text
         );
   end Start;

   procedure Continue
             (  Request : in out Set_Display_Request;
                Client  : in out Cube_Client'Class
             )  is
   begin
      null;
   end Continue;

end MAX_IO.Set_Display;
