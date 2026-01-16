--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_IO.Set_Schedule                         Luebeck            --
--  Implementation                                 Summer, 2015       --
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

package body MAX_IO.Set_Schedule is
   use Settings_Request_List.Doubly_Linked;

   function Image (Request : Set_Schedule_Request) return String is
   begin
      return "set schedule " & Image (Request.Thermostat);
   end Image;

   procedure Set_Thermostat_Schedule
             (  Box      : RF_Address;
                Device   : RF_Address;
                Schedule : Week_Schedule;
                Silent   : Boolean              := False;
                Handler  : Settings_Handler_Ptr := null
             )  is
      Request : constant Item := new Set_Schedule_Request;
      This    : Set_Schedule_Request'Class renames
                Set_Schedule_Request'Class (Request.all);
   begin
      This.Cube        := Box;
      This.Thermostat  := Device;
      This.Schedule    := Schedule;
      This.Silent_Mode := Silent;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Set_Thermostat_Schedule;

   procedure Start
             (  Request : in out Set_Schedule_Request;
                Client  : in out Cube_Client'Class;
                List    : Cube_List.Map
             )  is
   begin
--        if Request.Is_Handled then
--           Mode := S_Command;
--        end if;
      Trace
      (  "Waking up thermostat " & Image (Request.Thermostat),
         Message_Text
      );
      Client.Wake_Up (Request.Thermostat);
      Request.Expected := Request.Expected + 1; -- A-response
      Request.Status   := Starting;
      Request.Continue (Client);
   end Start;

   procedure Continue
             (  Request : in out Set_Schedule_Request;
                Client  : in out Cube_Client'Class
             )  is
   begin
      case Request.Status is
         when Starting =>
            Request.Day    := Week_Day'First;
            Request.Status := Working;
         when Working =>
            Client.Set_Thermostat_Schedule
            (  Request.Thermostat,
               Request.Day,
               Request.Schedule (Request.Day).Points,
               S_Response
            );
            if Request.Day = Week_Day'Last then
               Request.Status := Finished;
               return;
            end if;
            Request.Day := Week_Day'Succ (Request.Day);
         when Finished =>
            return;
      end case;
      loop
         if Client.Compare_Schedule
            (  Request.Thermostat,
               Request.Day,
               Request.Schedule
            )  then -- Different schedule
            Trace
            (  (  "Setting thermostat "
               &  Image (Request.Thermostat)
               &  " "
               &  Image (Request.Day, False)
               &  " schedule to "
               &  Image (Request.Schedule (Request.Day))
               ),
               Message_Text
            );
            Client.Set_Thermostat_Schedule
            (  Request.Thermostat,
               Request.Day,
               Request.Schedule (Request.Day).Points,
               S_Command
            );
            Request.Expected := Request.Expected + 2;
            return;
         end if;
         Trace
         (  (  "Skipping thermostat "
            &  Image (Request.Thermostat)
            &  " "
            &  Image (Request.Day, False)
            &  " schedule, it is same"
            ),
            Message_Text
         );
         exit when Request.Day = Week_Day'Last;
         Request.Day := Week_Day'Succ (Request.Day);
      end loop;
      Request.Status := Finished;
   end Continue;

end MAX_IO.Set_Schedule;
