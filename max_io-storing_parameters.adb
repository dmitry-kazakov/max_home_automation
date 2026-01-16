--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_IO.Storing_Parameters                   Luebeck            --
--  Implementation                                 Summer, 2015       --
--                                                                    --
--                                Last revision :  17:11 07 Feb 2021  --
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

package body MAX_IO.Storing_Parameters is
   use Settings_Request_List.Doubly_Linked;

   function Image (Request : Topology_Request) return String is
   begin
      return "store " & Image (Request.Cube) & " metadata";
   end Image;

   function Image (Request : Store_Request) return String is
   begin
      return "store " & Image (Request.Cube) & " configuration";
   end Image;

   procedure Start
             (  Request : in out Topology_Request;
                Client  : in out Cube_Client'Class;
                List    : Cube_List.Map
             )  is
   begin
      if Request.Wake_Up then
         Request.Wake_Up := False;
         Trace ("Waking up cube " & Image (Request.Cube), Message_Text);
         Client.Wake_Up;
         Request.Expected := Request.Expected + 1; -- A-response
      end if;
      Trace
      (  (  "Restoring cube "
         &  Image (Request.Cube)
         &  " topology to "
         &  Image_Metadata (Request.Topology)
         ),
         Message_Text
      );
      Reset_Metadata (Client, Request.Topology, Request.Expected);
   end Start;

   procedure Start
             (  Request : in out Store_Request;
                Client  : in out Cube_Client'Class;
                List    : Cube_List.Map
             )  is
   begin
      if Request.Wake_Up then
         Request.Wake_Up := False;
         Trace
         (  (  "Waking up thermostat "
            &  Image (Request.Thermostat)
            ),
            Message_Text
         );
         Client.Wake_Up (Request.Thermostat);
         Request.Expected := Request.Expected + 1; -- A-response
      end if;
      Request.Continue (Client);
   end Start;

   procedure Continue
             (  Request : in out Topology_Request;
                Client  : in out Cube_Client'Class
             )  is
   begin
      null;
   end Continue;

   procedure Continue
             (  Request : in out Store_Request;
                Client  : in out Cube_Client'Class
             )  is
      Parameters : Device_Parameters renames Request.Parameters;
      procedure Set_Parameters is
      begin
         if Parameters.Kind_Of = Wall_Thermostat then
            Client.Set_Thermostat_Parameters
            (  Address     => Request.Thermostat,
               Comfort     => Parameters.Comfort,
               Eco         => Parameters.Eco,
               Max         => Parameters.Max,
               Min         => Parameters.Min,
               Offset      => Parameters.Offset,
               Window_Open => Parameters.Window_Open,
               Mode        => Request.Mode
            );
         else
            Client.Set_Thermostat_Parameters
            (  Address     => Request.Thermostat,
               Comfort     => Parameters.Comfort,
               Eco         => Parameters.Eco,
               Max         => Parameters.Max,
               Min         => Parameters.Min,
               Offset      => Parameters.Offset,
               Window_Open => Parameters.Window_Open,
               Window_Time => Parameters.Window_Time,
               Mode        => Request.Mode
            );
         end if;
      end Set_Parameters;

      procedure Set_Schedule is
      begin
         Client.Set_Thermostat_Schedule
         (  Request.Thermostat,
            Request.Day,
            Parameters.Schedule (Request.Day).Points,
            Request.Mode
         );
      end Set_Schedule;

      procedure Set_Valve is
      begin
         Client.Set_Thermostat_Valve
         (  Address         => Request.Thermostat,
            Boost_Time      => Parameters.Boost_Time,
            Boost_Valve     => Parameters.Boost_Valve,
            Max_Valve       => Parameters.Max_Valve,
            Valve_Offset    => Parameters.Valve_Offset,
            Decalcification => Parameters.Decalcification,
            Mode            => S_Command
         );
      end Set_Valve;
   begin
      case Request.Action is
         when Store_Schedule =>
            loop
               if Request.Mode = S_Response then
                  if Client.Compare_Schedule
                     (  Request.Thermostat,
                        Request.Day,
                        Parameters
                     )  then -- Different schedule
                     Set_Schedule; -- Successfully updated schedule
                  end if;
                  if Request.Day = Week_Day'Last then
                     return;
                  end if;
                  Request.Day  := Week_Day'Succ (Request.Day);
                  Request.Mode := S_Command;
               end if;
               if Client.Compare_Schedule
                  (  Request.Thermostat,
                     Request.Day,
                     Parameters
                  )  then -- Different schedule
                  Trace
                  (  (  "Setting thermostat "
                     &  Image (Request.Thermostat)
                     &  " "
                     &  Image (Request.Day, False)
                     &  " schedule to "
                     &  Image (Parameters.Schedule (Request.Day))
                     ),
                     Message_Text
                  );
                  Set_Schedule;
                  Request.Mode     := S_Response;
                  Request.Expected := Request.Expected + 2;
                  exit;
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
               Request.Mode := S_Response;
            end loop;
         when Store_Parameters =>
            if Request.Mode = S_Command then
               if Client.Compare_Parameters
                  (  Request.Thermostat,
                     Parameters
                  )  then -- Different parameters
                  Trace
                  (  (  "Setting thermostat "
                     &  Image (Request.Thermostat)
                     &  " parameters..."
                     ),
                     Message_Text
                  );
                  Set_Parameters;
                  Request.Mode     := S_Response;
                  Request.Expected := Request.Expected + 1;
               else
                  Trace
                  (  (  "Skipping thermostat "
                     &  Image (Request.Thermostat)
                     &  " parameters, they are same"
                     ),
                     Message_Text
                  );
               end if;
            else
               Set_Parameters;
            end if;
         when Store_Valve =>
            if Request.Mode = S_Command then
               if Client.Compare_Valve
                  (  Request.Thermostat,
                     Parameters
                  )  then
                  Trace
                  (  (  "Setting thermostat "
                     &  Image (Request.Thermostat)
                     &  " valve parameters..."
                     ),
                     Message_Text
                  );
                  Set_Valve;
                  Request.Mode     := S_Response;
                  Request.Expected := Request.Expected + 1;
               else
                  Trace
                  (  (  "Skipping thermostat "
                     &  Image (Request.Thermostat)
                     &  " valve parameters, they are same"
                     ),
                     Message_Text
                  );
               end if;
            else
               Set_Valve;
            end if;
      end case;
   end Continue;

   procedure Store_Topology
             (  Box      : RF_Address;
                Topology : String;
                Handler  : Settings_Handler_Ptr := null
             )  is
      Request : constant Item := new Topology_Request (Topology'Length);
      This    : Topology_Request'Class renames
                Topology_Request'Class (Request.all);
   begin
      This.Cube     := Box;
      This.Topology := Topology;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Store_Topology;

   procedure Store_Configuration
             (  Box        : RF_Address;
                Device     : RF_Address;
                Parameters : Device_Parameters;
                Action     : Store_Action;
                Handler    : Settings_Handler_Ptr := null
             )  is
      Request : constant Item :=
                         new Store_Request
                             (  Kind_Of     => Parameters.Kind_Of,
                                Name_Length => Parameters.Name_Length
                             );
      This    : Store_Request'Class renames
                Store_Request'Class (Request.all);
   begin
      This.Cube       := Box;
      This.Thermostat := Device;
      This.Parameters := Parameters;
      This.Action     := Action;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Store_Configuration;

end MAX_IO.Storing_Parameters;
