--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_IO.Moving                              Luebeck            --
--  Implementation                                 Winter, 2019       --
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

with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

package body MAX_IO.Moving is
   use Settings_Request_List.Doubly_Linked;

   procedure Move
             (  Box     : RF_Address;
                Address : RF_Address;
                Kind_Of : Device_Type;
                Room    : Room_ID;
                Name    : String;
                Silent  : Boolean              := False;
                Handler : Settings_Handler_Ptr := null
             )  is
      Request : constant Item := new Move_Request (Name'Length);
      This : Move_Request'Class renames
             Move_Request'Class (Request.all);
   begin
      This.Cube        := Box;
      This.Address     := Address;
      This.Kind_Of     := Kind_Of;
      This.Room        := Room;
      This.Name        := Name;
      This.Silent_Mode := Silent;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Move;

   function Image (Request : Move_Request) return String is
   begin
      return     "move " & Image (Request.Kind_Of) &
                     " " & Image (Request.Address) &
         " to the room " & Quote (Request.Name);
   end Image;

   procedure Start
             (  Request : in out Move_Request;
                Client  : in out Cube_Client'Class;
                List    : Cube_List.Map
             )  is
   begin
      Trace
      (  (  "Moving "
         &  Image (Request.Kind_Of)
         &  " "
         &  Image (Integer (Request.Address))
         &  " of the cube "
         &  Image (Request.Cube)
         &  " to the room "
         &  Quote (Request.Name)
         ),
         Message_Text
      );
      Client.Detach_Device
      (  Device     => Request.Address,
         S_Commands => Request.Expected,
         Mode       => S_Command
      );
   end Start;

   procedure Continue
             (  Request : in out Move_Request;
                Client  : in out Cube_Client'Class
             )  is
      Expected : Natural;
   begin
      loop
         case Request.Action is
            when Detach_Command =>
               Request.Action := Detach_Response;
               Client.Detach_Device
               (  Device     => Request.Address,
                  S_Commands => Expected,
                  Mode       => S_Response
               );
               Trace_To_File
               (  "Device "
               &  Image (Request.Kind_Of)
               &  " "
               &  Image (Request.Address)
               &  " detached. New topology: "
               &  Image_Metadata (Client.Get_Metadata)
               );
               Expected := Expected + 1; -- A-response awaited
            when Detach_Response =>
               Request.Action := Attach_Command;
               if Request.Room = No_Room then
                  Trace_To_File
                  (  "Attaching "
                  &  Image (Request.Kind_Of)
                  &  " "
                  &  Image (Request.Address)
                  &  " to new room "
                  &  Quote (Request.Name)
                  );
                  Client.Attach_Device
                  (  Device     => Request.Address,
                     Name       => Request.Name,
                     S_Commands => Expected,
                     Mode       => S_Command
                  );
               else
                  Trace_To_File
                  (  "Attaching "
                  &  Image (Request.Kind_Of)
                  &  " "
                  &  Image (Request.Address)
                  &  " to "
                  &  Image (Integer (Request.Room))
                  );
                  Client.Attach_Device
                  (  Device     => Request.Address,
                     ID         => Request.Room,
                     S_Commands => Expected,
                     Mode       => S_Command
                  );
               end if;
            when Attach_Command =>
               Request.Action := Attach_Response;
               if Request.Room = No_Room then
                  Client.Attach_Device
                  (  Device     => Request.Address,
                     Name       => Request.Name,
                     S_Commands => Expected,
                     Mode       => S_Response
                  );
                  Expected := Expected + 1; -- A-response awaited
                  Trace_To_File
                  (  "Device "
                  &  Image (Request.Kind_Of)
                  &  " "
                  &  Image (Request.Address)
                  &  " re-attached creating new room "
                  &  Quote (Request.Name)
                  &  ". New topology: "
                  &  Image_Metadata (Client.Get_Metadata)
                  );
               else
                  Client.Attach_Device
                  (  Device     => Request.Address,
                     ID         => Request.Room,
                     S_Commands => Expected,
                     Mode       => S_Response
                  );
                  Expected := Expected + 1; -- A-response awaited
                  Trace_To_File
                  (  "Device "
                  &  Image (Request.Kind_Of)
                  &  " "
                  &  Image (Request.Address)
                  &  " re-attached to "
                  &  Image (Integer (Request.Room))
                  &  ". New topology: "
                  &  Image_Metadata (Client.Get_Metadata)
                  );
               end if;
            when Attach_Response =>
               Request.Action := Done;
               Expected := 0;
               Client.Configuration_Updated
               (  (Kind_Of => Topology_Update)
               );
               exit;
            when Done =>
               Expected := 0;
               exit;
         end case;
         exit when Expected /= 0;
      end loop;
      Request.Expected := Request.Expected + Expected;
   end Continue;

end MAX_IO.Moving;
