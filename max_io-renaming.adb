--                                                                    --
--  MAX Home Automation             Copyright (c)  Dmitry A. Kazakov  --
--     MAX_IO.Renaming                              Luebeck            --
--  Implementation                                 Winter, 2019       --
--                                                                    --
--                                Last revision :  13:36 28 Jan 2022  --
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

package body MAX_IO.Renaming is
   use Settings_Request_List.Doubly_Linked;

   procedure Rename
             (  Box     : RF_Address;
                Address : RF_Address;
                Name    : String;
                Silent  : Boolean              := False;
                Handler : Settings_Handler_Ptr := null
             )  is
      Request : constant Item :=
                         new Rename_Request (Name'Length);
      This : Rename_Request'Class renames
             Rename_Request'Class (Request.all);
   begin
      This.Cube        := Box;
      This.Address     := Address;
      This.Name        := Name;
      This.Silent_Mode := Silent;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Rename;

   procedure Rename
             (  Box     : RF_Address;
                Room    : Room_ID;
                Name    : String;
                Silent  : Boolean              := False;
                Handler : Settings_Handler_Ptr := null
             )  is
      Request : constant Item :=
                         new Rename_Request (Name'Length);
      This : Rename_Request'Class renames
             Rename_Request'Class (Request.all);
   begin
      This.Cube        := Box;
      This.Room        := Room;
      This.Name        := Name;
      This.Silent_Mode := Silent;
      Register (Handler, Request);
      Worker.Enqueue (Request);
   end Rename;

   function Image (Request : Rename_Request) return String is
   begin
      if Request.Room = No_Room then
         return "rename device " & Image (Request.Address) &
                " to "           & Quote (Request.Name);
      else
         return "rename room " & Image (Integer (Request.Room)) &
                " to "         & Quote (Request.Name);
      end if;
   end Image;

   procedure Start
             (  Request : in out Rename_Request;
                Client  : in out Cube_Client'Class;
                List    : Cube_List.Map
             )  is
   begin
      if Request.Room = No_Room then -- Rename device
         Trace
         (  (  "Renaming device "
            &  Image (Request.Address)
            &  " of the cube "
            &  Image (Request.Cube)
            &  " to "
            &  Quote (Request.Name)
            ),
            Message_Text
         );
         Client.Rename_Device
         (  Address => Request.Address,
            Name    => Request.Name
         );
      else -- Rename room
         Trace
         (  (  "Renaming room "
            &  Image (Integer (Request.Room))
            &  " of the cube "
            &  Image (Request.Cube)
            &  " to "
            &  Quote (Request.Name)
            ),
            Message_Text
         );
         Client.Rename_Room
         (  ID   => Request.Room,
            Name => Request.Name
         );
      end if;
      Request.Expected := 1;
   end Start;

   procedure Continue
             (  Request : in out Rename_Request;
                Client  : in out Cube_Client'Class
             )  is
   begin
      null;
   end Continue;

end MAX_IO.Renaming;
