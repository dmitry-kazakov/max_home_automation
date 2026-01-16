--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Py.Generic_Class.                           Luebeck            --
--        Generic_Member                           Summer, 2025       --
--  Interface                                                                  --
--                                Last revision :  22:30 19 Jun 2025  --
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
--
--  Add new member to the Python class, postopned until creation
--
--     Name - The name of the method
--     Doc  - The documentation text
--     Get  - The method getter
--     Set  - The method setter
--
generic
   Name : String;
   Doc  : String;
   with function Get (Data : Value_Type) return Handle is <>;
   with procedure Set
                  (  Data  : in out Value_Type;
                     Value : Handle
                  )  is <>;
package Py.Generic_Class.Generic_Member is
private
   function Getter (Self : Object; Closure : System.Address)
      return Object;
   pragma Convention (C, Getter);
   Get_Ptr : constant getter_Ptr := Getter'Access;

   function Setter
            (  Self    : Object;
               Value   : Object;
               Closure : System.Address
            )  return int;
   pragma Convention (C, Setter);
   Set_Ptr : constant setter_Ptr := Setter'Access;
end Py.Generic_Class.Generic_Member;
