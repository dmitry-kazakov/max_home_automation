--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Py.Generic_Class.                           Luebeck            --
--        Generic_Member                           Summer, 2025       --
--  Implementation                                                    --
--                                Last revision :  12:06 02 Sep 2025  --
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

with Strings_Edit.Quoted;  use Strings_Edit.Quoted;

package body Py.Generic_Class.Generic_Member is

   function Getter
            (  Self    : Object;
               Closure : System.Address
            )  return Object is
      Result : Handle;
      Data   : Value_Type renames To_Value_Ptr (Self).all;
   begin
      Result := Get (Data);
      if Result.Ptr /= Null_Object then
         Links.IncRef (Result.Ptr);
      end if;
      return Result.Ptr;
   exception
      when Python_Error =>
         return Null_Object;
      when others =>
         Throw_TypeError (Error_Setting & Quote (Name));
         return Null_Object;
   end Getter;

   function Setter
            (  Self    : Object;
               Value   : Object;
               Closure : System.Address
            )  return int is
      Item : Handle;
      Data : Value_Type renames To_Value_Ptr (Self).all;
   begin
      if Value /= Null_Object then
         Item.Ptr := Value;
         Links.IncRef (Item.Ptr);
      end if;
      Set (Data, Item);
      return 0;
   exception
      when Python_Error =>
         return -1;
      when others =>
         Throw_TypeError (Error_Setting & Quote (Name));
         return -1;
   end Setter;

begin
   Add_Member
   (  (  Name    => New_String (Name),
         Get     => Get_Ptr,
         Set     => Set_Ptr,
         Doc     => New_String (Doc),
         Closure => System.Null_Address
   )  );
end Py.Generic_Class.Generic_Member;
