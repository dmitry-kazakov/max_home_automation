--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Py.Generic_Class.                           Luebeck            --
--        Generic_Positional_Method                Summer, 2025       --
--  Implementation                                                    --
--                                Last revision :  16:03 30 Oct 2025  --
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

package body Py.Generic_Class.Generic_Positional_Method is

   function Call (Self : Object; Args : Object) return Object is
      Data : Value_Type renames To_Value_Ptr (Self).all;
   begin
      declare
         List   : Actual_Argument_List
                  (  1
                  .. Argument_Position (Links.Tuple_Size (Args))
                  );
         Result : Handle;
      begin
         for Index in List'Range loop
            List (Index).Ptr :=
               Links.Tuple_GetItem (Args, ssize_t (Index) - 1);
            if List (Index).Ptr = Null_Object then
               Check_Error;
            else
               Links.IncRef (List (Index).Ptr); -- Borrowed reference
            end if;
         end loop;
         Method (Data, List, Result);
         if Result.Ptr /= Null_Object then
            Links.IncRef (Result.Ptr);
         end if;
         return Result.Ptr;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Call;
begin
   Add_Method
   (  (  Name  => New_String (Name),
         Meth  => (False, Call_Ptr),
         Flags => METH_VARARGS + METH_KEYWORDS,
         Doc   => New_String (Doc)
   )  );
end Py.Generic_Class.Generic_Positional_Method;
