--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Py.Generic_Class.Generic_Keyed_Method       Luebeck            --
--  Interface                                      Summer, 2025       --
--                                                                    --
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
--  Add new method to the Python class, postopned until creation
--
--     Name    - The name of the method
--     Doc     - The documentation text
--     Profile - The arguments list of the method
--     Method  - The Ada implementation
--
generic
   Name    : String;
   Doc     : String;
   Profile : Argument_List;
   with procedure Method
                  (  Data      : in out Value_Type;
                     Arguments : Actual_Argument_List;
                     Result    : out Handle
                  );
package Py.Generic_Class.Generic_Keyed_Method is
private
   function Call
            (  Self     : Object;
               Args     : Object;
               Keywords : Object
            )  return Object;
   pragma Convention (C, Call);
   Call_Ptr : constant CFunctionWithKeywords := Call'Access;
end Py.Generic_Class.Generic_Keyed_Method;
