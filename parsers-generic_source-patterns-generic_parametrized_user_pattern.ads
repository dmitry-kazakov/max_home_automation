--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.Patterns.            Luebeck            --
--        Generic_Parametrized_User_Pattern        Summer, 2025       --
--  Interface                                                                  --
--                                Last revision :  10:32 12 Jul 2025  --
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
--  Match -- The function doing matching
--
--     Line    - The source line
--     Pointer - To start at, advanced on successful matching
--
--  On exception Pointer indicates the error location.
--
--  Returns :
--
--     The result
--
--  Exceptions :
--
--      Any exception aborts matching
--
--  Non_Volatile -- True if matching an empty string is a failure
--
generic
   type Parameter_Type (<>) is private;
   with function Match
                 (  Line      : String;
                    Pointer   : access Integer;
                    Parameter : Parameter_Type
                 )  return Boolean is <>;
   Not_Voidable : Boolean;
package Parsers.Generic_Source.Patterns.
        Generic_Parametrized_User_Pattern is
--
-- Pattern -- Variable that can be assigned upon patter matching
--
--    Parameter - The parameter
--
   function Pattern (Parameter : Parameter_Type) return Pattern_Type;

private
   type Parameter_Ptr is access Parameter_Type;
   type User_Defined_Pattern is new Pattern_Object with record
      Parameter : Parameter_Ptr;
   end record;
   procedure Finalize (Pattern : in out User_Defined_Pattern);
   function Image (Pattern : User_Defined_Pattern) return String;
   function Match
            (  Pattern : User_Defined_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : User_Defined_Pattern;
               Recursive : Boolean
            )  return Boolean;

end Parsers.Generic_Source.Patterns.Generic_Parametrized_User_Pattern;
