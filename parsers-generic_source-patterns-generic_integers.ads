--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.Patterns.            Luebeck            --
--        Generic_Integers                         Winter, 2025       --
--  Interface                                                         --
--                                Last revision :  12:17 04 Jan 2026  --
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

with Strings_Edit;  use Strings_Edit;

with Generic_Unbounded_Array;
with Strings_Edit.Integer_Edit;

generic
   with package Edit is new Strings_Edit.Integer_Edit (<>);
package Parsers.Generic_Source.Patterns.Generic_Integers is
   subtype Number is Edit.Number;

   type Integer_Array is array (Positive range <>) of Number;
   package Integer_Arrays is
      new Generic_Unbounded_Array
          (  Index_Type        => Positive,
             Object_Type       => Number,
             Object_Array_Type => Integer_Array,
             Null_Element      => Number'First
          );
   use Integer_Arrays;
--
-- Integer_Pattern -- Variable that can be assigned upon patter matching
--
   function Integer_Pattern
            (  Values  : access Unbounded_Array;
               Pointer : access Positive;
               Base    : NumberBase := 10;
               First   : Number'Base := Number'First;
               Last    : Number'Base := Number'Last;
               ToFirst : Boolean := False;
               ToLast  : Boolean := False
            )  return Pattern_Type;

private
   type Integer_Pattern_Type
        (  Values  : access Unbounded_Array;
           Pointer : access Positive;
           Base    : NumberBase;
           First   : Number'Base;
           Last    : Number'Base;
           ToFirst : Boolean;
           ToLast  : Boolean
        )  is new Pattern_Object with null record;
   function Image (Pattern : Integer_Pattern_Type) return String;
   function Match
            (  Pattern : Integer_Pattern_Type;
               Source  : access Source_Subtype;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Integer_Pattern_Type;
               Recursive : Boolean
            )  return Boolean;

   type Integer_Hook_Handler
        (  Pointer : access Positive
        )  is new Hook_Handler with null record;
   function Image (Hook : Integer_Hook_Handler) return String;
   procedure On_Failure
             (  Hook   : in out Integer_Hook_Handler;
                Source : in out Source_Subtype
             );
   function On_Line_Change
            (  Hook   : access Integer_Hook_Handler;
               Source : access Source_Subtype;
               Where  : Location_Subtype
            )  return Result_Type;
   procedure On_Success
             (  Hook   : in out Integer_Hook_Handler;
                Source : in out Source_Subtype;
                From   : Integer;
                To     : Integer
             );

end Parsers.Generic_Source.Patterns.Generic_Integers;
