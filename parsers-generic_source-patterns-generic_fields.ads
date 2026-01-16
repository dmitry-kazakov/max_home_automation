--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.Patterns.            Luebeck            --
--        Generic_Fields                           Winter, 2025       --
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

with Generic_Unbounded_Ptr_Array;

generic
package Parsers.Generic_Source.Patterns.Generic_Fields is

   type String_Ptr is access String;
   type String_Ptr_Array is array (Positive range <>) of String_Ptr;
   package String_Arrays is
      new Generic_Unbounded_Ptr_Array
          (  Index_Type            => Positive,
             Object_Type           => String,
             Object_Ptr_Type       => String_Ptr,
             Object_Ptr_Array_Type => String_Ptr_Array
          );
   use String_Arrays;
--
-- Field_Pattern -- Variable that can be assigned upon patter matching
--
   function Field_Pattern
            (  Values  : access Unbounded_Ptr_Array;
               Pointer : access Positive;
               List    : String
            )  return Pattern_Type;
   function Field_Pattern
            (  Values  : access Unbounded_Ptr_Array;
               Pointer : access Positive;
               List    : Unicode_Set
            )  return Pattern_Type;

private
   type Field_Pattern_Type
        (  Values  : access Unbounded_Ptr_Array;
           Pointer : access Positive
        )  is new Pattern_Object with
   record
      Set : Unicode_Set;
   end record;
   function Image (Pattern : Field_Pattern_Type) return String;
   function Match
            (  Pattern : Field_Pattern_Type;
               Source  : access Source_Subtype;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Field_Pattern_Type;
               Recursive : Boolean
            )  return Boolean;

   type Field_Hook_Handler
        (  Pointer : access Positive
        )  is new Hook_Handler with null record;
   function Image (Hook : Field_Hook_Handler) return String;
   procedure On_Failure
             (  Hook   : in out Field_Hook_Handler;
                Source : in out Source_Subtype
             );
   function On_Line_Change
            (  Hook   : access Field_Hook_Handler;
               Source : access Source_Subtype;
               Where  : Location_Subtype
            )  return Result_Type;
   procedure On_Success
             (  Hook   : in out Field_Hook_Handler;
                Source : in out Source_Subtype;
                From   : Integer;
                To     : Integer
             );

end Parsers.Generic_Source.Patterns.Generic_Fields;
