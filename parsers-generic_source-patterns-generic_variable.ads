--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.Patterns.            Luebeck            --
--        Generic_Variable                         Winter, 2025       --
--  Interface                                                                  --
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

with Ada.Finalization;
with Ada.Unchecked_Conversion;

generic
   with procedure Add
                  (  Value  : String;
                     Where  : Location_Type;
                     Append : Boolean
                  )  is <>;
   with procedure Delete (Append : Boolean) is <>;
   with function On_Line_Change
                 (  Where : Location_Type
                 )  return Result_Type is <>;
package Parsers.Generic_Source.Patterns.Generic_Variable is
--
-- Append/Set -- A pattern that calls Add upon pattern matching
--
   function Append (Pattern : Pattern_Type) return Pattern_Type;
   function Set    (Pattern : Pattern_Type) return Pattern_Type;

private
--
-- We have to convert local access types to the global ones. The  object
-- Finalization_Guard  zeroes  all  pointers  upon  finalization so that
-- surviving hook patterns would not crash the program.
--
   type Finalization_Guard is new Ada.Finalization.Limited_Controlled
      with null record;
   procedure Finalize (Object : in out Finalization_Guard);
   procedure On_Failure
             (  Hook   : in out Variable_Assignment_Handler;
                Source : in out Source_Type
             );
   type On_Failure_Ref is access procedure
        (  Hook   : in out Variable_Assignment_Handler;
           Source : in out Source_Type
        );
   function "+" is
      new Ada.Unchecked_Conversion (On_Failure_Ref, On_Failure_Ptr);
   On_Failure_Body : On_Failure_Ptr := +On_Failure'Access;

   function On_Line_Change
            (  Hook   : access Variable_Assignment_Handler;
               Source : access Source_Type;
               Where  : Location_Type
            )  return Result_Type;
   type On_Line_Change_Ref is access function
        (  Hook   : access Variable_Assignment_Handler;
           Source : access Source_Type;
           Where  : Location_Type
        )  return Result_Type;
   function "+" is
      new Ada.Unchecked_Conversion
          (  On_Line_Change_Ref,
             On_Line_Change_Ptr
          );
   On_Line_Change_Body : On_Line_Change_Ptr := +On_Line_Change'Access;

   procedure On_Success
             (  Hook   : in out Variable_Assignment_Handler;
                Source : in out Source_Type;
                From   : Integer;
                To     : Integer
             );
   type On_Success_Ref is access procedure
        (  Hook   : in out Variable_Assignment_Handler;
           Source : in out Source_Type;
           From   : Integer;
           To     : Integer
        );
   function "+" is
      new Ada.Unchecked_Conversion (On_Success_Ref, On_Success_Ptr);
   On_Success_Body : On_Success_Ptr := +On_Success'Access;

end Parsers.Generic_Source.Patterns.Generic_Variable;
