--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.Patterns.            Luebeck            --
--        Generic_Parametrized_User_Pattern        Summer, 2025       --
--  Implementation                                                    --
--                                Last revision :  11:24 12 Jul 2025  --
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

with Ada.Exceptions;  use Ada.Exceptions;

with Ada.Unchecked_Deallocation;

package body Parsers.Generic_Source.Patterns.
             Generic_Parametrized_User_Pattern is

   procedure Free is
      new Ada.Unchecked_Deallocation (Parameter_Type, Parameter_Ptr);

   procedure Finalize (Pattern : in out User_Defined_Pattern) is
   begin
      Free (Pattern.Parameter);
      Finalize (Pattern_Object (Pattern));
   end Finalize;

   function Image (Pattern : User_Defined_Pattern) return String is
   begin
      return "User_defined";
   end Image;

   function Match
            (  Pattern : User_Defined_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      Line    : Line_Ptr_Type;
      Pointer : aliased Integer;
      Last    : Integer;
      Start   : Integer;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      Start := Pointer;
      if Match
         (  Line (Pointer..Last),
            Pointer'Access,
            Pattern.Parameter.all
         )  then
         if Start = Pointer and Not_Voidable then
            return Unmatched;
         end if;
         Set_Pointer (Source.all, Pointer, False);
         return Matched;
      else
         return Unmatched;
      end if;
   exception
      when Error : others =>
         declare
            Message : constant String := Exception_Message (Error);
         begin
            return
            (  Aborted,
               Message'Length,
               Direct_Link (Source.all, Start, Pointer - 1),
               Message
            );
         end;
   end Match;

   function Pattern (Parameter : Parameter_Type) return Pattern_Type is
      Result_Ptr : constant Pattern_Ptr := new User_Defined_Pattern;
      This       : User_Defined_Pattern renames
                      User_Defined_Pattern (Result_Ptr.all);
   begin
      This.Parameter := new Parameter_Type'(Parameter);
      return Ref (Result_Ptr);
   end Pattern;

   function Voidable
            (  Pattern   : User_Defined_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return not Not_Voidable;
   end Voidable;

end Parsers.Generic_Source.Patterns.Generic_Parametrized_User_Pattern;
