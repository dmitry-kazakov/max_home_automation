--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.Patterns.            Luebeck            --
--        Generic_Integers                         Winter, 2025       --
--  Implementation                                                    --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body Parsers.Generic_Source.Patterns.Generic_Integers is

   function Image (Pattern : Integer_Pattern_Type) return String is
   begin
      return "Integer (Base " & Image (Integer (Pattern.Base)) & ')';
   end Image;

   function Image (Hook : Integer_Hook_Handler) return String is
   begin
      return "";
   end Image;

   function Match
            (  Pattern : Integer_Pattern_Type;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      Line    : Line_Ptr_Type;
      Pointer : aliased Integer;
      Last    : Integer;
      Start   : Integer;
      Value   : Number;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      Start := Pointer;
      begin
         Edit.Get
         (  Source  => Line (Pointer..Last),
            Pointer => Pointer,
            Value   => Value,
            Base    => Pattern.Base,
            First   => Pattern.First,
            Last    => Pattern.Last,
            ToFirst => Pattern.ToFirst,
            ToLast  => Pattern.ToLast
         );
      exception
         when others =>
            return Unmatched;
      end;
      Set_Pointer (Source.all, Pointer, False);
      Put (Pattern.Values.all, Pattern.Pointer.all, Value);
      Pattern.Pointer.all := Pattern.Pointer.all + 1;
      return Matched;
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

   procedure On_Failure
             (  Hook   : in out Integer_Hook_Handler;
                Source : in out Source_Type
             )  is
   begin
      Hook.Pointer.all := Hook.Pointer.all - 1;
   end On_Failure;

   function On_Line_Change
            (  Hook   : access Integer_Hook_Handler;
               Source : access Source_Type;
               Where  : Location_Type
            )  return Result_Type is
   begin
      return Matched;
   end On_Line_Change;

   procedure On_Success
             (  Hook   : in out Integer_Hook_Handler;
                Source : in out Source_Type;
                From   : Integer;
                To     : Integer
             )  is
   begin
      null;
   end On_Success;

   function Integer_Pattern
            (  Values  : access Unbounded_Array;
               Pointer : access Positive;
               Base    : NumberBase := 10;
               First   : Number'Base := Number'First;
               Last    : Number'Base := Number'Last;
               ToFirst : Boolean := False;
               ToLast  : Boolean := False
            )  return Pattern_Type is
      use Hook_Handlers;
      Hook    : constant Hook_Handlers.Handle :=
                   Ref (  new Integer_Hook_Handler
                              (  Pointer.all'Unchecked_Access
                       )      );
      Pattern : constant Pattern_Type :=
                   Ref (new Integer_Pattern_Type
                            (  Values  => Values.all'Unchecked_Access,
                               Pointer => Pointer.all'Unchecked_Access,
                               Base    => Base,
                               First   => First,
                               Last    => Last,
                               ToFirst => ToFirst,
                               ToLast  => ToLast
                       )   );
      Result : Pattern_Ptr;
   begin
      Result := new Hook_Pattern (Ptr (Hook));
      Hook_Pattern (Result.all).Handler := Hook;
      Hook_Pattern (Result.all).Pattern := Pattern;
      return Ref (Result);
   end Integer_Pattern;

   function Voidable
            (  Pattern   : Integer_Pattern_Type;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

end Parsers.Generic_Source.Patterns.Generic_Integers;
