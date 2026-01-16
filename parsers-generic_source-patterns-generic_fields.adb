--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.Patterns.            Luebeck            --
--        Generic_Fields                           Winter, 2025       --
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

with Strings_Edit.Quoted;  use Strings_Edit.Quoted;

with Strings_Edit.UTF8.Categorization;
use  Strings_Edit.UTF8.Categorization;

package body Parsers.Generic_Source.Patterns.Generic_Fields is

   function Image (Pattern : Field_Pattern_Type) return String is
   begin
      return "Field (" & Quote (To_Sequence (Pattern.Set)) & ')';
   end Image;

   function Image (Hook : Field_Hook_Handler) return String is
   begin
      return "";
   end Image;

   function Match
            (  Pattern : Field_Pattern_Type;
               Source  : access Parsers.Generic_Source.Source_Type;
               State   : access Match_State
            )  return Result_Type is
      use Strings_Edit.UTF8;
      Line      : Line_Ptr_Type;
      Pointer   : aliased Integer;
      Last      : Integer;
      Current   : Integer;
      Start     : Integer;
      Next      : Integer;
      Character : UTF8_Code_Point;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      Start := Pointer;
      while Pointer <= Last loop
         Current := Pointer;
         Get (Line (Pointer..Last), Pointer, Character);
         if Is_In (Character, Pattern.Set) then
            Pointer := Current;
            exit;
         end if;
      end loop;
      if Pointer = Start then
         return Unmatched;
      else
         Next := Pointer;
         while Pointer >= Start loop
            Current := Start;
            Get (Line (Start..Last), Start, Character);
            if Character /= 16#9# and then not Is_Space (Character) then
               Start := Current;
               exit;
            end if;
         end loop;
         while Pointer >= Start loop
            Current := Pointer;
            Get_Backwards (Line (Start..Last), Pointer, Character);
            if Character /= 16#9# and then not Is_Space (Character) then
               Pointer := Current;
               exit;
            end if;
         end loop;
         if Pointer = Start then
            return Unmatched;
         end if;
         Set_Pointer (Source.all, Next, False);
         Put
         (  Pattern.Values.all,
            Pattern.Pointer.all,
            new String'(Line (Start..Pointer - 1))
         );
         Pattern.Pointer.all := Pattern.Pointer.all + 1;
         return Matched;
      end if;
   exception
      when Ada.Text_IO.Data_Error =>
         return
         (  Aborted,
            Encoding_Error'Length,
            Link (Source.all),
            Encoding_Error
         );
   end Match;

   procedure On_Failure
             (  Hook   : in out Field_Hook_Handler;
                Source : in out Source_Type
             )  is
   begin
      Hook.Pointer.all := Hook.Pointer.all - 1;
   end On_Failure;

   function On_Line_Change
            (  Hook   : access Field_Hook_Handler;
               Source : access Source_Type;
               Where  : Location_Type
            )  return Result_Type is
   begin
      return Matched;
   end On_Line_Change;

   procedure On_Success
             (  Hook   : in out Field_Hook_Handler;
                Source : in out Source_Type;
                From   : Integer;
                To     : Integer
             )  is
   begin
      null;
   end On_Success;

   function Field_Pattern
            (  Values  : access Unbounded_Ptr_Array;
               Pointer : access Positive;
               List    : Unicode_Set
            )  return Pattern_Type is
      use Hook_Handlers;
      Hook    : constant Hook_Handlers.Handle :=
                   Ref (  new Field_Hook_Handler
                              (  Pointer.all'Unchecked_Access
                       )      );
      Pattern : constant Pattern_Type :=
                   Ref (new Field_Pattern_Type
                            (  Values  => Values.all'Unchecked_Access,
                               Pointer => Pointer.all'Unchecked_Access
                       )   );
      This   : Field_Pattern_Type renames
                  Field_Pattern_Type (Ptr (Pattern).all);
      Result : Pattern_Ptr;
   begin
      This.Set := List;
      Result := new Hook_Pattern (Ptr (Hook));
      Hook_Pattern (Result.all).Handler := Hook;
      Hook_Pattern (Result.all).Pattern := Pattern;
      return Ref (Result);
   end Field_Pattern;

   function Field_Pattern
            (  Values  : access Unbounded_Ptr_Array;
               Pointer : access Positive;
               List    : String
            )  return Pattern_Type is
   begin
      return Field_Pattern (Values, Pointer, To_Set (List));
   end Field_Pattern;

   function Voidable
            (  Pattern   : Field_Pattern_Type;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

end Parsers.Generic_Source.Patterns.Generic_Fields;
