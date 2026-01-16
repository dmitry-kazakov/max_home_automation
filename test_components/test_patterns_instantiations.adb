--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Patterns_Instantiations                Luebeck            --
--  Implementation                                 Summer, 2025       --
--                                                                    --
--                                Last revision :  21:31 04 Jan 2026  --
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

with Strings_Edit.Integers;
with Strings_Edit.UTF8.Categorization;
with Strings_Edit.UTF8.Normalization;

package body Test_Patterns_Instantiations is

   procedure Add
             (  Variable : in out Matrix_Variable;
                Source   : in out Parsers.Multiline_Patterns.
                                  Source_Subtype;
                Value    : String;
                Where    : Parsers.Multiline_Patterns.Location_Subtype;
                Append   : Boolean
             )  is
   begin
      if not Append then
         Variable.Row    := 1;
         Variable.Column := 1;
      end if;
      Variable.Value (Variable.Row, Variable.Column) :=
         Strings_Edit.Integers.Value (Value);
      if Variable.Column = Variable.Columns then
         Variable.Row    := Variable.Row + 1;
         Variable.Column := 1;
      else
         Variable.Column := Variable.Column + 1;
      end if;
   end Add;

   procedure Delete
             (  Variable : in out Matrix_Variable;
                Source   : in out Parsers.Multiline_Patterns.
                                  Source_Subtype;
                Append   : Boolean
             )  is
   begin
      if Variable.Column = 1 then
         Variable.Row    := Variable.Row - 1;
         Variable.Column := Variable.Columns;
      else
         Variable.Column := Variable.Column - 1;
      end if;
   end Delete;

   function Match_Identifier
            (  Line    : String;
               Pointer : access Integer
            )  return Boolean is
      use Strings_Edit.UTF8;
      use Strings_Edit.UTF8.Categorization;
      Index      : Integer := Pointer.all;
      Last       : Integer;
      Point      : UTF8_Code_Point;
      Underscore : Boolean := False;
   begin
      if Index > Line'Last then
         return False;
      end if;
      Get (Line, Index, Point);
      if not Is_Letter (Point) then
         return False;
      end if;
      Last := Index;
      while Index <= Line'Last loop
         Get (Line, Index, Point);
         if Point = Character'Pos ('_') then
            Pointer.all := Last;
            if Underscore then
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "Doubled underscore"
               );
            end if;
            Underscore := True;
         elsif Is_Alphanumeric (Point) then
            Underscore := False;
         elsif Underscore then
            exit;
         else
            Pointer.all := Last;
            return True;
         end if;
         Last := Index;
      end loop;
      Pointer.all := Last;
      if Underscore then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Underscore at end"
         );
      end if;
      return True;
   end Match_Identifier;

   function Match_Normalized
            (  Line    : String;
               Pointer : access Integer;
               Text    : String
            )  return Boolean is
      use Strings_Edit.UTF8.Normalization;
   begin
      return Is_Normalized_Prefix
             (  Prefix     => Text,
                Source     => Line,
                Pointer    => Pointer,
                Form       => NFC,
                Normalized => First
             );
   end Match_Normalized;

   function Match_Number
            (  Line    : String;
               Pointer : access Integer;
               Base    : Number_Base
            )  return Boolean is
      Digit : Integer;
      Index : Integer := Pointer.all;
   begin
      while Index <= Line'Last loop
         case Line (Index) is
            when '0'..'9' =>
               Digit := Character'Pos (Line (Index))
                      - Character'Pos ('0');
            when 'A'..'F' =>
               Digit := Character'Pos (Line (Index))
                      - Character'Pos ('A')
                      + 10;
            when 'a'..'f' =>
               Digit := Character'Pos (Line (Index))
                      - Character'Pos ('a')
                      + 10;
            when others =>
               exit;
         end case;
         exit when Digit not in 0..Integer (Base) - 1;
         Index := Index + 1;
      end loop;
      if Pointer.all = Index then
         return False;
      else
         Pointer.all := Index;
         return True;
      end if;
   end Match_Number;

   function On_Line_Change
            (  Variable   : access Matrix_Variable;
               Source     : access Parsers.Multiline_Patterns.
                                   Source_Subtype;
               Where      : Parsers.Multiline_Patterns.Location_Subtype
            )  return Parsers.Multiline_Patterns.Result_Type is
   begin
      return Parsers.Multiline_Patterns.Matched;
   end On_Line_Change;

end Test_Patterns_Instantiations;
