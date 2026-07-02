--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Get_Numeric_Literal                      Winter, 2004       --
--  Separate body implementation                                      --
--                                Last revision :  10:48 02 Jul 2026  --
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

with Strings_Edit.Unbounded_Integer_Edit;
with Strings_Edit.Unbounded_Rational_Edit;

separate (Parsers.Generic_Ada_Parser) 
   procedure Get_Numeric_Literal
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is
   function To_Digit (Symbol : Character) return Integer;
   pragma Inline (To_Digit);

   function To_Digit (Symbol : Character) return Integer is
   begin
      case Symbol is
         when '0'     => return 0;
         when '1'     => return 1;
         when '2'     => return 2;
         when '3'     => return 3;
         when '4'     => return 4;
         when '5'     => return 5;
         when '6'     => return 6;
         when '7'     => return 7;
         when '8'     => return 8;
         when '9'     => return 9;
         when 'a'|'A' => return 10;
         when 'b'|'B' => return 11;
         when 'c'|'C' => return 12;
         when 'd'|'D' => return 13;
         when 'e'|'E' => return 14;
         when 'f'|'F' => return 15;
         when others  => return 16;
      end case;
   end To_Digit;

   procedure Get_Mantissa
             (  Value   : in out String;
                Pointer : Integer;
                To      : Integer
             )  is
      Symbol : Character;
      Index  : Integer := Value'First;
      From   : Integer := Pointer;
   begin
      while From <= To loop
         Symbol := Line (From);
         case Symbol is
            when '.' | '_' =>
               null;
            when others =>
               Value (Index) := Symbol;
               Index := Index + 1;
         end case;
         From := From + 1;
      end loop;
   end Get_Mantissa;

   procedure Get_Numeral
             (  Line      : String;
                Pointer   : in out Integer;
                Base      : Integer;
                Length    : out Natural;
                Malformed : in out Boolean
             )  is
      Underline : Boolean := False;
      Symbol    : Character;
   begin
      Length := 0;
      while Pointer <= Line'Last loop
         Symbol := Line (Pointer);
         if To_Digit (Symbol) < Base then
            Length    := Length + 1;
            Underline := False;
         elsif '_' = Symbol then
            Malformed := Malformed or Underline;
            Underline := True;
         else
            exit;
         end if;
         Pointer := Pointer + 1;
      end loop;
      Malformed := Malformed or Underline;
   end Get_Numeral;

   function To_Unsigned (Line : String; Upper : Integer)
      return Integer is
      Result : Integer := 0;
      Symbol : Character;
   begin
      for Index in Line'Range loop
         Symbol := Line (Index);
         if '_' /= Symbol then
            Result := Result * 10 + To_Digit (Symbol);
            exit when Result > Upper;
         end if;
      end loop;
      return Result;
   end To_Unsigned;

   Max_Exponent : constant := Integer'Last / 10 - 10;

   type Arena_Ptr is access Numeric_Literal'Class;
   for Arena_Ptr'Storage_Pool use Context.Pool.all;

   Result    : Arena_Ptr;
   Base      : Integer := 10;
   Exponent  : Integer := 0;
   From      : Integer := Pointer; -- The first character of mantissa
   To        : Integer;            -- The last character of mantissa
   Fore      : Natural;
   Aft       : Natural := 0;
   Malformed : Boolean := False;
   Real      : Boolean := False;
   Based     : Boolean := False;
   Index     : Integer := Pointer;

begin
   Get_Numeral (Line, Index, 10, Fore, Malformed);
   if Index <= Line'Last and then '#' = Line (Index) then
      Base := To_Unsigned (Line (Pointer..Index - 1), 16);
      if Base < 2 or else Base > 16 then
         Set_Pointer (Code, Index);
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "The base of a numeric literal is not in 2..16 at " &
            Image (Link (Code))
         );
      end if;
      Based := True;
      Index := Index + 1;
      From  := Index;
      Get_Numeral (Line, Index, Base, Fore, Malformed);
   end if;
   if (  Index <= Line'Last
      and then
         '.' = Line (Index)
      and then
         (Index >= Line'Last or else '.' /= Line (Index + 1))
      )
   then
      Real  := True;
      Index := Index + 1;
      Get_Numeral (Line, Index, Base, Aft, Malformed);
      Malformed := Malformed or Aft = 0;
   end if;
   To := Index - 1;
   if Based then
      if Index > Line'Last or else '#' /= Line (Index) then
         Set_Pointer (Code, Index);
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            (  "Missing '#' in the numeric literal at "
            &  Image (Link (Code))
         )  );
      end if;
      Index := Index + 1;
   end if;
   if (  Index <= Line'Last
      and then
         (  'e' = Line (Index)
         or else
            'E' = Line (Index)
      )  )
   then
      Index := Index + 1;
      declare
         Pointer  : Integer := Index;
         Length   : Natural := 0;
         Negative : Boolean := False;
      begin
         if Index <= Line'Last then
            if '-' = Line (Index) then
               Pointer  := Pointer + 1;
               Index    := Index   + 1;
               Negative := True;
            elsif '+' = Line (Index) then
               Pointer  := Pointer + 1;
               Index    := Index   + 1;
            end if;
            Get_Numeral (Line, Index, 10, Length, Malformed);
            Exponent :=
               To_Unsigned
               (  Line (Pointer..Index - 1),
                  Max_Exponent
               );
            if Exponent > Max_Exponent then
               if Negative then
                  Exponent := Integer'First;
               else
                  Exponent := Integer'Last;
               end if;
            else
               if Negative then
                  Exponent := - Exponent - Aft;
               else
                  Exponent := Exponent - Aft;
               end if;
            end if;
         end if;
         if Length = 0 then
            Set_Pointer (Code, Index);
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "Missing exponent part in the numeric literal at "
               &  Image (Link (Code))
            )  );
         end if;
      end;
   else
      Exponent := -Aft;
   end if;
   Malformed :=
      (  Malformed
      or else
         (  Index <= Line'Last
         and then
            Is_Letter (Line (Index))
      )  );
   for I in From..To loop -- Trim leading zeros
      case Line (I) is
         when '_' =>
            null;
         when '0' =>
            Fore := Fore - 1;
         when others =>
            exit;
      end case;
      From := From + 1;
   end loop;
   --  if Real then
   --     for I in reverse From..To loop -- Trim trailing zeros
   --        case Line (I) is
   --           when '0' =>
   --              Aft      := Aft - 1;
   --              Exponent := Exponent + 1;
   --           when '_' =>
   --              null;
   --           when others =>
   --              exit;
   --        end case;
   --        To := To - 1;
   --     end loop;
   --  end if;
   if Fore + Aft = 0 then
      if Real then
         if not Malformed and then Context.Fold then
            Argument.Value :=
               new Universal_Real'
                   (  Negative    => False,
                      Numerator   => null,
                      Denominator => null,
                      Value       => new Unbounded_Rational'
                                         (  Unbounded_Rationals.Zero
                   )                     );
            Set_Pointer (Code, Index);
            Argument.Location := Link (Code);
            return;
         end if;
         Result := new Real_Literal (1);
      else
         Malformed := Malformed or Exponent < 0;
         if not Malformed and then Context.Fold then
            Argument.Value :=
               new Universal_Integer'
                   (  Negative => False,
                      Data     => null,
                      Value    => new Unbounded_Integer'
                                      (  Unbounded_Integers.Zero
                   )                  );
            Set_Pointer (Code, Index);
            Argument.Location := Link (Code);
            return;
         end if;
         Result := new Integer_Literal (1);
      end if;
      declare
         This : Numeric_Literal'Class renames Result.all;
      begin
         This.Malformed := Malformed;
         This.Base      := Base;
         This.Exponent  := 0;
         This.Value     := "0";
      end;
   else
      if Real then
         if not Malformed and then Context.Fold then
            declare
               function Get_Value return Unbounded_Rational is
                  use Strings_Edit.Unbounded_Rational_Edit;
                  Text : String (1..Fore + Aft);
               begin
                  Get_Mantissa (Text, From, To);
                  if Exponent >= 0 then
                     return Value (Text, Base)
                          * From_Half_Word (Half_Word (Base))
                         ** Bit_Count (Exponent);
                  else
                     return Value (Text, Base)
                          / From_Half_Word (Half_Word (Base))
                         ** Bit_Count (-Exponent);
                  end if;
               end Get_Value;
            begin
               Argument.Value :=
                  new Universal_Real'
                      (  Negative    => False,
                         Numerator   => null,
                         Denominator => null,
                         Value       => new Unbounded_Rational'
                                            (  Get_Value
                      )                     );
               Set_Pointer (Code, Index);
               Argument.Location := Link (Code);
               return;
            exception
               when others => -- Do not fold on errors
                  null;
            end;
         end if;
         Result := new Real_Literal (Fore + Aft);
      else
         Malformed := Malformed or Exponent < 0;
         if not Malformed and then Context.Fold then
            declare
               function Get_Value return Unbounded_Integer is
                  use Strings_Edit.Unbounded_Unsigned_Edit;
                  Text : String (1..Fore + Aft);
               begin
                  Get_Mantissa (Text, From, To);
                  return Compose
                         (  Value (Text, Base)
                          * From_Half_Word (Half_Word (Base))
                         ** Bit_Count (Exponent)
                         );
               end Get_Value;
            begin
               Argument.Value :=
                  new Universal_Integer'
                      (  Negative => False,
                         Data     => null,
                         Value    => new Unbounded_Integer'(Get_Value)
                      );
               Set_Pointer (Code, Index);
               Argument.Location := Link (Code);
               return;
            exception
               when others => -- Do not fold on errors
                  null;
            end;
         end if;
         Result := new Integer_Literal (Fore + Aft);
      end if;
      declare
         This : Numeric_Literal'Class renames Result.all;
      begin
         This.Malformed := Malformed;
         This.Base      := Base;
         This.Exponent  := Exponent;
         Get_Mantissa (This.Value, From, To);
      end;
   end if;
   Set_Pointer (Code, Index);
   Argument.Value    := Result.all'Unchecked_Access;
   Argument.Location := Link (Code);
end Get_Numeric_Literal;

