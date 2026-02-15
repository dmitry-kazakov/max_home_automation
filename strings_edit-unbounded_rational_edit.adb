--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.                               Luebeck            --
--        Unbounded_Rational_Edit                  Spring, 2025       --
--  Interface                                                         --
--                                Last revision :  21:44 03 Feb 2026  --
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

with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Strings_Edit.Fields;    use Strings_Edit.Fields;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Unbounded_Integers;     use Unbounded_Integers;
with Unbounded_Unsigneds;    use Unbounded_Unsigneds;

with Strings_Edit.Unbounded_Unsigned_Edit;
use  Strings_Edit.Unbounded_Unsigned_Edit;

package body Strings_Edit.Unbounded_Rational_Edit is

   procedure Get_Exponent
             (  Source   : in String;
                Pointer  : in out Integer;
                Value    : out Integer
             )  is
      Index : Integer := Pointer;
   begin
      Get (Source, Index, SpaceAndTab);
      if (  Index > Source'Last
         or else
            (  Source (Index) /= 'E'
            and
               Source (Index) /= 'e'
         )  )
      then
         Value := 0;
      else
         Index := Index + 1;
         Get (Source, Index, SpaceAndTab);
         Get (Source, Index, Value);
         Pointer := Index;
      end if;
   exception
      when End_Error | Data_Error | Layout_Error =>
         Value := 0;
   end Get_Exponent;

   procedure Get_Internal
             (  Source        : String;
                Pointer       : in out Integer;
                Value         : out Unbounded_Rational;
                Base          : NumberBase;
                Have_Exponent : Boolean
             )  is
      Radix        : constant Unbounded_Unsigned :=
                              From_Half_Word (Half_Word (Base));
      After_Length : Integer := 0;
      Before       : Unbounded_Unsigned;
      After        : Unbounded_Unsigned;
      Exponent     : Integer;
      Result       : Unbounded_Rational;
      Sign         : Integer := 0;
      Index        : Integer := Pointer;
   begin
      if (  Pointer < Source'First
         or else
            (  Pointer > Source'Last
            and then
               Pointer - 1 > Source'Last
         )  )
      then
         raise Layout_Error;
      end if;
      if Pointer <= Source'Last then
         case Source (Index) is
            when '-' =>
               Index := Index + 1;
               Sign := -1;
               Get (Source, Index, SpaceAndTab);
            when '+' =>
               Index := Index + 1;
               Sign := 1;
               Get (Source, Index, SpaceAndTab);
            when others =>
               null;
         end case;
      end if;
      begin
         Get (Source, Index, Before, Base);
      exception
         when End_Error =>
            null;
      end;
      if Index + 1 <= Source'Last and then Source (Index) = '.' then
         begin
            Index := Index + 1;
            After_Length := Index;
            Get (Source, Index, After, Base);
         exception
            when End_Error =>
               if Index = Pointer + 1 then
                  raise Data_Error; -- Only decimal point is present
               end if;
         end;
      end if;
      After_Length := Index - After_Length;
      if Have_Exponent then
         Get_Exponent (Source, Index, Exponent);
      else
         Exponent := 0;
      end if;
      if 0 = Sign then
         if Pointer = Index then
            raise End_Error; -- No fraction part - no number
         end if;
         Sign := 1;
      else
         if Pointer = Index - 1 then
            raise Data_Error; -- Only a sign
         end if;
      end if;
      Result := Compose
                (  Numerator   => After,
                   Denominator => Radix ** Bit_Count (After_Length)
                )
              + Before;
      if Exponent /= 0 then
         if Exponent < 0 then
            Result := Result / Radix ** Bit_Count (-Exponent);
         else
            Result := Result * Radix ** Bit_Count (Exponent);
         end if;
      end if;
      if Sign = -1 then
         Value := -Result;
      else
         Value := Result;
      end if;
      Pointer := Index;
   end Get_Internal;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Unbounded_Rational;
                Base    : NumberBase := 10
             )  is
   begin
      Get_Internal (Source, Pointer, Value, Base, True);
   end Get;

   procedure Get_Recurring
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Unbounded_Rational;
                Base    : NumberBase := 10
             )  is
      Negative  : Boolean := False;
      Have_Sign : Boolean := False;
      Index     : Integer := Pointer;
      Fraction  : Integer;
      Length    : Integer;
      Result    : Unbounded_Rational;
      Factor    : Unbounded_Rational;
      Period    : Unbounded_Unsigned;
      Radix     : constant Unbounded_Unsigned :=
                           From_Half_Word (Half_Word (Base));
   begin
      if Index <= Source'Last then
         case Source (Index) is
            when '+' =>
               Index := Index + 1;
               Have_Sign := True;
            when '-' =>
               Negative := True;
               Index    := Index + 1;
               Have_Sign := True;
            when others =>
               null;
         end case;
      end if;
      begin
         Get_Internal (Source, Index, Result, Base, False);
      exception
          when End_Error =>
            if Have_Sign then
               raise Data_Error;
            else
               raise;
            end if;
      end;
      if Index > Source'Last or else Source (Index) /= '(' then
         Value   := Result;
         Pointer := Index;
         return;
      end if;
      Fraction := Pointer;
      while Fraction < Index loop
         if Source (Fraction) = '.' then
            Fraction := Fraction + 1;
            exit;
         end if;
         Fraction := Fraction + 1;
      end loop;
      if Source (Fraction - 1) /= '.' then
         raise Data_Error;
      end if;
      Fraction := Index - Fraction;
      Index    := Index + 1;
      Length   := Index;
      Get (Source, Index, Period, Base);
      if Index > Source'Last or else Source (Index) /= ')' then
         raise Data_Error;
      end if;
      for I in Length..Index - 1 loop
         exit when Source (I) /= '0';
         Mul (Period, Radix);
         Fraction := Fraction + 1;
      end loop;
      Length := Index - Length;
      Index  := Index + 1;
      Factor := To_Unbounded_Rational (Radix ** Bit_Count (Length) - 1);
      Invert (Factor);
      Factor := Factor * Period;
      Factor := Factor / Radix ** Bit_Count (Fraction);
      Value  := Result + Factor;
      if Negative then
         Value := -Value;
      end if;
      Pointer := Index;
   end Get_Recurring;

   function Value
            (  Source : String;
               Base   : NumberBase := 10
            )  return Unbounded_Rational is
      Result  : Unbounded_Rational;
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, SpaceAndTab);
      Get (Source, Pointer, Result, Base);
      Get (Source, Pointer, SpaceAndTab);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

   function Value_Recurring
            (  Source : String;
               Base   : NumberBase := 10
            )  return Unbounded_Rational is
      Result  : Unbounded_Rational;
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, SpaceAndTab);
      Get_Recurring (Source, Pointer, Result, Base);
      Get (Source, Pointer, SpaceAndTab);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value_Recurring;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Unbounded_Rational;
                Base        : NumberBase := 10;
                PutPlus     : Boolean    := False;
                Fraction    : Natural    := 6;
                Field       : Natural    := 0;
                Justify     : Alignment  := Left;
                Fill        : Character  := ' '
             )  is
     Out_Field : constant Natural :=
                    Get_Output_Field (Destination, Pointer, Field);
     subtype Output is String (Pointer..Pointer + Out_Field - 1);
     Text  : Output renames
                Destination (Pointer..Pointer + Out_Field - 1);
     Index : Integer := Pointer;
  begin
     if Is_Zero (Value) then
        Put (Text, Index, "0");
     else
        if Is_Negative (Value) then
           Put (Text, Index, "-");
        elsif PutPlus then
           Put (Text, Index, "+");
        end if;
        if Fraction = 0 then
           Put
           (  Text,
              Index,
              Get_Mantissa (Round (abs Value)),
              Base
           );
        else
           declare
              Radix   : constant Unbounded_Unsigned :=
                           From_Half_Word (Half_Word (Base));
              Shifted : constant Unbounded_Rational :=
                           abs Value * Radix ** Bit_Count (Fraction);
              Start   : constant Integer := Index;
           begin
              Put
              (  Text,
                 Index,
                 Get_Mantissa (Round (Shifted)),
                 Base
              );
              declare
                 Length : constant Integer := Index - Start;
              begin
                 if Length > Fraction then
                    declare
                       Point_At : constant Integer := Index - Fraction;
                    begin
                       Put (Text, Index, " ");
                       Text (Point_At + 1..Index - 1) :=
                          Text (Point_At..Index - 2);
                       Text (Point_At) := '.';
                    end;
                 else
                    Index := Start + Fraction + 2;
                    if Index - 1 > Text'Last then
                       raise Layout_Error;
                    end if;
                    Text (Index - Length..Index - 1) :=
                       Text (Start..Start + Length - 1);
                    Text (Start..Start + 1) := "0.";
                    Text (Start + 2..Index - Length - 1) :=
                       (others => '0');
                 end if;
              end;
           end;
        end if;
     end if;
      -- The output is done in Text using Index as the pointer
     Adjust_Output_Field
     (  Destination,
        Pointer,
        Index,
        Out_Field,
        Field,
        Justify,
        Fill
     );
   end Put;

   procedure Put_Recurring
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Unbounded_Rational;
                Base        : NumberBase := 10;
                PutPlus     : Boolean    := False;
                Field       : Natural    := 0;
                Justify     : Alignment  := Left;
                Fill        : Character  := ' '
             )  is
     Out_Field : constant Natural :=
                    Get_Output_Field (Destination, Pointer, Field);
     subtype Output is String (Pointer..Pointer + Out_Field - 1);
     Text   : Output renames
                 Destination (Pointer..Pointer + Out_Field - 1);
     Index  : Integer  := Pointer;
  begin
     if Is_Zero (Value) then
        Put (Text, Index, "0");
     else
        if Is_Negative (Value) then
           Put (Text, Index, "-");
        elsif PutPlus then
           Put (Text, Index, "+");
        end if;
        declare
           Length : Positive := 1;
           Shift  : Natural  := 0;
           D : Unbounded_Unsigned := Get_Denominator (Value);
           N : Unbounded_Unsigned := Get_Numerator   (Value);
           R : Unbounded_Unsigned;
        begin
           Div (N, D, R);
           Put (Text, Index, N, Base => Base);
           if not Is_Zero (R) then
              Put (Text, Index, ".");
              while D mod Half_Word (Base) = 0 loop
                 Div (D, Half_Word (Base));
                 Shift := Shift + 1;
              end loop;
              case Base is
                 when 2 | 3 | 5 | 7 | 11 | 13 =>
                    null;
                 when 4 | 8 | 16 =>
                    while D mod 2 = 0 loop
                       Div (D, 2);
                       Mul (R, Half_Word (Base) / 2);
                       Shift := Shift + 1;
                    end loop;
                 when 6 | 12 =>
                    while D mod 3 = 0 loop
                       Div (D, 3);
                       Mul (R, Half_Word (Base) / 3);
                       Shift := Shift + 1;
                    end loop;
                    while D mod 2 = 0 loop
                       Div (D, 2);
                       Mul (R, Half_Word (Base) / 2);
                       Shift := Shift + 1;
                    end loop;
                 when 9 =>
                    while D mod 3 = 0 loop
                       Div (D, 3);
                       Mul (R, 3);
                       Shift := Shift + 1;
                    end loop;
                 when 10 =>
                    while D mod 5 = 0 loop
                       Div (D, 5);
                       Mul (R, 2);
                       Shift := Shift + 1;
                    end loop;
                    while D mod 2 = 0 loop
                       Div (D, 2);
                       Mul (R, 5);
                       Shift := Shift + 1;
                    end loop;
                 when 14 =>
                    while D mod 7 = 0 loop
                       Div (D, 7);
                       Mul (R, 2);
                       Shift := Shift + 1;
                    end loop;
                    while D mod 2 = 0 loop
                       Div (D, 2);
                       Mul (R, 7);
                       Shift := Shift + 1;
                    end loop;
                 when 15 =>
                    while D mod 5 = 0 loop
                       Div (D, 5);
                       Mul (R, 2);
                       Shift := Shift + 1;
                    end loop;
                    while D mod 3 = 0 loop
                       Div (D, 3);
                       Mul (R, 5);
                       Shift := Shift + 1;
                    end loop;
              end case;
              while Shift > 0 and then R mod Half_Word (Base) = 0 loop
                 Div (R, Half_Word (Base));
                 Shift := Shift - 1;
              end loop;
              Swap (N, R);
              Div (N, D, R);
              if Is_Zero (N) then
                 for I in 1..Shift loop
                    Put (Text, Index, '0');
                 end loop;
              else
                 Put
                 (  Destination => Text,
                    Pointer     => Index,
                    Value       => N,
                    Base        => Base,
                    Field       => Shift,
                    Justify     => Right,
                    Fill        => '0'
                 );
              end if;
              Mul (R, Half_Word (Base) - 1);
              Set (N, R);
              while not Is_Zero (R mod D) loop
                 Mul (R, Half_Word (Base));
                 Add (R, N);
                 Length := Length + 1;
              end loop;
              Div (R, D);
              if not Is_Zero (R) then
                 Put (Text, Index, "(");
                 Put
                 (  Destination => Text,
                    Pointer     => Index,
                    Value       => R,
                    Base        => Base,
                    Field       => Length,
                    Justify     => Right,
                    Fill        => '0'
                 );
                 Put (Text, Index, ")");
              end if;
           end if;
        end;
     end if;
     -- The output is done in Text using Index as the pointer
     Adjust_Output_Field
     (  Destination,
        Pointer,
        Index,
        Out_Field,
        Field,
        Justify,
        Fill
     );
   end Put_Recurring;

   function Image
            (  Value    : Unbounded_Rational;
               Base     : NumberBase := 10;
               PutPlus  : Boolean    := False;
               Fraction : Natural    := 6
            )  return String is
      Length : Positive := 120;
   begin
      loop
         declare
            Text    : String (1..Length);
            Pointer : Integer := Text'First;
         begin
            Put (Text, Pointer, Value, Base, PutPlus, Fraction);
            return Text (Text'First..Pointer - 1);
         exception
            when Layout_Error =>
               Length := Length * 2;
         end;
      end loop;
   end Image;

   function Image_Recurring
            (  Value   : Unbounded_Rational;
               Base    : NumberBase := 10;
               PutPlus : Boolean    := False
            )  return String is
      Length : Positive := 120;
   begin
      loop
         declare
            Text    : String (1..Length);
            Pointer : Integer := Text'First;
         begin
            Put_Recurring (Text, Pointer, Value, Base, PutPlus);
            return Text (Text'First..Pointer - 1);
         exception
            when Layout_Error =>
               Length := Length * 2;
         end;
      end loop;
   end Image_Recurring;

end Strings_Edit.Unbounded_Rational_Edit;
