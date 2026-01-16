--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Unbounded_Rationals                    Luebeck            --
--  Test                                           Spring, 2025       --
--                                                                    --
--                                Last revision :  08:56 30 Jun 2025  --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Text_IO;          use Ada.Text_IO;
with Strings_Edit.Quoted;  use Strings_Edit.Quoted;
with Unbounded_Rationals;  use Unbounded_Rationals;
with Unbounded_Integers;   use Unbounded_Integers;
with Unbounded_Unsigneds;  use Unbounded_Unsigneds;

with Unbounded_Rationals.Continued_Fractions;
use  Unbounded_Rationals.Continued_Fractions;

with Strings_Edit.Continued_Fraction_Edit;
use  Strings_Edit.Continued_Fraction_Edit;

with Strings_Edit.Unbounded_Rational_Edit;
use  Strings_Edit.Unbounded_Rational_Edit;

with Strings_Edit.Unbounded_Integer_Edit;
with Strings_Edit.Unbounded_Unsigned_Edit;

with Unbounded_Rationals.Elementary_Functions;
use  Unbounded_Rationals.Elementary_Functions;

procedure Test_Unbounded_Rationals is
   use Strings_Edit;

   function Equal (Left : Word; Right : Integer) return Boolean is
   begin
      if Right < 0 then
         return Left = Word (-1 - Right) + 1;
      else
         return Left = Word (Right);
      end if;
   end Equal;

   function Img (Left : Unbounded_Rational) return String is
      use Strings_Edit.Unbounded_Unsigned_Edit;
   begin
      if Is_Negative (Left) then
         return '-'                          &
                Image (Get_Numerator (Left)) &
                '/'                          &
                Image (Get_Denominator (Left));
      else
         return Image (Get_Numerator (Left)) &
                '/'                          &
                Image (Get_Denominator (Left));
      end if;
   end Img;

   function "/" (Left, Right : Integer) return Unbounded_Rational is
   begin
      if Left < 0 xor Right < 0 then
         return Compose
                (  From_Half_Word (Half_Word (abs Left)),
                   From_Half_Word (Half_Word (abs Right)),
                   True
                );
      else
         return Compose
                (  From_Half_Word (Half_Word (abs Left)),
                   From_Half_Word (Half_Word (abs Right)),
                   False
                );
      end if;
   end "/";
begin
   Put_Line ("Is_Zero test");
   declare
      X : Unbounded_Rational;
   begin
      X := To_Unbounded_Rational (0);
      if not Is_Zero (X) then
         Raise_Exception
         (  Data_Error'Identity,
            "Is_Zero is False when True expected"
         );
      end if;
   end;
   Put_Line ("To_Integer test");
   declare
      procedure Check (Value : Integer) is
         X : Unbounded_Rational;
      begin
         X := To_Unbounded_Rational (Value);
         if To_Integer (X) /= Value then
            Raise_Exception
            (  Data_Error'Identity,
               (  "To_Integer "
               &  Integer'Image (To_Integer (X))
               &  " /= "
               &  Integer'Image (Value)
               &  " (expected)"
            )  );
         elsif not Equal (To_Word (Get_Numerator (X)), Value) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "To_Integer (numerator)"
               &  Word'Image (To_Word (Get_Numerator (X)))
               &  " /= |"
               &  Integer'Image (Value)
               &  "| (expected)"
            )  );
         elsif Get_Denominator (X) /= Unbounded_Unsigneds.One then
            Raise_Exception
            (  Data_Error'Identity,
               (  "To_Integer (denominator) "
               &  Integer'Image
                  (  Integer (To_Half_Word (Get_Numerator (X)))
                  )
               &  " /= 1 (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (0);
      Check (1);
      Check (-1);
      Check (12345);
      Check (Integer'First);
      Check (Integer'Last);
   end;
   Put_Line ("Value test");
   declare
      procedure Check
                (  S           : String;
                   Numerator   : Half_Word;
                   Denominator : Half_Word
                )  is
         use Strings_Edit.Unbounded_Unsigned_Edit;
         X : Unbounded_Rational;
      begin
         X := Value (S);
         if Get_Numerator (X) /= From_Half_Word (Numerator) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Value ("
               &  S
               &  ") numerator "
               &  Image (Get_Numerator (X))
               &  " /= "
               &  Image (From_Half_Word (Numerator))
               &  " (expected)"
            )  );
         elsif Get_Denominator (X) /= From_Half_Word (Denominator) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Value ("
               &  S
               &  ") denominator "
               &  Image (Get_Denominator (X))
               &  " /= "
               &  Image (From_Half_Word (Denominator))
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ("0",        0, 1);
      Check ("0.5",      1, 2);
      Check ("1.5",      3, 2);
      Check ("1.5 E2", 150, 1);
   end;
   Put_Line ("rational + rational test");
   declare
      procedure Check
                (  L, R, Result : Unbounded_Rational
                )  is
         X : constant Unbounded_Rational := L + R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &  " + "
               &  Img (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3,  1/3,  2/3);
      Check (-1/3,  1/3,  0/1);
      Check ( 1/3, -1/3,  0/1);
      Check (-1/3, -1/3, -2/3);
   end;
   Put_Line ("rational + integer test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   R      : Unbounded_Integer;
                   Result : Unbounded_Rational
                )  is
         use Strings_Edit.Unbounded_Integer_Edit;
         X : constant Unbounded_Rational := L + R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &   "+ "
               &  Image (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3,  Unbounded_Integers.One,  4/3);
      Check (-1/3,  Unbounded_Integers.One,  2/3);
      Check ( 1/3, -Unbounded_Integers.One, -2/3);
      Check (-1/3, -Unbounded_Integers.One, -4/3);
      Check (-4/3,  Unbounded_Integers.One, -1/3);
   end;
   Put_Line ("rational + unsigned test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   R      : Unbounded_Unsigned;
                   Result : Unbounded_Rational
                )  is
         use Strings_Edit.Unbounded_Unsigned_Edit;
         X : constant Unbounded_Rational := L + R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &   "+ "
               &  Image (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3, Unbounded_Unsigneds.One,  4/3);
      Check (-1/3, Unbounded_Unsigneds.One,  2/3);
      Check (-4/3, Unbounded_Unsigneds.One, -1/3);
   end;
   Put_Line ("rational + half_word test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   R      : Half_Word;
                   Result : Unbounded_Rational
                )  is
         X : constant Unbounded_Rational := L + R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &   "+"
               &  Half_Word'Image (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3, 1,  4/3);
      Check (-1/3, 1,  2/3);
      Check (-4/3, 1, -1/3);
   end;
   Put_Line ("rational - rational test");
   declare
      procedure Check
                (  L, R, Result : Unbounded_Rational
                )  is
         X : constant Unbounded_Rational := L - R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &  " - "
               &  Img (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3,  1/3,  0/1);
      Check (-1/3,  1/3, -2/3);
      Check ( 1/3, -1/3,  2/3);
      Check (-1/3, -1/3,  0/1);
   end;
   Put_Line ("rational - integer test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   R      : Unbounded_Integer;
                   Result : Unbounded_Rational
                )  is
         use Strings_Edit.Unbounded_Integer_Edit;
         X : constant Unbounded_Rational := L - R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &   " - "
               &  Image (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3,  Unbounded_Integers.One, -2/3);
      Check (-1/3,  Unbounded_Integers.One, -4/3);
      Check ( 1/3, -Unbounded_Integers.One,  4/3);
      Check (-1/3, -Unbounded_Integers.One,  2/3);
      Check (-4/3,  Unbounded_Integers.One, -7/3);
      Check ( 4/3,  Unbounded_Integers.One,  1/3);
   end;
   Put_Line ("rational - unsigned test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   R      : Unbounded_Unsigned;
                   Result : Unbounded_Rational
                )  is
         use Strings_Edit.Unbounded_Unsigned_Edit;
         X : constant Unbounded_Rational := L - R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &   " - "
               &  Image (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3, Unbounded_Unsigneds.One, -2/3);
      Check ( 4/3, Unbounded_Unsigneds.One,  1/3);
      Check (-1/3, Unbounded_Unsigneds.One, -4/3);
      Check (-4/3, Unbounded_Unsigneds.One, -7/3);
   end;
   Put_Line ("rational - half_word test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   R      : Half_Word;
                   Result : Unbounded_Rational
                )  is
         X : constant Unbounded_Rational := L - R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &   " - "
               &  Half_Word'Image (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (    1/3, 1,    -2/3);
      Check (    4/3, 1,     1/3);
      Check (   -1/3, 1,    -4/3);
      Check (   -4/3, 1,    -7/3);
      Check (-5662/1, 1, -5663/1);
   end;
   Put_Line ("rational * rational test");
   declare
      procedure Check
                (  L, R, Result : Unbounded_Rational
                )  is
         X : constant Unbounded_Rational := L * R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &  " * "
               &  Img (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3,  1/3,  1/9);
      Check (-1/3,  1/3, -1/9);
      Check ( 1/3, -1/3, -1/9);
      Check (-1/3, -1/3,  1/9);
   end;
   Put_Line ("rational * integer test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   R      : Unbounded_Integer;
                   Result : Unbounded_Rational
                )  is
         use Strings_Edit.Unbounded_Integer_Edit;
         X : constant Unbounded_Rational := L * R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &  " * "
               &  Image (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3,  To_Unbounded_Integer (15),  5/1);
      Check (-1/3,  To_Unbounded_Integer (15), -5/1);
      Check ( 1/3, -To_Unbounded_Integer (15), -5/1);
      Check (-1/3, -To_Unbounded_Integer (15),  5/1);
   end;
   Put_Line ("rational * unsigned test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   R      : Unbounded_Unsigned;
                   Result : Unbounded_Rational
                )  is
         use Strings_Edit.Unbounded_Unsigned_Edit;
         X : constant Unbounded_Rational := L * R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &  " * "
               &  Image (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3, From_Half_Word (15),  5/1);
      Check (-1/3, From_Half_Word (15), -5/1);
   end;
   Put_Line ("rational * half word test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   R      : Half_Word;
                   Result : Unbounded_Rational
                )  is
         X : constant Unbounded_Rational := L * R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &  " *"
               &  Half_Word'Image (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3, 15,  5/1);
      Check (-1/3, 15, -5/1);
   end;
   Put_Line ("rational / rational test");
   declare
      procedure Check
                (  L, R, Result : Unbounded_Rational
                )  is
         X : constant Unbounded_Rational := L / R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &  " / "
               &  Img (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3,  1/3,  1/1);
      Check (-1/3,  1/3, -1/1);
      Check ( 1/3, -1/3, -1/1);
      Check (-1/3, -1/3,  1/1);
   end;
   Put_Line ("rational / integer test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   R      : Unbounded_Integer;
                   Result : Unbounded_Rational
                )  is
         use Strings_Edit.Unbounded_Integer_Edit;
         X : constant Unbounded_Rational := L / R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &  " / "
               &  Image (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3,  To_Unbounded_Integer (15),  1/45);
      Check (-1/3,  To_Unbounded_Integer (15), -1/45);
      Check ( 1/3, -To_Unbounded_Integer (15), -1/45);
      Check (-1/3, -To_Unbounded_Integer (15),  1/45);
   end;
   Put_Line ("rational / unsigned test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   R      : Unbounded_Unsigned;
                   Result : Unbounded_Rational
                )  is
         use Strings_Edit.Unbounded_Unsigned_Edit;
         X : constant Unbounded_Rational := L / R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &  " / "
               &  Image (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3, From_Half_Word (15),  1/45);
      Check (-1/3, From_Half_Word (15), -1/45);
   end;
   Put_Line ("rational / half word test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   R      : Half_Word;
                   Result : Unbounded_Rational
                )  is
         X : constant Unbounded_Rational := L / R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &  " /"
               &  Half_Word'Image (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3, 15,  1/45);
      Check (-1/3, 15, -1/45);
   end;
   Put_Line ("** test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   R      : Natural;
                   Result : Unbounded_Rational
                )  is
         X : constant Unbounded_Rational := L ** R;
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  Img (L)
               &  " **"
               &  Natural'Image (R)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/3, 3,  1/27);
      Check (-1/3, 3, -1/27);
      Check ( 2/3, 2,  4/9 );
      Check (-2/3, 2,  4/9 );
   end;
   Put_Line ("Round test 1");
   declare
      use Strings_Edit.Unbounded_Integer_Edit;
      X : Unbounded_Rational;
   begin
      X := 1000000000/41;
      if Round (X) /= 24390244 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Round 1000000000/41 "
            &  Image (Round (X))
            &  " /= 24390244 (expected)"
         )  );
      end if;
      X := 45300 / 8;
      if Round (X) /= To_Unbounded_Integer (5663) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Round 5662.5 "
            &  Image (Round (X))
            &  " /= 5663 (expected)"
         )  );
      end if;
      X := 1/2;
      if Round (X) /= 1 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Round 0.5 "
            &  Image (Round (X))
            &  " /= 1 (expected)"
         )  );
      end if;
      X := -1/2;
      if Round (X) /= -1 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Round -0.5 "
            &  Image (Round (X))
            &  " /= -1 (expected)"
         )  );
      end if;
      X := 1/3;
      if Round (X) /= 0 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Round 1/3 "
            &  Image (Round (X))
            &  " /= 0 (expected)"
         )  );
      end if;
   end;
   Put_Line ("Round test 2");
   declare
      procedure Check
                (  X, Y  : Unbounded_Rational;
                   Power : Natural
                )  is
         Z : Unbounded_Rational := X;
      begin
         Round (Z, Power);
         if Z /= Y then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Round "
               &  Img (X)
               &  " by"
               &  Integer'Image (Power)
               &  " = "
               &  Img (Z)
               &  " /= "
               &  Img (Y)
               &  " (expected)"
            )  );
         end if;
      end Check;
      procedure Check
                (  X     : String;
                   Y     : Unbounded_Rational;
                   Power : Natural;
                   Base  : Positive := 10
                )  is
         X1 : constant Unbounded_Rational := Value (X, Base => Base);
         Z  : Unbounded_Rational := X1;
      begin
         Round (Z, Power);
         if Z /= Y then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Round "
               &  Image (X1, Base => 2, Fraction => 30)
               &  " by"
               &  Integer'Image (Power)
               &  " = "
               &  Image (Z, Base => 2, Fraction => 30)
               &  " ["
               &  Image (Z, Fraction => 12)
               &  "] /= "
               &  Image (Y, Base => 2, Fraction => 30)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (2#10001#/2#10000#,  2#1001#/2#1000#,  3);
      Check (2#10001#/2#10000#, 2#10001#/2#10000#, 4);
      Check (2#10001#/2#10000#,        1/1,        2);
      Check (       1/3,               0/1,        0);
      Check
      (  X     => "9.9666444221470",
         Y     => Value ("1001.1111011101110110000001", Base => 2),
         Power => 22,
         Base  => 10
      );
      Check
      (  X     => "0.0621915459632",
         Y     => Value ("0.0000111111101011110010", Base => 2),
         Power => 22,
         Base  => 10
      );
   end;
   Put_Line ("Put (Fraction = 0) test 1");
   declare
      Text    : String (1..15) := (others => '#');
      Pointer : Integer  := 1;
      X       : constant Unbounded_Rational :=
                         To_Unbounded_Rational (12345);
   begin
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => X,
         Fraction    => 0,
         Field       => 10,
         Justify     => Right,
         Fill        => '*'
      );
      if Pointer /= 11 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Put"
            &  Integer'Image (Pointer)
            &  " /= 11  (expected)"
         )  );
      elsif Text /= "*****12345#####" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Put "
            &  Quote (Text)
            &  " /= ""*****12345#####"" (expected)"
         )  );
      end if;
   end;
   Put_Line ("Put (Fraction = 0) test 2");
   declare
      Text    : String (1..15) := (others => '#');
      Pointer : Integer  := 1;
      X       : Unbounded_Rational := To_Unbounded_Rational (12345);
   begin
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => X,
         Fraction    => 0,
         Field       => 10,
         Justify     => Left,
         Fill        => '*'
      );
      if Pointer /= 11 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Put"
            &  Integer'Image (Pointer)
            &  " /= 11  (expected)"
         )  );
      elsif Text /= "12345*****#####" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Put "
            &  Quote (Text)
            &  " /= ""12345*****#####"" (expected)"
         )  );
      end if;
      Pointer := 1;
      Get (Text, Pointer, X);
      if Pointer /= 6 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get"
            &  Integer'Image (Pointer)
            &  " /= 6  (expected)"
         )  );
      elsif X /= To_Unbounded_Rational (12345) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get "
            &  Image (X)
            &  " /= 1234 (expected)"
         )  );
      end if;
   end;
   Put_Line ("Put (Fraction = 0) test 3");
   declare
      Text    : String (1..15) := (others => '#');
      Pointer : Integer  := 1;
      X       : constant Unbounded_Rational :=
                         To_Unbounded_Rational (12345);
   begin
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => X,
         Fraction    => 0,
         Field       => 10,
         Justify     => Center,
         Fill        => '*'
      );
      if Pointer /= 11 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Put"
            &  Integer'Image (Pointer)
            &  " /= 11  (expected)"
         )  );
      elsif Text /= "**12345***#####" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Put "
            &  Quote (Text)
            &  " /= ""**12345***#####"" (expected)"
         )  );
      end if;
   end;
   Put_Line ("Image (Fraction = 3) test");
   declare
      X : constant Unbounded_Rational := Unbounded_Rationals.One / 3;
   begin
      if Image (X, Fraction => 3) /= "0.333" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Image 1/3 "
            &  Quote (Image (X, Fraction => 3))
            &  " /= ""0.333"" (expected)"
         )  );
      end if;
   end;
   Put_Line ("Image (Fraction = 4) test");
   declare
      X : constant Unbounded_Rational :=
                   To_Unbounded_Rational (453) / 8;
   begin
      if Image (X, Fraction => 4) /= "56.6250" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Image 453/8 "
            &  Quote (Image (X, Fraction => 4))
            &  " /= ""56.6250"" (expected)"
         )  );
      end if;
   end;
   Put_Line ("Image (Fraction = 7) test");
   declare
      X : constant Unbounded_Rational :=
                   To_Unbounded_Rational (1) / 41;
   begin
      if Image (X, Fraction => 7) /= "0.0243902" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Image 1/41 "
            &  Quote (Image (X, Fraction => 7))
            &  " /= ""0.0243902"" (expected)"
         )  );
      end if;
   end;
   Put_Line ("Image (Fraction = 8) test");
   declare
      X : constant Unbounded_Rational :=
                   To_Unbounded_Rational (1) / 41;
   begin
      if Image (X, Fraction => 8) /= "0.02439024" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Image 1/41 "
            &  Quote (Image (X, Fraction => 8))
            &  " /= ""0.02439024"" (expected)"
         )  );
      end if;
   end;
   Put_Line ("Image (Fraction = 9) test");
   declare
      X : constant Unbounded_Rational :=
                   To_Unbounded_Rational (1) / 41;
   begin
      if Image (X, Fraction => 9) /= "0.024390244" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Image 1/41 "
            &  Quote (Image (X, Fraction => 9))
            &  " /= ""0.024390244"" (expected)"
         )  );
      end if;
   end;
   Put_Line ("Image (Fraction = 10) test");
   declare
      X : constant Unbounded_Rational :=
                   To_Unbounded_Rational (1) / 41;
   begin
      if Image (X, Fraction => 10) /= "0.0243902439" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Image 1/41 "
            &  Quote (Image (X, Fraction => 10))
            &  " /= ""0.0243902439"" (expected)"
         )  );
      end if;
   end;
   Put_Line ("Image (Fraction = 6) test");
   declare
      X : constant Unbounded_Rational :=
                   To_Unbounded_Rational (65882) / 295245;
   begin
      if Image (X, Fraction => 6) /= "0.223143" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Image 65882/29524 "
            &  Quote (Image (X, Fraction => 6))
            &  " /= ""0.223143"" (expected)"
         )  );
      end if;
   end;
   Put_Line ("Image (Fraction = 6) + negative test");
   declare
      X : constant Unbounded_Rational :=
                   -To_Unbounded_Rational (65882) / 295245;
   begin
      if Image (X, Fraction => 6) /= "-0.223143" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Image -65882/29524 "
            &  Quote (Image (X, Fraction => 6))
            &  " /= ""-0.223143"" (expected)"
         )  );
      end if;
   end;
   Put_Line ("Image (Base = 2, Fraction = 22) test");
   declare
      X : constant Unbounded_Rational :=
                   To_Unbounded_Rational (65882) / 295245;
      S : constant String := Image (X, Base => 2, Fraction => 22);
   begin
      if S /= "0.0011100100011111111100" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Image 65882/29524 "
            &  Quote (S)
            &  " /= ""0.0011100100011111111100"" ["
            &  Image (Value (S, Base => 2), Fraction => 6)
            &  "] (expected)"
         )  );
      end if;
   end;
   Put_Line ("Value test");
   declare
      procedure Check
                (  S    : String;
                   R    : Unbounded_Rational;
                   Base : Positive := 10
                )  is
         X : constant Unbounded_Rational := Value (S, Base => Base);
      begin
         if X /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Value "
               &  Quote (S)
               &  " = "
               &  Img (X)
               &  " /= "
               &  Img (R)
               &  " (expected)"
            )  );
         end if;
      end;
   begin
      Check ("  0  ",      0/1    );
      Check ("1    ",      1/1    );
      Check ("1.1  ",     11/10   );
      Check ("1.1",        3/2,  2);
      Check ("-.1  ",     -1/10   );
      Check ("-0.1 E+1" , -1/1    );
      Check ("0.1 E -1" ,  1/100  );
   end;
   Put_Line ("GE test");
   declare
      X : constant Unbounded_Rational :=
                   To_Unbounded_Rational (1) / 2;
   begin
      if not (X >= Half) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "1/2 >= 1/2 is False, True (expected)"
         )  );
      end if;
   end;
   Put_Line ("Floor test");
   declare
      X : constant Unbounded_Rational :=
                   To_Unbounded_Rational (45300) / 8;
      Y : constant Unbounded_Rational :=
                   To_Unbounded_Rational (123);
   begin
      if Floor (X) /= To_Unbounded_Integer (5662) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Floor 5662.5 "
            &  Img (X)
            &  " /= ""5662 (expected)"
         )  );
      elsif Floor (-X) /= To_Unbounded_Integer (-5663) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Floor -5662.5 "
            &  Img (X)
            &  " /= ""-5663 (expected)"
         )  );
      end if;
      if Floor (Y) /= To_Unbounded_Integer (123) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Floor 123 "
            &  Img (Y)
            &  " /= ""123 (expected)"
         )  );
      elsif Floor (-Y) /= To_Unbounded_Integer (-123) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Floor -123 "
            &  Img (Y)
            &  " /= ""-123 (expected)"
         )  );
      end if;
   end;
   Put_Line ("Ceiling test");
   declare
      X : constant Unbounded_Rational :=
                   To_Unbounded_Rational (45300) / 8;
      Y : constant Unbounded_Rational :=
                   To_Unbounded_Rational (123);
   begin
      if Ceiling (X) /= To_Unbounded_Integer (5663) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Ceiling 5662.5 "
            &  Img (X)
            &  " /= ""5662 (expected)"
         )  );
      elsif Ceiling (-X) /= To_Unbounded_Integer (-5662) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Ceiling -5662.5 "
            &  Img (X)
            &  " /= ""-5663 (expected)"
         )  );
      end if;
      if Ceiling (Y) /= To_Unbounded_Integer (123) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Ceiling 123 "
            &  Img (Y)
            &  " /= ""123 (expected)"
         )  );
      elsif Floor (-Y) /= To_Unbounded_Integer (-123) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Ceiling -123 "
            &  Img (Y)
            &  " /= ""-123 (expected)"
         )  );
      end if;
   end;
   Put_Line ("Image (Fraction = 2) test");
   declare
      X : constant Unbounded_Rational :=
                   To_Unbounded_Rational (453) / 8;
   begin
      if Image (X, Fraction => 2) /= "56.63" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Image 453/8 "
            &  Quote (Image (X, Fraction => 2))
            &  " /= ""56.63"" (expected)"
         )  );
      end if;
   end;
   Put_Line ("Rational <-> float test");
   declare
      procedure Check
                (  X : Float;
                   Y : Unbounded_Rational
                )  is
         R1 : constant Unbounded_Rational :=
                       To_Unbounded_Rational (X);
         R2 : constant Float := To_Float (Y);
      begin
         if Y /= R1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Conversion "
               &  Float'Image (X)
               &  " = "
               &  Img (R1)
               &  " /= "
               &  Img (Y)
               &  " (expected)"
            )  );
         elsif X /= R2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Conversion "
               &  Img (Y)
               &  " = "
               &  Float'Image (R2)
               &  " /= "
               &  Float'Image (X)
               &  " (expected) ["
               &  Float'Image (X - R2)
               &  "]"
            )  );
         end if;
      end Check;
   begin
      Check (        0.125,       1/8);
      Check (        2.0,         2/1);
      Check (        0.0,         0/1);
      Check (        1.0,         1/1);
      Check (        0.5,         1/2);
      Check (        1.5,         3/2);
      Check (     1234.5,      2469/2);
      Check (        7.125,      57/8);
      Check (  1234567.0,   1234567/1);
      Check (200000000.0, 200000000/1);
   end;
   Put_Line ("Rational <-> fixed test");
   declare
      type Fixed is delta 0.125 range -2.0..100.0;
      package Fixed_Conversions is new Fixed_Point_Conversions (Fixed);
      use Fixed_Conversions;
      procedure Check
                (  X : Fixed;
                   Y : Unbounded_Rational
                )  is
         R1 : constant Unbounded_Rational :=
                       To_Unbounded_Rational (X);
         R2 : constant Fixed := From_Unbounded_Rational (Y);
      begin
         if Y /= R1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Conversion "
               &  Fixed'Image (X)
               &  " = "
               &  Img (R1)
               &  " /= "
               &  Img (Y)
               &  " (expected)"
            )  );
         elsif False then --X /= R2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Conversion "
               &  Img (Y)
               &  " ="
               &  Fixed'Image (R2)
               &  " /="
               &  Fixed'Image (X)
               &  " (expected) ["
               &  Fixed'Image (X - R2)
               &  "]"
            )  );
         end if;
      end Check;
   begin
      Check ( 1.0,    1/1);
      Check (-1.0,   -1/1);
      Check ( 0.125,  1/8);
      Check (-0.125, -1/8);
      Check ( 2.0,    2/1);
      Check (-2.0,   -2/1);
      Check ( 0.0,    0/1);
      Check ( 0.5,    1/2);
      Check (-0.5,   -1/2);
      Check ( 1.5,    3/2);
      Check (-1.5,   -3/2);
   end;
   Put_Line ("Error test");
   declare
      procedure Check
                (  L, R   : Unbounded_Rational;
                   Power  : Integer;
                   Result : Boolean
                )  is
         X : constant Boolean := Error (L, R, Power);
      begin
         if X /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error ("
               &  Img (L)
               &  ", "
               &  Img (R)
               &  ","
               &  Integer'Image (Power)
               &  ") = "
               &  Boolean'Image (X)
               &  " /= "
               &  Boolean'Image (Result)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (3/1, 5/1,   0, True );
      Check (3/1, 5/1,   1, False);
      Check (1/3, 1/3, -10, False);
      Check (1/3, 1/5,   2, False);
      Check (1/3, 1/5,   0, False);
      Check (1/3, 1/5,  -1, False);
      Check (1/3, 1/5,  -2, False);
      Check (1/3, 1/5,  -3, True );
   end;
   Put_Line ("Split test 1");
   declare
      procedure Check
                (  V : Unbounded_Rational;
                   N : Unbounded_Integer;
                   F : Unbounded_Rational
                )  is
         use Strings_Edit.Unbounded_Integer_Edit;
         N_1 : Unbounded_Integer;
         F_1 : Unbounded_Rational;
      begin
         Split (V, N_1, F_1);
         if N /= N_1 or else F /= F_1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Split "
               &  Image (V, Fraction => 10)
               &  " = "
               &  Image (N_1)
               &  " + "
               &  Image (F_1, Fraction => 10)
               &  " /= "
               &  Image (N)
               &  " + "
               &  Image (F, Fraction => 10)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 0/1, To_Unbounded_Integer ( 0), 0/1);
      Check ( 3/1, To_Unbounded_Integer ( 3), 0/1);
      Check (-3/1, To_Unbounded_Integer (-3), 0/1);
      Check ( 3/5, To_Unbounded_Integer ( 0), 3/5);
      Check (-3/5, To_Unbounded_Integer (-1), 2/5);
      Check ( 8/5, To_Unbounded_Integer ( 1), 3/5);
      Check (-8/5, To_Unbounded_Integer (-2), 2/5);
   end;
   Put_Line ("Split test 2");
   declare
      procedure Check
                (  V : Unbounded_Rational;
                   N : Unbounded_Integer;
                   F : Unbounded_Rational
                )  is
         use Strings_Edit.Unbounded_Integer_Edit;
         N_1 : Unbounded_Integer;
         F_1 : Unbounded_Rational := V;
      begin
         Split (F_1, N_1);
         if N /= N_1 or else F /= F_1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "In-place split "
               &  Image (V, Fraction => 10)
               &  " = "
               &  Image (N_1)
               &  " + "
               &  Image (F_1, Fraction => 10)
               &  " /= "
               &  Image (N)
               &  " + "
               &  Image (F, Fraction => 10)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 0/1, To_Unbounded_Integer ( 0), 0/1);
      Check ( 3/1, To_Unbounded_Integer ( 3), 0/1);
      Check (-3/1, To_Unbounded_Integer (-3), 0/1);
      Check ( 3/5, To_Unbounded_Integer ( 0), 3/5);
      Check (-3/5, To_Unbounded_Integer (-1), 2/5);
      Check ( 8/5, To_Unbounded_Integer ( 1), 3/5);
      Check (-8/5, To_Unbounded_Integer (-2), 2/5);
   end;
   Put_Line ("Continued fraction test 1");
   declare
      use Strings_Edit.Unbounded_Integer_Edit;
      X, Z : Unbounded_Rational;
      Y    : Continued_Fraction;
   begin
      X := 5/19 + 3;
      Set (Y, X);
      if Get_Length (Y) /= 4 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Conversion "
            &  Img (X)
            &  " length"
            &  Integer'Image (Get_Length (Y))
            &  " /= 4 (expected)"
         )  );
      elsif Get_Term (Y, 1) /= 3 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Conversion "
            &  Img (X)
            &  " term 1 "
            &  Image (Get_Term (Y, 1))
            &  " /= 3 (expected)"
         )  );
      elsif Get_Term (Y, 2) /= 3 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Conversion "
            &  Img (X)
            &  " term 2 "
            &  Image (Get_Term (Y, 2))
            &  " /= 3 (expected)"
         )  );
      elsif Get_Term (Y, 3) /= 1 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Conversion "
            &  Img (X)
            &  " term 3 "
            &  Image (Get_Term (Y, 3))
            &  " /= 1 (expected)"
         )  );
      elsif Get_Term (Y, 4) /= 4 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Conversion "
            &  Img (X)
            &  " term 4 "
            &  Image (Get_Term (Y, 4))
            &  " /= 4 (expected)"
         )  );
      end if;
      Z := Get_Value (Y);
      if X /= Z then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Conversion "
            &  Image (Y)
            &  " = "
            &  Img (Z)
            &  " /= "
            &  Img (X)
            &  " (expected)"
         )  );
      end if;
      if Image (Y) /= "[3; 3, 1, 4]" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Image "
            &  Image (Y)
            &  " = "
            &  Image (Y)
            &  " /= "
            &  "[3; 3, 1, 4]"
            &  " (expected)"
         )  );
      end if;
      if Image_Rational (X) /= "[3; 3, 1, 4]" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Image "
            &  Image (X)
            &  " = "
            &  Image_Rational (X)
            &  " /= "
            &  "[3; 3, 1, 4]"
            &  " (expected)"
         )  );
      end if;
      Z := Value_Rational ("[3; 3, 1, 4]");
      if X /= Z then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Value [3; 3, 1, 4]"
            &  " = "
            &  Img (Z)
            &  " /= "
            &  Img (X)
            &  " (expected)"
         )  );
      end if;
   end;
   Put_Line ("Continued fraction test 2");
   declare
      use Strings_Edit.Unbounded_Integer_Edit;
      X, Z : Unbounded_Rational;
      Y    : Continued_Fraction;
   begin
      X := -50399/32085;
      Set (Y, X);
      if Get_Length (Y) /= 6 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Conversion "
            &  Img (X)
            &  " length"
            &  Integer'Image (Get_Length (Y))
            &  " /= 6 (expected)"
         )  );
      elsif Get_Term (Y, 1) /= -2 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Conversion "
            &  Img (X)
            &  " term 1 "
            &  Image (Get_Term (Y, 1))
            &  " /= -2 (expected)"
         )  );
      elsif Get_Term (Y, 2) /= 2 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Conversion "
            &  Img (X)
            &  " term 2 "
            &  Image (Get_Term (Y, 2))
            &  " /= 2 (expected)"
         )  );
      elsif Get_Term (Y, 3) /= 3 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Conversion "
            &  Img (X)
            &  " term 3 "
            &  Image (Get_Term (Y, 3))
            &  " /= 3 (expected)"
         )  );
      elsif Get_Term (Y, 4) /= 31 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Conversion "
            &  Img (X)
            &  " term 4 "
            &  Image (Get_Term (Y, 4))
            &  " /= 31 (expected)"
         )  );
      elsif Get_Term (Y, 5) /= 1 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Conversion "
            &  Img (X)
            &  " term 5 "
            &  Image (Get_Term (Y, 5))
            &  " /= 1 (expected)"
         )  );
      elsif Get_Term (Y, 6) /= 141 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Conversion "
            &  Img (X)
            &  " term 6 "
            &  Image (Get_Term (Y, 6))
            &  " /= 141 (expected)"
         )  );
      end if;
      Z := Get_Value (Y);
      if X /= Z then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Conversion "
            &  Image (Y)
            &  " = "
            &  Img (Z)
            &  " /= "
            &  Img (X)
            &  " (expected)"
         )  );
      end if;
      if Image (Y) /= "[-2; 2, 3, 31, 1, 141]" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Image "
            &  Image (Y)
            &  " = "
            &  Image (Y)
            &  " /= "
            &  "[-2; 2, 3, 31, 1, 141]"
            &  " (expected)"
         )  );
      end if;
      if Image_Rational (X) /= "[-2; 2, 3, 31, 1, 141]" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Image "
            &  Image (X)
            &  " = "
            &  Image_Rational (X)
            &  " /= "
            &  "[-2; 2, 3, 31, 1, 141]"
            &  " (expected)"
         )  );
      end if;
      Z := Value_Rational ("[-2; 2, 3, 31, 1, 141]");
      if X /= Z then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Value [-2; 2, 3, 31, 1, 141]"
            &  " = "
            &  Img (Z)
            &  " /= "
            &  Img (X)
            &  " (expected)"
         )  );
      end if;
   end;
   Put_Line ("ln 2 test");
   declare
      Value : constant Unbounded_Rational := ln_2 (-21);
   begin
      if Image (Value) /= "0.693147" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "ln 2 "
            &  Image (Value)
            &  " ("
            &  Img (Value)
            &  ") /= 0.693147 (expected)"
         )  );
      end if;
   end;
   Put_Line ("Pi (Nilkantha) test");
   declare
      Value : constant Unbounded_Rational := Pi_Nilkantha (-21);
   begin
      if Image (Value) /= "3.141593" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Pi "
            &  Image (Value)
            &  " ("
            &  Img (Value)
            &  ") /= 3.141593 (expected)"
         )  );
      end if;
   end;
   Put_Line ("Pi (Chudnovsky) test");
   declare
      Value : constant Unbounded_Rational := Pi_Chudnovsky (-21);
   begin
      if Image (Value) /= "3.141593" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Pi "
            &  Image (Value)
            &  " ("
            &  Img (Value)
            &  ") /= 3.141593 (expected)"
         )  );
      end if;
   end;
   declare
      Value : constant Unbounded_Rational := Pi_Chudnovsky (-100);
   begin --                      123456789012345678901234567
      if (  Image (Value, Fraction => 27)
         /= "3.141592653589793238462643383"
         )  then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Pi "
            &  Image (Value)
            &  " ("
            &  Img (Value)
            &  ") /= 3.141592653589793238462643383 (expected)"
         )  );
      end if;
   end;
   Put_Line ("arccos test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Arccos (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "arccos "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 0/1,  "1.570796");
      Check ( 1/2,  "1.047198");
      Check (-1/2,  "2.094395");
      Check ( 9/10, "0.451027");
      Check (-9/10, "0.451027");
      Check ( 1/1,  "0"       );
   end;
   Put_Line ("arcosh test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Arccosh (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "arccosh "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (  1/1, "0"       );
      Check (15/10, "0.962424");
      Check (  2/1, "1.316958");
      Check (  3/1, "1.762747");
   end;
   Put_Line ("arcsin test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Arcsin (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "arcsin "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 0/1,  "0"       );
      Check ( 1/2,  "0.523599");
      Check (-1/2, "-0.523599");
      Check ( 1/1,  "1.570796");
   end;
   Put_Line ("arcsinh test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Arcsinh (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "arcsinh "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 0/1,  "0");
      Check ( 1/2,  "0.481212");
      Check (-1/2, "-0.481212");
      Check ( 1/1,  "0.881374");
      Check ( 2/1,  "1.443635");
      Check ( 3/1,  "1.818446");
   end;
   Put_Line ("arctan test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Arctan (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "arctan "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/10, "0.099669");
      Check ( 1/2,  "0.463648");
      Check ( 1/1,  "0.785398");
      Check ( 8/1,  "1.446441");
      Check ( 4/1,  "1.325818");
      Check ( 2/1,  "1.107149");
      Check ( 0/1,  "0"       );
      Check (-8/1, "-1.446441");
   end;
   Put_Line ("arctan 2 test");
   declare
      procedure Check
                (  L1, L2 : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Arctan (L1, L2, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "arctan 2 "
               &  Img (L1)
               &  ", "
               &  Img (L2)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/1, 2/1,  "0.463648");
      Check ( 2/1, 1/1,  "1.107149");
      Check (-2/1, 1/1, "-1.107149");
   end;
   Put_Line ("arctanh test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Arctanh (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "arctanh "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 0/1,   "0"       );
      Check ( 1/10,  "0.100335");
      Check (-1/10, "-0.100335");
      Check ( 9/10,  "1.472219");
   end;
   Put_Line ("cos test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Cos (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "cos "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (357793/1000000, "0.936672");
      Check ( 15708/10000,  "-0.000004");
      Check (     0/1,       "1.000000");
      Check (     1/10,      "0.995004");
      Check (     7/10,      "0.764842");
      Check (    -7/10,      "0.764842");
      Check (     2/1,      "-0.416147");
      Check (     3/1,      "-0.989992");
      Check (     4/1,      "-0.653644");
      Check (     5/1,       "0.283662");
      Check (     6/1,       "0.960170");
      Check (     7/1,       "0.753902");
   end;
   Put_Line ("cosh test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Cosh (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "cosh "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (357793/1000000, "1.064694");
      Check ( 15708/10000,   "2.509187");
      Check (     0/1,       "1.000000");
      Check (     1/10,      "1.005004");
      Check (     7/10,      "1.255169");
      Check (    -7/10,      "1.255169");
      Check (     2/1,       "3.762196");
      Check (     3/1,      "10.067662");
      Check (     4/1,      "27.308233");
      Check (     5/1,      "74.209949");
      Check (     6/1,     "201.715636");
      Check (     7/1,     "548.317035");
   end;
   Put_Line ("cot test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Cot (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "cot "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (157071/100000,  "0.000086");
      Check (357793/1000000, "2.674617");
      Check ( 15707/10000,   "0.000096");
      Check (     1/10,      "9.966644");
      Check (     7/10,      "1.187242");
      Check (    -7/10,     "-1.187242");
   end;
   Put_Line ("coth test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Coth (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "coth "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (357793/1000000, "2.913171");
      Check ( 15708/10000,   "1.090331");
      Check (     1/10,     "10.033311");
      Check (     7/10,      "1.654622");
      Check (    -7/10,     "-1.654622");
      Check (     2/1,       "1.037315");
      Check (     3/1,       "1.004970");
      Check (     4/1,       "1.000671");
      Check (     5/1,       "1.000091");
      Check (     6/1,       "1.000012");
      Check (     7/1,       "1.000002");
   end;
   Put_Line ("exp test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Exp (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "exp "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (      1/8,          "1.133148");
      Check (      1/2,          "1.648721");
      Check (      5/8,          "1.868246");
      Check (      7/8,          "2.398875");
      Check (     10/1,      "22026.465795");
      Check (     -1/1,          "0.367879");
      Check (    -10/1,          "0.000045");
      Check (98908671/100000000, "2.688778");
   end;
   Put_Line ("ln test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String;
                   Error  : Integer := -21
                )  is
         X : constant Unbounded_Rational := Ln (L, Error);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "ln "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (  75/1000,   "-2.590267");
      Check (  75/100000, "-7.195437");
      Check (  75/10000,  "-4.892852");
      Check (  75/100,    "-0.287682");
      Check (   8/10,     "-0.223144");
      Check (1536/1000,    "0.429182");
      Check (   2/1,       "0.693147");
      Check (   1/10,     "-2.302585");
   end;
   Put_Line ("log2 test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String;
                   Error  : Integer := -22
                )  is
         X : constant Unbounded_Rational := Log2 (L, Error);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "log2 "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (158057/1,  "17.250000", -2);
      Check (158057/1,  "17.000000",  0);
      Check (     7/10, "-0.514573"    );
      Check (     1/8,  "-3.000000"    );
      Check (     1/4,  "-2.000000"    );
      Check (     1/2,  "-1.000000"    );
      Check (    83/1,   "6.375039"    );
      Check (    81/10,  "3.017922"    );
      Check (     8/1,   "3.000000"    );
      Check (     4/1,   "2.000000"    );
      Check (     2/1,   "1.000000"    );
      Check (     1/1,   "0"           );
   end;
   Put_Line ("mod Pi/2 test");
   declare
      procedure Check
                (  L        : Unbounded_Rational;
                   Result   : String;
                   Quadrant : Quadrant_Type;
                   Error    : Integer := -22
                )  is
         X       : Unbounded_Rational;
         Q       : Quadrant_Type;
         Half_Pi : Unbounded_Rational;
      begin
         Mod_Half_Pi (L, Error, X, Q, Half_Pi);
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "mod Pi/2 "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         elsif Q /= Quadrant then
            Raise_Exception
            (  Data_Error'Identity,
               (  "mod Pi/2 "
               &  Img (L)
               &  " quadrant ="
               &  Quadrant_Type'Image (Q)
               &  " /="
               &  Quadrant_Type'Image (Quadrant)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 1/2, "0.500000", I);
      Check ( 1/1, "1.000000", I);
      Check (-1/1, "1.000000", IV);
      Check ( 2/1, "0.429204", II);
      Check (-2/1, "0.429204", III);
   end;
   Put_Line ("power test");
   declare
      procedure Check
                (  L, R   : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Power (L, R, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "power "
               &  Img (L)
               &  ", "
               &  Img (R)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (  4/1,    -2/1,        "0.062500");
      Check (  2/100, -71/100,     "16.079358");
      Check (  2/100,  71/100,      "0.062191");
      Check ( 86/10,   73/10, "6634965.883220");
      Check ( 86/10,    3/10,       "1.906995");
      Check (  1/2,    -2/1,        "4.000000");
      Check (  1/2,     2/1,        "0.250000");
      Check ( 16/1,    -1/2,        "0.250000");
      Check ( 16/1,     1/2,        "4.000000");
      Check (  4/1,    -2/1,        "0.062500");
      Check (  0/1,     2/1,        "0"       );
      Check (  4/1,     0/1,        "1.000000");
      Check (  4/1,     2/1,       "16.000000");
   end;
   Put_Line ("root test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   N      : Positive;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Root (L, N, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "root"
               &  Integer'Image (N)
               &  "th "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 0/1, 5, "0"       );
      Check ( 1/1, 5, "1.000000");
      Check (87/1, 4, "3.054076");
   end;
   Put_Line ("sin test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Sin (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "sin "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 573204/364913, "1.000000");
      Check (357793/1000000, "0.350208");
      Check ( 15708/10000,   "1.000000");
      Check (     0/1,       "0"       );
      Check (     1/10,      "0.099833");
      Check (     7/10,      "0.644218");
      Check (    -7/10,     "-0.644218");
      Check (     2/1,       "0.909297");
      Check (     3/1,       "0.141120");
      Check (     4/1,      "-0.756803");
      Check (     5/1,      "-0.958924");
      Check (     6/1,      "-0.279416");
      Check (     7/1,       "0.656987");
   end;
   Put_Line ("sin (X) / X test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Sin_Div_X (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "sin (X) / X "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check ( 0/1,          "1.000000");
      Check ( 1/100,        "0.999983");
      Check ( 1/10,         "0.998334");
      Check ( 1/1,          "0.841471");
      Check ( 14/10,        "0.703893");
      Check (573204/364913, "0.636620");
   end;
   Put_Line ("sinh test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Sinh (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "sinh "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (357793/1000000, "0.365476");
      Check ( 15708/10000,   "2.301308");
      Check (     0/1,       "0"       );
      Check (     1/10,      "0.100167");
      Check (     7/10,      "0.758584");
      Check (    -7/10,     "-0.758584");
      Check (     2/1,       "3.626860");
      Check (     3/1,      "10.017875");
      Check (     4/1,      "27.289917");
      Check (     5/1,      "74.203211");
      Check (     6/1,     "201.713157");
      Check (    7/1,      "548.316123");
   end;
   Put_Line ("Sqrt test");
   declare
      procedure Check
                (  L     : Unbounded_Rational;
                   Power : Integer
                )  is
         X : constant Unbounded_Rational := Sqrt (L, Power);
      begin
         if abs (X*X - L) > Unbounded_Rationals.Two ** Power then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Sqrt ("
               &  Img (L)
               &  ") ** 2 = "
               &  Image (X*X)
               &  " /= "
               &  Image (L)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (4/1, -10);
      Check (2/1, -21);
      Put_Line ("Sqrt 2 = " & Image (Sqrt (2/1, -20)));
   end;
   Put_Line ("tan continued fraction test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   N      : Positive;
                   Result : String
                )  is
         Y : constant Unbounded_Rational :=
                Tan_Continued_Fraction (L, N);
      begin
         if Image (Y) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "tan continued fraction "
               &  Img (L)
               &  " = "
               &  Image (Y)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (698132/1000000, 7,  "0.839100");
      Check (     7/10,      7,  "0.842288");
      Check (     1/1,       7,  "1.557408");
      Check (     2/1,       9, "-2.185040");
   end;
   Put_Line ("tan test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Tan (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "tan "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (157071/100000, "11583.888858");
      Check (357793/1000000,    "0.373885");
      Check (     0/1,          "0"       );
      Check (     1/10,         "0.100335");
      Check (     7/10,         "0.842288");
      Check (    -7/10,        "-0.842288");
      Check ( 15707/10000, "10381.327418" );
   end;
   Put_Line ("tanh test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   Result : String
                )  is
         X : constant Unbounded_Rational := Tanh (L, -22);
      begin
         if Image (X) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "tanh "
               &  Img (L)
               &  " = "
               &  Image (X)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (357793/1000000, "0.343269");
      Check ( 15708/10000,   "0.917153");
      Check (     0/1,       "0"       );
      Check (     1/10,      "0.099668");
      Check (     7/10,      "0.604368");
      Check (    -7/10,     "-0.604368");
      Check (     2/1,       "0.964028");
      Check (     3/1,       "0.995055");
      Check (     4/1,       "0.999329");
      Check (     5/1,       "0.999909");
      Check (     6/1,       "0.999988");
      Check (     7/1,       "0.999998");
   end;
   Put_Line ("tanh continued fraction test");
   declare
      procedure Check
                (  L      : Unbounded_Rational;
                   N      : Positive;
                   Result : String
                )  is
         Y : constant Unbounded_Rational :=
                Tanh_Continued_Fraction (L, N);
      begin
         if Image (Y) /= Result then
            Raise_Exception
            (  Data_Error'Identity,
               (  "tan continued fraction "
               &  Img (L)
               &  " = "
               &  Image (Y)
               &  " /= "
               &  Result
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (     1/2,       4, "0.462117");
      Check (     1/1,       5, "0.761594");
      Check (698132/1000000, 5, "0.603181");
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Unbounded_Rationals;
