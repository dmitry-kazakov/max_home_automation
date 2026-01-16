--                                                                    --
--  package Unbounded_Rationals     Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2025       --
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

with Unbounded_Integers;   use Unbounded_Integers;
with Unbounded_Unsigneds;  use Unbounded_Unsigneds;

with Strings_Edit.Lexicographical_Order;

package Unbounded_Rationals is
   use Strings_Edit.Lexicographical_Order;

   type Unbounded_Rational is private;
   Zero  : constant Unbounded_Rational;
   Half  : constant Unbounded_Rational;
   One   : constant Unbounded_Rational;
   Two   : constant Unbounded_Rational;
   Three : constant Unbounded_Rational;
   Four  : constant Unbounded_Rational;
   Five  : constant Unbounded_Rational;

   function "=" (Left, Right : Unbounded_Rational) return Boolean;
   function "=" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Boolean;
   function "=" (Left : Unbounded_Integer;  Right : Unbounded_Rational)
      return Boolean;
   function "=" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Boolean;
   function "=" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Boolean;
   function "=" (Left : Unbounded_Rational; Right : Half_Word)
      return Boolean;
   function "=" (Left : Half_Word; Right : Unbounded_Rational)
      return Boolean;

   function "<"  (Left, Right : Unbounded_Rational) return Boolean;
   function "<" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Boolean;
   function "<" (Left : Unbounded_Integer;  Right : Unbounded_Rational)
      return Boolean;
   function "<" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Boolean;
   function "<" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Boolean;
   function "<" (Left : Unbounded_Rational; Right : Half_Word)
      return Boolean;
   function "<" (Left : Half_Word; Right : Unbounded_Rational)
      return Boolean;

   function "<=" (Left, Right : Unbounded_Rational) return Boolean;
   function "<=" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Boolean;
   function "<=" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Boolean;
   function "<=" (Left : Unbounded_Integer;  Right : Unbounded_Rational)
      return Boolean;
   function "<=" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Boolean;
   function "<=" (Left : Unbounded_Rational; Right : Half_Word)
      return Boolean;
   function "<=" (Left : Half_Word; Right : Unbounded_Rational)
      return Boolean;

   function ">"  (Left, Right : Unbounded_Rational) return Boolean;
   function ">" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Boolean;
   function ">" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Boolean;
   function ">" (Left : Unbounded_Integer;  Right : Unbounded_Rational)
      return Boolean;
   function ">" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Boolean;
   function ">" (Left : Unbounded_Rational; Right : Half_Word)
      return Boolean;
   function ">" (Left : Half_Word; Right : Unbounded_Rational)
      return Boolean;

   function ">=" (Left, Right : Unbounded_Rational) return Boolean;
   function ">=" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Boolean;
   function ">=" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Boolean;
   function ">=" (Left : Unbounded_Integer;  Right : Unbounded_Rational)
      return Boolean;
   function ">=" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Boolean;
   function ">=" (Left : Unbounded_Rational; Right : Half_Word)
      return Boolean;
   function ">=" (Left : Half_Word; Right : Unbounded_Rational)
      return Boolean;

   function "abs" (Left : Unbounded_Rational) return Unbounded_Rational;

   function "+" (Left : Unbounded_Rational) return Unbounded_Rational;
   function "-" (Left : Unbounded_Rational) return Unbounded_Rational;

   function "+" (Left, Right : Unbounded_Rational)
      return Unbounded_Rational;
   function "+" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Unbounded_Rational;
   function "+" (Left : Unbounded_Integer;  Right : Unbounded_Rational)
      return Unbounded_Rational;
   function "+" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Unbounded_Rational;
   function "+" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Unbounded_Rational;
   function "+" (Left : Unbounded_Rational; Right : Half_Word)
      return Unbounded_Rational;
   function "+" (Left : Half_Word; Right : Unbounded_Rational)
      return Unbounded_Rational;

   function "-" (Left, Right : Unbounded_Rational)
      return Unbounded_Rational;
   function "-" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Unbounded_Rational;
   function "-" (Left : Unbounded_Integer;  Right : Unbounded_Rational)
      return Unbounded_Rational;
   function "-" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Unbounded_Rational;
   function "-" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Unbounded_Rational;
   function "-" (Left : Unbounded_Rational; Right : Half_Word)
      return Unbounded_Rational;
   function "-" (Left : Half_Word; Right : Unbounded_Rational)
      return Unbounded_Rational;

   function "*" (Left, Right : Unbounded_Rational)
      return Unbounded_Rational;
   function "*" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Unbounded_Rational;
   function "*" (Left : Unbounded_Integer;  Right : Unbounded_Rational)
      return Unbounded_Rational;
   function "*" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Unbounded_Rational;
   function "*" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Unbounded_Rational;
   function "*" (Left : Unbounded_Rational; Right : Half_Word)
      return Unbounded_Rational;
   function "*" (Left : Half_Word; Right : Unbounded_Rational)
      return Unbounded_Rational;

   function "/" (Left, Right : Unbounded_Rational)
      return Unbounded_Rational;
   function "/" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Unbounded_Rational;
   function "/" (Left : Unbounded_Integer;  Right : Unbounded_Rational)
      return Unbounded_Rational;
   function "/" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Unbounded_Rational;
   function "/" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Unbounded_Rational;
   function "/" (Left : Unbounded_Rational; Right : Half_Word)
      return Unbounded_Rational;
   function "/" (Left : Half_Word; Right : Unbounded_Rational)
      return Unbounded_Rational;

   function "**" (Left : Unbounded_Rational; Right : Integer)
      return Unbounded_Rational;

   function Compare (Left, Right : Unbounded_Rational)
      return Precedence;
   function Compare
            (  Left  : Unbounded_Rational;
               Right : Unbounded_Integer
            )  return Precedence;
   function Compare
            (  Left  : Unbounded_Integer;
               Right : Unbounded_Rational
            )  return Precedence;
   function Compare
            (  Left  : Unbounded_Rational;
               Right : Unbounded_Unsigned
            )  return Precedence;
   function Compare
            (  Left  : Unbounded_Unsigned;
               Right : Unbounded_Rational
            )  return Precedence;
   function Compare
            (  Left  : Unbounded_Rational;
               Right : Half_Word
            )  return Precedence;
   function Compare
            (  Left  : Half_Word;
               Right : Unbounded_Rational
            )  return Precedence;

   function Max (Left, Right : Unbounded_Rational)
      return Unbounded_Rational;
   function Min (Left, Right : Unbounded_Rational)
      return Unbounded_Rational;
--
-- Ceiling -- The smallest integer value greater or equal to Left
--
--    Left - The number
--
-- Returns :
--
--    The value
--
   function Ceiling (Left : Unbounded_Rational)
      return Unbounded_Integer;
--
-- Compose -- From sign, numerator and denominator
--
--    Numerator   - The number's numerator
--    Denominator - The number's denominator
--    Negative    - True if negative
--
-- Returns :
--
--    The corresponding unbounded integer
--
-- Exceptions :
--
--    Constraint_Error - Denominator is zero
--
   function Compose
            (  Numerator   : Unbounded_Unsigned;
               Denominator : Unbounded_Unsigned;
               Negative    : Boolean := False
            )  return Unbounded_Rational;
--
-- Copy -- Copy value
--
--    Destination - The number to copy into
--    Source      - The number to copy from
--
-- This procedure copies Source into Destination. Unlike assignment or
-- Set it does not attempt to share Source with Destination.
--
   procedure Copy
             (  Destination : in out Unbounded_Rational;
                Source      : Unbounded_Rational
             );
--
-- Error -- Iteration error test
--
--    Left, Right - Numbers to compare
--    Error       - Error's binary logarithm
--
-- Returns :
--
--    True if abs (Left - Right) > 2**Power_Of_Two
--
   function Error
            (  Left, Right : Unbounded_Rational;
               Error       : Integer
            )  return Boolean;
--
-- Floor -- The largest integer value less or equal to Left
--
--    Left - The number
--
-- Returns :
--
--    The value
--
   function Floor (Left : Unbounded_Rational) return Unbounded_Integer;
--
-- Get_Denominator -- The number's denominator
--
--    Left - The number
--
-- Returns :
--
--    The denominator
--
   function Get_Denominator (Left : Unbounded_Rational)
      return Unbounded_Unsigned;
--
-- Get_Numerator -- The number's numerator
--
--    Left - The number
--
-- Returns :
--
--    The numerator
--
   function Get_Numerator (Left : Unbounded_Rational)
      return Unbounded_Unsigned;
--
-- Greater_Than_One -- Value test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is less than -1 and greater than 1
--
   function Greater_Than_One (Left : Unbounded_Rational) return Boolean;
--
-- Invert -- 1/X
--
--    Left - The number
--
-- This procedure evaluates 1/Left.
--
-- Exceptions :
--
--    Constraint_Error - Left is zero
--
   procedure Invert (Left : in out Unbounded_Rational);
--
-- Invert -- 1/X
--
--    Left - The number
--
-- Returns :
--
--    1/Left
--
-- Exceptions :
--
--    Constraint_Error - Left is zero
--
   function Invert (Left : Unbounded_Rational)
      return Unbounded_Rational;
--
-- Is_Negative -- Test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is negative
--
   function Is_Negative (Left : Unbounded_Rational) return Boolean;
--
-- Is_One -- Test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is 1
--
   function Is_One (Left : Unbounded_Rational) return Boolean;
--
-- Is_Two -- Test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is 2
--
   function Is_Two (Left : Unbounded_Rational) return Boolean;
--
-- Is_Zero -- Zero test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is zero
--
   function Is_Zero (Left : Unbounded_Rational) return Boolean;
--
-- Less_Than_One -- Value test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is greater than -1 and less than 1
--
   function Less_Than_One (Left : Unbounded_Rational) return Boolean;
--
-- Round -- The number
--
--    Left - The number
--
-- The result is  the rounded value of Left.  The rounding  is away from
-- zero.
--
-- Returns :
--
--    The rounded value
--
   function Round (Left : Unbounded_Rational) return Unbounded_Integer;
--
-- Round -- The number
--
--    Left  - The number
--    Power - The last binary digit to round to, i.e. to 2**(-Digit)
--
-- The result is  the rounded value of Left.  The rounding  is away from
-- zero.
--
-- Returns :
--
--    The rounded value
--
   procedure Round
             (  Left  : in out Unbounded_Rational;
                Power : Natural
             );
--
-- Set -- Assignment
--
--    Destination - The target
--    Source      - The source
--
-- This variant of assignment keeps the memory allocated for Destination
-- if possible. Differently to Destination := Source  which would always
-- finalize Destination.
--
   procedure Set
             (  Destination : in out Unbounded_Rational;
                Source      : Unbounded_Rational
             );
--
-- Split -- Into whole number and fraction
--
--    Value    - The number
--    Integer  - The integer part of the number
--    Fraction - The fractional part of the number
--
-- This procedure splits Value into integer and fractional parts:
--
--    Value = Integer + Fraction,  0 <= Fraction < 1
--
-- Note that Fraction is positive even if Value is negative.
--
   procedure Split
             (  Value    : Unbounded_Rational;
                Integer  : out Unbounded_Integer;
                Fraction : out Unbounded_Rational
             );
--
-- Split -- Into whole number and fraction
--
--    Value    - The number
--    Integer  - The integer part of the number
--
-- This variant splits Value in-place.
--
   procedure Split
             (  Value   : in out Unbounded_Rational;
                Integer : out Unbounded_Integer
             );
--
-- Square -- Squared value
--
--    Left - The number
--
-- The implementation uses optimized O(N**2) algorithm.
--
-- Returns :
--
--    Left ** 2
--
   function Square (Left : Unbounded_Rational)
      return Unbounded_Rational;
--
-- Swap -- Swap numbers
--
--    Left  - The number to swap with another
--    Right - The number
--
-- This procedure swaps Left and Right
--
   procedure Swap (Left, Right : in out Unbounded_Rational);
--
-- To_Float -- Conversion to float
--
--    Left - The number
--
-- Returns :
--
--    The number
--
-- Exceptions :
--
--    Constraint_Error - The number is too large
--
   function To_Float (Left : Unbounded_Rational) return Float;
--
-- To_Integer -- Conversion to integer
--
--    Left - The number
--
-- Returns :
--
--    The number truncated
--
-- Exceptions :
--
--    Constraint_Error - The number is too large
--
   function To_Integer (Left : Unbounded_Rational) return Integer;
--
-- To_Unbounded_Integer -- Conversion to unbounded integer
--
--    Left - The number
--
-- Returns :
--
--    The number truncated
--
-- Exceptions :
--
--    Constraint_Error - The number is too large
--
   function To_Unbounded_Integer (Left : Unbounded_Rational)
      return Unbounded_Integer;
--
-- To_Unbounded_Rational -- Conversion to unbounded rational
--
--    Left - The number
--
-- Returns :
--
--    The value
--
   function To_Unbounded_Rational (Left : Float)
      return Unbounded_Rational;
--
-- To_Unbounded_Rational -- Conversion to unbounded rational
--
--    Left - The number
--
-- Returns :
--
--    The value
--
   function To_Unbounded_Rational (Left : Integer)
      return Unbounded_Rational;
--
-- To_Unbounded_Rational -- Conversion to unbounded rational
--
--    Left - The number
--
-- Returns :
--
--    The value
--
   function To_Unbounded_Rational (Left : Unbounded_Integer)
      return Unbounded_Rational;
--
-- To_Unbounded_Rational -- Conversion to unbounded rational
--
--    Left - The number
--
-- Returns :
--
--    The value
--
   function To_Unbounded_Rational (Left : Unbounded_Unsigned)
      return Unbounded_Rational;
--
-- Conversions -- Generic bounded signed numbers conversions
--
   generic
      type Number is delta <>;
   package Fixed_Point_Conversions is
      function From_Unbounded_Rational (Left : Unbounded_Rational)
         return Number;
      function To_Unbounded_Rational (Left : Number)
         return Unbounded_Rational;
   end Fixed_Point_Conversions;
--
-- Conversions -- Generic bounded unsigned numbers conversions
--
   generic
      type Number is digits <>;
   package Floating_Point_Conversions is
      function From_Unbounded_Rational (Left : Unbounded_Rational)
         return Number;
      function To_Unbounded_Rational (Left : Number)
         return Unbounded_Rational;
   end Floating_Point_Conversions;

private
   type Unbounded_Rational is record
      Sign        : Boolean            := False;
      Numerator   : Unbounded_Unsigned := Unbounded_Unsigneds.Zero;
      Denominator : Unbounded_Unsigned := Unbounded_Unsigneds.One;
   end record;

   Zero  : constant Unbounded_Rational :=
           (False, Unbounded_Unsigneds.Zero,  Unbounded_Unsigneds.One);
   Half  : constant Unbounded_Rational :=
           (False, Unbounded_Unsigneds.One,   Unbounded_Unsigneds.Two);
   One   : constant Unbounded_Rational :=
           (False, Unbounded_Unsigneds.One,   Unbounded_Unsigneds.One);
   Two   : constant Unbounded_Rational :=
           (False, Unbounded_Unsigneds.Two,   Unbounded_Unsigneds.One);
   Three : constant Unbounded_Rational :=
           (False, Unbounded_Unsigneds.Three, Unbounded_Unsigneds.One);
   Four  : constant Unbounded_Rational :=
           (False, Unbounded_Unsigneds.Four,  Unbounded_Unsigneds.One);
   Five  : constant Unbounded_Rational :=
           (False, Unbounded_Unsigneds.Five,  Unbounded_Unsigneds.One);
--
-- Log2 -- Binary logarithm upper bound (number of bits)
--
   function Log2 (Value : Unbounded_Integer ) return Integer;
   function Log2 (Value : Unbounded_Rational) return Integer;
   function Log2 (Value : Unbounded_Unsigned) return Integer;
   function "+" (N : Half_Word) return Unbounded_Rational;

   pragma Inline ("<", "<=", "=", ">=", ">");
   pragma Inline (Compare);
   pragma Inline (Greater_Than_One);
   pragma Inline (Invert);
   pragma Inline (Is_Negative);
   pragma Inline (Is_One);
   pragma Inline (Is_Two);
   pragma Inline (Is_Zero);
   pragma Inline (Less_Than_One);
   pragma Inline (Log2);

end Unbounded_Rationals;
