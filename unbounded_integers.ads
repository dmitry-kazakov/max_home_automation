--                                                                    --
--  package Unbounded_Integers      Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2024       --
--                                                                    --
--                                Last revision :  17:48 17 Jun 2025  --
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

with Strings_Edit.Lexicographical_Order;
with Unbounded_Unsigneds;  use Unbounded_Unsigneds;

package Unbounded_Integers is
   use Strings_Edit.Lexicographical_Order;

   type Unbounded_Integer is private;
   Zero  : constant Unbounded_Integer;
   One   : constant Unbounded_Integer;
   Two   : constant Unbounded_Integer;
   Three : constant Unbounded_Integer;
   Four  : constant Unbounded_Integer;
   Five  : constant Unbounded_Integer;

   function "=" (Left, Right : Unbounded_Integer) return Boolean;
   function "=" (Left : Unbounded_Integer; Right : Integer)
      return Boolean;
   function "=" (Left : Integer; Right : Unbounded_Integer)
      return Boolean;
   function "<" (Left, Right : Unbounded_Integer) return Boolean;
   function "<" (Left : Unbounded_Integer; Right : Integer)
      return Boolean;
   function "<" (Left : Integer; Right : Unbounded_Integer)
      return Boolean;
   function "<=" (Left, Right : Unbounded_Integer) return Boolean;
   function "<=" (Left : Unbounded_Integer; Right : Integer)
      return Boolean;
   function "<=" (Left : Integer; Right : Unbounded_Integer)
      return Boolean;
   function ">" (Left, Right : Unbounded_Integer) return Boolean;
   function ">" (Left : Unbounded_Integer; Right : Integer)
      return Boolean;
   function ">" (Left : Integer; Right : Unbounded_Integer)
      return Boolean;
   function ">=" (Left, Right : Unbounded_Integer) return Boolean;
   function ">=" (Left : Unbounded_Integer; Right : Integer)
      return Boolean;
   function ">=" (Left : Integer; Right : Unbounded_Integer)
      return Boolean;

   function "abs" (Left : Unbounded_Integer) return Unbounded_Integer;

   function "+" (Left : Unbounded_Integer) return Unbounded_Integer;
   function "-" (Left : Unbounded_Integer) return Unbounded_Integer;

   function "+" (Left, Right : Unbounded_Integer)
      return Unbounded_Integer;
   function "+" (Left : Unbounded_Integer; Right : Integer)
      return Unbounded_Integer;
   function "+" (Left : Integer; Right : Unbounded_Integer)
      return Unbounded_Integer;
   function "-" (Left, Right : Unbounded_Integer)
      return Unbounded_Integer;
   function "-" (Left : Unbounded_Integer; Right : Integer)
      return Unbounded_Integer;
   function "-" (Left : Integer; Right : Unbounded_Integer)
      return Unbounded_Integer;
   function "*" (Left, Right : Unbounded_Integer)
      return Unbounded_Integer;
   function "*" (Left : Unbounded_Integer; Right : Integer)
      return Unbounded_Integer;
   function "*" (Left : Integer; Right : Unbounded_Integer)
      return Unbounded_Integer;
   function "/" (Left, Right : Unbounded_Integer)
      return Unbounded_Integer;
   function "/" (Left : Unbounded_Integer; Right : Integer)
      return Unbounded_Integer;
   function "/" (Left : Integer; Right : Unbounded_Integer)
      return Unbounded_Integer;

   function "mod" (Left, Right : Unbounded_Integer)
      return Unbounded_Integer;
   function "mod" (Left : Unbounded_Integer; Right : Integer)
      return Unbounded_Integer;
   function "mod" (Left : Integer; Right : Unbounded_Integer)
      return Unbounded_Integer;
   function "rem" (Left, Right : Unbounded_Integer)
      return Unbounded_Integer;
   function "rem" (Left : Unbounded_Integer; Right : Integer)
      return Unbounded_Integer;
   function "rem" (Left : Integer; Right : Unbounded_Integer)
      return Unbounded_Integer;
   function "**" (Left : Unbounded_Integer; Right : Bit_Count)
      return Unbounded_Integer;

   function Compare (Left, Right : Unbounded_Integer) return Precedence;
   function Compare (Left : Unbounded_Integer; Right : Integer)
      return Precedence;
   function Compare (Left : Integer; Right : Unbounded_Integer)
      return Precedence;

   function Max (Left, Right : Unbounded_Integer)
      return Unbounded_Integer;
   function Min (Left, Right : Unbounded_Integer)
      return Unbounded_Integer;
--
-- Compose -- From sign and mantissa
--
--    Mantissa - The number's mantissa
--    Negative - True if negative
--
-- Returns :
--
--    The corresponding unbounded integer
--
   function Compose
            (  Mantissa : Unbounded_Unsigned;
               Negative : Boolean := False
            )  return Unbounded_Integer;
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
             (  Destination : in out Unbounded_Integer;
                Source      : Unbounded_Integer
             );
--
-- Div_By_Power_of_Two -- Division by power of 2
--
--    Dividend - The number to be divided
--    Power    - The power
--
-- Returns :
--
--    Dividend / 2 ** Power
--
   function Div_By_Power_of_Two
            (  Dividend : Unbounded_Integer;
               Power    : Bit_Count
            )  return Unbounded_Integer;
--
-- Erase -- Set the number to zero
--
--    Left - The first number
--
-- The procedure zeroes the number without releasing memory it occupies.
--
   procedure Erase (Left : in out Unbounded_Integer);
--
-- Greatest_Common_Divisor -- The greatest common divisor
--
--    Left  - The first number
--    Right - The second
--
-- The implementation uses the Euclidean algorithm.
--
-- Returns :
--
--    The greates common divisor of Left and Right
--
   function Greatest_Common_Divisor
            (  Left, Right : Unbounded_Integer
            )  return Unbounded_Integer;
--
-- Get_Mantissa -- Get absolute value
--
--    Left - The number
--
-- Returns :
--
--    The absolute value as an unbounded unsigned
--
   function Get_Mantissa (Left : Unbounded_Integer)
      return Unbounded_Unsigned;
--
-- Is_Even -- Test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is even
--
   function Is_Even (Left : Unbounded_Integer) return Boolean;
--
-- Is_Odd -- Test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is odd
--
   function Is_Odd (Left : Unbounded_Integer) return Boolean;
--
-- Is_Negative -- Test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is negative
--
   function Is_Negative (Left : Unbounded_Integer) return Boolean;
--
-- Is_One -- Test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is 1
--
   function Is_One (Left : Unbounded_Integer) return Boolean;
--
-- Is_Two -- Test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is 2
--
   function Is_Two (Left : Unbounded_Integer) return Boolean;
--
-- Is_Zero -- Zero test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is zero
--
   function Is_Zero (Left : Unbounded_Integer) return Boolean;
--
-- Mul_By_Power_of_Two -- Multiplication by power of 2
--
--    Multiplicand - The number to be multiplied
--    Power        - The power
--
-- Returns :
--
--    Multiplicand * 2 ** Power
--
   function Mul_By_Power_of_Two
            (  Multiplicand : Unbounded_Integer;
               Power        : Bit_Count
            )  return Unbounded_Integer;
--
-- Power_of_Two -- 2 ** Power
--
--    Power - The power
--
-- Returns :
--
--    2 ** Power
--
   function Power_of_Two (Power : Bit_Count) return Unbounded_Integer;
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
             (  Destination : in out Unbounded_Integer;
                Source      : Unbounded_Integer
             );
--
-- Sqrt -- Square root
--
--    Left - The number
--
-- Returns :
--
--    The greatest value such that Left >= Root ** 2
--
-- Exceptions :
--
--    Constraint_Error - Left is negative
--
   function Sqrt (Left : Unbounded_Integer) return Unbounded_Integer;
--
-- Sqrt -- Square root
--
--    Left      - The number
--    Root      - The result
--    Remainder - The remainder
--
-- The procedure calculates  square root  and remainder  using Karatsuba
-- algorithm.
--
--    Left = Root ** 2 + Remainder
--
-- Exceptions :
--
--    Constraint_Error - Left is negative
--
   procedure Sqrt
             (  Left      : Unbounded_Integer;
                Root      : out Unbounded_Integer;
                Remainder : out Unbounded_Integer
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
   function Square (Left : Unbounded_Integer)
      return Unbounded_Integer;
--
-- Swap -- Swap numbers
--
--    Left  - The number to swap with another
--    Right - The number
--
-- This procedure swaps Left and Right
--
   procedure Swap (Left, Right : in out Unbounded_Integer);
--
-- To_Integer -- Conversion to bounded integer
--
--    Left - The unbounded integer
--
-- Returns :
--
--    The corresponding bounded number
--
-- Exceptions :
--
--    Constraint_Error - Out of range
--
   function To_Integer (Left : Unbounded_Integer) return Integer;
--
-- To_Unbounded_Integer -- Conversion to unbounded number
--
--    Left - The bounded number
--
-- Returns :
--
--    The corresponding unbounded number
--
   function To_Unbounded_Integer (Left : Integer)
      return Unbounded_Integer;
--
-- Conversions -- Generic bounded signed numbers conversions
--
   generic
      type Number is range <>;
   package Signed_Conversions is
      pragma Assert (Number'Last < Modulus);

      function From_Unbounded_Integer (Left : Unbounded_Integer)
         return Number;
      function To_Unbounded_Integer (Left : Number)
         return Unbounded_Integer;
   end Signed_Conversions;
--
-- Conversions -- Generic bounded unsigned numbers conversions
--
   generic
      type Number is mod <>;
   package Unsigned_Conversions is
      function From_Unbounded_Integer (Left : Unbounded_Integer)
         return Number;
      function To_Unbounded_Integer (Left : Number)
         return Unbounded_Integer;
   end Unsigned_Conversions;

private
   type Unbounded_Integer is record
      Sign     : Boolean := False;
      Mantissa : Unbounded_Unsigned;
   end record;

   Zero  : constant Unbounded_Integer :=
              (False, Unbounded_Unsigneds.Zero);
   One   : constant Unbounded_Integer :=
              (False, Unbounded_Unsigneds.One);
   Two   : constant Unbounded_Integer :=
              (False, Unbounded_Unsigneds.Two);
   Three : constant Unbounded_Integer :=
              (False, Unbounded_Unsigneds.Three);
   Four  : constant Unbounded_Integer :=
              (False, Unbounded_Unsigneds.Four);
   Five  : constant Unbounded_Integer :=
              (False, Unbounded_Unsigneds.Five);

   pragma Inline ("<", "<=", "=", ">=", ">");
   pragma Inline (Compare);
   pragma Inline (Erase);
   pragma Inline (Is_Even);
   pragma Inline (Is_Negative);
   pragma Inline (Is_Odd);
   pragma Inline (Is_One);
   pragma Inline (Is_Two);
   pragma Inline (Is_Zero);

end Unbounded_Integers;
