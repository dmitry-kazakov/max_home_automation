--                                                                    --
--  package Unbounded_Unsigneds     Copyright (c)  Dmitry A. Kazakov  --
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

with Ada.Finalization;
with Interfaces.C;
with Strings_Edit.Lexicographical_Order;
with System;

package Unbounded_Unsigneds is
   use Strings_Edit.Lexicographical_Order;

   type Unbounded_Unsigned is private;
   Zero  : constant Unbounded_Unsigned;
   One   : constant Unbounded_Unsigned;
   Two   : constant Unbounded_Unsigned;
   Three : constant Unbounded_Unsigned;
   Four  : constant Unbounded_Unsigned;
   Five  : constant Unbounded_Unsigned;

   Modulus : constant := System.Max_Binary_Modulus;
   type Word is mod Modulus;

   pragma Assert (2 ** Word'Size = Modulus);
   Half_Word_Modulus : constant := 2 ** (Word'Size / 2);
   type Half_Word is mod Half_Word_Modulus;
   pragma Assert (2 ** Half_Word'Size = Half_Word_Modulus);

   type Bit_Offset      is new Integer;
   subtype Bit_Count    is Bit_Offset range 0..Bit_Offset'Last;
   subtype Bit_Position is Bit_Offset range 1..Bit_Offset'Last;

   function "="  (Left, Right : Unbounded_Unsigned) return Boolean;
   function "<"  (Left, Right : Unbounded_Unsigned) return Boolean;
   function "<=" (Left, Right : Unbounded_Unsigned) return Boolean;
   function ">"  (Left, Right : Unbounded_Unsigned) return Boolean;
   function ">=" (Left, Right : Unbounded_Unsigned) return Boolean;

   function "+" (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned;
   function "-" (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned;
   function "*" (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned;
   function "/" (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned;

   function "mod" (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned;
   function "rem" (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned;
   function "**" (Left : Unbounded_Unsigned; Right : Bit_Count)
      return Unbounded_Unsigned;

   function Max (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned;
   function Min (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned;

   function Shift_Left (Value : Word; Amount : Natural) return Word;
   function Shift_Left (Value : Half_Word; Amount : Natural)
      return Half_Word;
   pragma Import (Intrinsic, Shift_Left);

   function Shift_Right (Value : Word; Amount : Natural) return Word;
   function Shift_Right (Value : Half_Word; Amount : Natural)
      return Half_Word;
   pragma Import (Intrinsic, Shift_Right);

   function "=" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Boolean;
   function "<" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Boolean;
   function "<=" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Boolean;
   function ">" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Boolean;
   function ">="(Left : Unbounded_Unsigned; Right : Half_Word)
      return Boolean;

   function "+" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Unbounded_Unsigned;
   function "-" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Unbounded_Unsigned;
   function "*" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Unbounded_Unsigned;
   function "/" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Unbounded_Unsigned;

   function "mod" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Half_Word;
   function "rem" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Half_Word;

   function "=" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Boolean;
   function "<" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Boolean;
   function "<=" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Boolean;
   function ">" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Boolean;
   function ">=" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Boolean;

   function "+" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Unbounded_Unsigned;
   function "-" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Half_Word;
   function "*" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Unbounded_Unsigned;
   function "/" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Half_Word;

   function "mod" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Unbounded_Unsigned;
   function "rem" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Unbounded_Unsigned;

   type Digit_Offset is new Integer;
   subtype Digit_Count is Digit_Offset range 1..Digit_Offset'Last;
--
-- Add -- Addition
--
--    Accumulator - The number to be added to
--    Increment   - The increment to add
--
   procedure Add
             (  Accumulator : in out Unbounded_Unsigned;
                Increment   : Half_Word
             );
   procedure Add
             (  Accumulator : in out Unbounded_Unsigned;
                Increment   : Unbounded_Unsigned
             );
--
-- Add -- Addition X + Y * Half_Word_Modulus ** K
--
--    Accumulator - The number to be added to
--    Increment   - The increment * Half_Word_Modulus ** Shift
--    Shift       - The Shift
--
   procedure Add
             (  Accumulator : in out Unbounded_Unsigned;
                Increment   : Unbounded_Unsigned;
                Shift       : Digit_Offset
             );
--
-- Add -- Addition X + Y * Multiplier * Half_Word_Modulus ** K
--
--    Accumulator - The number to be added to
--    Increment   - The increment
--    Multiplier  - The multiplier
--    Shift       - The Shift
--
   procedure Add
             (  Accumulator : in out Unbounded_Unsigned;
                Increment   : Unbounded_Unsigned;
                Multiplier  : Half_Word;
                Shift       : Digit_Offset
             );
--
-- Clear_Bit -- Clear bit
--
--    Left     - The number
--    Position - The position, 1 is the least significant bit
--
-- Clears bit at the position.
--
   procedure Clear_Bit
             (  Left     : in out Unbounded_Unsigned;
                Position : Bit_Position
             );
--
-- Clear_Slice -- Clear binary slice
--
--    Left  - The number
--    From  - The position where the slice starts
--    To    - The position where the slice ends
--
-- This procedure erases slice From..To in Left.
--
   procedure Clear_Slice
             (  Left : in out Unbounded_Unsigned;
                From : Bit_Position;
                To   : Bit_Position
             );
--
-- Compare -- Comparison
--
--    Left  - The number to compare
--    Right - Another number
--
-- Returns :
--
--    Less, Equal or Greater
--
   function Compare (Left, Right : Unbounded_Unsigned)
      return Precedence;
   function Compare (Left : Unbounded_Unsigned; Right : Half_Word)
      return Precedence;
   function Compare (Left : Half_Word; Right : Unbounded_Unsigned)
      return Precedence;
--
-- Complement -- 2's complement
--
--    Left   - The number
--    Length - The number of digits
--
-- This procedure computes  2's complement of Left within Length digits.
-- When Lenght is not specified it is Get_Length (Left)
--
   procedure Complement (Left : in out Unbounded_Unsigned);
   procedure Complement
             (  Left   : in out Unbounded_Unsigned;
                Length : Digit_Count
             );
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
             (  Destination : in out Unbounded_Unsigned;
                Source      : Unbounded_Unsigned
             );
--
-- Div -- Division
--
--    Dividend  - The number to be divided
--    Divisor   - The divisor
--    Remainder - The remainder
--
-- If no quotient is  needed consider  using sufficiently more effective
-- mod-operator.
--
-- Exceptions :
--
--    Constraint_Error - Zero divide
--
   procedure Div
             (  Dividend  : in out Unbounded_Unsigned;
                Divisor   : Half_Word;
                Remainder : out Half_Word
             );
   procedure Div
             (  Dividend  : in out Unbounded_Unsigned;
                Divisor   : Half_Word
             );
   procedure Div
             (  Dividend  : in out Unbounded_Unsigned;
                Divisor   : Unbounded_Unsigned;
                Remainder : out Unbounded_Unsigned
             );
   procedure Div
             (  Dividend  : in out Unbounded_Unsigned;
                Divisor   : Unbounded_Unsigned
             );
--
-- Div_By_Power_of_Two -- Division by power of 2
--
--    Dividend - The number to be divided
--    Power    - The power
--
   procedure Div_By_Power_of_Two
             (  Dividend : in out Unbounded_Unsigned;
                Power    : Bit_Count
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
            (  Dividend : Unbounded_Unsigned;
               Power    : Bit_Count
            )  return Unbounded_Unsigned;
--
-- Erase -- Set the number to zero
--
--    Left - The first number
--
-- The procedure zeroes the number without releasing memory it occupies.
--
   procedure Erase (Left : in out Unbounded_Unsigned);
--
-- From_[Half_]Word -- Conversion to unbounded unsigned
--
--    Left - The bounded unsigned
--
-- Returns :
--
--    The corresponding unbounded unsigned number
--
   function From_Half_Word (Left : Half_Word)
      return Unbounded_Unsigned;
   function From_Word (Left : Word) return Unbounded_Unsigned;
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
   function Greatest_Common_Divisor (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned;
--
-- Get_Bit -- A 2 ** (N - 1) test
--
--    Left     - The number
--    Position - The position, 1 is the least significant bit
--
-- Returns :
--
--    True is when Left / 2 ** (Position - 1) is odd
--
   function Get_Bit
            (  Left     : Unbounded_Unsigned;
               Position : Bit_Position
            )  return Boolean;
--
-- Get_Digit -- A digit (Half_Word)
--
--    Left  - The number
--    Index - 1..
--
-- The result is 0 when Index is greater than Get_Length (Left)
-- Returns :
--
--    The digit by its position (little-endian)
--
   function Get_Digit
            (  Left  : Unbounded_Unsigned;
               Index : Digit_Count
            )  return Half_Word;
--
-- Get_Length -- Number of half-words used
--
--    Left - The number
--    Used - Used or allocated digits
--
-- Returns :
--
--    The number of digits (Half_Word) used or allocated
--
   function Get_Length
            (  Left : Unbounded_Unsigned;
               Used : Boolean := True
            )  return Digit_Offset;
--
-- Get_Multiplication_Threshold -- Karatsuba threshold
--
-- The multiplication  implementation uses the classroom method when the
-- length of the arguments in half-words exceeds this threshold.
--
-- Returns :
--
--    The threshold length
--
   function Get_Multiplication_Threshold return Digit_Count;
--
-- Get_MSB -- The most significant bit
--
--    Left - The number
--
-- Returns :
--
--    The position of the most significant bit 2**(MSB-1)
--
   function Get_MSB (Left : Unbounded_Unsigned) return Bit_Count;
--
-- Get_Slice -- Binary slice
--
--    Left - The number
--    From - The position where the slice starts
--    To   - The position where the slice ends
--
-- Returns :
--
--    Bit slice corresponding to 2 ** (From - 1) .. 2 ** (To - 1)
--
   function Get_Slice
            (  Left : Unbounded_Unsigned;
               From : Bit_Position;
               To   : Bit_Position
            )  return Unbounded_Unsigned;
--
-- Get_Slice -- Binary slice
--
--    Left - The number
--    From - The position where the slice starts
--    To   - The position where the slice ends
--
--  The bit slice  corresponding to  2 ** (From - 1) .. 2 ** (To - 1) is
--  stored in Left.
--
   procedure Get_Slice
             (  Left   : in out Unbounded_Unsigned;
                From   : Bit_Position;
                To     : Bit_Position
             );
--
-- Get_Wiping_Mode -- Data wiping mode
--
-- When a number is finalized  its data are wipped if the wiping mode is
-- on.
--
-- Returns :
--
--    True if wipping data is on
--
   function Get_Wiping_Mode return Boolean;
--
-- Inverse -- Multiplicative inverse in 2 ** Half_Word_Modulus
--
--    Left - The number
--
-- Returns value I, such that I * Left = 1 (mod 2 ** Half_Word_Modulus).
--
-- Returns :
--
--    The inverse
--
-- Exceptions :
--
--    Constraint_Error - Left is even
--
   function Inverse (Left : Unbounded_Unsigned) return Half_Word;
   function Inverse (Left : Half_Word) return Half_Word;
--
-- Inverse -- Multiplicative inverse in 2 ** Modulus
--
--    Left - The number
--
-- Returns value I, such that I * Left = 1 (mod 2 ** Modulus).
--
-- Returns :
--
--    The inverse
--
-- Exceptions :
--
--    Constraint_Error - Left is even
--
   function Inverse (Left : Word) return Word;
--
-- Inverse -- Multiplicative inverse in multiple of half-words
--
--    Left   - The number
--    Count  - Number of half-words
--    Result - The inverse
--
-- The Result is I, such that I * Left = 1 (mod 2 ** (Modulus * Count).
--
-- Exceptions :
--
--    Constraint_Error - No inverse exists
--
   procedure Inverse
             (  Left   : Unbounded_Unsigned;
                Count  : Digit_Count;
                Result : out Unbounded_Unsigned
             );
--
-- Inverse -- Multiplicative inverse in multiple of half-words
--
--    Left  - The number
--    Count - Number of half-words
--
-- Returns value I, such that I * Left = 1 (mod 2 ** (Modulus * Count).
--
-- Returns :
--
--    The inverse
--
-- Exceptions :
--
--    Constraint_Error - No inverse exists
--
   function Inverse
            (  Left  : Unbounded_Unsigned;
               Count : Digit_Count
            )  return Unbounded_Unsigned;
--
-- Invert_Slice -- Invert binary slice
--
--    Left - The number
--    From - The position where the slice starts
--    To   - The position where the slice ends
--
-- This procedure inverts bits of the slice From..To in Left.
--
   procedure Invert_Slice
             (  Left : in out Unbounded_Unsigned;
                From : Bit_Position;
                To   : Bit_Position
             );
--
-- Is_Even -- Test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is even
--
   function Is_Even (Left : Unbounded_Unsigned) return Boolean;
--
-- Is_Mersenne -- Test 2 ** K - 1
--
--    Left - The number
--
-- The value of K can be obtained as Get_MSB (Left).
--
-- Returns :
--
--    True if Left is a Mersenne number 2 ** K - 1
--
   function Is_Mersenne (Left : Unbounded_Unsigned) return Boolean;
--
-- Is_Odd -- Test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is odd
--
   function Is_Odd (Left : Unbounded_Unsigned) return Boolean;
--
-- Is_One -- Test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is 1
--
   function Is_One (Left : Unbounded_Unsigned) return Boolean;
--
-- Is_Power_Of_Two -- Test 2 ** K
--
--    Left - The number
--
-- The value of K can be obtained as Get_MSB (Left) - 1
--
-- Returns :
--
--    True if Left = 2 ** K
--
   function Is_Power_Of_Two (Left : Unbounded_Unsigned) return Boolean;
--
-- Is_Proth -- Test N * 2 ** K + 1
--
--    Left - The number
--
-- A Proth number has the form N * 2 ** K + 1 such that N < 2 ** K.
--
-- Returns :
--
--    0 if Left is not a Proth number and K otherwise
--
   function Is_Proth (Left : Unbounded_Unsigned) return Bit_Count;
--
-- Is_Two -- Test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is 2
--
   function Is_Two (Left : Unbounded_Unsigned) return Boolean;
--
-- Is_Zero -- Zero test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is zero
--
   function Is_Zero (Left : Unbounded_Unsigned) return Boolean;

   subtype Mod_2_Remainder is Integer range -1..2;
--
-- Log2 -- Logarithm
--
--    Left - The number
--
-- Returns :
--
--    The result, truncated
--
-- Exceptions :
--
--    Constraint_Error - Left is zero
--
   function Log2 (Left : Half_Word) return Natural;
--
-- Log2 -- Logarithm
--
--    Left      - The number
--    Power     - The result
--    Remainder - -1, 0, 1, other
--
-- The procedure computes binary logarithm of  Left.  When the remainder
-- is in the range -1..1, the result:
--
--    2 ** Power + Remainder
--
-- When the remainder is out of range then Remainder is set to 2. It can
-- be obtained is Bit_Clear (Left, Power + 1).
--
-- Exceptions :
--
--    Constraint_Error - Left is zero
--
   procedure Log2
             (  Left      : Unbounded_Unsigned;
                Power     : out Bit_Count;
                Remainder : out Mod_2_Remainder
             );
--
-- Modulo -- Modulus
--
--    Left  - The number to be divided and the remainder
--    Count - The modulus
--
-- This  procedure  computes   Left mod Half_Word_Modulus ** Count.  The
-- result is stored in Left.
--
   procedure Modulo
             (  Left  : in out Unbounded_Unsigned;
                Count : Digit_Count
             );
   pragma Inline (Modulo);
--
-- Modulo -- Modulus
--
--    Left - The number to be divided and the remainder
--
-- Returns :
--
--    Left mod Half_Word_Modulus ** Count
--
   function Modulo
            (  Left  : Unbounded_Unsigned;
               Count : Digit_Count
            )  return Unbounded_Unsigned;
   pragma Inline (Modulo);
--
-- Modulo -- Modulus
--
--    Left  - The number to be divided and the remainder
--    Right - The modulus
--
-- This procedure  computes  Left (mod Right).  The result  is stored in
-- Left.
--
-- Exceptions :
--
--    Constraint_Error - Zero divide
--
   procedure Modulo
             (  Left  : in out Unbounded_Unsigned;
                Right : Unbounded_Unsigned
             );
--
-- Modulo_By_Power_Of_Two -- Modulus 2 ** K
--
--    Left  - The number to be divided and the remainder
--    Right - The modulus power
--
-- Returns :
--
--    Left (mod 2 ** Right)
--
   function Modulo_By_Power_Of_Two
            (  Left  : Unbounded_Unsigned;
               Right : Bit_Count
            )  return Unbounded_Unsigned;
--
-- Modulo_By_Power_Of_Two -- Modulus 2 ** K
--
--    Left  - The number to be divided and the remainder
--    Right - The modulus power
--
-- This procedure  computes  Left (mod 2 ** Right). The result is stored
-- in Left.
--
   procedure Modulo_By_Power_Of_Two
             (  Left  : in out Unbounded_Unsigned;
                Right : Bit_Count
             );
--
-- Mod_Inv -- Modular multiplicative inverse
--
--    Left    - The number to find inverse for
--    Modulus - The modulus
--
-- !!WARNING!! Left and Modulus must be coprimes
--
-- Returns :
--
--    Inverse: Result * Left = 1 (mod Modulus)
--
-- Exceptions :
--
--    Constraint_Error - No inverse exists
--
   function Mod_Inv
            (  Left    : Unbounded_Unsigned;
               Modulus : Unbounded_Unsigned
            )  return Unbounded_Unsigned;
--
-- Mod_Inv -- Modular multiplicative inverse
--
--    Left    - The number to find inverse for
--    Modulus - The modulus
--    Result  - Inverse: Result * Left = 1 (mod Modulus)
--
-- !!WARNING!! Left and Modulus must be coprimes
--
-- Exceptions :
--
--    Constraint_Error - No inverse exists
--
   procedure Mod_Inv
             (  Left    : Unbounded_Unsigned;
                Modulus : Unbounded_Unsigned;
                Result  : out Unbounded_Unsigned
             );
--
-- Mod_Inv_In_Power_Of_Two -- Modular multiplicative inverse in 2**K
--
--    Left   - The number to find inverse for
--    Power  - The modulus' power
--
-- Returns :
--
--    Inverse: Result * Left = 1 (mod 2 ** Power)
--
-- Exceptions :
--
--    Constraint_Error - No inverse exists
--
   function Mod_Inv_In_Power_Of_Two
            (  Left   : Unbounded_Unsigned;
               Power  : Bit_Position
            )  return Unbounded_Unsigned;
--
-- Mod_Inv_In_Power_Of_Two -- Modular multiplicative inverse in 2**K
--
--    Left   - The number to find inverse for
--    Power  - The modulus' power
--    Result - Inverse: Result * Left = 1 (mod 2 ** Power)
--
-- Exceptions :
--
--    Constraint_Error - No inverse exists
--
   procedure Mod_Inv_In_Power_Of_Two
             (  Left   : Unbounded_Unsigned;
                Power  : Bit_Position;
                Result : out Unbounded_Unsigned
             );
--
-- Mod_Pow -- Modular power
--
--    Left    - The number to be powered
--    Right   - The exponent
--    Modulus - The modulus
--
-- Returns :
--
--    Left ** Right mod Modulus
--
   function Mod_Pow (Left, Right, Modulus : Unbounded_Unsigned)
      return Unbounded_Unsigned;
   function Mod_Pow
            (  Left    : Unbounded_Unsigned;
               Right   : Half_Word;
               Modulus : Unbounded_Unsigned
            )  return Unbounded_Unsigned;
--
-- Mod_Pow -- Modular power
--
--    Left    - The number to be powered
--    Right   - The exponent
--    Modulus - The modulus
--    Result  - X ** Y mod M
--
   procedure Mod_Pow
             (  Left, Right, Modulus : Unbounded_Unsigned;
                Result               : out Unbounded_Unsigned
             );
   procedure Mod_Pow
             (  Left    : Unbounded_Unsigned;
                Right   : Half_Word;
                Modulus : Unbounded_Unsigned;
                Result  : out Unbounded_Unsigned
            ) ;
--
-- Mod_Pow_By_Power_Of_Two -- Modular power
--
--    Left    - The number to be powered
--    Right   - The power
--    Modulus - The modulus' power
--
-- Returns :
--
--    X ** Y mod 2 ** M
--
   function Mod_Pow_By_Power_Of_Two
            (  Left, Right : Unbounded_Unsigned;
               Modulus     : Bit_Count
            )  return Unbounded_Unsigned;
   function Mod_Pow_By_Power_Of_Two
            (  Left    : Unbounded_Unsigned;
               Right   : Half_Word;
               Modulus : Bit_Count
            )  return Unbounded_Unsigned;
--
-- Mod_Pow_By_Power_Of_Two -- Modular power
--
--    Left    - The number to be powered
--    Right   - The exponent
--    Modulus - The modulus
--    Result  - Left ** Right mod Modulus
--
   procedure Mod_Pow_By_Power_Of_Two
             (  Left, Right : Unbounded_Unsigned;
                Modulus     : Bit_Count;
                Result      : out Unbounded_Unsigned
             );
   procedure Mod_Pow_By_Power_Of_Two
             (  Left    : Unbounded_Unsigned;
                Right   : Half_Word;
                Modulus : Bit_Count;
                Result  : out Unbounded_Unsigned
             );
--
-- Mod_Pow_Of_Two -- 2 ** N mod M
--
--    Power   - The power
--    Modulus - The modulus
--    Result  - The result
--
-- The procedure is  optimized for powers  of two and mudulus having the
-- form 2 ** M + K or 2 ** M - K.
--
--    2 ** M = (2 ** M + K) - K = -K (mod Modulus)
--    2 ** M = (2 ** M - K) + K =  K (mod Modulus)
--
-- So
--
--    2 ** N = Sum Ai * (2 ** M) ** i =
--              i
--           = Al * (2 ** M) ** l
--
-- Where l = N / M and
--      Al = 2 ** N / 2 ** (N / M) = 2 ** (N - N / M) =
--         = 2 ** (N mod M)
--
--    2 ** N = 2 ** (N mod M) * (2 ** M) ** l =
--           = 2 ** (N mod M) * K ** l (mod Modulus)
--
   function Mod_Pow_Of_Two
            (  Power   : Unbounded_Unsigned;
               Modulus : Unbounded_Unsigned
            )  return Unbounded_Unsigned;
   procedure Mod_Pow_Of_Two
             (  Power   : Unbounded_Unsigned;
                Modulus : Unbounded_Unsigned;
                Result  : out Unbounded_Unsigned
             );
--
-- Mul -- Multiplication
--
--    Multiplicand - The number to be multiplied
--    Multiplier   - The multiplier
--
   procedure Mul
             (  Multiplicand : in out Unbounded_Unsigned;
                Multiplier   : Half_Word
             );
   procedure Mul
             (  Multiplicand : in out Unbounded_Unsigned;
                Multiplier   : Unbounded_Unsigned
             );
--
-- Mul -- Multiplication
--
--    Multiplicand - The number to be multiplied
--    Multiplier   - The multiplier
--    Result       - The result Multiplicand * Multiplier
--
   procedure Mul
             (  Multiplicand : Unbounded_Unsigned;
                Multiplier   : Half_Word;
                Result       : out Unbounded_Unsigned
             );
   procedure Mul
             (  Multiplicand : Unbounded_Unsigned;
                Multiplier   : Unbounded_Unsigned;
                Result       : out Unbounded_Unsigned
             );
--
-- Mul_By_Power_of_Two -- Multiplication by power of 2
--
--    Multiplicand - The number to be multiplied
--    Power        - The power
--
   procedure Mul_By_Power_Of_Two
             (  Multiplicand : in out Unbounded_Unsigned;
                Power        : Bit_Count
             );
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
   function Mul_By_Power_Of_Two
            (  Multiplicand : Unbounded_Unsigned;
               Power        : Bit_Count
            )  return Unbounded_Unsigned;
--
-- Mul_Classroom -- Multiplication O(N**2)
--
--    Left  - Non-zero multiplied
--    Right - Non-zero multiplier
--
-- Returns :
--
--    Left * Right
--
   function Mul_Classroom (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned;
--
-- Mul_Classroom -- Multiplication O(N**2)
--
--    Left   - Non-zero multiplied
--    Right  - Non-zero multiplier
--    Result - The result (Left * Right)
--
   procedure Mul_Classroom
             (  Left, Right : Unbounded_Unsigned;
                Result      : out Unbounded_Unsigned
             );
--
-- Mul_Karatsuba -- Multiplication Karatsuba algorithm
--
--    Left      - The number to be multiplied
--    Right     - The multiplier
--    Threshold - The length when to switch back to classroom method
--
-- The algorithm is asymptotically  faster than  the standard  classroom
-- multiplication.  It recursively split  multiplicands  into halves:
--
--             N                    N
--    |   H1   |   L1  | x |   H2   |   L2   | =
--    |________|_______|   |________|________|
--
--    = Z0 + Z1 * 2**N + Z2 * 2**(N+1)
--
--      Z0 = L1 * L2
--      Z1 = (L1 + H1) * (L2 + H2) - Z0 - Z2
--      Z2 = H1 * H2
--
   function Mul_Karatsuba
            (  Left, Right : Unbounded_Unsigned;
               Threshold   : Digit_Count
            )  return Unbounded_Unsigned;
--
-- Mul_Karatsuba -- Multiplication Karatsuba algorithm
--
--    Left      - The number to be multiplied
--    Right     - The multiplier
--    Threshold - The length when to switch back to classroom method
--    Result    - Left * Right
--
   procedure Mul_Karatsuba
             (  Left, Right : Unbounded_Unsigned;
                Threshold   : Digit_Count;
                Result      : out Unbounded_Unsigned
             );
--
-- Phi -- Euler's totient function
--
--    Left - The number
--
-- Returns :
--
--    The totient
--
-- Exceptions :
--
--    Constraint_Error - Left is zero
--
   function Phi (Left : Unbounded_Unsigned) return Unbounded_Unsigned;
--
-- Power_of_Two -- 2 ** Power
--
--    Power  - The power
--    Result - 2 ** Power
--
   procedure Power_of_Two
             (  Power  : Bit_Count;
                Result : out Unbounded_Unsigned
             );
--
-- Power_of_Two -- 2 ** Power
--
--    Power - The power
--
-- Returns :
--
--    2 ** Power
--
   function Power_of_Two (Power : Bit_Count) return Unbounded_Unsigned;
--
-- Replace_Slice -- Replace binary slice
--
--    Left  - The number
--    Right - The number containing replacement bits
--    From  - The position where the slice starts
--    To    - The position where the slice ends
--
-- This procedure replaces slice  From..To in Left with bits from Right.
-- If Right has not enough bits 0's are used.
--
   procedure Replace_Slice
             (  Left  : in out Unbounded_Unsigned;
                Right : Unbounded_Unsigned;
                From  : Bit_Position;
                To    : Bit_Position
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
             (  Destination : in out Unbounded_Unsigned;
                Source      : Unbounded_Unsigned
             );
   procedure Set
             (  Destination : in out Unbounded_Unsigned;
                Source      : Half_Word
             );
   procedure Set_Word
             (  Destination : in out Unbounded_Unsigned;
                Source      : Word
             );
--
-- Set_Bit -- Set bit
--
--    Left     - The number
--    Position - The position, 1 is the least significant bit
--
-- Sets bit at the position.
--
   procedure Set_Bit
             (  Left     : in out Unbounded_Unsigned;
                Position : Bit_Position
             );
--
-- Set_Bits_Count -- The number of set bits
--
--    Left - The number
--
-- Returns :
--
--    The number of bits set in Left
--
   function Set_Bits_Count (Left : Unbounded_Unsigned) return Bit_Count;
   function Set_Bits_Count (Left : Half_Word)          return Bit_Count;
--
-- Set_Multiplication_Threshold -- Karatsuba threshold
--
--    Threshold - The threshold length
--
   procedure Set_Multiplication_Threshold (Threshold : Digit_Count);
--
-- Set_Slice -- Clear binary slice
--
--    Left  - The number
--    From  - The position where the slice starts
--    To    - The position where the slice ends
--
-- This procedure sets bits in the slice From..To in Left.
--
   procedure Set_Slice
             (  Left : in out Unbounded_Unsigned;
                From : Bit_Position;
                To   : Bit_Position
             );
--
-- Set_Wiping_Mode -- Set data wipping mode
--
--    On - The mode
--
-- If the wiping mode is  on the memory  used by a number  is wiped upon
-- deallocated.
--
   procedure Set_Wiping_Mode (On : Boolean);
--
-- Shift_Left -- Shift by words
--
--    Left  - The number
--    Shift - By modulus of Half_Word
--
   procedure Shift_Left
             (  Left  : in out Unbounded_Unsigned;
                Shift : Digit_Offset
             );
--
-- Shift_Right -- Shift by words
--
--    Left  - The number
--    Shift - By modulus of Half_Word
--
   procedure Shift_Right
             (  Left  : in out Unbounded_Unsigned;
                Shift : Digit_Offset
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
   function Sqrt (Left : Unbounded_Unsigned) return Unbounded_Unsigned;
   function Sqrt (Left : Half_Word) return Half_Word;
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
   procedure Sqrt
             (  Left      : Unbounded_Unsigned;
                Root      : out Unbounded_Unsigned;
                Remainder : out Unbounded_Unsigned
             );
   procedure Sqrt
             (  Left      : Half_Word;
                Root      : out Half_Word;
                Remainder : out Half_Word
             );
--
-- Square -- Squared value
--
--    Left - The number
--
-- The implementation  uses  either  classroom  or  Karatsuba  algorithm
-- depending on the value set by Set_Multiplication_Threshold.
--
-- Returns :
--
--    Left ** 2
--
   function Square (Left : Unbounded_Unsigned)
      return Unbounded_Unsigned;
--
-- Square -- Squared value
--
--    Left   - The number
--    Result - Left ** 2
--
-- The implementation  uses  either  classroom  or  Karatsuba  algorithm
-- depending on the value set by Set_Multiplication_Threshold.
--
   procedure Square
             (  Left   : Unbounded_Unsigned;
                Result : out Unbounded_Unsigned
             );
--
-- Square_Classroom -- Squared value
--
--    Left - The number
--
-- The implementation uses optimized O(N**2) algorithm.
--
-- Returns :
--
--    Left ** 2
--
   function Square_Classroom (Left : Unbounded_Unsigned)
      return Unbounded_Unsigned;
--
-- Square_Classroom -- Squared value
--
--    Left   - The number
--    Result - The result Left ** 2
--
   procedure Square_Classroom
             (  Left   : Unbounded_Unsigned;
                Result : out Unbounded_Unsigned
             );
--
-- Square_Karatsuba -- Karatsuba algorithm for suaring
--
--    Left      - The multiplicand
--    Threshold - The length when to switch back to classroom method
--
-- The algorithm is asymptotically  faster than  the standard  classroom
-- method. It recursively split the multiplicand into halves:
--
--              N
--    |    H    |    L    | ** 2 =
--    |_________|_________|
--
--    = Z0 + Z1 * 2**N + Z2 * 2**(N+1)
--
--      Z0 = L ** 2
--      Z1 = (L + H) ** 2 - Z0 - Z2
--      Z2 = H ** 2
--
   function Square_Karatsuba
            (  Left      : Unbounded_Unsigned;
               Threshold : Digit_Count
            )  return Unbounded_Unsigned;
--
-- Square_Karatsuba -- Karatsuba algorithm for suaring
--
--    Left      - The multiplicand
--    Threshold - The length when to switch back to classroom method
--    Result    - Left ** 2
--
   procedure Square_Karatsuba
             (  Left      : Unbounded_Unsigned;
                Threshold : Digit_Count;
                Result    : out Unbounded_Unsigned
             );
--
-- Sub -- Subtraction
--
--    Minuend    - The number to subract from
--    Subtrahend - The decrement to subtract
--
-- Exceptions :
--
--    Constraint_Error - Minuend < Subtrahend
--
   procedure Sub
             (  Minuend    : in out Unbounded_Unsigned;
                Subtrahend : Half_Word
             );
   procedure Sub
             (  Minuend    : in out Unbounded_Unsigned;
                Subtrahend : Unbounded_Unsigned
             );
--
-- Sub -- Subtraction
--
--    Minuend    - The number to subract from
--    Subtrahend - The decrement to subtract shifted by Shift
--    Shift      - The power
--
-- This  procedure  substracts   Subtrahend * Half_Word_Modulus ** Shift
-- from Minuend.
--
-- Exceptions :
--
--    Constraint_Error - Minuend < Subtrahend
--
   procedure Sub
             (  Minuend    : in out Unbounded_Unsigned;
                Subtrahend : Unbounded_Unsigned;
                Shift      : Digit_Offset
             );
--
-- Sub -- Subtraction
--
--    Minuend    - The number to subract from
--    Subtrahend - The decrement to multiply and subtract shifted
--    Multiplier - The multiplier
--    Shift      - The power
--
-- This procedure substracts
--
--    Subtrahend * Multiplier * Half_Word_Modulus ** Shift
--
-- from Minuend.
--
-- Exceptions :
--
--    Constraint_Error - Minuend < Subtrahend
--
   procedure Sub
             (  Minuend    : in out Unbounded_Unsigned;
                Subtrahend : Unbounded_Unsigned;
                Multiplier : Half_Word;
                Shift      : Digit_Offset
             );
--
-- Sub_2 -- Subtraction
--
--    Minuend    - The number to subract from
--    Subtrahend - The decrement to subtract and the result
--
-- This variant stores the result in Subtrahend
--
-- Exceptions :
--
--    Constraint_Error - Minuend < Subtrahend
--
   procedure Sub_2
             (  Minuend    : Unbounded_Unsigned;
                Subtrahend : in out Unbounded_Unsigned
             );
--
-- Sub_From_Power_Of_Two -- Subtraction
--
--    Power      - 2 ** Power to subract from
--    Subtrahend - The decrement to subtract and the result
--
-- 2 ** Power - Subtrahend is stored in Subtrahend
--
-- Exceptions :
--
--    Constraint_Error - 2 ** Power < Subtrahend
--
   procedure Sub_From_Power_Of_Two
             (  Power      : Bit_Position;
                Subtrahend : in out Unbounded_Unsigned
             );
--
-- Sub_From_Power_Of_Half_Word -- Subtraction
--
--    Count      - The number of half-words
--    Subtrahend - The decrement to subtract and the result
--
-- Half_Word_Modulus * Count - Subtrahend is stored in Subtrahend
--
-- Exceptions :
--
--    Constraint_Error - Half_Word_Modulus * Count < Subtrahend
--
   procedure Sub_From_Power_Of_Half_Word
             (  Count      : Digit_Count;
                Subtrahend : in out Unbounded_Unsigned
             );
--
-- Swap -- Swap numbers
--
--    Left  - The number to swap with another
--    Right - The number
--
-- This procedure swaps Left and Right
--
   procedure Swap (Left, Right : in out Unbounded_Unsigned);
--
-- To_[Half_]Word -- Conversion to bounded unsigned
--
--    Left - The unbounded unsigned
--
-- Returns :
--
--    The corresponding bounded unsigned number
--
-- Exceptions :
--
--    Constraint_Error - Out of range
--
   function To_Half_Word (Left : Unbounded_Unsigned) return Half_Word;
   function To_Word      (Left : Unbounded_Unsigned) return Word;
--
-- Truncate -- Division by power of 2 without remainder
--
--    Left    - The number to be divided
--  [ Power ] - The power of two by which Left was divided
--
   procedure Truncate (Left : in out Unbounded_Unsigned);
   procedure Truncate
             (  Left  : in out Unbounded_Unsigned;
                Power : out Bit_Count
             );
--
-- Generic_Replace_Slice -- Replace binary slice
--
--    Left  - The number
--    Right - The number containing replacement bits
--    From  - The position where the slice starts
--    To    - The position where the slice ends
--
-- This procedure  replaces slice  From..To  in Left with  bits combined
-- by  the function Operation  with the bits  from Right.  The operation
-- most be bit position independent.
--
   generic
      with function Operation (Left, Right : Half_Word)
               return Half_Word;
   procedure Generic_Replace_Slice
             (  Left      : in out Unbounded_Unsigned;
                Right     : Unbounded_Unsigned;
                From      : Bit_Position;
                To        : Bit_Position
             );
--
-- Conversions -- Generic bounded unsigned numbers conversions
--
   generic
      type Number is mod <>;
   package Unsigned_Conversions is
      function From_Unbounded_Unsigned (Left : Unbounded_Unsigned)
         return Number;
      function To_Unbounded_Unsigned (Left : Number)
         return Unbounded_Unsigned;
   end Unsigned_Conversions;

   procedure Dump_Hex
             (  X      : Unbounded_Unsigned;
                Prefix : String := "";
                Join   : Boolean := False
             );
   procedure Dump (X : Unbounded_Unsigned; Prefix : String := "");
--
-- Get_Use_Count -- The number's vector reference count
--
--    Object - The object
--
-- Returns :
--
--    The reference count
--
   function Get_Use_Count (Object : Unbounded_Unsigned) return Natural;

private
   Minimal_Size   : constant := 64;
   Increment      : constant := 30;
   Half_Word_Mask : constant := Half_Word_Modulus - 1;
   Bit_Width      : constant := Half_Word'Size;

   type Count_Type is mod 2**32;
   for Count_Type'Size use 32;
   type Half_Word_Array is array (Digit_Count range <>) of Half_Word;
   type Vector (Size : Digit_Count) is record
      Count : aliased Count_Type := 1;
      Data  : Half_Word_Array (1..Size);
   end record;
   type Vector_Ptr is access Vector;

   procedure Dump_Hex
             (  X      : Half_Word_Array;
                Prefix : String  := "";
                Join   : Boolean := False
             );
   function Image (Value : Half_Word) return String;
   function Image (Value : Word) return String;

   type Unbounded_Unsigned is
      new Ada.Finalization.Controlled with
   record
      Length : Digit_Offset := 0;
      Value  : Vector_Ptr;
   end record;

   procedure Add
             (  Accumulator : in out Unbounded_Unsigned;
                Increment   : Half_Word_Array
             );
   procedure Adjust (Object : in out Unbounded_Unsigned);
   procedure Allocate
             (  Container : in out Vector_Ptr;
                Size       : Digit_Count
             );
   procedure Finalize (Object : in out Unbounded_Unsigned);
   procedure Clone
             (  Container : in out Vector_Ptr;
                Length    : Digit_Count
             );
   function Compare
            (  Left       : Unbounded_Unsigned;
               Right      : Half_Word_Array;
               Multiplier : Half_Word;
               Shift      : Digit_Offset
            )  return Precedence;
   function Compare
            (  Left  : Half_Word_Array;
               Right : Half_Word_Array
            )  return Precedence;
   procedure Mul_Classroom
             (  Left   : Half_Word_Array;
                Right  : Half_Word_Array;
                Result : out Unbounded_Unsigned
             );
   procedure Put
             (  Container : in out Vector_Ptr;
                Index     : Digit_Count;
                Element   : Half_Word
             );
   procedure Release (Container : in out Vector_Ptr);
   procedure Set
             (  Destination : in out Unbounded_Unsigned;
                Source      : Half_Word_Array
             );
   procedure Sqrt
             (  Left      : Word;
                Root      : out Word;
                Remainder : out Word
             );
   procedure Square_Classroom
             (  Left   : Half_Word_Array;
                Result : out Unbounded_Unsigned
             );
   procedure Sub
             (  Minuend    : in out Unbounded_Unsigned;
                Subtrahend : Half_Word_Array;
                Borrow     : out Half_Word
             );
   procedure Sub
             (  Minuend    : in out Unbounded_Unsigned;
                Subtrahend : Half_Word_Array;
                Shift      : Digit_Offset
             );
   procedure Sub
             (  Minuend    : in out Unbounded_Unsigned;
                Subtrahend : Half_Word_Array;
                Multiplier : Half_Word;
                Shift      : Digit_Offset;
                Borrow     : out Half_Word
             );
   procedure Sub_2
             (  Minuend    : Half_Word_Array;
                Subtrahend : in out Unbounded_Unsigned
             );

   Zero  : constant Unbounded_Unsigned :=
                    (  Ada.Finalization.Controlled
                    with
                       Length => 0,
                       Value  => null
                    );
   One   : constant Unbounded_Unsigned :=
                    (  Ada.Finalization.Controlled
                    with
                       Length => 1,
                       Value  => new Vector'(1, 1, (1..1 => 1))
                    );
   Two   : constant Unbounded_Unsigned :=
                    (  Ada.Finalization.Controlled
                    with
                       Length => 1,
                       Value  => new Vector'(1, 1, (1..1 => 2))
                    );
   Three : constant Unbounded_Unsigned :=
                    (  Ada.Finalization.Controlled
                    with
                       Length => 1,
                       Value  => new Vector'(1, 1, (1..1 => 3))
                    );
   Four  : constant Unbounded_Unsigned :=
                    (  Ada.Finalization.Controlled
                    with
                       Length => 1,
                       Value  => new Vector'(1, 1, (1..1 => 4))
                    );
   Five  : constant Unbounded_Unsigned :=
                    (  Ada.Finalization.Controlled
                    with
                       Length => 1,
                       Value  => new Vector'(1, 1, (1..1 => 5))
                    );

   pragma Inline ("<", "<=", "=", ">=", ">");
   pragma Inline (Allocate);
   pragma Inline (Clone);
   pragma Inline (Compare);
   pragma Inline (Erase);
   pragma Inline (Is_Even);
   pragma Inline (Is_Odd);
   pragma Inline (Is_One);
   pragma Inline (Is_Two);
   pragma Inline (Is_Zero);
   pragma Inline (Put);
   pragma Inline (Release);
   pragma Inline (Set);

   Mul_Threshold : Digit_Count := 500;
   pragma Atomic (Mul_Threshold);
--
-- Minimal signed arithmetc for algorithms requiring negative numbers
--
   type Signed is record
      Sign  : Boolean := False;
      Value : Unbounded_Unsigned;
   end record;
   procedure Dump_Hex (X : Signed; Prefix : String := "");
   procedure Add (X : in out Signed; Y : Signed);
   procedure Mul (X : in out Signed; Y : Signed);
   procedure Mul (X, Y : Signed; Result : out Signed);
   procedure Mul_By_Power_Of_Two (X : in out Signed; Power : Bit_Count);
   procedure Neg (X : in out Signed);
   procedure Set (X : in out Signed; Y : Signed);
   procedure Set (X : in out Signed; Y : Half_Word);
   procedure Square (X : Signed; Result : out Signed);
   procedure Sub (X : in out Signed; Y : Signed);
   procedure Sub (X : in out Signed; Y : Half_Word);
   procedure Swap (X, Y : in out Signed);

   function Inc
            (  Ptr      : access Count_Type;
               Val      : Count_Type       := 1;
               Memorder : Interfaces.C.int := 0
            )  return Count_Type;
   pragma Import (Intrinsic, Inc, "__atomic_add_fetch_4");

   function Dec
            (  Ptr      : access Count_Type;
               Val      : Count_Type       := 1;
               Memorder : Interfaces.C.int := 0
            )  return Count_Type;
   pragma Import (Intrinsic, Dec, "__atomic_sub_fetch_4");

   function Load
            (  Ptr      : access Count_Type;
               Memorder : Interfaces.C.int := 0
            )  return Count_Type;
   pragma Import (Intrinsic, Load, "__atomic_load_4");

end Unbounded_Unsigneds;
