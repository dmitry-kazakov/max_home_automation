--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Unbounded_Unsigneds.Montgomery              Luebeck            --
--  Interface                                      Winter, 2024       --
--                                                                    --
--                                Last revision :  10:32 12 Jul 2025  --
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

package Unbounded_Unsigneds.Montgomery is
--
-- Multiplication_Algorithm
--
--    CIOS          - Coarsely Integrated Operand Scanning
--    Dusse_Kaliski - Dusse-Kaliski
--
   type Multiplication_Algorithm is (CIOS, Dusse_Kaliski);
--
-- Montgomery_Number -- Montgomery domain number
--
   type Montgomery_Number is private;
--
-- Montgomery_Domain -- Montgomery domain
--
--    Modulus         - The modulus (N)
--    Reducer         - R = Half_Word_Modulus ** Half_Words mod Modulus
--    Squared_Reducer - R * R mod Modulus
--    Modulus_Inverse - N': N * N' =-1 (mod 2 ** Half_Word_Modulus)
--    Half_Words      - Number half-words needed to contain Modulus
--    Bits            - Half_Words multiplied by Bit_Width
--
-- The  negative   multiplicative  inverse  of  N: -1/N (mod R)  can  be
-- calculated as R - N':
--
--    N * (R - N') = N * R - N * N' = -1 (mod R)
--
   type Montgomery_Domain is record
      Modulus         : Unbounded_Unsigned;
      Reducer         : Unbounded_Unsigned;
      Squared_Reducer : Unbounded_Unsigned;
      Modulus_Inverse : Unbounded_Unsigned;
      Half_Words      : Digit_Count;
      Bits            : Bit_Count;
   end record;
--
-- Create -- Domain
--
--    Modulus - The modulus
--
-- Exceptions :
--
--    Constraint_Error -- Modulus is not odd
--
   function Create (Modulus : Unbounded_Unsigned)
      return Montgomery_Domain;

   function Max (Left, Right : Montgomery_Number)
      return Montgomery_Number;
   function Min (Left, Right : Montgomery_Number)
      return Montgomery_Number;

   function "="  (Left, Right : Montgomery_Number) return Boolean;
   function "<"  (Left, Right : Montgomery_Number) return Boolean;
   function "<=" (Left, Right : Montgomery_Number) return Boolean;
   function ">"  (Left, Right : Montgomery_Number) return Boolean;
   function ">=" (Left, Right : Montgomery_Number) return Boolean;
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
   function Compare (Left, Right : Montgomery_Number)
      return Precedence;
--
-- Erase -- Set the number to zero
--
--    Left - The first number
--
-- The procedure zeroes the number without releasing memory it occupies.
--
   procedure Erase (Left : in out Montgomery_Number);
--
-- From_Domain -- Conversion
--
--    Domain - The domain
--    Value  - The value to comvert from the domain
--    Result - The original number
--
   procedure From_Domain
             (  Domain : Montgomery_Domain;
                Value  : Montgomery_Number;
                Result : out Unbounded_Unsigned
             );
--
-- From_Domain -- Conversion
--
--    Domain - The domain
--    Value  - The value to comvert from the domain
--
-- Returns :
--
--    The original number
--
   function From_Domain
            (  Domain : Montgomery_Domain;
               Value  : Montgomery_Number
            )  return Unbounded_Unsigned;
--
-- From_Unbounded_Unsigned --Unchecked conversion
--
--    Value - To convert
--
-- Returns :
--
--    Value converted to Unbounded_Unsigned
--
   function From_Unbounded_Unsigned (Value : Unbounded_Unsigned)
      return Montgomery_Number;
--
-- Get_Multiplication_Algorithm -- The algorithm used in Mod_Mul
--
-- Returns :
--
--    The algorithm
--
   function Get_Multiplication_Algorithm
      return Multiplication_Algorithm;
--
-- Is_Zero -- Zero test
--
--    Left - The number
--
-- Returns :
--
--    True if Left is zero
--
   function Is_Zero (Left : Montgomery_Number) return Boolean;
--
-- Is_One -- 1 test
--
--    Domain - The domain
--    Left   - The number
--
-- Returns :
--
--    True if Left is one
--
   function Is_One
            (  Domain : Montgomery_Domain;
               Left   : Montgomery_Number
            )  return Boolean;
--
-- Mod_Add -- Addition
--
--    Domain      - The domain
--    Accumulator - The number to be added to
--    Increment   - The increment to add
--
   procedure Mod_Add
             (  Domain      : Montgomery_Domain;
                Accumulator : in out Montgomery_Number;
                Increment   : Montgomery_Number
             );
--
-- Mod_Add -- Addition
--
--    Domain      - The domain
--    Accumulator - The number to be added to
--    Increment   - The increment to add
--    Result      - The result
--
   procedure Mod_Add
             (  Domain      : Montgomery_Domain;
                Accumulator : Montgomery_Number;
                Increment   : Montgomery_Number;
                Result      : out Montgomery_Number
             );
--
-- Mod_Double - Montgomery multiplication by two
--
--    Domain       - The domain
--    Multiplicand - The multiplicand
--
-- This procedure multiplies Multiplicand by two.
--
--    X * R mod N * 2 * R mod N * R' = X' * 2 mod N
--
   procedure Mod_Double
             (  Domain       : Montgomery_Domain;
                Multiplicand : in out Montgomery_Number
             );
--
-- Mod_Mul - Montgomery multiplication
--
--    Domain       - The domain
--    Multiplicand - The multiplicand
--    Multiplier   - The multiplier
--    Result       - The result
--
-- This procedure computes  Multiplicand * Multiplier * R' mod N,  where
-- R is  the multiplicative inverse  of R (Reducer_Inverse) and N is the
-- domain modulus (Domian.Modulus). Considering
--
--    Multiplicand = X * R mod N
--    Multiplier   = Y * R mod N
--
-- being two numbers  from the Montgomery  domain,  then  the  result of
-- Mod_Mul reduced = multiplied by R' and mod N gives
--
--    (X * R mod N) * (Y * R mod N) * R' mod N =
--    X * R * Y * R * R' * R' mod N = X * Y
--
-- In other words,  reducing  the result  would give the  product of the
-- originals. So multiplication can be implemented as:
--
--   From_Domain
--   (  Domain,
--      Mod_Mul
--      (  Domain,
--         To_Domain (Domain, X),
--         To_Domain (Domain, Y) ,
--         Result
--   )  );
--
-- The algorithm is chosen according to Get_Multiplication_Algorithm.
--
-- Exceptions :
--
--    Constraint_Error - Multiplicand  or  Multiplier  is larger in size
--                       than the domain modulus
--
   procedure Mod_Mul
             (  Domain       : Montgomery_Domain;
                Multiplicand : Montgomery_Number;
                Multiplier   : Montgomery_Number;
                Result       : out Montgomery_Number
             );
--
-- Mod_Mul -- Montgomery multiplication
--
--    Domain       - The domain
--    Multiplicand - The multiplicand
--    Multiplier   - The multiplier
--
-- The algorithm is chosen according to Get_Multiplication_Algorithm.
--
-- Returns :
--
--    Multiplicand * Multiplier * R' mod N
--
-- Exceptions :
--
--    Constraint_Error - Multiplicand  or  Multiplier  is larger in size
--                       than the domain modulus
--
   function Mod_Mul
            (  Domain       : Montgomery_Domain;
               Multiplicand : Montgomery_Number;
               Multiplier   : Montgomery_Number
            )  return Montgomery_Number;
--
-- Mod_Mul_CIOS - Montgomery multiplication
--
--    Domain       - The domain
--    Multiplicand - The multiplicand
--    Multiplier   - The multiplier
--    Result       - The result
--
-- This procedure computes  Multiplicand * Multiplier * R' mod N,  where
-- R is  the multiplicative inverse  of R (Reducer_Inverse) and N is the
-- domain  modulus  (Domian.Modulus).  The procedure uses  the  Coarsely
-- Integrated Operand Scanning algorithm.
--
   procedure Mod_Mul_CIOS
             (  Domain       : Montgomery_Domain;
                Multiplicand : Montgomery_Number;
                Multiplier   : Montgomery_Number;
                Result       : out Montgomery_Number
             );
--
-- Mod_Mul_CIOS -- Montgomery multiplication
--
--    Domain       - The domain
--    Multiplicand - The multiplicand
--    Multiplier   - The multiplier
--
-- Returns :
--
--    Multiplicand * Multiplier * R' mod N
--
-- Exceptions :
--
--    Constraint_Error - Multiplicand  or  Multiplier  is larger in size
--                       than the domain modulus
--
   function Mod_Mul_CIOS
            (  Domain       : Montgomery_Domain;
               Multiplicand : Montgomery_Number;
               Multiplier   : Montgomery_Number
            )  return Montgomery_Number;
--
-- Mod_Mul_Dusse_Kaliski - Montgomery multiplication
--
--    Domain       - The domain
--    Multiplicand - The multiplicand
--    Multiplier   - The multiplier
--    Result       - The result
--
-- This procedure computes  Multiplicand * Multiplier * R' mod N,  where
-- R is  the multiplicative inverse  of R (Reducer_Inverse) and N is the
-- domain  modulus  (Domian.Modulus).  The procedure uses  the Separated
-- Operand Scanning algorithm.
--
   procedure Mod_Mul_Dusse_Kaliski
             (  Domain       : Montgomery_Domain;
                Multiplicand : Montgomery_Number;
                Multiplier   : Montgomery_Number;
                Result       : out Montgomery_Number
             );
--
-- Mod_Mul_Dusse_Kaliski -- Montgomery multiplication
--
--    Domain       - The domain
--    Multiplicand - The multiplicand
--    Multiplier   - The multiplier
--
-- Returns :
--
--    Multiplicand * Multiplier * R' mod N
--
-- Exceptions :
--
--    Constraint_Error - Multiplicand  or  Multiplier  is larger in size
--                       than the domain modulus
--
   function Mod_Mul_Dusse_Kaliski
            (  Domain       : Montgomery_Domain;
               Multiplicand : Montgomery_Number;
               Multiplier   : Montgomery_Number
            )  return Montgomery_Number;
--
-- Mod_Pow -- Montgomery mdoular exponentiation
--
--    Domain - The domain
--    Left   - The value to exponentiate
--    Right  - The exponent
--    Result - The result Left ** Right mod Domain.Modulus
--
-- This  procedure  computes  Left ** Right * R' mod N,  where R is  the
-- multiplicative  inverse  of R (Reducer_Inverse)  and N  is the domain
-- modulus (Domian.Modulus).
--
-- Exceptions :
--
--    Constraint_Error - Left is larger in size than the domain modulus
--
   procedure Mod_Pow
             (  Domain : Montgomery_Domain;
                Left   : Montgomery_Number;
                Right  : Unbounded_Unsigned;
                Result : out Montgomery_Number
             );
   procedure Mod_Pow
             (  Domain : Montgomery_Domain;
                Left   : Montgomery_Number;
                Right  : Half_Word;
                Result : out Montgomery_Number
             );
--
-- Mod_Pow -- Montgomery mdoular exponentiation
--
--    Left  - The value exponentiate
--    Right - The exponent
--
-- Returns :
--
--    Left ** Right mod Domain
--
-- Exceptions :
--
--    Constraint_Error - Left is larger in size than the domain modulus
--
   function Mod_Pow
            (  Domain : Montgomery_Domain;
               Left   : Montgomery_Number;
               Right  : Unbounded_Unsigned
            )  return Montgomery_Number;
   function Mod_Pow
            (  Domain : Montgomery_Domain;
               Left   : Montgomery_Number;
               Right  : Half_Word
            )  return Montgomery_Number;
--
-- Mod_Pow_Of_Two -- Montgomery mdoular exponentiation of 2
--
--    Domain - The domain
--    Power  - The exponent
--    Result - The result 2 ** Power mod Domain.Modulus
--
-- This  procedure   computes  2 ** Power * R' mod N,   where R  is  the
-- multiplicative  inverse  of R (Reducer_Inverse)  and N  is the domain
-- modulus (Domian.Modulus).
--
   procedure Mod_Pow_Of_Two
             (  Domain : Montgomery_Domain;
                Power  : Unbounded_Unsigned;
                Result : out Montgomery_Number
             );
--
-- Mod_Pow_Of_Two -- Montgomery mdoular exponentiation of 2
--
--    Domain - The domain
--    Power  - The exponent
--
-- Returns :
--
--    2 ** Right mod Domain.Modulus
--
   function Mod_Pow_Of_Two
            (  Domain : Montgomery_Domain;
               Power  : Unbounded_Unsigned
            )  return Montgomery_Number;
--
-- Mod_Sub -- Subtraction
--
--    Domain     - The domain
--    Minuend    - The number to subracted from
--    Subtrahend - The decrement to subtract
--
   procedure Mod_Sub
             (  Domain     : Montgomery_Domain;
                Minuend    : in out Montgomery_Number;
                Subtrahend : Montgomery_Number
             );
--
-- Mod_Sub -- Subtraction
--
--    Domain     - The domain
--    Minuend    - The number to subracted from
--    Subtrahend - The decrement to subtract
--    Result     - The result
--
   procedure Mod_Sub
             (  Domain     : Montgomery_Domain;
                Minuend    : Montgomery_Number;
                Subtrahend : Montgomery_Number;
                Result     : out Montgomery_Number
             );
--
-- Mod_Sub_2 -- Subtraction
--
--    Domain     - The domain
--    Minuend    - The number to subracted from
--    Subtrahend - The decrement to subtract and the result
--
-- This variant stores the result in Subtrahend
--
   procedure Mod_Sub_2
             (  Domain     : Montgomery_Domain;
                Minuend    : Montgomery_Number;
                Subtrahend : in out Montgomery_Number
             );
--
-- Mod_Square - Montgomery squaring
--
--    Domain - The domain
--    Left   - The number
--    Result - The result
--
-- This  procedure  computes   Left ** 2 * R' mod N,   where  R  is  the
-- multiplicative inverse  of  R (Reducer_Inverse)  and N  is the domain
-- modulus (Domian.Modulus).
--
-- Exceptions :
--
--    Constraint_Error - Left is larger in size than the domain modulus
--
   procedure Mod_Square
             (  Domain : Montgomery_Domain;
                Left   : Montgomery_Number;
                Result : out Montgomery_Number
             );
--
-- Mod_Square - Montgomery squaring
--
--    Domain - The domain
--    Left   - The number
--
-- Returns :
--
--    Left ** 2 * R' mod N, where N is Domain.Modulus
--
-- Exceptions :
--
--    Constraint_Error - Left is larger in size than the domain modulus
--
   function Mod_Square
            (  Domain : Montgomery_Domain;
               Left   : Montgomery_Number
            )  return Montgomery_Number;
--
-- Mod_Square_CIOS - Montgomery squaring using CIOS algorithm
--
--    Domain - The domain
--    Left   - The number
--    Result - The result
--
-- This  procedure  computes   Left ** 2 * R' mod N,   where  R  is  the
-- multiplicative inverse  of  R (Reducer_Inverse)  and N  is the domain
-- modulus (Domian.Modulus).
--
-- Exceptions :
--
--    Constraint_Error - Left is larger in size than the domain modulus
--
   procedure Mod_Square_CIOS
             (  Domain : Montgomery_Domain;
                Left   : Montgomery_Number;
                Result : out Montgomery_Number
             );
--
-- Mod_Square_CIOS - Montgomery squaring using CIOS algorithm
--
--    Domain - The domain
--    Left   - The number
--
-- Returns :
--
--    Left ** 2 * R' mod N, where N is Domain.Modulus
--
-- Exceptions :
--
--    Constraint_Error - Left is larger in size than the domain modulus
--
   function Mod_Square_CIOS
            (  Domain : Montgomery_Domain;
               Left   : Montgomery_Number
            )  return Montgomery_Number;
--
-- Mod_Square_Dusse_Kaliski - Montgomery squaring  using  Dusse-Kalinski
--                            algorithm
--
--    Domain - The domain
--    Left   - The number
--    Result - The result
--
-- This  procedure  computes   Left ** 2 * R' mod N,   where  R  is  the
-- multiplicative inverse  of  R (Reducer_Inverse)  and N  is the domain
-- modulus (Domian.Modulus).
--
-- Exceptions :
--
--    Constraint_Error - Left is larger in size than the domain modulus
--
   procedure Mod_Square_Dusse_Kaliski
             (  Domain : Montgomery_Domain;
                Left   : Montgomery_Number;
                Result : out Montgomery_Number
             );
--
-- Mod_Square_Dusse_Kaliski - Montgomery squaring  using  Dusse-Kalinski
--                            algorithm
--
--    Domain - The domain
--    Left   - The number
--
-- Returns :
--
--    Left ** 2 * R' mod N, where N is Domain.Modulus
--
-- Exceptions :
--
--    Constraint_Error - Left is larger in size than the domain modulus
--
   function Mod_Square_Dusse_Kaliski
            (  Domain : Montgomery_Domain;
               Left   : Montgomery_Number
            )  return Montgomery_Number;
--
-- Mul -- Montgomery multiplication
--
--    Domain - The domain
--    Left   - The multiplicand
--    Right  - The multiplier
--    Result - Multiplier * Multiplier mod Domain.Modulus
--
-- Exceptions :
--
--    Constraint_Error - Multiplicand or Multiplier is not less than the
--                       domain modulus
--
   procedure Mul
             (  Domain : Montgomery_Domain;
                Left   : Unbounded_Unsigned;
                Right  : Unbounded_Unsigned;
                Result  : out Unbounded_Unsigned
             );
--
-- Mul -- Montgomery multiplication
--
--    Domain - The domain
--    Left   - The multiplicand
--    Right  - The multiplier
--
-- Returns :
--
--    Multiplier * Multiplier mod Domain.Modulus
--
-- Exceptions :
--
--    Constraint_Error - Multiplicand or Multiplier is not less than the
--                       domain modulus
--
   function Mul
            (  Domain : Montgomery_Domain;
               Left   : Unbounded_Unsigned;
               Right  : Unbounded_Unsigned
            )  return Unbounded_Unsigned;
--
-- Pow -- Montgomery exponentiation
--
--    Domain - The domain
--    Left   - The base
--    Right  - The exponent
--    Result  - Left ** Power mod N, where N is Domain.Modulus
--
-- Exceptions :
--
--    Constraint_Error - Left is not less than the domain modulus.
--
   procedure Pow
             (  Domain : Montgomery_Domain;
                Left   : Unbounded_Unsigned;
                Right  : Unbounded_Unsigned;
                Result : out Unbounded_Unsigned
             );
--
-- Pow -- Montgomery exponentiation
--
--    Domain - The domain
--    Left   - The base
--    Right  - The exponent
--
-- Returns :
--
--    Left ** Power mod N, where N is Domain.Modulus
--
-- Exceptions :
--
--    Constraint_Error - Left is not less than the domain modulus.
--
   function Pow
            (  Domain : Montgomery_Domain;
               Left   : Unbounded_Unsigned;
               Right  : Unbounded_Unsigned
            )  return Unbounded_Unsigned;
--
-- Pow_Of_Two -- Montgomery exponentiation of 2
--
--    Domain - The domain
--    Power  - The exponent
--    Result - 2 ** Power mod N, where N is Domain.Modulus
--
   procedure Pow_Of_Two
             (  Domain : Montgomery_Domain;
                Power  : Unbounded_Unsigned;
                Result : out Unbounded_Unsigned
             );
--
-- Pow_Of_Two -- Montgomery exponentiation of 2
--
--    Domain - The domain
--    Left   - The base
--    Right  - The exponent
--
-- Returns :
--
--    2 ** Power mod N, where N is Domain.Modulus
--
   function Pow_Of_Two
            (  Domain : Montgomery_Domain;
               Power  : Unbounded_Unsigned
            )  return Unbounded_Unsigned;
--
-- Reduce -- Montgomery reduction
--
--    Domain - The domain
--    Value  - The value to reduce
--
-- Returns :
--
--    Reduced value = Value * R' mod N, where Domain.Modulus
--
   function Reduce
            (  Domain : Montgomery_Domain;
               Value  : Montgomery_Number
            )  return Montgomery_Number;
--
-- Reduce -- Montgomery reduction
--
--    Domain - The domain
--    Value  - The value to reduce
--    Result - The result = Value * R' mod N, where Domain.Modulus
--
   procedure Reduce
             (  Domain : Montgomery_Domain;
                Value  : Montgomery_Number;
                Result : out Montgomery_Number
             );
--
-- Reducer_Inverse -- The multiplicative inverse of R
--
--    Domain - The domain
--
-- The function returns value R'  such  the R * R' = 1 mod N, where N is
-- the Domain value.  Conversions to and from  the Montgomery domain are
-- defined in terms of R and R':
-- and R':
--
--    Orignal space      Montgomery domain
--                X  ->  X  * R   mod N
--    X' * R' mod N  <-  X'
--
-- However in both cases more efficient algorithms are used.
--
-- Returns :
--
--    The reducer multiplicative inverse R'
--
   function Reducer_Inverse (Domain : Montgomery_Domain)
      return Unbounded_Unsigned;
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
             (  Destination : in out Montgomery_Number;
                Source      : Montgomery_Number
             );
--
-- Set_Multiplication_Algorithm -- Set the algorithm used in Mod_Mul
--
--    Algorithm - The algorithm
--
   procedure Set_Multiplication_Algorithm
             (  Algorithm : Multiplication_Algorithm
             );
--
-- Set_One -- Assignment of 1'
--
--    Domain      - The domain
--    Destination - The target
--
-- This procedure sets Destination to 1'.
--
   procedure Set_One
             (  Domain      : Montgomery_Domain;
                Destination : out Montgomery_Number
             );
--
-- Set_Unchecked -- Unchecked type conversion
--
--    Destination - The target
--    Source      - The source
--
   procedure Set_Unchecked
             (  Destination : in out Unbounded_Unsigned;
                Source      : Montgomery_Number
             );
   procedure Set_Unchecked
             (  Destination : in out Montgomery_Number;
                Source      : Unbounded_Unsigned
             );
--
-- Square - Montgomery squaring
--
--    Domain - The domain
--    Left   - The number
--    Result - Left ** 2 mod Domain.Modulus
--
-- Exceptions :
--
--    Constraint_Error - Multiplicand or Multiplier is not less than the
--                       domain modulus
--
   procedure Square
             (  Domain : Montgomery_Domain;
                Left   : Unbounded_Unsigned;
                Result : out Unbounded_Unsigned
             );
--
-- Square - Montgomery squaring
--
--    Domain - The domain
--    Left   - The number
--
-- Returns :
--
--    Left ** 2 mod Domain.Modulus
--
-- Exceptions :
--
--    Constraint_Error - Multiplicand or Multiplier is not less than the
--                       domain modulus
--
   function Square
            (  Domain : Montgomery_Domain;
               Left   : Unbounded_Unsigned
            )  return Unbounded_Unsigned;
--
-- Swap -- Swap numbers
--
--    Left  - The number to swap with another
--    Right - The number
--
-- This procedure swaps Left and Right
--
   procedure Swap (Left, Right : in out Montgomery_Number);
--
-- Swap_Unchecked -- Swap numbers with unchecked type conversion
--
--    Left  - The number to swap with another
--    Right - The number
--
   procedure Swap_Unchecked
             (  Left  : in out Unbounded_Unsigned;
                Right : in out Montgomery_Number
             );
--
-- To_Domain -- Conversion
--
--    Domain - The domain
--    Value  - The value to comvert into the domain
--    Result - Montgomery number
--
   procedure To_Domain
             (  Domain : Montgomery_Domain;
                Value  : Unbounded_Unsigned;
                Result : out Montgomery_Number
             );
   procedure To_Domain
             (  Domain : Montgomery_Domain;
                Value  : Half_Word;
                Result : out Montgomery_Number
             );
--
-- To_Domain -- Conversion
--
--    Domain - The domain
--    Value  - The value to comvert into the domain
--
-- Returns :
--
--    Montgomery number
--
   function To_Domain
            (  Domain : Montgomery_Domain;
               Value  : Unbounded_Unsigned
            )  return Montgomery_Number;
   function To_Domain
            (  Domain : Montgomery_Domain;
               Value  : Half_Word
            )  return Montgomery_Number;
--
-- To_Unbounded_Unsigned --Unchecked conversion
--
--    Value - To convert
--
-- Returns :
--
--    Value converted to Unbounded_Unsigned
--
   function To_Unbounded_Unsigned (Value : Montgomery_Number)
      return Unbounded_Unsigned;

   procedure Dump_Hex
             (  X      : Montgomery_Number;
                Prefix : String := "";
                Join   : Boolean := False
             );
   procedure Dump (X : Montgomery_Number; Prefix : String := "");
--
-- Get_Use_Count -- The number's vector reference count
--
--    Object - The object
--
-- Returns :
--
--    The reference count
--
   function Get_Use_Count (Object : Montgomery_Number) return Natural;

private
   type Montgomery_Number is new Unbounded_Unsigned with null record;
   function "+" (Left : Half_Word; Right : Montgomery_Number)
      return Montgomery_Number;
   function "*" (Left : Half_Word; Right : Montgomery_Number)
      return Montgomery_Number;
   function "mod" (Left : Half_Word; Right : Montgomery_Number)
      return Montgomery_Number;
   function "rem" (Left : Half_Word; Right : Montgomery_Number)
      return Montgomery_Number;

   pragma Inline ("+", "-");
   pragma Inline ("<", "<=", "=", ">=", ">");
   pragma Inline (Add, Compare, Erase, Is_One, Is_Zero);
   pragma Inline (Min, Max, Modulo, Set, Sub, Sub_2, Swap);
   pragma Inline (Mod_Square, Mod_Mul);
   pragma Inline (From_Unbounded_Unsigned);
   pragma Inline (To_Unbounded_Unsigned);

   function "+" (Left, Right : Montgomery_Number)
      return Montgomery_Number;
   function "-" (Left, Right : Montgomery_Number)
      return Montgomery_Number;

   function "+" (Left : Montgomery_Number; Right : Half_Word)
      return Montgomery_Number;
   function "-" (Left : Montgomery_Number; Right : Half_Word)
      return Montgomery_Number;

   function "*" (Left, Right : Montgomery_Number)
      return Montgomery_Number;
   function "/" (Left, Right : Montgomery_Number)
      return Montgomery_Number;

   function "mod" (Left, Right : Montgomery_Number)
      return Montgomery_Number;
   function "rem" (Left, Right : Montgomery_Number)
      return Montgomery_Number;

   function "**" (Left : Montgomery_Number; Right : Bit_Count)
      return Montgomery_Number;

   function "*" (Left : Montgomery_Number; Right : Half_Word)
      return Montgomery_Number;
   function "/" (Left : Montgomery_Number; Right : Half_Word)
      return Montgomery_Number;

   function Div_By_Power_of_Two
            (  Dividend : Montgomery_Number;
               Power    : Bit_Count
            )  return Montgomery_Number;
   function From_Half_Word (Left : Half_Word)
      return Montgomery_Number;
   function From_Word (Left : Word) return Montgomery_Number;

   function Greatest_Common_Divisor (Left, Right : Montgomery_Number)
      return Montgomery_Number;
   function Get_Slice
            (  Left : Montgomery_Number;
               From : Bit_Position;
               To   : Bit_Position
            )  return Montgomery_Number;
   function Inverse
            (  Left  : Montgomery_Number;
               Count : Digit_Count
            )  return Montgomery_Number;
   function Modulo
            (  Left  : Montgomery_Number;
               Power : Digit_Count
            )  return Montgomery_Number;
   function Modulo_By_Power_Of_Two
            (  Left  : Montgomery_Number;
               Right : Bit_Count
            )  return Montgomery_Number;
   function Mod_Inv
            (  Left    : Montgomery_Number;
               Modulus : Montgomery_Number
            )  return Montgomery_Number;
   function Mod_Inv_In_Power_Of_Two
            (  Left   : Montgomery_Number;
               Power  : Bit_Position
            )  return Montgomery_Number;
   function Mod_Pow (Left, Right, Modulus : Montgomery_Number)
      return Montgomery_Number;
   function Mod_Pow
            (  Left    : Montgomery_Number;
               Right   : Half_Word;
               Modulus : Montgomery_Number
            )  return Montgomery_Number;
   function Mod_Pow_By_Power_Of_Two
            (  Left, Right : Montgomery_Number;
               Modulus     : Bit_Count
            )  return Montgomery_Number;
   function Mod_Pow_By_Power_Of_Two
            (  Left    : Montgomery_Number;
               Right   : Half_Word;
               Modulus : Bit_Count
            )  return Montgomery_Number;
   function Mod_Pow_Of_Two
            (  Power   : Montgomery_Number;
               Modulus : Montgomery_Number
            )  return Montgomery_Number;
   function Mul_By_Power_of_Two
            (  Multiplicand : Montgomery_Number;
               Power        : Bit_Count
            )  return Montgomery_Number;
   function Mul_Classroom (Left, Right : Montgomery_Number)
      return Montgomery_Number;
   function Mul_Karatsuba
            (  Left, Right : Montgomery_Number;
               Threshold   : Digit_Count
            )  return Montgomery_Number;
   function Phi (Left : Montgomery_Number) return Montgomery_Number;
   function Power_of_Two (Power : Bit_Count) return Montgomery_Number;
   function Sqrt (Left : Montgomery_Number) return Montgomery_Number;
   function Square (Left : Montgomery_Number)
      return Montgomery_Number;
   function Square_Classroom (Left : Montgomery_Number)
      return Montgomery_Number;
   function Square_Karatsuba
            (  Left      : Montgomery_Number;
               Threshold : Digit_Count
            )  return Montgomery_Number;

   procedure Mod_Add
             (  Domain      : Montgomery_Domain;
                Accumulator : in out Montgomery_Number;
                Increment   : Half_Word_Array
             );
   procedure Mod_Mul
             (  Domain       : Montgomery_Domain;
                Multiplicand : Half_Word_Array;
                Multiplier   : Half_Word_Array;
                Result       : out Montgomery_Number
             );
   procedure Mod_Square
             (  Domain : Montgomery_Domain;
                Left   : Half_Word_Array;
                Result : out Montgomery_Number
             );
   procedure Mod_Sub
             (  Domain     : Montgomery_Domain;
                Minuend    : in out Montgomery_Number;
                Subtrahend : Half_Word_Array
             );
--
-- Reduce -- Montgomery reduction Value * R' mod N
--
--    Domain - The domain
--    Value  - The value to reduce
--    Result - The result
--
   procedure Reduce
             (  Domain : Montgomery_Domain;
                Value  : Unbounded_Unsigned;
                Result : out Unbounded_Unsigned
             );

end Unbounded_Unsigneds.Montgomery;
