--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Unbounded_Unsigneds.Barrett                 Luebeck            --
--  Interface                                      Winter, 2025       --
--                                                                    --
--                                Last revision :  17:53 15 Jan 2025  --
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

package Unbounded_Unsigneds.Barrett is
--
-- Barrett_Reducer -- Montgomery domain
--
--    Modulus    - The modulus (N)
--    Reducer    - R = 4 ** K / N
--    Half_Words - Number half-words needed to contain Modulus
--    Bits       - K, Half_Words multiplied by Bit_Width
--
   type Barrett_Reducer is record
      Modulus    : Unbounded_Unsigned;
      Reducer    : Unbounded_Unsigned;
      Half_Words : Digit_Count;
      Bits       : Bit_Count;
   end record;
--
-- Create -- Domain
--
--    Modulus - The modulus
--
-- Exceptions :
--
--    Constraint_Error -- Modulus is less than 5 or a power of two
--
   function Create (Modulus : Unbounded_Unsigned)
      return Barrett_Reducer;
--
-- Mod_Mul - Modular multiplication
--
--    Reducer      - The Barrett reducer
--    Multiplicand - The multiplicand
--    Multiplier   - The multiplier
--    Result       - The result
--
-- This procedure computes  Multiplicand * Multiplier mod N,  where N is
-- the modulus. Multiplicand * Multiplier must be less than N ** 2.
--
   procedure Mod_Mul
             (  Reducer      : Barrett_Reducer;
                Multiplicand : Unbounded_Unsigned;
                Multiplier   : Unbounded_Unsigned;
                Result       : out Unbounded_Unsigned
             );
--
-- Mod_Mul - Modular multiplication
--
--    Reducer      - The Barrett reducer
--    Multiplicand - The multiplicand
--    Multiplier   - The multiplier
--
-- This procedure computes  Multiplicand * Multiplier mod N,  where N is
-- the modulus. Multiplicand * Multiplier must be less than N ** 2.
--
-- Returns :
--
--    Multiplicand * Multiplier mod N
--
   function Mod_Mul
            (  Reducer      : Barrett_Reducer;
               Multiplicand : Unbounded_Unsigned;
               Multiplier   : Unbounded_Unsigned
            )  return Unbounded_Unsigned;
--
-- Mod_Pow - Modular exponentiation
--
--    Reducer - The Barrett reducer
--    Left    - The value to expontiate
--    Right   - The exponent
--    Result  - The result
--
-- This procedure computes  Left ** Right mod N, where N is the modulus.
-- Left be less than N.
--
   procedure Mod_Pow
             (  Reducer : Barrett_Reducer;
                Left    : Unbounded_Unsigned;
                Right   : Unbounded_Unsigned;
                Result  : out Unbounded_Unsigned
             );
--
-- Mod_Mul - Modular multiplication
--
--    Reducer - The Barrett reducer
--    Left    - The value to expontiate
--    Right   - The exponent
--
-- Returns :
--
--    Left ** Right mod N
--
   function Mod_Pow
            (  Reducer : Barrett_Reducer;
               Left    : Unbounded_Unsigned;
               Right   : Unbounded_Unsigned
            )  return Unbounded_Unsigned;
--
-- Mod_Square - Modular squaring
--
--    Reducer - The Barrett reducer
--    Left    - The number
--    Result  - The result
--
-- This procedure  computes  Left ** 2 mod N,  where N  is  the modulus.
-- Left must be less than N.
--
   procedure Mod_Square
             (  Reducer : Barrett_Reducer;
                Left    : Unbounded_Unsigned;
                Result  : out Unbounded_Unsigned
             );
--
-- Mod_Square - Modular squaring
--
--    Reducer - The Barrett reducer
--    Left    - The number
--    Result  - The result
--
-- This procedure  computes  Left ** 2 mod N,  where N  is  the modulus.
-- Left must be less than N.
--
-- Returns :
--
--    Left ** 2 mod N
--
   function Mod_Square
            (  Reducer : Barrett_Reducer;
               Left    : Unbounded_Unsigned
            )  return Unbounded_Unsigned;
--
-- Reduce - Barrett reduction
--
--    Reducer - The Barrett reducer
--    Value   - The multiplicand
--    Result  - The result reduced to the modulus N (Reducer.Modulus).
--
   procedure Reduce
             (  Reducer : Barrett_Reducer;
                Value   : Unbounded_Unsigned;
                Result  : out Unbounded_Unsigned
             );
--
-- Reduce - Barrett reduction
--
--    Reducer - The Barrett reducer
--    Value   - The multiplicand
--
-- Returns :
--
--    Result  - The result reduced to the modulus N (Reducer.Modulus)
--
   function Reduce
            (  Reducer : Barrett_Reducer;
               Value   : Unbounded_Unsigned
            )  return Unbounded_Unsigned;

end Unbounded_Unsigneds.Barrett;
