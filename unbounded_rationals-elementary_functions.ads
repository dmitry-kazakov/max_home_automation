--                                                                    --
--  package Unbounded_Rationals.    Copyright (c)  Dmitry A. Kazakov  --
--             Elementary_Functions                Luebeck            --
--  Interface                                      Spring, 2025       --
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

package Unbounded_Rationals.Elementary_Functions is

   type Unbounded_Rational_Array is
      array (Positive range <>) of Unbounded_Rational;
--
-- Arccos -- Arccos function
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
-- Exceptions :
--
--    Constraint_Error - X is not in -1..1
--
   function Arccos (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Arccosh -- Arcsinh function
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
-- Exceptions :
--
--    Constraint_Error - X is less than 1
--
   function Arccosh (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Arcsin -- Arcsin function
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
-- Exceptions :
--
--    Constraint_Error - X is not in -1..1
--
   function Arcsin (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Arcsinh -- Arcsinh function
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
   function Arcsinh (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Arctan -- Arctan function
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
   function Arctan (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Arctan -- Arctan function
--
--    Y, X  - The arguments
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
-- Exceptions :
--
--    Constraint_Error - X and Y are zero
--
   function Arctan (Y, X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Arctanh -- Arctanh function
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
-- Exceptions :
--
--    Constraint_Error - X is not in the range ]-1, 1[.
--
   function Arctanh (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Cos -- Cosine function
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
   function Cos (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Cosh -- Hyberbolic cosine function
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
   function Cosh (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Cot -- Cot function
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
-- Exceptions :
--
--    Constraint_Error - The argument is of range -Pi/2..Pi/2 or zero
--
   function Cot (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Coth -- Coth function
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
-- Exceptions :
--
--    Constraint_Error - The argument is zero
--
   function Coth (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Exp -- Exponential function
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
   function Exp (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Ln -- Natural logarithm
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
-- Exceptions :
--
--    Constraint_Error - Left is not positive
--
   function Ln (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Ln_2 -- Natural logarithm of 2
--
--    Error - The required accuracy 2**Error
--
-- This function evaluates natural logarith of 2 using the series:
--
--   Sum (-1)**(N-1) / N * 2**N + Sum (-1)**(N-1) / N * 3**N
--   N=1                          N=1
--
-- Resturns :
--
--    The result
--
   function Ln_2 (Error : Integer) return Unbounded_Rational;
--
-- Log2 -- Binary logarithm
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
-- Exceptions :
--
--    Constraint_Error - Left is not positive
--
   function Log2 (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Mod_Half_Pi -- Range reduction to 0..Pi/2
--
--    X        - The argument
--    Error    - The error 2**Error
--    Reduced  - The result X - N * Pi_2
--    Quadrant - The quadrant N mod 4
--    Half_Pi  - The estimated Pi/2 to 2**Error / N
--
   type Quadrant_Type is (I, II, III, IV);
   procedure Mod_Half_Pi
             (  X        : Unbounded_Rational;
                Error    : Integer;
                Reduced  : out Unbounded_Rational;
                Quadrant : out Quadrant_Type;
                Half_Pi  : out Unbounded_Rational
             );
--
-- Pi_Chudnovsky -- Pi value
--
--    Error - The required accuracy 2**Error
--
-- This function evaluates Pi using Chudnovsky algorithm.
--
-- Resturns :
--
--    The result
--
   function Pi_Chudnovsky (Error : Integer) return Unbounded_Rational;
--
-- Pi_Nilkantha -- Pi value
--
--    Error - The required accuracy 2**Error
--
-- This function evaluates Pi using Nilkantha's series
--
--    3 + 4 / (2*3*4) - 4 / (4*5*6) + 4 / (6*7*8) - ...
--
-- Resturns :
--
--    The result
--
   function Pi_Nilkantha (Error : Integer) return Unbounded_Rational;
--
-- Power -- Eponentiation
--
--    Left  - The first argument
--    Right - The second argument
--    Error - The required accuracy 2**Error
--
-- Returns :
--
--    Left ** Right
--
-- Exceptions :
--
--    Constraint_Error - Left is negative or 0**0
--
   function Power (Left, Right : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Root -- nth root
--
--    X     - The argument
--    N     - The root's degree
--    Error - The required accuracy 2**Error
--
-- This function evaluates nth root using the Newton method:
--
--    Xn+1 = Xn - f(Xn) / f'(Xn)
--
-- which for f(X) = X**N gives:
--
--    Xn+1 = ((N - 1) * Xn + Left / Xn**(N-1)) / N
--
-- Resturns :
--
--    The result
--
-- Exceptions :
--
--    Constraint_Error - Left is negative
--
   function Root
            (  X    : Unbounded_Rational;
               N    : Positive;
               Error : Integer
            )  return Unbounded_Rational;
--
-- Sinh -- Hyberbolic sine function
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
   function Sinh (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Sin -- Sine function
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
   function Sin (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Sin_Div_X -- Sin (X) / X
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
   function Sin_Div_X (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Sqrt -- Square root
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- This function evaluates square root using the Newton method:
--
--    Xn+1 = Xn - f(Xn) / f'(Xn)
--
-- which for f(X) = X**2 gives:
--
--    Xn+1 = (Xn + Left / Xn) / 2
--
-- Resturns :
--
--    The result
--
-- Exceptions :
--
--    Constraint_Error - Left is negative
--
   function Sqrt (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Tan -- Tan function
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
-- Exceptions :
--
--    Constraint_Error - The argument is of range -Pi/2..Pi/2
--
   function Tan (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Tan_Continued_Fraction -- Tan function by continued fraction
--
--    X - The argument
--    N - The number of continued fraction to sum
--
-- Resturns :
--
--    The result
--
   function Tan_Continued_Fraction
            (  X : Unbounded_Rational;
               N : Positive
            )  return Unbounded_Rational;
--
-- Tanh -- Hyberbolic tangent function
--
--    X     - The argument
--    Error - The required accuracy 2**Error
--
-- Resturns :
--
--    The result
--
   function Tanh (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational;
--
-- Tanh_Continued_Fraction -- Tanh function by continued fraction
--
--    X - The argument
--    N - The number of continued fraction to sum
--
-- Resturns :
--
--    The result
--
   function Tanh_Continued_Fraction
            (  X : Unbounded_Rational;
               N : Positive
            )  return Unbounded_Rational;

end Unbounded_Rationals.Elementary_Functions;
