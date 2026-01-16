--                                                                    --
--  package Generic_FFT             Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2025       --
--                                                                    --
--                                Last revision :  10:38 30 Jan 2025  --
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

with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Elementary_Functions;

generic
   type Index_Type is range <>;
   with package Complex_Types is
      new Ada.Numerics.Generic_Complex_Types (<>);
   use Complex_Types;
   type Complex_Vector is
      array (Index_Type range <>) of Complex;
   with package Elementary_Functions is
      new Ada.Numerics.Generic_Elementary_Functions (Real);
package Generic_FFT is
--
-- Permutation_Pair -- Vector elements permutation pair
--
   type Permutation_Pair is record
      I, J : Index_Type'Base;
   end record;
--
-- Permutation_Array -- Vector elements permutation array
--
   type Permutation_Array is
      array (Positive range <>) of Permutation_Pair;
--
-- FFT -- Fast Fourier transform
--
--    Vector - To transform
--    Invert - Inverse transformation if True
--
-- Exceptions :
--
--    Constraint_Error - Vector length is not a power of two
--
   procedure FFT
             (  Vector : in out Complex_Vector;
                Invert : Boolean := False
             );
--
-- FFT -- Fast Fourier transform with pre-computed data
--
--    Vector      - To transform
--    Permutation - Bit-reversal permutation
--    Factors     - Pre-computed exponents
--    Invert      - Inverse transformation if True
--
-- The parameter Permutation is an array of indices 0..Vector'Length - 1
-- to swap Vector elements.  The parameter Factors  must have  length of
-- log2 (Vector'Length):
--
-- Invert is False:
--
--    ( exp (-2*Pij/2), exp (-2*Pij/4), exp (-2*Pij/8) ... )
--
-- or, when Invert is True
--
--    ( exp (+2*Pij/2), exp (+2*Pij/4), exp (+2*Pij/8) ... )
--
-- Exceptions :
--
--    Constraint_Error - Vector length is not a power of two or Factors'
--                       length is not Log2 (Vector'Length)
--
   procedure FFT
             (  Vector      : in out Complex_Vector;
                Permutation : Permutation_Array;
                Factors     : Complex_Vector;
                Invert      : Boolean := False
             );
--
-- FFT -- Fast Fourier transform
--
--    Vector - To transform
--    Invert - Inverse transformation if True
--
-- The input is padded by zeros to the length of power of two.
--
-- Returns :
--
--    The result
--
-- Exceptions :
--
--    Constraint_Error - Vector is empty
--
   function FFT
            (  Vector : Complex_Vector;
               Invert : Boolean := False
            )  return Complex_Vector;
--
-- FFT_Factors -- Fast Fourier transform factors
--
--    N      - The binary logarithm of vector's length
--    Invert - Inverse transformation if True
--
-- Returns :
--
--    Pre-computed factors for FFT
--
   function FFT_Factors
            (  N      : Positive;
               Invert : Boolean := False
            )  return Complex_Vector;
--
-- FFT_Permutation -- Fast Fourier transform bit reversal permutation
--
--    N - The binary logarithm of vector's length
--
-- Returns :
--
--    Permutation array
--
   function FFT_Permutation (N : Positive) return Permutation_Array;

end Generic_FFT;
