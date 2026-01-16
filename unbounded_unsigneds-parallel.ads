--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Unbounded_Unsigneds.Parallel                Luebeck            --
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

with Job_Servers;  use Job_Servers;

with Unbounded_Unsigneds.Montgomery;
use  Unbounded_Unsigneds.Montgomery;

package Unbounded_Unsigneds.Parallel is

   type Unbounded_Unsigned_Array is
      array (Positive range <>) of aliased Unbounded_Unsigned;
--
-- Mod_Mul -- Parallel Montgomery domain multiplication
--
--    Domain    - Montgomery domain
--    Left      - The number to be multiplied
--    Right     - The multiplier
--    Cache     - The block of partial results
--    Server    - The job server
--    Threshold - The length when to switch back to classroom method
--    Result    - Left * Right mod Domain.Modulus
--
   procedure Mod_Mul
             (  Domain    : Montgomery_Domain;
                Left      : Montgomery_Number;
                Right     : Montgomery_Number;
                Server    : in out Job_Server'Class;
                Cache     : in out Unbounded_Unsigned_Array;
                Threshold : Digit_Count;
                Result    : out Montgomery_Number
             );
--
-- Mod_Pow -- Parallel Montgomery mdoular exponentiation
--
--    Domain    - The domain
--    Left      - The value exponentiate
--    Right     - The exponent
--    Cache     - The block of partial results
--    Server    - The job server
--    Threshold - The length when to switch back to classroom method
--    Result    - The result Left ** Right mod Domain.Modulus
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
             (  Domain    : Montgomery_Domain;
                Left      : Montgomery_Number;
                Right     : Unbounded_Unsigned;
                Server    : in out Job_Server'Class;
                Cache     : in out Unbounded_Unsigned_Array;
                Threshold : Digit_Count;
                Result    : out Montgomery_Number
             );
--
-- Mod_Pow -- Modular power
--
--    Left      - The number to be powered
--    Right     - The exponent
--    Modulus   - The modulus
--    Server    - The job server
--    Threshold - The length when to switch back to classroom method
--    Result     - Left ** Right mod Modulus
--
   procedure Mod_Pow_Karatsuba
             (  Left      : Unbounded_Unsigned;
                Right     : Unbounded_Unsigned;
                Modulus   : Unbounded_Unsigned;
                Server    : in out Job_Server'Class;
                Threshold : Digit_Count;
                Result    : out Unbounded_Unsigned
             );
--
-- Mul -- Parallel multiplication
--
--    Left      - The number to be multiplied
--    Right     - The multiplier
--    Server    - The job server
--    Cache     - The block of partial results
--    Threshold - The length when to switch back to classroom method
--    Result    - Left * Right
--
-- The procedure  splits the largest argument  into chunks multiplied by
-- the shorter argument in parallel.  The results are stored into  Cache
-- and then summed  up into Result. The length of cache  need not exceed
-- the number of working tasks.
--
   procedure Mul
             (  Left      : Unbounded_Unsigned;
                Right     : Unbounded_Unsigned;
                Server    : in out Job_Server'Class;
                Cache     : in out Unbounded_Unsigned_Array;
                Threshold : Digit_Count;
                Result    : out Unbounded_Unsigned
             );
--
-- Mul -- Parallel Montgomery multiplication
--
--    Domain    - The domain
--    Left      - The number to be multiplied
--    Right     - The multiplier
--    Server    - The job server
--    Cache     - The block of partial results
--    Threshold - The length when to switch back to classroom method
--    Result    - Left * Right mod N, where N is Domain.Modulus
--
   procedure Mul
             (  Domain    : Montgomery_Domain;
                Left      : Unbounded_Unsigned;
                Right     : Unbounded_Unsigned;
                Server    : in out Job_Server'Class;
                Cache     : in out Unbounded_Unsigned_Array;
                Threshold : Digit_Count;
                Result    : out Unbounded_Unsigned
             );
--
-- Mul_Karatsuba -- Parallel multiplication by Karatsuba algorithm
--
--    Left      - The number to be multiplied
--    Right     - The multiplier
--    Server    - The job server
--    Threshold - The length when to switch back to classroom method
--    Result    - Left * Right
--
   procedure Mul_Karatsuba
             (  Left      : Unbounded_Unsigned;
                Right     : Unbounded_Unsigned;
                Server    : in out Job_Server'Class;
                Threshold : Digit_Count;
                Result    : out Unbounded_Unsigned
             );
--
-- Pow -- Parallel Montgomery exponentiation
--
--    Domain    - The domain
--    Left      - The base
--    Right     - The exponent
--    Server    - The job server
--    Cache     - The block of partial results
--    Threshold - The length when to switch back to classroom method
--    Result     - Left ** Power mod N, where N is Domain.Modulus
--
-- Exceptions :
--
--    Constraint_Error - Left is not less than the domain modulus.
--
   procedure Pow
             (  Domain    : Montgomery_Domain;
                Left      : Unbounded_Unsigned;
                Right     : Unbounded_Unsigned;
                Server    : in out Job_Server'Class;
                Cache     : in out Unbounded_Unsigned_Array;
                Threshold : Digit_Count;
                Result    : out Unbounded_Unsigned
             );
--
-- Square_Karatsuba -- Karatsuba algorithm for suaring
--
--    Left      - The multiplicand
--    Server    - The job server
--    Threshold - The length when to switch back to classroom method
--    Result    - Left ** 2
--
   procedure Square_Karatsuba
             (  Left      : Unbounded_Unsigned;
                Server    : in out Job_Server'Class;
                Threshold : Digit_Count;
                Result    : out Unbounded_Unsigned
             );

private
   type Half_Word_Array_Ptr    is access all Half_Word_Array;
   type Unbounded_Unsigned_Ptr is access all Unbounded_Unsigned;
   type Unbounded_Unsigned_Ref is access constant Unbounded_Unsigned;

   type Mul_Job is new Abstract_Job with record
      Left       : Vector_Ptr;
      Left_First : Digit_Count;
      Left_Last  : Digit_Count;
      Right      : Unbounded_Unsigned_Ref;
      Result     : Unbounded_Unsigned_Ptr;
   end record;

   procedure Execute
             (  Job    : in out Mul_Job;
                Server : in out Job_Server'Class
             );

   type Mul_Karatsuba_Job is new Abstract_Job with record
      Left        : Vector_Ptr;
      Left_First  : Digit_Count;
      Left_Last   : Digit_Count;
      Right       : Vector_Ptr;
      Right_First : Digit_Count;
      Right_Last  : Digit_Count;
      Threshold   : Digit_Count;
      Result      : Unbounded_Unsigned;
   end record;

   procedure Execute
             (  Job    : in out Mul_Karatsuba_Job;
                Server : in out Job_Server'Class
             );

   type Square_Karatsuba_Job is new Abstract_Job with record
      Left       : Vector_Ptr;
      Left_First : Digit_Count;
      Left_Last  : Digit_Count;
      Threshold  : Digit_Count;
      Result     : Unbounded_Unsigned;
   end record;

   procedure Execute
             (  Job    : in out Square_Karatsuba_Job;
                Server : in out Job_Server'Class
             );

end Unbounded_Unsigneds.Parallel;
