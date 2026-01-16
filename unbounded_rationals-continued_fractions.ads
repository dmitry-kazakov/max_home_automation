--                                                                    --
--  package Unbounded_Rationals.    Copyright (c)  Dmitry A. Kazakov  --
--             Continued_Fractions                 Luebeck            --
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

with Ada.Finalization;
with Generic_Unbounded_Array;

package Unbounded_Rationals.Continued_Fractions is
--
-- Continued_Fraction -- Continued fraction
--
--                  1
--    T1 +  ----------------------
--                     1
--         T2 + ------------------
--                        1
--              T3 + -------------
--                            1
--                   T4 + --------
--                        T5 + ...
--
   type Continued_Fraction is limited private;
--
-- Add_Term -- Add term to the fraction
--
--    Fraction - The fraction
--    Term     - The term to add
--  [ Sign ]   - Of the term
--
-- Exceptions :
--
--    Constraint_Error - Non-zero term, which is not the first one
--
   procedure Add
             (  Fraction : in out Continued_Fraction;
                Term     : Unbounded_Integer
             );
   procedure Add
             (  Fraction : in out Continued_Fraction;
                Term     : Half_Word;
                Sign     : Boolean
             );
--
-- Delete_Term -- Remove the last term from the fraction
--
--    Fraction - The fraction
--    Count    - The number of terms to remove
--
   procedure Delete_Term
             (  Fraction : in out Continued_Fraction;
                Count    : Natural := 1
             );
--
-- Enumerate -- Terms of a continued fraction
--
--    Value   - The rational number
--    Visitor - The visitor object
--
-- This procedure calls On_Term  of Vistior  for each term  of continued
-- fraction represntation of Value.
--
   type Term_Visitor is abstract
      new Ada.Finalization.Limited_Controlled with null record;
   procedure On_Term
             (  Visitor : in out Term_Visitor;
                Value   : Unbounded_Rational;
                Term    : Unbounded_Integer;
                Done    : out Boolean
             )  is abstract;
   procedure Enumerate
             (  Value   : Unbounded_Rational;
                Visitor : in out Term_Visitor'Class
             );
--
-- Generic_Enumerate -- Terms of a continued fraction (generic)
--
--    Value - The rational number
--
-- This procedure calls On_Term  of Vistior  for each term  of continued
-- fraction represntation of Value.  The result is  true to continue the
-- process.
--
   generic
      with function On_Term
                    (  Value : Unbounded_Rational;
                       Term  : Unbounded_Integer
                    )  return Boolean;
   procedure Generic_Enumerate (Value : Unbounded_Rational);
--
-- Erase -- Set fraction to zero
--
--    Fraction - The fraction
--
   procedure Erase (Fraction : in out Continued_Fraction);
--
-- Get_Length -- The number of terms
--
--    Fraction - The fraction
--
-- Returns :
--
--    The number of continued fraction terms
--
   function Get_Length (Fraction : Continued_Fraction) return Natural;
--
-- Get_Term -- A term of continued fraction
--
--    Fraction - The fraction
--    Index    - The term number 1..Get_Length
--
-- Returns :
--
--    The term
--
-- Exceptions :
--
--    Constraint_Error - Invalid index
--
   function Get_Term
            (  Fraction : Continued_Fraction;
               Index    : Positive
            )  return Unbounded_Integer;
--
-- Get_Value -- Get value of a fraction
--
--    Fraction - The fraction
--
-- Returns :
--
--    The corresponding rational number
--
   function Get_Value (Fraction : Continued_Fraction)
      return Unbounded_Rational;
--
-- Is_Zero -- Zero test
--
--    Fraction - To test
--
-- Returns :
--
--    True if the fraction is zero
--
   function Is_Zero (Fraction : Continued_Fraction) return Boolean;
--
-- Set -- Set fraction from a rational number
--
--    Fraction - To set
--    Value    - To convert
--
   procedure Set
             (  Fraction : in out Continued_Fraction;
                Value    : Unbounded_Rational
             );
private
   type Term
        (  Unbounded : Boolean := False;
           Sign      : Boolean := False
        )  is
   record
      case Unbounded is
         when False =>
            Short : Half_Word;
         when True =>
            Long  : Unbounded_Unsigned;
      end case;
   end record;
   function To_Term   (Value : Unbounded_Integer) return Term;
   function From_Term (Value : Term) return Unbounded_Integer;
   type Terms_Array is array (Positive range <>) of Term;

   package Fraction_Terms_Arrays is
      new Generic_Unbounded_Array
          (  Index_Type        => Positive,
             Object_Type       => Term,
             Object_Array_Type => Terms_Array,
             Null_Element      => (False, False, 0)
          );
   use Fraction_Terms_Arrays;
   type Continued_Fraction is record
      Length : Natural := 0;
      Terms  : Unbounded_Array;
   end record;

end Unbounded_Rationals.Continued_Fractions;
