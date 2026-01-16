--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.                               Luebeck            --
--        Continued_Fraction_Edit                  Spring, 2025       --
--  Interface                                                         --
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

with Unbounded_Rationals;  use Unbounded_Rationals;

with Unbounded_Rationals.Continued_Fractions;
use  Unbounded_Rationals.Continued_Fractions;

package Strings_Edit.Continued_Fraction_Edit is
--
-- Continued fraction
--
--                1
--    T1 +  ----------------
--                   1
--         T2 + ------------
--                     1
--              T3 + -------
--                        1
--                   T4 + --
--                        T5
--
-- has syntax
--
--   [T1; T2, T3, T4, T5]
--
-- Fraction terms can be surrounded by spaces and tabs.
--
-- Get[_Rational] -- Get a continued fraction from the string
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Value   - The result
--    Base    - The base of the expected number
--
-- This  procedure  gets  an  integer number from the string Source.
--
-- Exceptions:
--
--    Constraint_Error - Zero divide
--    Data_Error       - Syntax error in the number
--    End_Error        - There is no any number
--    Layout_Error     - Pointer not in Source'First..Source'Last + 1
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Continued_Fraction;
                Base    : NumberBase := 10
             );
   procedure Get_Rational
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Unbounded_Rational;
                Base    : NumberBase := 10
             );
--
-- Value[_Rational] -- String to continued fraction conversion
--
--    Source  - The string to be processed
--  [ Value ] - The result if fraction
--    Base    - The base of the expected number
--
-- This function gets an integer number  from  the  string  Source.  The
-- number  can be surrounded by spaces and tabs. The whole string Source
-- should be matched. Otherwise the exception Data_Error is propagated.
--
-- Returns :
--
--    The value (if Value_Rational)
--
-- Exceptions:
--
--    Constraint_Error - Zero divide
--    Data_Error       - Syntax error in the number
--    End_Error        - There is no any number
--
   procedure Value
             (  Source : String;
                Value  : in out Continued_Fraction;
                Base   : NumberBase := 10
             );
   function Value_Rational
            (  Source : String;
               Base   : NumberBase := 10
            )  return Unbounded_Rational;
--
-- Put[_Rational] -- Put a continued fraction into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Value       - The value to be put
--    Base        - The base used for the output
--    PutPlus     - The plus should placed for positive numbers
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This procedure places the number specified  by  the  parameter  Value
-- into  the  output  string Destination. The string is written starting
-- from  Destination  (Pointer). The parameter Base indicates the number
-- base used for the output. Base itself does not appear in the  output.
-- The  exponent  part  (if  used)  is always decimal. PutPlus indicates
-- whether the plus sign should be placed if  the  number  is  positive.
-- If Fraction is zero, no decimal point is used.  Otherwise,  it is the
-- number of digits after the point.  The output value is rounded.  Zero
-- is alwyas output as "0".
--
-- The parameter  Field  defines  the output  size.  If it has the value
-- zero, then the output field is equal to the output length.
--
-- When the parameter Field is not zero then Justify specifies alignment
-- and Fill is the character used for filling.  When  Field  is  greater
-- than Destination'Last - Pointer + 1,  the  latter  is  used  instead.
-- After  successful  completion  Pointer  is  advanced  to  the   first
-- character following the output or to Destination'Last + 1.
--
-- Exceptions:
--
--    Layout_Error -- Pointer is not in Destination'Range or there is no
--                    room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Continued_Fraction;
                Base        : NumberBase := 10;
                PutPlus     : Boolean    := False;
                Field       : Natural    := 0;
                Justify     : Alignment  := Left;
                Fill        : Character  := ' '
             );
   procedure Put_Rational
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Unbounded_Rational;
                Base        : NumberBase := 10;
                PutPlus     : Boolean    := False;
                Field       : Natural    := 0;
                Justify     : Alignment  := Left;
                Fill        : Character  := ' '
             );
--
-- Image[_Rational] -- Continued fraction to string conversion
--
--    Value   - The value to be converted
--    Base    - The base used for the output
--    PutPlus - The plus should placed for positive numbers
--
-- This  procedure converts the parameter Value to String. The parameter
-- Base  indicates the number base used for the output. Base itself does
-- not appear in the output. The  exponent  part  (if  used)  is  always
-- decimal. PutPlus indicates whether the plus sign should be placed  if
-- the number is positive. For precision parameters see Put.
--
-- Returns :
--
--    The result string
--
   function Image
            (  Value    : Continued_Fraction;
               Base     : NumberBase := 10;
               PutPlus  : Boolean    := False
            )  return String;
   function Image_Rational
            (  Value    : Unbounded_Rational;
               Base     : NumberBase := 10;
               PutPlus  : Boolean    := False
            )  return String;

end Strings_Edit.Continued_Fraction_Edit;
