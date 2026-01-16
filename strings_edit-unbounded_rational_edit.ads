--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.                               Luebeck            --
--        Unbounded_Rational_Edit                  Spring, 2025       --
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

package Strings_Edit.Unbounded_Rational_Edit is
--
-- Get -- Get a rational number from the string
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Value   - The result
--    Base    - The base of the expected number
--
-- This  procedure  gets  an  integer number from the string Source. The
-- process starts from Source (Pointer). The  parameter  Base  indicates
-- the base of the expected number. The number may have exponent part.
--
-- Exceptions:
--
--    Constraint_Error - Number is too large
--    Data_Error       - Syntax error in the number
--    End_Error        - There is no any number
--    Layout_Error     - Pointer not in Source'First..Source'Last + 1
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Unbounded_Rational;
                Base    : NumberBase := 10
             );
--
-- Value -- String to rational number conversion
--
--    Source - The string to be processed
--    Base   - The base of the expected number
--
-- This function gets an integer number  from  the  string  Source.  The
-- number  can be surrounded by spaces and tabs. The whole string Source
-- should be matched. Otherwise the exception Data_Error is propagated.
--
-- Returns :
--
--    The value
--
-- Exceptions:
--
--    Constraint_Error - Number is too large
--    Data_Error       - Syntax error in the number
--    End_Error        - There is no any number
--
   function Value
            (  Source : String;
               Base   : NumberBase := 10
            )  return Unbounded_Rational;
--
-- Put -- Put a rational number into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Value       - The value to be put
--    Base        - The base used for the output
--    PutPlus     - The plus should placed for positive numbers
--    Fraction    - Number of digits after the decimal point
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
-- The parameter Fraction specifies  the length  of the fractional part.
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
                Value       : Unbounded_Rational;
                Base        : NumberBase := 10;
                PutPlus     : Boolean    := False;
                Fraction    : Natural    := 6;
                Field       : Natural    := 0;
                Justify     : Alignment  := Left;
                Fill        : Character  := ' '
             );
--
-- Image -- Rational number to string conversion
--
--    Value    - The value to be converted
--    Base     - The base used for the output
--    PutPlus  - The plus should placed for positive numbers
--    Fraction - Number of digits after the decimal point
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
            (  Value    : Unbounded_Rational;
               Base     : NumberBase := 10;
               PutPlus  : Boolean    := False;
               Fraction : Natural    := 6
            )  return String;


end Strings_Edit.Unbounded_Rational_Edit;
