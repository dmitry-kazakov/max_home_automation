--                                                                    --
--  package Unbounded_Integers      Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

package body Unbounded_Integers is

   function "="  (Left, Right : Unbounded_Integer) return Boolean is
   begin
      return Left.Sign = Right.Sign and then
             Left.Mantissa = Right.Mantissa;
   end "=";

   function "=" (Left : Unbounded_Integer; Right : Integer)
      return Boolean is
   begin
      return Compare (Left, Right) = Equal;
   end "=";

   function "=" (Left : Integer; Right : Unbounded_Integer)
      return Boolean is
   begin
      return Compare (Left, Right) = Equal;
   end "=";

   function "<" (Left, Right : Unbounded_Integer) return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function "<" (Left : Unbounded_Integer; Right : Integer)
      return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function "<" (Left : Integer; Right : Unbounded_Integer)
      return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function "<=" (Left, Right : Unbounded_Integer) return Boolean is
   begin
      return Compare (Left, Right) /= Greater;
   end "<=";

   function "<=" (Left : Unbounded_Integer; Right : Integer)
      return Boolean is
   begin
      return Compare (Left, Right) /= Greater;
   end "<=";

   function "<="  (Left : Integer; Right : Unbounded_Integer)
      return Boolean is
   begin
      return Compare (Left, Right) /= Greater;
   end "<=";

   function ">" (Left, Right : Unbounded_Integer) return Boolean is
   begin
      return Compare (Left, Right) = Greater;
   end ">";

   function ">" (Left : Unbounded_Integer; Right : Integer)
      return Boolean is
   begin
      return Compare (Left, Right) = Greater;
   end ">";

   function ">"  (Left : Integer; Right : Unbounded_Integer)
      return Boolean is
   begin
      return Compare (Left, Right) = Greater;
   end ">";

   function ">=" (Left, Right : Unbounded_Integer) return Boolean is
   begin
      return Compare (Left, Right) /= Less;
   end ">=";

   function ">=" (Left : Unbounded_Integer; Right : Integer)
      return Boolean is
   begin
      return Compare (Left, Right) /= Less;
   end ">=";

   function ">="  (Left : Integer; Right : Unbounded_Integer)
      return Boolean is
   begin
      return Compare (Left, Right) /= Less;
   end ">=";

   function "abs" (Left : Unbounded_Integer) return Unbounded_Integer is
   begin
      return (False, Left.Mantissa);
   end "abs";

   function "+" (Left : Unbounded_Integer) return Unbounded_Integer is
   begin
      return Left;
   end "+";

   function "-" (Left : Unbounded_Integer) return Unbounded_Integer is
   begin
       if Is_Zero (Left) then
          return Left;
       else
          return (not Left.Sign, Left.Mantissa);
       end if;
   end "-";

   function "+" (Left, Right : Unbounded_Integer)
      return Unbounded_Integer is
   begin
      if Left.Sign then
         if Right.Sign then
            return (True, Left.Mantissa + Right.Mantissa);
         else
            case Compare (Left.Mantissa, Right.Mantissa) is
               when Less =>
                  return (False, Right.Mantissa - Left.Mantissa);
               when Equal =>
                  return Zero;
               when Greater =>
                  return (True, Left.Mantissa - Right.Mantissa);
            end case;
         end if;
      else
         if Right.Sign then
            case Compare (Left.Mantissa, Right.Mantissa) is
               when Less =>
                  return (True, Right.Mantissa - Left.Mantissa);
               when Equal =>
                  return Zero;
               when Greater =>
                  return (False, Left.Mantissa - Right.Mantissa);
            end case;
         else
            return (False, Left.Mantissa + Right.Mantissa);
         end if;
      end if;
   end "+";

   function "+" (Left : Unbounded_Integer; Right : Integer)
      return Unbounded_Integer is
   begin
      return Left + To_Unbounded_Integer (Right);
   end "+";

   function "+" (Left : Integer; Right : Unbounded_Integer)
      return Unbounded_Integer is
   begin
      return To_Unbounded_Integer (Left) + Right;
   end "+";

   function "-" (Left, Right : Unbounded_Integer)
      return Unbounded_Integer is
   begin
      if Left.Sign then
         if Right.Sign then
            case Compare (Left.Mantissa, Right.Mantissa) is
               when Less =>
                  return (False, Right.Mantissa - Left.Mantissa);
               when Equal =>
                  return Zero;
               when Greater =>
                  return (True, Left.Mantissa - Right.Mantissa);
            end case;
         else
            return (True, Left.Mantissa + Right.Mantissa);
         end if;
      else
         if Right.Sign then
            return (False, Left.Mantissa + Right.Mantissa);
         else
            case Compare (Left.Mantissa, Right.Mantissa) is
               when Less =>
                  return (True, Right.Mantissa - Left.Mantissa);
               when Equal =>
                  return Zero;
               when Greater =>
                  return (False, Left.Mantissa - Right.Mantissa);
            end case;
         end if;
      end if;
   end "-";

   function "-" (Left : Unbounded_Integer; Right : Integer)
      return Unbounded_Integer is
   begin
      return Left - To_Unbounded_Integer (Right);
   end "-";

   function "-" (Left : Integer; Right : Unbounded_Integer)
      return Unbounded_Integer is
   begin
      return To_Unbounded_Integer (Left) - Right;
   end "-";

   function "*" (Left, Right : Unbounded_Integer)
      return Unbounded_Integer is
   begin
      if Is_Zero (Left) or else Is_Zero (Right) then
         return Zero;
      else
         return
         (  Left.Sign xor Right.Sign,
            Left.Mantissa * Right.Mantissa
         );
      end if;
   end "*";

   function "*" (Left : Unbounded_Integer; Right : Integer)
      return Unbounded_Integer is
   begin
      return Left * To_Unbounded_Integer (Right);
   end "*";

   function "*" (Left : Integer; Right : Unbounded_Integer)
      return Unbounded_Integer is
   begin
      return To_Unbounded_Integer (Left) * Right;
   end "*";

   function "/" (Left, Right : Unbounded_Integer)
      return Unbounded_Integer is
   begin
      if Is_Zero (Right) then
         raise Constraint_Error;
      elsif Is_Zero (Left) then
         return Zero;
      else
         return
         (  Left.Sign xor Right.Sign,
            Left.Mantissa / Right.Mantissa
         );
      end if;
   end "/";

   function "/" (Left : Unbounded_Integer; Right : Integer)
      return Unbounded_Integer is
   begin
      return Left / To_Unbounded_Integer (Right);
   end "/";

   function "/" (Left : Integer; Right : Unbounded_Integer)
      return Unbounded_Integer is
   begin
      return To_Unbounded_Integer (Left) / Right;
   end "/";

   function "mod" (Left, Right : Unbounded_Integer)
      return Unbounded_Integer is
      Remainder : constant Unbounded_Unsigned :=
                           Left.Mantissa mod Right.Mantissa;
   begin
      if Is_Zero (Remainder) then
         return Zero;
      elsif Left.Sign then
         if Right.Sign then
            return (True, Remainder);
         else
            return (False, Right.Mantissa - Remainder);
         end if;
      else
         if Right.Sign then
            return (True, Right.Mantissa - Remainder);
         else
            return (False, Remainder);
         end if;
      end if;
   end "mod";

   function "mod" (Left : Unbounded_Integer; Right : Integer)
      return Unbounded_Integer is
   begin
      return Left mod To_Unbounded_Integer (Right);
   end "mod";

   function "mod" (Left : Integer; Right : Unbounded_Integer)
      return Unbounded_Integer is
   begin
      return To_Unbounded_Integer (Left) mod Right;
   end "mod";

   function "rem" (Left, Right : Unbounded_Integer)
       return Unbounded_Integer is
      Remainder : constant Unbounded_Unsigned :=
                           Left.Mantissa mod Right.Mantissa;
   begin
      if Is_Zero (Remainder) then
         return Zero;
      elsif Left.Sign then
         return (True, Remainder);
      else
         return (False, Remainder);
      end if;
   end "rem";

   function "rem" (Left : Unbounded_Integer; Right : Integer)
      return Unbounded_Integer is
   begin
      return Left rem To_Unbounded_Integer (Right);
   end "rem";

   function "rem" (Left : Integer; Right : Unbounded_Integer)
      return Unbounded_Integer is
   begin
      return To_Unbounded_Integer (Left) rem Right;
   end "rem";

   function "**" (Left : Unbounded_Integer; Right : Bit_Count)
      return Unbounded_Integer is
   begin
      if Right = 0 then
         return One;
      elsif Is_Zero (Left) then
         return Zero;
      elsif Is_One (Left.Mantissa) then
         if Left.Sign and Right mod 2 = 1 then
            return (True, Unbounded_Unsigneds.One);
         else
            return One;
         end if;
      else
         if Left.Sign and Right mod 2 = 1 then
            return (True, Left.Mantissa ** Right);
         else
            return (False, Left.Mantissa ** Right);
         end if;
      end if;
   end "**";

   function Compare (Left, Right : Unbounded_Integer)
      return Precedence is
   begin
      if Left.Sign then
         if Right.Sign then
            return Compare (Left.Mantissa, Right.Mantissa);
         else
            return Less;
         end if;
      else
         if Right.Sign then
            return Greater;
         else
            return Compare (Right.Mantissa, Left.Mantissa);
         end if;
      end if;
   end Compare;

   function Compare (Left : Unbounded_Integer; Right : Integer)
      return Precedence is
   begin
      if Left.Sign then
         if Right < 0 then
            if Get_Length (Left.Mantissa) > 2 then
               return Less;
            else
               declare
                  L : constant Word := To_Word (Left.Mantissa);
                  R : constant Word := Word (-1 - Right) + 1;
               begin
                  if L = R then
                     return Equal;
                  elsif L < R then
                     return Greater;
                  else
                     return Less;
                  end if;
               end;
            end if;
         else
            return Less;
         end if;
      else
         if Right < 0 then
            return Greater;
         else
            if Get_Length (Left.Mantissa) > 2 then
               return Greater;
            else
               declare
                  L : constant Word := To_Word (Left.Mantissa);
                  R : constant Word := Word (Right);
               begin
                  if L = R then
                     return Equal;
                  elsif L < R then
                     return Less;
                  else
                     return Greater;
                  end if;
               end;
            end if;
         end if;
      end if;
   end Compare;

   function Compare (Left : Integer; Right : Unbounded_Integer)
      return Precedence is
   begin
      case Compare (Right, Left) is
         when Less =>
            return Greater;
         when Equal =>
            return Equal;
         when Greater =>
            return Less;
      end case;
   end Compare;

   function Compose
            (  Mantissa : Unbounded_Unsigned;
               Negative : Boolean := False
            )  return Unbounded_Integer is
   begin
      if Is_Zero (Mantissa) then
         return Zero;
      else
         return (Negative, Mantissa);
      end if;
   end Compose;

   procedure Copy
             (  Destination : in out Unbounded_Integer;
                Source      : Unbounded_Integer
             )  is
   begin
      Destination.Sign := Source.Sign;
      Copy (Destination.Mantissa, Source.Mantissa);
   end Copy;

   function Div_By_Power_of_Two
            (  Dividend : Unbounded_Integer;
               Power    : Bit_Count
            )  return Unbounded_Integer is
   begin
      return
      (  Dividend.Sign,
         Div_By_Power_of_Two (Dividend.Mantissa, Power)
      );
   end Div_By_Power_of_Two;

   procedure Erase (Left : in out Unbounded_Integer) is
   begin
      Left.Sign := False;
      Erase (Left.Mantissa);
   end Erase;

   function Greatest_Common_Divisor
            (  Left, Right : Unbounded_Integer
            )  return Unbounded_Integer is
   begin
      return
      (  False,
         Greatest_Common_Divisor (Left.Mantissa, Right.Mantissa)
      );
   end Greatest_Common_Divisor;

   function Get_Mantissa (Left : Unbounded_Integer)
      return Unbounded_Unsigned is
   begin
      return Left.Mantissa;
   end Get_Mantissa;

   function Is_Even (Left : Unbounded_Integer) return Boolean is
   begin
      return Is_Even (Left.Mantissa);
   end Is_Even;

   function Is_Negative (Left : Unbounded_Integer) return Boolean is
   begin
      return Left.Sign;
   end Is_Negative;

   function Is_Odd (Left : Unbounded_Integer) return Boolean is
   begin
      return Is_Odd (Left.Mantissa);
   end Is_Odd;

   function Is_One (Left : Unbounded_Integer) return Boolean is
   begin
      return not Left.Sign and then Is_One (Left.Mantissa);
   end Is_One;

   function Is_Two (Left : Unbounded_Integer) return Boolean is
   begin
      return not Left.Sign and then Is_Two (Left.Mantissa);
   end Is_Two;

   function Is_Zero (Left : Unbounded_Integer) return Boolean is
   begin
      return Is_Zero (Left.Mantissa);
   end Is_Zero;

   function Max (Left, Right : Unbounded_Integer)
      return Unbounded_Integer is
   begin
      if Compare (Left, Right) = Greater then
         return Left;
      else
         return Right;
      end if;
   end Max;

   function Min (Left, Right : Unbounded_Integer)
      return Unbounded_Integer is
   begin
      if Compare (Left, Right) = Less then
         return Left;
      else
         return Right;
      end if;
   end Min;

   function Mul_By_Power_of_Two
            (  Multiplicand : Unbounded_Integer;
               Power        : Bit_Count
            )  return Unbounded_Integer is
   begin
      return
      (  Multiplicand.Sign,
         Mul_By_Power_of_Two (Multiplicand.Mantissa, Power)
      );
   end Mul_By_Power_of_Two;

   function Power_of_Two (Power : Bit_Count) return Unbounded_Integer is
   begin
       return (False, Power_of_Two (Power));
   end Power_of_Two;

   procedure Set
             (  Destination : in out Unbounded_Integer;
                Source      : Unbounded_Integer
             )  is
   begin
      Destination.Sign := Source.Sign;
      Set (Destination.Mantissa, Source.Mantissa);
   end Set;

   function Sqrt (Left : Unbounded_Integer)
      return Unbounded_Integer is
      Root      : Unbounded_Integer;
      Remainder : Unbounded_Integer;
   begin
      Sqrt (Left, Root, Remainder);
      return Root;
   end Sqrt;

   procedure Sqrt
             (  Left      : Unbounded_Integer;
                Root      : out Unbounded_Integer;
                Remainder : out Unbounded_Integer
             )  is
   begin
      if Left.Sign then
         raise Constraint_Error;
      else
         Root.Sign := False;
         Remainder.Sign := False;
         Sqrt (Left.Mantissa, Root.Mantissa, Remainder.Mantissa);
      end if;
   end Sqrt;

   function Square (Left : Unbounded_Integer)
      return Unbounded_Integer is
   begin
      return (False, Square (Left.Mantissa));
   end Square;

   procedure Swap (Left, Right : in out Unbounded_Integer) is
      Sign : constant Boolean := Left.Sign;
   begin
      Left.Sign  := Right.Sign;
      Right.Sign := Sign;
      Swap (Left.Mantissa, Right.Mantissa);
   end Swap;

   package body Signed_Conversions is

      function From_Unbounded_Integer (Left : Unbounded_Integer)
         return Number is
         Result : Word;
      begin
         case Get_Length (Left.Mantissa) is
            when 0 =>
               return 0;
            when 1 =>
               Result := Word (Get_Digit (Left.Mantissa, 1));
            when 2 =>
               Result := Word (Get_Digit (Left.Mantissa, 1))
                       + Half_Word_Modulus
                       * Word (Get_Digit (Left.Mantissa, 2));
            when others =>
               raise Constraint_Error;
         end case;
         if Left.Sign then
            return -1 - Number (Result - 1);
         else
            return Number (Result);
         end if;
      end From_Unbounded_Integer;

      function To_Unbounded_Integer (Left : Number)
         return Unbounded_Integer is
         Mantissa : Word;
         Result   : Unbounded_Unsigned;
      begin
         if Left = 0 then
            return Zero;
         elsif Left < 0 then
            Mantissa := Word (-1 - Left) + 1;
         else
            Mantissa := Word (Left);
         end if;
         if Mantissa >= Half_Word_Modulus then
            Add (Result, Half_Word (Mantissa / Half_Word_Modulus));
            Shift_Left (Result, 1);
         end if;
         Add (Result, Half_Word (Mantissa mod Half_Word_Modulus));
         return (Left < 0, Result);
      end To_Unbounded_Integer;

   end Signed_Conversions;

   package body Unsigned_Conversions is
      package Conversions is
         new Unbounded_Unsigneds.Unsigned_Conversions (Number);

      function From_Unbounded_Integer (Left : Unbounded_Integer)
         return Number is
      begin
         if Left.Sign then
            raise Constraint_Error;
         else
            return Conversions.From_Unbounded_Unsigned (Left.Mantissa);
         end if;
      end From_Unbounded_Integer;

      function To_Unbounded_Integer (Left : Number)
        return Unbounded_Integer is
      begin
         return (False, Conversions.To_Unbounded_Unsigned (Left));
      end To_Unbounded_Integer;

   end Unsigned_Conversions;

   package Integer_Conversions is new Signed_Conversions (Integer);

   function To_Integer (Left : Unbounded_Integer) return Integer
      renames Integer_Conversions.From_Unbounded_Integer;

   function To_Unbounded_Integer (Left : Integer)
      return Unbounded_Integer renames
         Integer_Conversions.To_Unbounded_Integer;

end Unbounded_Integers;
