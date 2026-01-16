--                                                                    --
--  package Unbounded_Rationals     Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2025       --
--                                                                    --
--                                Last revision :  08:56 30 Jun 2025  --
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

package body Unbounded_Rationals is

   function "="  (Left, Right : Unbounded_Rational) return Boolean is
   begin
      return Left.Sign        = Right.Sign      and then
             Left.Numerator   = Right.Numerator and then
             Left.Denominator = Right.Denominator;
   end "=";

   function "=" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Boolean is
   begin
      return Left.Sign        = Is_Negative (Right)  and then
             Left.Numerator   = Get_Mantissa (Right) and then
             Left.Denominator = Unbounded_Unsigneds.One;
   end "=";

   function "=" (Left : Unbounded_Integer; Right : Unbounded_Rational)
      return Boolean is
   begin
      return Right.Sign        = Is_Negative (Left)  and then
             Right.Numerator   = Get_Mantissa (Left) and then
             Right.Denominator = Unbounded_Unsigneds.One;
   end "=";

   function "=" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Boolean is
   begin
      return not Left.Sign            and then
             Left.Numerator   = Right and then
             Left.Denominator = Unbounded_Unsigneds.One;
   end "=";

   function "=" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Boolean is
   begin
      return not Right.Sign           and then
             Right.Numerator   = Left and then
             Right.Denominator = Unbounded_Unsigneds.One;
   end "=";

   function "=" (Left : Unbounded_Rational; Right : Half_Word)
      return Boolean is
   begin
      return not Left.Sign            and then
             Left.Numerator   = Right and then
             Left.Denominator = Unbounded_Unsigneds.One;
   end "=";

   function "=" (Left : Half_Word; Right : Unbounded_Rational)
      return Boolean is
   begin
      return not Right.Sign           and then
             Right.Numerator   = Left and then
             Right.Denominator = Unbounded_Unsigneds.One;
   end "=";

   function "<" (Left, Right : Unbounded_Rational) return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function "<" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function "<" (Left : Unbounded_Integer; Right : Unbounded_Rational)
      return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function "<" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function "<" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function "<" (Left : Unbounded_Rational; Right : Half_Word)
      return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function "<" (Left : Half_Word; Right : Unbounded_Rational)
      return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function "<=" (Left, Right : Unbounded_Rational) return Boolean is
   begin
      return Compare (Left, Right) /= Greater;
   end "<=";

   function "<=" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Boolean is
   begin
      return Compare (Left, Right) /= Greater;
   end "<=";

   function "<=" (Left : Unbounded_Integer; Right : Unbounded_Rational)
      return Boolean is
   begin
      return Compare (Left, Right) /= Greater;
   end "<=";

   function "<=" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Boolean is
   begin
      return Compare (Left, Right) /= Greater;
   end "<=";

   function "<=" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Boolean is
   begin
      return Compare (Left, Right) /= Greater;
   end "<=";

   function "<=" (Left : Unbounded_Rational; Right : Half_Word)
      return Boolean is
   begin
      return Compare (Left, Right) /= Greater;
   end "<=";

   function "<=" (Left : Half_Word; Right : Unbounded_Rational)
      return Boolean is
   begin
      return Compare (Left, Right) /= Greater;
   end "<=";

   function ">" (Left, Right : Unbounded_Rational) return Boolean is
   begin
      return Compare (Left, Right) = Greater;
   end ">";

   function ">" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Boolean is
   begin
      return Compare (Left, Right) = Greater;
   end ">";

   function ">" (Left : Unbounded_Integer; Right : Unbounded_Rational)
      return Boolean is
   begin
      return Compare (Left, Right) = Greater;
   end ">";

   function ">" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Boolean is
   begin
      return Compare (Left, Right) = Greater;
   end ">";

   function ">" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Boolean is
   begin
      return Compare (Left, Right) = Greater;
   end ">";

   function ">" (Left : Unbounded_Rational; Right : Half_Word)
      return Boolean is
   begin
      return Compare (Left, Right) = Greater;
   end ">";

   function ">" (Left : Half_Word; Right : Unbounded_Rational)
      return Boolean is
   begin
      return Compare (Left, Right) = Greater;
   end ">";

   function ">=" (Left, Right : Unbounded_Rational) return Boolean is
   begin
      return Compare (Left, Right) /= Less;
   end ">=";

   function ">=" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Boolean is
   begin
      return Compare (Left, Right) /= Less;
   end ">=";

   function ">=" (Left : Unbounded_Integer; Right : Unbounded_Rational)
      return Boolean is
   begin
      return Compare (Left, Right) /= Less;
   end ">=";

   function ">=" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Boolean is
   begin
      return Compare (Left, Right) /= Less;
   end ">=";

   function ">=" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Boolean is
   begin
      return Compare (Left, Right) /= Less;
   end ">=";

   function ">=" (Left : Unbounded_Rational; Right : Half_Word)
      return Boolean is
   begin
      return Compare (Left, Right) /= Less;
   end ">=";

   function ">=" (Left : Half_Word; Right : Unbounded_Rational)
      return Boolean is
   begin
      return Compare (Left, Right) /= Less;
   end ">=";

   function "abs" (Left : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      return (False, Left.Numerator, Left.Denominator);
   end "abs";

   function "+" (Left : Unbounded_Rational) return Unbounded_Rational is
   begin
      return Left;
   end "+";

   function "+" (N : Half_Word) return Unbounded_Rational is
   begin
      return (  Numerator   => From_Half_Word (N),
                Denominator => Unbounded_Unsigneds.One,
                Sign        => False
             );
   end "+";

   function "-" (Left : Unbounded_Rational) return Unbounded_Rational is
   begin
       if Is_Zero (Left) then
          return Left;
       else
          return (not Left.Sign, Left.Numerator, Left.Denominator);
       end if;
   end "-";

   function "+" (Left, Right : Unbounded_Rational)
      return Unbounded_Rational is
      Left_Numerator  : constant Unbounded_Unsigned :=
                        Left.Numerator   * Right.Denominator;
      Right_Numerator : constant Unbounded_Unsigned :=
                        Right.Numerator  * Left.Denominator;
      Denominator     : constant Unbounded_Unsigned :=
                        Left.Denominator * Right.Denominator;
   begin
      if Left.Sign then
         if Right.Sign then
            return Compose
                   (  Left_Numerator + Right_Numerator,
                      Denominator,
                      True
                   );
         else
            case Compare (Left_Numerator, Right_Numerator) is
               when Less =>
                  return Compose
                         (  Right_Numerator - Left_Numerator,
                            Denominator,
                            False
                         );
               when Equal =>
                  return Zero;
               when Greater =>
                  return Compose
                         (  Left_Numerator - Right_Numerator,
                            Denominator,
                            True
                         );
            end case;
         end if;
      else
         if Right.Sign then
            case Compare (Left_Numerator, Right_Numerator) is
               when Less =>
                  return Compose
                         (  Right_Numerator - Left_Numerator,
                            Denominator,
                            True
                         );
               when Equal =>
                  return Zero;
               when Greater =>
                  return Compose
                         (  Left_Numerator - Right_Numerator,
                            Denominator,
                            False
                         );
            end case;
         else
            return Compose
                   (  Left_Numerator + Right_Numerator,
                      Denominator,
                      False
                   );
         end if;
      end if;
   end "+";

   function "+" (Left : Unbounded_Rational; Right : Half_Word)
      return Unbounded_Rational is
      Right_Numerator : constant Unbounded_Unsigned :=
                                 Left.Denominator * Right;
   begin
      if Left.Sign then
         case Compare (Left.Numerator, Right_Numerator) is
            when Less =>
               return Compose
                      (  Right_Numerator - Left.Numerator,
                         Left.Denominator,
                         False
                      );
            when Equal =>
               return Zero;
            when Greater =>
               return Compose
                      (  Left.Numerator - Right_Numerator,
                         Left.Denominator,
                         True
                      );
         end case;
      else
         return Compose
                (  Left.Numerator + Right_Numerator,
                   Left.Denominator,
                   False
                );
      end if;
   end "+";

   function "+" (Left : Half_Word; Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      return Right + Left;
   end "+";

   function "+" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Unbounded_Rational is
      Right_Numerator : constant Unbounded_Unsigned :=
                                 Right * Left.Denominator;
   begin
      if Left.Sign then
         case Compare (Left.Numerator, Right_Numerator) is
            when Less =>
               return Compose
                      (  Right_Numerator - Left.Numerator,
                         Left.Denominator,
                         False
                      );
            when Equal =>
               return Zero;
            when Greater =>
               return Compose
                      (  Left.Numerator - Right_Numerator,
                         Left.Denominator,
                         True
                      );
         end case;
      else
         return Compose
                (  Left.Numerator + Right_Numerator,
                   Left.Denominator,
                   False
                );
      end if;
   end "+";

   function "+" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      return Right + Left;
   end "+";

   function "+" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Unbounded_Rational is
   begin
      if Is_Negative (Right) then
         return Left - Get_Mantissa (Right);
      else
         return Left + Get_Mantissa (Right);
      end if;
   end "+";

   function "+" (Left : Unbounded_Integer; Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      return Right + Left;
   end "+";

   function "-" (Left, Right : Unbounded_Rational)
       return Unbounded_Rational is
      Left_Numerator  : constant Unbounded_Unsigned :=
                        Left.Numerator * Right.Denominator;
      Right_Numerator : constant Unbounded_Unsigned :=
                        Right.Numerator * Left.Denominator;
      Denominator     : constant Unbounded_Unsigned :=
                        Left.Denominator * Right.Denominator;
   begin
      if Left.Sign then
         if Right.Sign then
            case Compare (Left_Numerator, Right_Numerator) is
               when Less =>
                  return Compose
                         (  Right_Numerator - Left_Numerator,
                            Denominator,
                            False
                         );
               when Equal =>
                  return Zero;
               when Greater =>
                  return Compose
                         (  Left_Numerator - Right_Numerator,
                            Denominator,
                            True
                         );
            end case;
         else
            return Compose
                   (  Left_Numerator + Right_Numerator,
                      Denominator,
                      True
                   );
         end if;
      else
         if Right.Sign then
            return Compose
                   (  Left_Numerator + Right_Numerator,
                      Denominator,
                      False
                   );
         else
            case Compare (Left_Numerator, Right_Numerator) is
               when Less =>
                  return Compose
                         (  Right_Numerator - Left_Numerator,
                            Denominator,
                            True
                         );
               when Equal =>
                  return Zero;
               when Greater =>
                  return Compose
                         (  Left_Numerator - Right_Numerator,
                            Denominator,
                            False
                         );
            end case;
         end if;
      end if;
   end "-";

   function "-" (Left : Unbounded_Rational; Right : Half_Word)
       return Unbounded_Rational is
      Right_Numerator : constant Unbounded_Unsigned :=
                        Left.Denominator * Right;
   begin
      if Left.Sign then
         return Compose
                (  Left.Numerator + Right_Numerator,
                   Left.Denominator,
                   True
                );
      else
         case Compare (Left.Numerator, Right_Numerator) is
            when Less =>
               return Compose
                      (  Right_Numerator - Left.Numerator,
                         Left.Denominator,
                         True
                      );
            when Equal =>
               return Zero;
            when Greater =>
               return Compose
                      (  Left.Numerator - Right_Numerator,
                         Left.Denominator,
                         False
                      );
         end case;
      end if;
   end "-";

   function "-" (Left : Half_Word; Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      return -(Right - Left);
   end "-";

   function "-" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
       return Unbounded_Rational is
      Right_Numerator : constant Unbounded_Unsigned :=
                        Right * Left.Denominator;
   begin
      if Left.Sign then
         return Compose
                (  Left.Numerator + Right_Numerator,
                   Left.Denominator,
                   True
                );
      else
         case Compare (Left.Numerator, Right_Numerator) is
            when Less =>
               return Compose
                      (  Right_Numerator - Left.Numerator,
                         Left.Denominator,
                         True
                      );
            when Equal =>
               return Zero;
            when Greater =>
               return Compose
                      (  Left.Numerator - Right_Numerator,
                         Left.Denominator,
                         False
                      );
         end case;
      end if;
   end "-";

   function "-" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      return -(Right - Left);
   end "-";

   function "-" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Unbounded_Rational is
   begin
      if Is_Negative (Right) then
         return Left + Get_Mantissa (Right);
      else
         return Left - Get_Mantissa (Right);
      end if;
   end "-";

   function "-" (Left : Unbounded_Integer; Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      if Is_Negative (Left) then
         return -Right + Get_Mantissa (Left);
      else
         return -Right - Get_Mantissa (Left);
      end if;
   end "-";

   function "*" (Left, Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      if Is_Zero (Left) or else Is_Zero (Right) then
         return Zero;
      elsif Is_One (Left.Numerator)   and then
            Is_One (Left.Denominator) then
         return (  Numerator   => Right.Numerator,
                   Denominator => Right.Denominator,
                   Sign        => Left.Sign xor Right.Sign
                );
      elsif Is_One (Right.Numerator)   and then
            Is_One (Right.Denominator) then
         return (  Numerator   => Left.Numerator,
                   Denominator => Left.Denominator,
                   Sign         => Left.Sign xor Right.Sign
                );
      else
         return Compose
                (  Numerator   => Left.Numerator   * Right.Numerator,
                   Denominator => Left.Denominator * Right.Denominator,
                   Negative    => Left.Sign xor Right.Sign
                );
      end if;
   end "*";

   function "*" (Left : Unbounded_Rational; Right : Half_Word)
      return Unbounded_Rational is
   begin
      if Is_Zero (Left) or else Right = 0 then
         return Zero;
      else
         return Compose
                (  Left.Numerator * Right,
                   Left.Denominator,
                   Left.Sign
                );
      end if;
   end "*";

   function "*" (Left : Half_Word; Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      if Is_Zero (Right) or else Left = 0 then
         return Zero;
      else
         return Compose
                (  Right.Numerator * Left,
                   Right.Denominator,
                   Right.Sign
                );
      end if;
   end "*";

   function "*" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Unbounded_Rational is
   begin
      if Is_Zero (Left) or else Is_Zero (Right) then
         return Zero;
      else
         return Compose
                (  Left.Numerator * Right,
                   Left.Denominator,
                   Left.Sign
                );
      end if;
   end "*";

   function "*" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      if Is_Zero (Left) or else Is_Zero (Right) then
         return Zero;
      else
         return Compose
                (  Right.Numerator * Left,
                   Right.Denominator,
                   Right.Sign
                );
      end if;
   end "*";

   function "*" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Unbounded_Rational is
   begin
      if Is_Negative (Right) then
         return -Left * Get_Mantissa (Right);
      else
         return Left * Get_Mantissa (Right);
      end if;
   end "*";

   function "*" (Left : Unbounded_Integer; Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      if Is_Negative (Left) then
         return -Get_Mantissa (Left) * Right;
      else
         return Get_Mantissa (Left) * Right;
      end if;
   end "*";

   function "/" (Left, Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      if Is_Zero (Right) then
         raise Constraint_Error;
      elsif Is_Zero (Left) then
         return Zero;
      elsif Is_One (Left.Numerator)   and then
            Is_One (Left.Denominator) then
         return (  Numerator   => Right.Denominator,
                   Denominator => Right.Numerator,
                   Sign        => Left.Sign xor Right.Sign
                );
      elsif Is_One (Right.Numerator)   and then
            Is_One (Right.Denominator) then
         return (  Numerator   => Left.Numerator,
                   Denominator => Left.Denominator,
                   Sign        => Left.Sign xor Right.Sign
                );
      else
         return Compose
                (  Numerator   => Left.Numerator   * Right.Denominator,
                   Denominator => Left.Denominator * Right.Numerator,
                   Negative    => Left.Sign xor Right.Sign
                );
      end if;
   end "/";

   function "/" (Left : Unbounded_Rational; Right : Half_Word)
      return Unbounded_Rational is
   begin
      if Right = 0 then
         raise Constraint_Error;
      elsif Is_Zero (Left) then
         return Zero;
      else
         return Compose
                (  Left.Numerator,
                   Left.Denominator * Right,
                   Left.Sign
                );
      end if;
   end "/";

   function "/" (Left : Half_Word; Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      if Is_Zero (Right.Numerator) then
         raise Constraint_Error;
      elsif Left = 0 then
         return Zero;
      else
         return Compose
                (  Right.Denominator * Left,
                   Right.Numerator,
                   Right.Sign
                );
      end if;
   end "/";

   function "/" (Left : Unbounded_Rational; Right : Unbounded_Unsigned)
      return Unbounded_Rational is
   begin
      if Is_Zero (Right) then
         raise Constraint_Error;
      elsif Is_Zero (Left) then
         return Zero;
      else
         return Compose
                (  Left.Numerator,
                   Left.Denominator * Right,
                   Left.Sign
                );
      end if;
   end "/";

   function "/" (Left : Unbounded_Unsigned; Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      if Is_Zero (Right.Numerator) then
         raise Constraint_Error;
      elsif Left = 0 then
         return Zero;
      else
         return Compose
                (  Right.Denominator * Left,
                   Right.Numerator,
                   Right.Sign
                );
      end if;
   end "/";

   function "/" (Left : Unbounded_Rational; Right : Unbounded_Integer)
      return Unbounded_Rational is
   begin
      if Is_Negative (Right) then
         return -Left / Get_Mantissa (Right);
      else
         return Left / Get_Mantissa (Right);
      end if;
   end "/";

   function "/" (Left : Unbounded_Integer; Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      if Is_Negative (Left) then
         return -Get_Mantissa (Left) / Right;
      else
         return Get_Mantissa (Left) / Right;
      end if;
   end "/";

   function "**" (Left : Unbounded_Rational; Right : Integer)
      return Unbounded_Rational is
      Power : Bit_Count;
   begin
      if Right = 0 then
         return One;
      elsif Is_Zero (Left) then
         return Zero;
      elsif Is_One (Left.Denominator) and then
            Is_One (Left.Numerator)   then
         if Left.Sign and Right mod 2 = 1 then
            return
            (  True,
               Unbounded_Unsigneds.One,
               Unbounded_Unsigneds.One
            );
         else
            return One;
         end if;
      else
         if Left.Sign and Right mod 2 = 1 then
            if Right > 0 then
               Power := Bit_Count (Right);
               return (  Numerator   => Left.Numerator   ** Power,
                         Denominator => Left.Denominator ** Power,
                         Sign        => True
                      );
            else
               Power := Bit_Count (-Right);
               return (  Numerator   => Left.Denominator ** Power,
                         Denominator => Left.Numerator   ** Power,
                         Sign        => True
                      );
            end if;
         else
            if Right > 0 then
               Power := Bit_Count (Right);
               return (  Numerator   => Left.Numerator   ** Power,
                         Denominator => Left.Denominator ** Power,
                         Sign        => False
                      );
            else
               Power := Bit_Count (-Right);
               return (  Numerator   => Left.Denominator ** Power,
                         Denominator => Left.Numerator   ** Power,
                         Sign        => False
                      );
            end if;
         end if;
      end if;
   end "**";

   function Ceiling (Left : Unbounded_Rational)
      return Unbounded_Integer is
      Result : constant Unbounded_Integer :=
                        To_Unbounded_Integer (Left);
   begin
      if Is_One (Left.Denominator) then
         return Result;
      elsif Is_Negative (Result) then
         return Result;
      else
         return Result + 1;
      end if;
   end Ceiling;

   function Compare
            (  Numerator   : Unbounded_Unsigned;
               Denominator : Unbounded_Unsigned;
               Right       : Unbounded_Rational
            )  return Precedence is
      pragma Inline (Compare);
   begin
      if Right.Sign then
         return Greater;
      else
         return Compare
                (  Numerator       * Right.Denominator,
                   Right.Numerator * Denominator

                );
      end if;
   end Compare;

   function Compare (Left, Right : Unbounded_Rational)
      return Precedence is
   begin
      if Left.Sign then
         if Right.Sign then
            return Compare
                   (  Right.Numerator * Left.Denominator,
                      Left.Numerator  * Right.Denominator
                   );
         else
            return Less;
         end if;
      else
         if Right.Sign then
            return Greater;
         else
            return Compare
                   (  Left.Numerator  * Right.Denominator,
                      Right.Numerator * Left.Denominator
                   );
         end if;
      end if;
   end Compare;

   function Compare
            (  Left  : Unbounded_Rational;
               Right : Unbounded_Integer
            )  return Precedence is
   begin
      if Left.Sign then
         if Is_Negative (Right) then
            return Compare
                   (  Left.Denominator * Get_Mantissa (Right),
                      Left.Numerator
                   );
         else
            return Less;
         end if;
      else
         if Is_Negative (Right) then
            return Greater;
         else
            return Compare
                   (  Left.Numerator,
                      Left.Denominator * Get_Mantissa (Right)
                   );
         end if;
      end if;
   end Compare;

   function Compare
            (  Left  : Unbounded_Integer;
               Right : Unbounded_Rational
            )  return Precedence is
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

   function Compare
            (  Left  : Unbounded_Rational;
               Right : Unbounded_Unsigned
            )  return Precedence is
   begin
      if Left.Sign then
         return Less;
      else
         return Compare (Left.Numerator, Left.Denominator * Right);
      end if;
   end Compare;

   function Compare
            (  Left  : Unbounded_Unsigned;
               Right : Unbounded_Rational
            )  return Precedence is
   begin
      if Right.Sign then
         return Greater;
      else
         return Compare (Right.Denominator * Left, Right.Numerator);
      end if;
   end Compare;

   function Compare
            (  Left  : Unbounded_Rational;
               Right : Half_Word
            )  return Precedence is
   begin
      if Left.Sign then
         return Less;
      else
         return Compare (Left.Numerator, Left.Denominator * Right);
      end if;
   end Compare;

   function Compare
            (  Left  : Half_Word;
               Right : Unbounded_Rational
            )  return Precedence is
   begin
      if Right.Sign then
         return Greater;
      else
         return Compare (Right.Denominator * Left, Right.Numerator);
      end if;
   end Compare;

   function Compose
            (  Numerator   : Unbounded_Unsigned;
               Denominator : Unbounded_Unsigned;
               Negative    : Boolean := False
            )  return Unbounded_Rational is
   begin
      if Is_Zero (Denominator) then
         raise Constraint_Error;
      elsif Is_Zero (Numerator) then
         return Zero;
      elsif Is_One (Denominator) then
         return (Negative, Numerator, Unbounded_Unsigneds.One);
      else
         declare
            GCD : constant Unbounded_Unsigned :=
                  Greatest_Common_Divisor (Numerator, Denominator);
         begin
            return (Negative, Numerator / GCD, Denominator / GCD);
         end;
      end if;
   end Compose;

   procedure Copy
             (  Destination : in out Unbounded_Rational;
                Source      : Unbounded_Rational
             )  is
   begin
      Destination.Sign := Source.Sign;
      Copy (Destination.Numerator,   Source.Numerator);
      Copy (Destination.Denominator, Source.Denominator);
   end Copy;

   function Floor (Left : Unbounded_Rational)
      return Unbounded_Integer is
      Result : constant Unbounded_Integer :=
                        To_Unbounded_Integer (Left);
   begin
      if Is_One (Left.Denominator) then
         return Result;
      elsif Is_Negative (Result) then
         return Result - 1;
      else
         return Result;
      end if;
   end Floor;

   function Error
            (  Left, Right : Unbounded_Rational;
               Error       : Integer
            )  return Boolean is
      L : Unbounded_Unsigned := Left.Numerator  * Right.Denominator;
      R : Unbounded_Unsigned := Right.Numerator * Left.Denominator;
   begin
      if Left.Sign xor Right.Sign then
         Add (L, R);
      else
         case Compare (L, R) is
            when Less =>
               Sub_2 (R, L);
            when Equal =>
               return False;
            when Greater =>
               Sub (L, R);
        end case;
      end if;
      Set (R, Left.Denominator);
      Mul (R, Right.Denominator);
      if Error < 0 then
         Mul_By_Power_Of_Two (L, Bit_Count (-Error));
      elsif Error > 0 then
         Mul_By_Power_Of_Two (R, Bit_Count (Error));
      end if;
      return Compare (L, R) = Greater;
   end Error;

   function Get_Denominator (Left : Unbounded_Rational)
      return Unbounded_Unsigned is
   begin
      return Left.Denominator;
   end Get_Denominator;

   function Get_Numerator (Left : Unbounded_Rational)
      return Unbounded_Unsigned is
   begin
      return Left.Numerator;
   end Get_Numerator;

   function Greater_Than_One (Left : Unbounded_Rational)
      return Boolean is
   begin
      case Compare (Left.Numerator, Left.Denominator) is
         when Greater =>
            return True;
         when Equal | Less =>
            return False;
      end case;
   end Greater_Than_One;

   procedure Invert (Left : in out Unbounded_Rational) is
   begin
      if Is_Zero (Left.Numerator) then
         raise Constraint_Error;
      else
         Swap (Left.Numerator, Left.Denominator);
      end if;
   end Invert;

   function Invert (Left : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      if Is_Zero (Left.Numerator) then
         raise Constraint_Error;
      else
         return (  Numerator   => Left.Denominator,
                   Denominator => Left.Numerator,
                   Sign        => Left.Sign
                );
      end if;
   end Invert;

   function Is_Negative (Left : Unbounded_Rational) return Boolean is
   begin
      return Left.Sign;
   end Is_Negative;

   function Is_One (Left : Unbounded_Rational) return Boolean is
   begin
      return not Left.Sign           and then
             Is_One (Left.Numerator) and then
             Is_One (Left.Denominator);
   end Is_One;

   function Is_Two (Left : Unbounded_Rational) return Boolean is
   begin
      return not Left.Sign           and then
             Is_Two (Left.Numerator) and then
             Is_One (Left.Denominator);
   end Is_Two;

   function Is_Zero (Left : Unbounded_Rational) return Boolean is
   begin
      return Is_Zero (Left.Numerator);
   end Is_Zero;

   function Less_Than_One (Left : Unbounded_Rational) return Boolean is
   begin
      return Compare (Left.Numerator, Left.Denominator) = Less;
   end Less_Than_One;

   function Log2 (Value : Unbounded_Integer) return Integer is
   begin
      if Is_Negative (Value) then
         raise Constraint_Error;
      else
         return Integer (Get_MSB (Get_Mantissa (Value)));
      end if;
   end Log2;

   function Log2 (Value : Unbounded_Rational) return Integer is
   begin
      return Log2 (To_Unbounded_Integer (Value));
   end Log2;

   function Log2 (Value : Unbounded_Unsigned) return Integer is
   begin
      return Integer (Get_MSB (Value));
   end Log2;

   function Max (Left, Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      if Compare (Left, Right) = Greater then
         return Left;
      else
         return Right;
      end if;
   end Max;

   function Min (Left, Right : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      if Compare (Left, Right) = Less then
         return Left;
      else
         return Right;
      end if;
   end Min;
--
-- Round - Rounding
--
--    Numerator   - The numerator
--    Denominator - The denominator
--
-- This procedure rounds Numerator / Denominator away from zero
--
   procedure Round
             (  Numerator   : in out Unbounded_Unsigned;
                Denominator : Unbounded_Unsigned
             )  is
   begin
      if not Is_Zero (Numerator) then
         case Compare (Numerator, Denominator) is
            when  Equal =>
               null;
            when Less =>
               case Compare
                    (  Numerator * 2,
                       Denominator
                    )  is
                  when Greater | Equal =>
                     Add (Numerator, 1);
                  when Less =>
                     null;
               end case;
            when Greater =>
               case Compare
                    (  (  (  Numerator
                          -  (Numerator / Denominator) * Denominator
                          )
                       *  2
                       ),
                       Denominator
                    )  is
                  when Greater | Equal =>
                     Add (Numerator, Denominator);
                  when Less =>
                     null;
               end case;
         end case;
      end if;
   end Round;

   function Round (Left : Unbounded_Rational)
      return Unbounded_Integer is
      Numerator : Unbounded_Unsigned := Left.Numerator;
   begin
      Round (Numerator, Left.Denominator);
      return Compose (Numerator / Left.Denominator, Left.Sign);
   end Round;

   procedure Round
             (  Left  : in out Unbounded_Rational;
                Power : Natural
             )  is
      Numerator   : Unbounded_UNsigned renames Left.Numerator;
      Denominator : Unbounded_UNsigned renames Left.Denominator;
      Remainder   : Unbounded_UNsigned;
      Rounding    : constant Bit_Count := Bit_Count (Power);
      Truncated   : Bit_Count;
   begin
      Truncate (Denominator, Truncated);
      if Truncated >= Rounding then
         Mul_By_Power_Of_Two (Denominator, Truncated - Rounding);
      else
         Mul_By_Power_Of_Two (Numerator, Rounding - Truncated);
      end if;
      Div (Numerator, Denominator, Remainder);
      if Remainder * 2 >= Denominator then
         Add (Numerator, 1);
      end if;
      Truncate (Numerator, Truncated);
      if Truncated >= Rounding then
         Mul_By_Power_Of_Two (Numerator, Truncated - Rounding);
         Denominator := Unbounded_Unsigneds.One;
      else
         Denominator := Power_Of_Two (Rounding - Truncated);
      end if;
   end Round;

   procedure Set
             (  Destination : in out Unbounded_Rational;
                Source      : Unbounded_Rational
             )  is
   begin
      Destination.Sign := Source.Sign;
      Set (Destination.Numerator,   Source.Numerator);
      Set (Destination.Denominator, Source.Denominator);
   end Set;

   procedure Split
             (  Value   : in out Unbounded_Rational;
                Integer : out Unbounded_Integer
             )  is
   begin
      if Is_Zero (Value) then
         Erase (Integer);
         return;
      end if;
      declare
         V : Unbounded_Unsigned := Value.Numerator;
         S : constant Boolean := Value.Sign;
      begin
         Value.Sign := False;
         Div (V, Value.Denominator, Value.Numerator);
         if Is_Zero (Value.Numerator) then
            Integer := Compose (V, S);
            Value.Denominator := Unbounded_Unsigneds.One;
         elsif S then
            Add (V, 1);
            Integer := Compose (V, True);
            Sub_2 (Value.Denominator, Value.Numerator);
         else
            Integer := Compose (V, False);
         end if;
      end;
   end Split;

   procedure Split
             (  Value    : Unbounded_Rational;
                Integer  : out Unbounded_Integer;
                Fraction : out Unbounded_Rational
             )  is
   begin
      if Is_Zero (Value) then
         Erase (Integer);
         Fraction := Zero;
         return;
      end if;
      declare
         V : Unbounded_Unsigned := Value.Numerator;
      begin
         Fraction.Sign := False;
         Div (V, Value.Denominator, Fraction.Numerator);
         if Is_Zero (Fraction.Numerator) then
            Integer := Compose (V, Value.Sign);
            Fraction.Denominator := Unbounded_Unsigneds.One;
         elsif Value.Sign then
            Add (V, 1);
            Integer := Compose (V, True);
            Sub_2 (Value.Denominator, Fraction.Numerator);
            Fraction.Denominator := Value.Denominator;
         else
            Integer := Compose (V, False);
            Fraction.Denominator := Value.Denominator;
         end if;
      end;
   end Split;

   function Square (Left : Unbounded_Rational)
      return Unbounded_Rational is
   begin
      return
      (  Sign        => False,
         Numerator   => Square (Left.Numerator),
         Denominator => Square (Left.Denominator)
      );
   end Square;

   procedure Swap (Left, Right : in out Unbounded_Rational) is
      Sign : constant Boolean := Left.Sign;
   begin
      Left.Sign  := Right.Sign;
      Right.Sign := Sign;
      Swap (Left.Numerator,   Right.Numerator);
      Swap (Left.Denominator, Right.Denominator);
   end Swap;

   function To_Integer (Left : Unbounded_Rational) return Integer is
   begin
      if Is_Zero (Left.Numerator) then
         return 0;
      else
         case Compare (Left.Numerator, Left.Denominator) is
            when Less =>
               return 0;
            when Equal =>
               if Left.Sign then
                  return -1;
               else
                  return 1;
               end if;
            when Greater =>
               if Left.Sign then
                  return To_Integer
                         (  Compose
                            (  Left.Numerator / Left.Denominator,
                               True
                         )  );
               else
                  return To_Integer
                         (  Compose
                            (  Left.Numerator / Left.Denominator,
                               False
                         )  );
               end if;
         end case;
      end if;
   end To_Integer;

   function To_Unbounded_Integer (Left : Unbounded_Rational)
      return Unbounded_Integer is
   begin
      if Is_Zero (Left.Numerator) then
         return Unbounded_Integers.Zero;
      else
         case Compare (Left.Numerator, Left.Denominator) is
            when Less =>
               return Unbounded_Integers.Zero;
            when Equal =>
               if Left.Sign then
                  return -Unbounded_Integers.One;
               else
                  return Unbounded_Integers.One;
               end if;
            when Greater =>
               if Left.Sign then
                  return Compose
                         (  Left.Numerator / Left.Denominator,
                            True
                         );
               else
                  return Compose
                         (  Left.Numerator / Left.Denominator,
                            False
                         );
               end if;
         end case;
      end if;
   end To_Unbounded_Integer;

   function To_Unbounded_Rational (Left : Integer)
      return Unbounded_Rational is
   begin
      if Left = 0 then
         return Zero;
      elsif Left < 0 then
         return
         (  True,
            Get_Mantissa (To_Unbounded_Integer (Left)),
            Unbounded_Unsigneds.One
         );
      else
         return
         (  False,
            Get_Mantissa (To_Unbounded_Integer (Left)),
            Unbounded_Unsigneds.One
         );
      end if;
   end To_Unbounded_Rational;

   function To_Unbounded_Rational (Left : Unbounded_Integer)
      return Unbounded_Rational is
   begin
      if Is_Zero (Left) then
         return Zero;
      else
         return
         (  Is_Negative (Left),
            Get_Mantissa (Left),
            Unbounded_Unsigneds.One
         );
      end if;
   end To_Unbounded_Rational;

   function To_Unbounded_Rational (Left : Unbounded_Unsigned)
      return Unbounded_Rational is
   begin
      if Is_Zero (Left) then
         return Zero;
      else
         return (False, Left, Unbounded_Unsigneds.One);
      end if;
   end To_Unbounded_Rational;

   package body Fixed_Point_Conversions is

      Numerator   : Unbounded_Unsigned;
      Denominator : Unbounded_Unsigned;

      function From_Unbounded_Rational (Left : Unbounded_Rational)
         return Number is
      begin
         if Left = Zero then
            return 0.0;
         end if;
         declare
            Mantissa : constant Word :=
               To_Word
               (  Get_Mantissa
                  (  Round
                     (  abs Left / Numerator * Denominator
               )  )  );
--            type Dummy is array (1..Mantissa) of Boolean;
         begin
            if Is_Negative (Left) then
               return Number (-Number'Small * Word'Pos (Mantissa));
            else
               return Number (Number'Small * Word'Pos (Mantissa));
            end if;
         end;
      end From_Unbounded_Rational;

      function To_Unbounded_Rational (Left : Number)
         return Unbounded_Rational is
      begin
         if Left = 0.0 then
            return Zero;
         end if;
         declare
            Mantissa : Word;
         begin
            if Left < 0.0 then
               Mantissa := Word (Left / (-Number'Small));
               return Compose
                      (  Negative    => True,
                         Denominator => Denominator,
                         Numerator   => Numerator * From_Word (Mantissa)
                      );
            else
               Mantissa := Word (Left / Number'Small);
               return Compose
                      (  Negative    => False,
                         Denominator => Denominator,
                         Numerator   => Numerator * From_Word (Mantissa)
                      );
            end if;
         end;
      end To_Unbounded_Rational;
   begin
      if Number'Small > 1.0 then
         Numerator   := From_Word (Word (Number'Small));
         Denominator := Unbounded_Unsigneds.One;
      else
         Numerator   := Unbounded_Unsigneds.One;
         Denominator := From_Word (Word (1.0 / Number'Small));
      end if;
   end Fixed_Point_Conversions;

   package body Floating_Point_Conversions is
      function From_Unbounded_Rational (Left : Unbounded_Rational)
         return Number is
      begin
         if Is_Zero (Left) then
            return 0.0;
         end if;
         declare
            Fraction    : Number'Base;
            Numerator   : Unbounded_Unsigned := Left.Numerator;
            Denominator : Unbounded_Unsigned := Left.Denominator;
            Exponent    : Integer :=
                             (  (Log2 (Numerator) - Log2 (Denominator))
                             /  Log2 (Number'Machine_Radix)
                             );
         begin
            if Exponent > 0 then
               Denominator :=
                  (  Denominator
                  *  (  From_Half_Word (Number'Machine_Radix)
                     ** Bit_Count (Exponent)
                  )  );
            elsif Exponent < 0 then
               Numerator :=
                  (  Numerator
                  *  (  From_Half_Word (Number'Machine_Radix)
                     ** Bit_Count (-Exponent)
                  )  );
            end if;
            while Compare (Numerator, Denominator, Half) /= Greater loop
               Numerator := Numerator * Number'Machine_Radix;
               Exponent  := Exponent - 1;
            end loop;
            while Compare (Numerator, Denominator, One) = Greater loop
               Denominator := Denominator * Number'Machine_Radix;
               Exponent    := Exponent + 1;
            end loop;
            Numerator :=
               (  Numerator
               *  (  From_Half_Word (Number'Machine_Radix)
                  ** Bit_Count (Number'Mantissa)
               )  );
            Exponent := Exponent - Number'Mantissa;
            Round (Numerator, Denominator);
            Fraction := Number'Base (To_Word (Numerator / Denominator));
            Exponent := Exponent + Number'Exponent (Fraction);
            if Left.Sign then
               return Number'Compose (-Fraction, Exponent);
            else
               return Number'Compose (Fraction, Exponent);
            end if;
         end;
      end From_Unbounded_Rational;

      function To_Unbounded_Rational (Left : Number)
         return Unbounded_Rational is
      begin
         if Left = 0.0 then
            return Zero;
         end if;
         declare
            Exponent  : constant Integer := Number'Exponent (Left);
            Mantissa  : constant Integer := Number'Machine_Mantissa;
            Numerator : constant Unbounded_Unsigned :=
               From_Word
               (  Word
                  (  Number'Compose
                     (  Number'Fraction (Left),
                        Number'Machine_Mantissa
               )  )  );
         begin
            if Exponent = Mantissa then
               return (Left < 0.0, Numerator, Unbounded_Unsigneds.One);
            elsif Exponent > Mantissa then
               return
               (  Left < 0.0,
                  (  Numerator
                  *  (  From_Word (Number'Machine_Radix)
                     ** Bit_Count (Exponent - Mantissa)
                  )  ),
                  Unbounded_Unsigneds.One
               );
            else
               return Compose
                      (  Numerator,
                         (  From_Word (Number'Machine_Radix)
                         ** Bit_Count (Mantissa - Exponent)
                         ),
                         Left < 0.0
                     );
            end if;
         end;
      end To_Unbounded_Rational;

   end Floating_Point_Conversions;

   package Float_Conversions is new Floating_Point_Conversions (Float);

   function To_Float (Left : Unbounded_Rational) return Float
      renames Float_Conversions.From_Unbounded_Rational;

   function To_Unbounded_Rational (Left : Float)
      return Unbounded_Rational renames
         Float_Conversions.To_Unbounded_Rational;

end Unbounded_Rationals;
