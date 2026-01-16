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

package body Unbounded_Unsigneds.Barrett is

   function Create (Modulus : Unbounded_Unsigned)
      return Barrett_Reducer is
   begin
      if Compare (Modulus, 4) /= Greater or else
         Is_Power_Of_Two (Modulus) then
         raise Constraint_Error;
      end if;
      declare
         Result : Barrett_Reducer;
      begin
         Result.Modulus := Modulus;
         Result.Half_Words := Modulus.Length;
         Result.Bits := Bit_Count (Result.Half_Words) * Bit_Width;
         Result.Reducer := Power_Of_Two (2 * Result.Bits);
         Div (Result.Reducer, Modulus);
         return Result;
      end;
   end Create;

   procedure Mod_Mul
             (  Reducer      : Barrett_Reducer;
                Multiplicand : Unbounded_Unsigned;
                Multiplier   : Unbounded_Unsigned;
                Result       : out Unbounded_Unsigned
             )  is
      Product : Unbounded_Unsigned;
   begin
      Mul (Multiplicand, Multiplier, Product);
      Reduce (Reducer, Product, Result);
   end Mod_Mul;

   function Mod_Mul
            (  Reducer      : Barrett_Reducer;
               Multiplicand : Unbounded_Unsigned;
               Multiplier   : Unbounded_Unsigned
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Mod_Mul (Reducer, Multiplicand, Multiplier, Result);
      return Result;
   end Mod_Mul;

   procedure Mod_Pow
             (  Reducer : Barrett_Reducer;
                Left    : Unbounded_Unsigned;
                Right   : Unbounded_Unsigned;
                Result  : out Unbounded_Unsigned
             )  is
   begin
      if Is_Zero (Left) then
         if Right.Length = 0 then
            Set (Result, 1);
         else
            Erase (Result);
         end if;
         return;
      elsif Is_One (Left) or else Right.Length = 0 then
         Set (Result, 1);
         return;
      end if;
      declare
         Base : Unbounded_Unsigned;
         X    : Unbounded_Unsigned;
      begin
         Set (Base, Left);
         if Is_Odd (Right) then -- Reducing the first step of the loop
            Set (Result, Base);
         else
            Set (Result, 1);
         end if;
         for Bit in 2..Get_MSB (Right) loop
            Square (Base, X);
            Reduce (Reducer, X, Base);
            if Get_Bit (Right, Bit) then
               Mul (Result, Base, X);
               Reduce (Reducer, X, Result);
            end if;
         end loop;
      end;
   end Mod_Pow;

   function Mod_Pow
            (  Reducer : Barrett_Reducer;
               Left    : Unbounded_Unsigned;
               Right   : Unbounded_Unsigned
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Mod_Pow (Reducer, Left, Right, Result);
      return Result;
   end Mod_Pow;

   procedure Mod_Square
             (  Reducer : Barrett_Reducer;
                Left    : Unbounded_Unsigned;
                Result  : out Unbounded_Unsigned
             )  is
      Product : Unbounded_Unsigned;
   begin
      Square (Left, Product);
      Reduce (Reducer, Product, Result);
   end Mod_Square;

   function Mod_Square
            (  Reducer : Barrett_Reducer;
               Left    : Unbounded_Unsigned
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Mod_Square (Reducer, Left, Result);
      return Result;
   end Mod_Square;

   procedure Reduce
             (  Reducer : Barrett_Reducer;
                Value   : Unbounded_Unsigned;
                Result  : out Unbounded_Unsigned
             )  is
   begin
      Mul (Value, Reducer.Reducer, Result);
      Shift_Right (Result, Reducer.Half_Words * 2);
      Mul (Result, Reducer.Modulus);
      Sub_2 (Value, Result);
      if Compare (Reducer.Modulus, Result) /= Greater then
         Sub (Result, Reducer.Modulus);
      end if;
   end Reduce;

   function Reduce
            (  Reducer : Barrett_Reducer;
               Value   : Unbounded_Unsigned
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Reduce (Reducer, Value, Result);
      return Result;
   end Reduce;

end Unbounded_Unsigneds.Barrett;
