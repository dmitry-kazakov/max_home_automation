--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Unbounded_Unsigneds.Montgomery              Luebeck            --
--  Interface                                      Winter, 2024       --
--                                                                    --
--                                Last revision :  10:32 12 Jul 2025  --
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

package body Unbounded_Unsigneds.Montgomery is

   Algorithm : Multiplication_Algorithm := CIOS;
   pragma Atomic (Algorithm);

   function Compare (Left, Right : Montgomery_Number)
      return Precedence is
   begin
      return Compare
             (  Unbounded_Unsigned (Left),
                Unbounded_Unsigned (Right)
             );
   end Compare;

   function Create (Modulus : Unbounded_Unsigned)
      return Montgomery_Domain is
   begin
      if Is_Even (Modulus) or else Is_One (Modulus) then
         raise Constraint_Error;
      end if;
      declare
         Result : Montgomery_Domain;
      begin
         Result.Modulus := Modulus;
         Result.Half_Words := Modulus.Length;
         Result.Bits := Bit_Count (Result.Half_Words) * Bit_Width;
--       Power_Of_Two (Result.Bits, Result.Reducer); -- R
--       Mod_Inv (Modulus, Result.Reducer, Result.Modulus_Inverse);
--          - or -
--       Mod_Inv_In_Power_Of_Two
--       (  Modulus,
--          Result.Bits,
--          Result.Modulus_Inverse
--       );
         Inverse (Modulus, Result.Half_Words, Result.Modulus_Inverse);
         case Compare (Result.Reducer, Result.Modulus_Inverse) is
            when Less | Equal =>
               Swap (Result.Squared_Reducer, Result.Modulus_Inverse);
               Power_Of_Two (Result.Bits, Result.Modulus_Inverse);
               Sub (Result.Modulus_Inverse, Result.Squared_Reducer);
            when Greater =>
               Sub_2 (Result.Reducer, Result.Modulus_Inverse);
         end case;
         Mod_Pow
         (  Left    => Two,
            Right   => Half_Word (Result.Bits),
            Modulus => Modulus,
            Result  => Result.Reducer
         );
         Square (Result.Reducer, Result.Squared_Reducer);
         Modulo (Result.Squared_Reducer, Modulus);
         return Result;
      end;
   end Create;

   procedure Dump_Hex
             (  X      : Montgomery_Number;
                Prefix : String := "";
                Join   : Boolean := False
             )  is
   begin
      Dump_Hex (Unbounded_Unsigned (X), Prefix, Join);
   end Dump_Hex;

   procedure Dump (X : Montgomery_Number; Prefix : String := "") is
   begin
      Dump (Unbounded_Unsigned (X), Prefix);
   end Dump;

   procedure Erase (Left : in out Montgomery_Number) is
   begin
      Erase (Unbounded_Unsigned (Left));
   end Erase;

   procedure From_Domain
             (  Domain : Montgomery_Domain;
                Value  : Montgomery_Number;
                Result : out Unbounded_Unsigned
             )  is
   begin
      Reduce (Domain, Unbounded_Unsigned (Value), Result);
   end From_Domain;

   function From_Domain
            (  Domain : Montgomery_Domain;
               Value  : Montgomery_Number
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Reduce (Domain, Unbounded_Unsigned (Value), Result);
      return Result;
   end From_Domain;

   function Get_Multiplication_Algorithm
      return Multiplication_Algorithm is
   begin
      return Algorithm;
   end Get_Multiplication_Algorithm;

   function Get_Use_Count (Object : Montgomery_Number)
      return Natural is
   begin
      if Object.Value = null then
         return 0;
      else
         return Natural (Load (Object.Value.Count'Access));
      end if;
   end Get_Use_Count;

   function Is_Zero (Left : Montgomery_Number) return Boolean is
   begin
      return Left.Length = 0;
   end Is_Zero;

   function Is_One
            (  Domain : Montgomery_Domain;
               Left   : Montgomery_Number
            )  return Boolean is
   begin
      return
         Compare (Unbounded_Unsigned (Left), Domain.Reducer) = Equal;
   end Is_One;

   procedure Mod_Add
             (  Domain      : Montgomery_Domain;
                Accumulator : in out Montgomery_Number;
                Increment   : Half_Word_Array
             )  is
   begin
      Add (Unbounded_Unsigned (Accumulator), Increment);
      case Compare (Unbounded_Unsigned (Accumulator), Domain.Modulus) is
         when Less =>
            null;
         when Equal =>
            Erase (Unbounded_Unsigned (Accumulator));
         when Greater =>
            Sub
            (  Unbounded_Unsigned (Accumulator),
               Domain.Modulus
            );
      end case;
   end Mod_Add;

   procedure Mod_Add
             (  Domain      : Montgomery_Domain;
                Accumulator : in out Montgomery_Number;
                Increment   : Montgomery_Number
             )  is
   begin
      Add
      (  Unbounded_Unsigned (Accumulator),
         Unbounded_Unsigned (Increment)
      );
      case Compare (Unbounded_Unsigned (Accumulator), Domain.Modulus) is
         when Less =>
            null;
         when Equal =>
            Erase (Unbounded_Unsigned (Accumulator));
         when Greater =>
            Sub
            (  Unbounded_Unsigned (Accumulator),
               Domain.Modulus
            );
      end case;
   end Mod_Add;

   procedure Mod_Add
             (  Domain      : Montgomery_Domain;
                Accumulator : Montgomery_Number;
                Increment   : Montgomery_Number;
                Result      : out Montgomery_Number
             )  is
   begin
      Set
      (  Unbounded_Unsigned (Result),
         Unbounded_Unsigned (Accumulator)
      );
      Add
      (  Unbounded_Unsigned (Result),
         Unbounded_Unsigned (Increment)
      );
      case Compare (Unbounded_Unsigned (Result), Domain.Modulus) is
         when Less =>
            null;
         when Equal =>
            Erase (Unbounded_Unsigned (Result));
         when Greater =>
            Sub
            (  Unbounded_Unsigned (Result),
               Domain.Modulus
            );
      end case;
   end Mod_Add;

   procedure Mod_Double
             (  Domain       : Montgomery_Domain;
                Multiplicand : in out Montgomery_Number
             )  is
   begin
      Mul_By_Power_Of_Two (Unbounded_Unsigned (Multiplicand), 1);
      case Compare
           (  Unbounded_Unsigned (Multiplicand),
              Domain.Modulus
           )  is
         when Less =>
            null;
         when Equal =>
            Erase (Unbounded_Unsigned (Multiplicand));
         when Greater =>
            Sub (Unbounded_Unsigned (Multiplicand), Domain.Modulus);
      end case;
   end Mod_Double;

   procedure Mod_Mul
             (  Domain       : Montgomery_Domain;
                Multiplicand : Montgomery_Number;
                Multiplier   : Montgomery_Number;
                Result       : out Montgomery_Number
             )  is
   begin
      case Algorithm is
         when CIOS =>
            Mod_Mul_CIOS
            (  Domain,
               Multiplicand,
               Multiplier,
               Result
            );
         when Dusse_Kaliski =>
            Mod_Mul_Dusse_Kaliski
            (  Domain,
               Multiplicand,
               Multiplier,
               Result
            );
      end case;
   end Mod_Mul;

   procedure Mod_Mul
             (  Domain       : Montgomery_Domain;
                Multiplicand : Half_Word_Array;
                Multiplier   : Half_Word_Array;
                Result       : out Montgomery_Number
             )  is
   begin
      if Multiplicand'Length = 0 or else Multiplier'Length = 0 then
         Erase (Result);
         return;
      elsif Multiplicand'Length > Domain.Modulus.Length or else
            Multiplier'Length   > Domain.Modulus.Length then
         raise Constraint_Error;
      end if;
      Allocate (Result.Value, Domain.Modulus.Length + 2);
      declare
         Carry : Word;
         A : Half_Word_Array renames Multiplicand;
         B : Half_Word_Array renames Multiplier;
         N : Half_Word_Array renames
                Domain.Modulus.Value.Data (1..Domain.Modulus.Length);
         T : Half_Word_Array renames Result.Value.Data;
         N_1   : constant Word := Word (N (1));
         N_Inv : constant Half_Word :=
                    Domain.Modulus_Inverse.Value.Data (1);
      begin
         for I in 1..Domain.Modulus.Length + 2 loop
            T (I) := 0;
         end loop;
         for I in N'Range loop
            if I + A'First - 1 in A'Range then
               Carry := 0;
               declare
                  A_I : constant Word := Word (A (I + A'First - 1));
               begin
                  for J in B'Range loop
                     declare
                        T_J : Half_Word renames T (J - B'First + 1);
                     begin
                        Carry :=
                           Carry + Word (T_J) + A_I * Word (B (J));
                        T_J := Half_Word (Carry and Half_Word_Mask);
                        Carry := Shift_Right (Carry, Bit_Width);
                     end;
                  end loop;
               end;
               if Carry /= 0 then  -- Propagate carry
                  for J in B'Length + 1..N'Last + 2 loop
                     Carry := Carry + Word (T (J));
                     T (J) := Half_Word (Carry and Half_Word_Mask);
                     Carry := Shift_Right (Carry, Bit_Width);
                     exit when Carry = 0;
                  end loop;
               end if;
            end if;
            declare
               M : constant Word := Word (T (1) * N_Inv);
            begin
               Carry := Shift_Right
                        (  Word (T (1)) + M * N_1,
                           Bit_Width
                        );
               for J in 2..N'Last loop
                  Carry := Carry + Word (T (J)) + M * Word (N (J));
                  T (J - 1) := Half_Word (Carry and Half_Word_Mask);
                  Carry := Shift_Right (Carry, Bit_Width);
               end loop;
            end;
            Carry := Carry + Word (T (N'Last + 1));
            T (N'Last) := Half_Word (Carry and Half_Word_Mask);
            T (N'Last + 1) :=
               Half_Word
               (  Word (T (N'Last + 2))
               +  Shift_Right (Carry, Bit_Width)
               );
         end loop;
         Result.Length := N'Length + 1; -- The highest one is not used
         while Result.Length > 0 and then T (Result.Length) = 0 loop
            Result.Length := Result.Length - 1;
         end loop;
         if (  Compare (Unbounded_Unsigned (Result), Domain.Modulus)
            /= Less
            )  then
            Sub (Unbounded_Unsigned (Result), Domain.Modulus);
         end if;
      end;
   end Mod_Mul;

   function Mod_Mul
            (  Domain       : Montgomery_Domain;
               Multiplicand : Montgomery_Number;
               Multiplier   : Montgomery_Number
            )  return Montgomery_Number is
      Result : Montgomery_Number;
   begin
      Mod_Mul (Domain, Multiplicand, Multiplier, Result);
      return Result;
   end Mod_Mul;

   procedure Mod_Mul_CIOS
             (  Domain       : Montgomery_Domain;
                Multiplicand : Montgomery_Number;
                Multiplier   : Montgomery_Number;
                Result       : out Montgomery_Number
             )  is
   begin
      if Multiplicand.Length = 0 or else Multiplier.Length = 0 then
         Erase (Result);
         return;
      elsif Multiplicand.Length > Domain.Modulus.Length or else
            Multiplier.Length   > Domain.Modulus.Length then
         raise Constraint_Error;
      end if;
      Allocate (Result.Value, Domain.Modulus.Length + 2);
      declare
         Carry : Word;
         A : Half_Word_Array renames
                Multiplicand.Value.Data (1..Multiplicand.Length);
         B : Half_Word_Array renames
                Multiplier.Value.Data (1..Multiplier.Length);
         N : Half_Word_Array renames
                Domain.Modulus.Value.Data (1..Domain.Modulus.Length);
         T : Half_Word_Array renames Result.Value.Data;
         N_1   : constant Word := Word (N (1));
         N_Inv : constant Half_Word :=
                    Domain.Modulus_Inverse.Value.Data (1);
      begin
         for I in 1..Domain.Modulus.Length + 2 loop
            T (I) := 0;
         end loop;
         for I in N'Range loop
            if I in A'Range then
               Carry := 0;
               declare
                  A_I : constant Word := Word (A (I));
               begin
                  for J in B'Range loop
                     Carry := Carry + Word (T (J)) + A_I * Word (B (J));
                     T (J) := Half_Word (Carry and Half_Word_Mask);
                     Carry := Shift_Right (Carry, Bit_Width);
                  end loop;
               end;
               if Carry /= 0 then  -- Propagate carry
                  for J in B'Last + 1..N'Last + 2 loop
                     Carry := Carry + Word (T (J));
                     T (J) := Half_Word (Carry and Half_Word_Mask);
                     Carry := Shift_Right (Carry, Bit_Width);
                     exit when Carry = 0;
                  end loop;
               end if;
            end if;
            declare
               M : constant Word := Word (T (1) * N_Inv);
            begin
               Carry := Shift_Right
                        (  Word (T (1)) + M * N_1,
                           Bit_Width
                        );
               for J in 2..N'Last loop
                  Carry := Carry + Word (T (J)) + M * Word (N (J));
                  T (J - 1) := Half_Word (Carry and Half_Word_Mask);
                  Carry := Shift_Right (Carry, Bit_Width);
               end loop;
            end;
            Carry := Carry + Word (T (N'Last + 1));
            T (N'Last) := Half_Word (Carry and Half_Word_Mask);
            T (N'Last + 1) :=
               Half_Word
               (  Word (T (N'Last + 2))
               +  Shift_Right (Carry, Bit_Width)
               );
         end loop;
         Result.Length := N'Length + 1; -- The highest one is not used
         while Result.Length > 0 and then T (Result.Length) = 0 loop
            Result.Length := Result.Length - 1;
         end loop;
         if (  Compare (Unbounded_Unsigned (Result), Domain.Modulus)
            /= Less
            )  then
            Sub (Unbounded_Unsigned (Result), Domain.Modulus);
         end if;
      end;
   end Mod_Mul_CIOS;

   function Mod_Mul_CIOS
            (  Domain       : Montgomery_Domain;
               Multiplicand : Montgomery_Number;
               Multiplier   : Montgomery_Number
            )  return Montgomery_Number is
      Result : Montgomery_Number;
   begin
      Mod_Mul_CIOS (Domain, Multiplicand, Multiplier, Result);
      return Result;
   end Mod_Mul_CIOS;

   procedure Mod_Mul_Dusse_Kaliski
             (  Domain       : Montgomery_Domain;
                Multiplicand : Montgomery_Number;
                Multiplier   : Montgomery_Number;
                Result       : out Montgomery_Number
             )  is
   begin
      if Multiplicand.Length = 0 or else Multiplier.Length = 0 then
         Erase (Result);
         return;
      elsif Multiplicand.Length > Domain.Modulus.Length or else
            Multiplier.Length   > Domain.Modulus.Length then
         raise Constraint_Error;
      end if;
      Allocate (Result.Value, Domain.Modulus.Length * 2 + 1);
      declare
         Carry : Word;
         A : Half_Word_Array renames
                Multiplicand.Value.Data (1..Multiplicand.Length);
         B : Half_Word_Array renames
                Multiplier.Value.Data (1..Multiplier.Length);
         T : Half_Word_Array renames
                Result.Value.Data (1..Domain.Modulus.Length * 2 + 1);
         N_Inv : constant Half_Word :=
                    Domain.Modulus_Inverse.Value.Data (1);
      begin
         for I in T'Range loop
            T (I) := 0;
         end loop;
         for I in A'Range loop
            Carry := 0;
            declare
               A_I : constant Word := Word (A (I));
            begin
               for J in B'Range loop
                  declare
                     T_I_J : Half_Word renames T (I + J - 1);
                  begin
                     Carry := Carry + Word (T_I_J) + A_I * Word (B (J));
                     T_I_J := Half_Word (Carry and Half_Word_Mask);
                     Carry := Shift_Right (Carry, Bit_Width);
                  end;
               end loop;
            end;
            T (I + B'Length) := Half_Word (Carry);
         end loop;
         Result.Length := T'Length;
         while Result.Length > 0 and then T (Result.Length) = 0 loop
            Result.Length := Result.Length - 1;
         end loop;
         for I in 1..Domain.Half_Words loop
            Add
            (  Unbounded_Unsigned (Result),
               Domain.Modulus,
               T (I) * N_Inv,
               I - 1
            );
         end loop;
         Shift_Right (Result, Domain.Half_Words);
         if (  Compare (Unbounded_Unsigned (Result), Domain.Modulus)
            /= Less
            )  then
            Sub (Unbounded_Unsigned (Result), Domain.Modulus);
         end if;
      end;
   end Mod_Mul_Dusse_Kaliski;

   function Mod_Mul_Dusse_Kaliski
            (  Domain       : Montgomery_Domain;
               Multiplicand : Montgomery_Number;
               Multiplier   : Montgomery_Number
            )  return Montgomery_Number is
      Result : Montgomery_Number;
   begin
      Mod_Mul_Dusse_Kaliski (Domain, Multiplicand, Multiplier, Result);
      return Result;
   end Mod_Mul_Dusse_Kaliski;

   procedure Mod_Pow
             (  Domain : Montgomery_Domain;
                Left   : Montgomery_Number;
                Right  : Unbounded_Unsigned;
                Result : out Montgomery_Number
             )  is
   begin
      if Is_Zero (Left) then
         if Right.Length = 0 then
            Set (Unbounded_Unsigned (Result), Domain.Reducer); -- 1'
         else
            Erase (Result); -- 0' = 0
         end if;
         return;
      elsif Is_One (Domain, Left) or else Right.Length = 0 then
         Set (Unbounded_Unsigned (Result), Domain.Reducer); -- 1'
         return;
      end if;
      declare
         Base : Montgomery_Number;
         X    : Montgomery_Number;
      begin
         Set (Base, Left);
         if Is_Odd (Right) then -- Reducing the first step of the loop
            Set (Unbounded_Unsigned (X), Domain.Reducer);
            Mod_Mul (Domain, Base, X, Result); -- Base * 1'
         else
            Set (Unbounded_Unsigned (Result), Domain.Reducer); -- 1'
         end if;
         for Bit in 2..Get_MSB (Right) loop
            Mod_Square (Domain, Base, X);
--          Mod_Mul (Domain, Base, Base, X);
            Swap (X, Base);
            if Get_Bit (Right, Bit) then
               Mod_Mul (Domain, Result, Base, X);
               Swap (X, Result);
            end if;
         end loop;
      end;
   end Mod_Pow;

   procedure Mod_Pow
             (  Domain : Montgomery_Domain;
                Left   : Montgomery_Number;
                Right  : Half_Word;
                Result : out Montgomery_Number
             )  is
   begin
      if Is_Zero (Left) then
         if Right = 0 then
            Set (Unbounded_Unsigned (Result), Domain.Reducer); -- 1'
         else
            Erase (Result); -- 0' = 0
         end if;
         return;
      elsif Is_One (Domain, Left) or else Right = 0 then
         Set (Unbounded_Unsigned (Result), Domain.Reducer); -- 1'
         return;
      end if;
      declare
         Power : Half_Word := Right;
         Base  : Montgomery_Number;
         X     : Montgomery_Number;
      begin
         Set (Base, Left);
         if (Power and 1) /= 0 then -- Reducing the loop
            Set (Unbounded_Unsigned (X), Domain.Reducer);
            Mod_Mul (Domain, Base, X, Result); -- Base * 1'
         else
            Set (Unbounded_Unsigned (Result), Domain.Reducer); -- 1'
         end if;
         while Power > 1 loop
            Power := Shift_Right (Power, 1);
            Mod_Square (Domain, Base, X);
--          Mod_Mul (Domain, Base, Base, X);
            Swap (X, Base);
            if (Power and 1) /= 0 then
               Mod_Mul (Domain, Result, Base, X);
               Swap (X, Result);
            end if;
         end loop;
     end;
   end Mod_Pow;

   function Mod_Pow
            (  Domain : Montgomery_Domain;
               Left   : Montgomery_Number;
               Right  : Unbounded_Unsigned
            )  return Montgomery_Number is
      Result : Montgomery_Number;
   begin
      Mod_Pow (Domain, Left, Right, Result);
      return Result;
   end Mod_Pow;

   function Mod_Pow
            (  Domain : Montgomery_Domain;
               Left   : Montgomery_Number;
               Right  : Half_Word
            )  return Montgomery_Number is
      Result : Montgomery_Number;
   begin
      Mod_Pow (Domain, Left, Right, Result);
      return Result;
   end Mod_Pow;

   procedure Mod_Pow_Of_Two
             (  Domain : Montgomery_Domain;
                Power  : Unbounded_Unsigned;
                Result : out Montgomery_Number
             )  is
   begin
      if Power.Length = 0 then
         Set (Unbounded_Unsigned (Result), Domain.Reducer); -- 1'
         return;
      end if;
      --
      -- 2 ** N = 2 ** (K * (N / K) + N mod K) =
      --        = R ** (N / K) * 2 ** (N mod K)   (mod N)
      --
      --      R = 2 ** K (mod N)
      --
      --      N / K = Sum { Ai * 2 ** I }
      --               i
      --
      -- 2 ** N = R ** Sum { Ai * 2 ** I } * 2 ** (N mod K) =
      --                i
      --        = Prod { (R ** 2) ** I } * 2 ** (N mod K)
      --            i | Ai=1
      --
      declare
         Base     : Montgomery_Number;
         Exponent : Unbounded_Unsigned;
         X        : Montgomery_Number;
         Two      : Unbounded_Unsigned renames Unbounded_Unsigned (X);
         R        : Half_Word;
         Bits     : Bit_Count := 2 ** (Log2 (Half_Word (Domain.Bits)));
      begin
         if Bits < Domain.Bits then
            Bits := Bits * 2;
         end if;
         Set (Two, Domain.Reducer);
         Mul_By_Power_Of_Two (Two, 1);
         if Compare (Two, Domain.Modulus) = Greater then
            Sub (Unbounded_Unsigned (Two), Domain.Modulus);
         end if;
         Set (Exponent, Power);
         Div (Exponent, Half_Word (Bits), R);
         if (  Set_Bits_Count (Power)
            <  Set_Bits_Count (Exponent) + Set_Bits_Count (R)
            )  then
            --
            -- The number of set  bits will be greater  so that a direct
            -- exponentiation will be faster.
            --
            Mod_Pow (Domain, X, Power, Result);
            return;
         end if;
      --
      -- Base = (2 ** K)' = 2 ** (K - M) * 2 ** M * R mod N =
      --      = 2 ** (K - M) * R ** 2  mod N = 2 ** (K - M)' * 1'
      -- X    = 2'        = 2 * R      mod N
      --
         Mod_Pow (Domain, X, Half_Word (Bits), Base);
        if Is_Odd (Exponent) then
            Mod_Pow (Domain, X, Half_Word (Bits) + R, Result);
         else
            Mod_Pow (Domain, X, R, Result);
         end if;
         for Bit in 2..Get_MSB (Exponent) loop
            Mod_Square (Domain, Base, X);
--          Mod_Mul (Domain, Base, Base, X);
            Swap (X, Base);
            if Get_Bit (Exponent, Bit) then
               Mod_Mul (Domain, Result, Base, X);
               Swap (X, Result);
            end if;
         end loop;
      end;
   end Mod_Pow_Of_Two;

   function Mod_Pow_Of_Two
            (  Domain : Montgomery_Domain;
               Power  : Unbounded_Unsigned
            )  return Montgomery_Number is
      Result : Montgomery_Number;
   begin
      Mod_Pow_Of_Two (Domain, Power, Result);
      return Result;
   end Mod_Pow_Of_Two;

   procedure Mod_Sub
             (  Domain     : Montgomery_Domain;
                Minuend    : in out Montgomery_Number;
                Subtrahend : Half_Word_Array
             )  is
      Borrow : Half_Word := 0;
   begin
      if Subtrahend'Length > 0 then
         if Minuend.Length = 0 then
            Set (Unbounded_Unsigned (Minuend), Domain.Modulus);
            Sub (Unbounded_Unsigned (Minuend), Subtrahend, Borrow);
         else
            case Compare
                 (  Minuend.Value.Data (1..Minuend.Length),
                    Subtrahend
                 )  is
               when Less =>
                  Add
                  (  Unbounded_Unsigned (Minuend),
                     Domain.Modulus
                  );
                  Sub
                  (  Unbounded_Unsigned (Minuend),
                     Subtrahend,
                     Borrow
                  );
               when Equal =>
                  Erase (Unbounded_Unsigned (Minuend));
               when Greater =>
                  Sub
                  (  Unbounded_Unsigned (Minuend),
                     Subtrahend,
                     Borrow
                  );
            end case;
         end if;
         if Borrow /= 0 then
            raise Program_Error;
         end if;
      end if;
   end Mod_Sub;

   procedure Mod_Sub
             (  Domain     : Montgomery_Domain;
                Minuend    : in out Montgomery_Number;
                Subtrahend : Montgomery_Number
             )  is
   begin
      if Subtrahend.Length > 0 then
         case Compare (Minuend, Subtrahend) is
            when Less =>
               Add
               (  Unbounded_Unsigned (Minuend),
                  Domain.Modulus
               );
               Sub
               (  Unbounded_Unsigned (Minuend),
                  Unbounded_Unsigned (Subtrahend)
               );
            when Equal =>
               Erase (Unbounded_Unsigned (Minuend));
            when Greater =>
               Sub
               (  Unbounded_Unsigned (Minuend),
                  Unbounded_Unsigned (Subtrahend)
               );
         end case;
      end if;
   end Mod_Sub;

   procedure Mod_Sub
             (  Domain     : Montgomery_Domain;
                Minuend    : Montgomery_Number;
                Subtrahend : Montgomery_Number;
                Result     : out Montgomery_Number
             )  is
   begin
      if Subtrahend.Length > 0 then
         case Compare (Minuend, Subtrahend) is
            when Less =>
               Set (Unbounded_Unsigned (Result), Domain.Modulus);
               Sub
               (  Unbounded_Unsigned (Result),
                  Unbounded_Unsigned (Subtrahend)
               );
               Add
               (  Unbounded_Unsigned (Result),
                  Unbounded_Unsigned (Minuend)
               );
            when Equal =>
               Erase (Unbounded_Unsigned (Result));
            when Greater =>
               Set (Result, Minuend);
               Sub
               (  Unbounded_Unsigned (Result),
                  Unbounded_Unsigned (Subtrahend)
               );
           end case;
      else
         Set (Result, Minuend);
      end if;
   end Mod_Sub;

   procedure Mod_Sub_2
             (  Domain     : Montgomery_Domain;
                Minuend    : Montgomery_Number;
                Subtrahend : in out Montgomery_Number
             )  is
   begin
      case Compare
           (  Unbounded_Unsigned (Minuend),
              Unbounded_Unsigned (Subtrahend)
           )  is
         when Less =>
            Add
            (  Unbounded_Unsigned (Subtrahend),
               Domain.Modulus
            );
            Sub_2
            (  Unbounded_Unsigned (Minuend),
               Unbounded_Unsigned (Subtrahend)
            );
         when Equal =>
            Erase (Unbounded_Unsigned (Subtrahend));
         when Greater =>
            Sub_2
            (  Unbounded_Unsigned (Minuend),
               Unbounded_Unsigned (Subtrahend)
            );
      end case;
   end Mod_Sub_2;

   procedure Mod_Square
             (  Domain : Montgomery_Domain;
                Left   : Montgomery_Number;
                Result : out Montgomery_Number
             )  is
   begin
      case Algorithm is
         when CIOS =>
            Mod_Square_CIOS (Domain, Left, Result);
         when Dusse_Kaliski =>
            Mod_Square_Dusse_Kaliski (Domain, Left, Result);
      end case;
   end Mod_Square;

   function Mod_Square
            (  Domain : Montgomery_Domain;
               Left   : Montgomery_Number
            )  return Montgomery_Number is
      Result : Montgomery_Number;
  begin
      Mod_Square (Domain, Left, Result);
      return Result;
  end Mod_Square;

   Mask : constant := Half_Word_Modulus / 2 - 1;

   procedure Mod_Square
             (  Domain : Montgomery_Domain;
                Left   : Half_Word_Array;
                Result : out Montgomery_Number
             )  is
   begin
      if Left'Length = 0 then
         Erase (Result);
         return;
      elsif Left'Length > Domain.Modulus.Length then
         raise Constraint_Error;
      end if;
      Allocate (Result.Value, Domain.Modulus.Length + 2);
      declare
         Carry : Word;
         A : Half_Word_Array renames Left;
         N : Half_Word_Array renames
                Domain.Modulus.Value.Data (1..Domain.Modulus.Length);
         T : Half_Word_Array renames Result.Value.Data;
         N_1    : constant Word := Word (N (1));
         Offset : constant Digit_Offset := A'First - 1;
         N_Inv  : Half_Word renames
                     Domain.Modulus_Inverse.Value.Data (1);
      begin
         for I in 1..Domain.Modulus.Length + 2 loop
            T (I) := 0;
         end loop;
         for I in N'Range loop
            if I + Offset <= A'Last then
               declare
                  A_I : constant Word := Word (A (I + Offset));
               begin
                  Carry := 0;
                  for J in 1..I - 1 loop
                     declare -- Handling possible overflow
                        Prod : constant Word :=
                                  A_I * Word (A (J + Offset));
                     begin
                        Carry := Carry
                               + Word (T (J))
                               + Shift_Left (Prod and Mask, 1);
                        T (J) := Half_Word (Carry and Half_Word_Mask);
                        Carry := Shift_Right (Carry, Bit_Width)
                               + Shift_Right (Prod, Bit_Width - 1);
                     end;
                  end loop;
                  declare -- Handling possible overflow
                     MSB : constant Word :=
                                    Shift_Right (Carry, Bit_Width);
                  begin
                     Carry := (Carry and Half_Word_Mask)
                            + Word (T (I))
                            + A_I * A_I;
                     T (I) := Half_Word (Carry and Half_Word_Mask);
                     Carry := Shift_Right (Carry, Bit_Width) + MSB;
                  end;
                  if Carry /= 0 then -- Propagate carry
                     for J in I + 1..N'Last + 2 loop
                        Carry := Carry + Word (T (J));
                        T (J) := Half_Word (Carry and Half_Word_Mask);
                        Carry := Shift_Right (Carry, Bit_Width);
                        exit when Carry = 0;
                     end loop;
                  end if;
               end;
            end if;
            declare
               M : constant Word := Word (T (1) * N_Inv);
            begin
               Carry := Shift_Right
                        (  Word (T (1)) + M * N_1,
                           Bit_Width
                        );
               for J in 2..N'Last loop
                  Carry := Carry + Word (T (J)) + M * Word (N (J));
                  T (J - 1) := Half_Word (Carry and Half_Word_Mask);
                  Carry := Shift_Right (Carry, Bit_Width);
               end loop;
            end;
            Carry := Carry + Word (T (N'Length + 1));
            T (N'Length) := Half_Word (Carry and Half_Word_Mask);
            T (N'Length + 1) :=
               Half_Word
               (  Word (T (N'Length + 2))
               +  Shift_Right (Carry, Bit_Width)
               );
         end loop;
         Result.Length := N'Length + 1; -- The highest one is not used
         while Result.Length > 0 and then T (Result.Length) = 0 loop
            Result.Length := Result.Length - 1;
         end loop;
         if (  Compare (Unbounded_Unsigned (Result), Domain.Modulus)
            /= Less
            )  then
            Sub (Unbounded_Unsigned (Result), Domain.Modulus);
         end if;
      end;
   end Mod_Square;

   procedure Mod_Square_CIOS
             (  Domain : Montgomery_Domain;
                Left   : Montgomery_Number;
                Result : out Montgomery_Number
             )  is
   begin
      if Left.Length = 0 then
         Erase (Result);
         return;
      elsif Left.Length > Domain.Modulus.Length then
         raise Constraint_Error;
      end if;
      Allocate (Result.Value, Domain.Modulus.Length + 2);
      declare
         Carry : Word;
         A : Half_Word_Array renames Left.Value.Data (1..Left.Length);
         N : Half_Word_Array renames
                Domain.Modulus.Value.Data (1..Domain.Modulus.Length);
         T : Half_Word_Array renames
                Result.Value.Data
                (  1
                .. Domain.Modulus.Length + 2
                );
         N_1   : constant Word := Word (N (1));
         N_Inv : constant Half_Word :=
                    Domain.Modulus_Inverse.Value.Data (1);
      begin
         for I in T'Range loop
            T (I) := 0;
         end loop;
         for I in N'Range loop
            if I <= A'Last then
               declare
                  A_I : constant Word := Word (A (I));
               begin
                  Carry := 0;
                  for J in 1..I - 1 loop
                     declare -- Handling possible overflow
                        Prod : constant Word := A_I * Word (A (J));
                     begin
                        Carry := Carry
                               + Word (T (J))
                               + Shift_Left (Prod and Mask, 1);
                        T (J) := Half_Word (Carry and Half_Word_Mask);
                        Carry := Shift_Right (Carry, Bit_Width)
                               + Shift_Right (Prod, Bit_Width - 1);
                     end;
                  end loop;
                  declare -- Handling possible overflow
                     MSB : constant Word :=
                                    Shift_Right (Carry, Bit_Width);
                  begin
                     Carry := (Carry and Half_Word_Mask)
                            + Word (T (I))
                            + A_I * A_I;
                     T (I) := Half_Word (Carry and Half_Word_Mask);
                     Carry := Shift_Right (Carry, Bit_Width) + MSB;
                  end;
                  if Carry /= 0 then -- Propagate carry
                     for J in I + 1..N'Last + 2 loop
                        Carry := Carry + Word (T (J));
                        T (J) := Half_Word (Carry and Half_Word_Mask);
                        Carry := Shift_Right (Carry, Bit_Width);
                        exit when Carry = 0;
                     end loop;
                  end if;
               end;
            end if;
            declare
               M : constant Word := Word (T (1) * N_Inv);
            begin
               Carry := Shift_Right
                        (  Word (T (1)) + M * N_1,
                           Bit_Width
                        );
               for J in 2..N'Last loop
                  Carry := Carry + Word (T (J)) + M * Word (N (J));
                  T (J - 1) := Half_Word (Carry and Half_Word_Mask);
                  Carry := Shift_Right (Carry, Bit_Width);
               end loop;
            end;
            Carry := Carry + Word (T (N'Length + 1));
            T (N'Length) := Half_Word (Carry and Half_Word_Mask);
            T (N'Length + 1) :=
               Half_Word
               (  Word (T (N'Length + 2))
               +  Shift_Right (Carry, Bit_Width)
               );
         end loop;
         Result.Length := N'Length + 1; -- The highest one is not used
         while Result.Length > 0 and then T (Result.Length) = 0 loop
            Result.Length := Result.Length - 1;
         end loop;
         if (  Compare (Unbounded_Unsigned (Result), Domain.Modulus)
            /= Less
            )  then
            Sub (Unbounded_Unsigned (Result), Domain.Modulus);
         end if;
      end;
   end Mod_Square_CIOS;

   function Mod_Square_CIOS
            (  Domain : Montgomery_Domain;
               Left   : Montgomery_Number
            )  return Montgomery_Number is
      Result : Montgomery_Number;
   begin
      Mod_Square (Domain, Left, Result);
      return Result;
   end Mod_Square_CIOS;

   procedure Mod_Square_Dusse_Kaliski
             (  Domain : Montgomery_Domain;
                Left   : Montgomery_Number;
                Result : out Montgomery_Number
             )  is
   begin
      if Left.Length = 0 then
         Erase (Result);
         return;
      elsif Left.Length > Domain.Modulus.Length then
         raise Constraint_Error;
      end if;
      Allocate (Result.Value, Domain.Modulus.Length * 2 + 1);
      Square_Classroom
      (  Left.Value.Data (1..Left.Length),
         Unbounded_Unsigned (Result)
      );
      declare
         T : Half_Word_Array renames
                Result.Value.Data (1..Domain.Half_Words);
         N_Inv : constant Half_Word :=
                    Domain.Modulus_Inverse.Value.Data (1);
      begin
         for I in T'Range loop
            Add
            (  Unbounded_Unsigned (Result),
               Domain.Modulus,
               T (I) * N_Inv,
               I - 1
            );
         end loop;
         Shift_Right (Result, Domain.Half_Words);
         if (  Compare (Unbounded_Unsigned (Result), Domain.Modulus)
            /= Less
            )  then
            Sub (Unbounded_Unsigned (Result), Domain.Modulus);
         end if;
      end;
   end Mod_Square_Dusse_Kaliski;

   function Mod_Square_Dusse_Kaliski
            (  Domain : Montgomery_Domain;
               Left   : Montgomery_Number
            )  return Montgomery_Number is
      Result : Montgomery_Number;
   begin
      Mod_Square_Dusse_Kaliski (Domain, Left, Result);
      return Result;
   end Mod_Square_Dusse_Kaliski;

   procedure Mul
            (  Domain : Montgomery_Domain;
               Left   : Unbounded_Unsigned;
               Right  : Unbounded_Unsigned;
               Result : out Unbounded_Unsigned
            )  is
   begin
      if Left >= Domain.Modulus  or Right >= Domain.Modulus then
         raise Constraint_Error;
      end if;
      From_Domain
      (  Domain,
         Mod_Mul
         (  Domain,
            To_Domain (Domain, Left),
            To_Domain (Domain, Right)
         ),
         Result
      );
   end Mul;

   function Mul
            (  Domain : Montgomery_Domain;
               Left   : Unbounded_Unsigned;
               Right  : Unbounded_Unsigned
            )  return Unbounded_Unsigned is
   begin
      if Left >= Domain.Modulus or Right >= Domain.Modulus then
         raise Constraint_Error;
      end if;
      return From_Domain
             (  Domain,
                Mod_Mul
                (  Domain,
                   To_Domain (Domain, Left),
                   To_Domain (Domain, Right)
             )  );
   end Mul;

   procedure Pow
             (  Domain : Montgomery_Domain;
                Left   : Unbounded_Unsigned;
                Right  : Unbounded_Unsigned;
                Result : out Unbounded_Unsigned
             )  is
   begin
      if Left >= Domain.Modulus then
         raise Constraint_Error;
      elsif Is_Zero (Left) then
         if Is_Zero (Right) then
            Set (Result, 1);
         else
            Erase (Result);
         end if;
      elsif Is_One (Left) then
         Set (Result, 1);
      else
         From_Domain
         (  Domain,
            Mod_Pow (Domain, To_Domain (Domain, Left), Right),
            Result
         );
      end if;
   end Pow;

   function Pow
            (  Domain : Montgomery_Domain;
               Left   : Unbounded_Unsigned;
               Right  : Unbounded_Unsigned
            )  return Unbounded_Unsigned is
   begin
      if Left >= Domain.Modulus then
         raise Constraint_Error;
      elsif Is_Zero (Left) then
         if Is_Zero (Right) then
            return One;
         else
            return Zero;
         end if;
      elsif Is_One (Left) then
         return One;
      end if;
      return From_Domain
             (  Domain,
                Mod_Pow (Domain, To_Domain (Domain, Left), Right)
             );
   end Pow;

   procedure Pow_Of_Two
             (  Domain : Montgomery_Domain;
                Power  : Unbounded_Unsigned;
                Result : out Unbounded_Unsigned
             )  is
      M : Bit_Count;
   begin
      if Is_Zero (Power) then
         Set (Result, 1);
         return;
      end if;
      M := Get_MSB (Domain.Modulus);
      --
      -- Checking for powers less or equal to modulus
      --
      case Compare (Power, Half_Word (M - 1)) is
         when Less =>
            Power_Of_Two (Bit_Count (To_Half_Word (Power)), Result);
            return;
         when Equal => -- 2**N <> Modulus
            if Is_Power_Of_Two (Domain.Modulus) then
               Erase (Result);
            else
               Power_Of_Two (Bit_Count (To_Half_Word (Power)), Result);
            end if;
            return;
         when Greater =>
            null;
      end case;
      --
      -- Checking if the modulus has a simple form
      --
      declare
         Remainder : Mod_2_Remainder;
      begin
         Log2 (Domain.Modulus, M, Remainder);
         case Remainder is
            when -1 =>
               --  Modulus = 2 ** M - 1
               --     2 ** M         (mod Modulus) =
               --   = 2 ** M - 1 + 1 (mod Modulus) =
               --   = 1              (mod Modulus)
               --
               --  2 ** N = 2 ** (M * (N / M) + N mod M) =
               --         = 2 ** M ** (N / M) * 2 ** (N mod M) =
               --         = 2 ** (N mod M) (mod Modulus)
               --
               Power_Of_Two
               (  Bit_Count (Power mod Half_Word (M)),
                  Result
               );
--               Mod_Pow
--               (  Two,
--                  Power mod Half_Word (M),
--                  Domain.Modulus,
--                  Result
--               );
               return;
            when 0 =>
               -- Modulus = 2 ** M
               --    2 ** M (mod Modulus) = 0
               Erase (Result);
               return;
            when 1 =>
               --  Modulus = 2 ** M + 1
               --     2 ** M         (mod Modulus) =
               --   = 2 ** M + 1 - 1 (mod Modulus) =
               --   = -1             (mod Modulus)
               --
               --  2 ** N = 2 ** (M * (N / M) + N mod M) =
               --         = 2 ** M ** (N / M) * 2 ** (N mod M) =
               --         = -1 ** (N / M) * 2 ** (N mod M) (mod Modulus)
               --
               declare
                  Q : Unbounded_Unsigned renames Result;
                  R : Half_Word;
               begin
                  Set (Q, Power);
                  Div (Q, Half_Word (M), R);
                  if Is_Even (Q) then
--                   Mod_Pow (Two, R, Domain.Modulus, Result);
                     Power_Of_Two (Bit_Count (R), Result);
                  else -- -1 = Modulus - 1 = 2 ** M (mod Modulus)
                     Mod_Pow
                     (  Two,
                        R + Half_Word (M),
                        Domain.Modulus,
                        Result
                     );
                  end if;
                  return;
               end;
            when others =>
               null;
         end case;
      end;
      From_Domain (Domain, Mod_Pow_Of_Two (Domain, Power), Result);
   end Pow_Of_Two;

   function Pow_Of_Two
            (  Domain : Montgomery_Domain;
               Power  : Unbounded_Unsigned
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Pow_Of_Two (Domain, Power, Result);
      return Result;
   end Pow_Of_Two;

   procedure Reduce
             (  Domain : Montgomery_Domain;
                Value  : Unbounded_Unsigned;
                Result : out Unbounded_Unsigned
             )  is
      --
      -- M := ((Value mod R) * N') mod R;
      -- T := (Value + M * N) / R;
      -- if T >= N then
      --    T := T - N;
      -- end if;
   begin
      Set (Result, Value);
      Modulo (Result, Domain.Half_Words);    -- This is a simple mask
      Mul (Result, Domain.Modulus_Inverse);
      Modulo (Result, Domain.Half_Words);
      Mul (Result, Domain.Modulus);
      Add (Result, Value);
      Shift_Right (Result, Domain.Half_Words);
      if Result >= Domain.Modulus then
         Sub (Result, Domain.Modulus);
      end if;
   end Reduce;

   procedure Reduce
             (  Domain : Montgomery_Domain;
                Value  : Montgomery_Number;
                Result : out Montgomery_Number
             )  is
   begin
      Reduce
      (  Domain,
         Unbounded_Unsigned (Value),
         Unbounded_Unsigned (Result)
      );
   end Reduce;

   function Reduce
            (  Domain : Montgomery_Domain;
               Value  : Montgomery_Number
            )  return Montgomery_Number is
      Result : Montgomery_Number;
   begin
      Reduce (Domain, Value, Result);
      return Result;
   end Reduce;

   function Reducer_Inverse (Domain : Montgomery_Domain)
      return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Mod_Inv (Power_Of_Two (Domain.Bits), Domain.Modulus, Result);
      return Result;
   end Reducer_Inverse;

   procedure Set
             (  Destination : in out Montgomery_Number;
                Source      : Montgomery_Number
             )  is
   begin
      Set
      (  Unbounded_Unsigned (Destination),
         Unbounded_Unsigned (Source)
      );
   end Set;

   procedure Set_Multiplication_Algorithm
             (  Algorithm : Multiplication_Algorithm
             )  is
   begin
      Unbounded_Unsigneds.Montgomery.Algorithm := Algorithm;
   end Set_Multiplication_Algorithm;

   procedure Set_Unchecked
             (  Destination : in out Unbounded_Unsigned;
                Source      : Montgomery_Number
             )  is
   begin
      Set (Destination, Unbounded_Unsigned (Source));
   end Set_Unchecked;

   procedure Set_Unchecked
             (  Destination : in out Montgomery_Number;
                Source      : Unbounded_Unsigned
             )  is
   begin
      Set (Unbounded_Unsigned (Destination), Source);
   end Set_Unchecked;

   procedure Square
             (  Domain : Montgomery_Domain;
                Left   : Unbounded_Unsigned;
                Result : out Unbounded_Unsigned
             )  is
   begin
      if Left >= Domain.Modulus then
         raise Constraint_Error;
      end if;
      From_Domain
      (  Domain,
         Mod_Square (Domain, To_Domain (Domain, Left)),
         Result
      );
   end Square;

   function Square
            (  Domain : Montgomery_Domain;
               Left   : Unbounded_Unsigned
            )  return Unbounded_Unsigned is
   begin
      if Left >= Domain.Modulus then
         raise Constraint_Error;
      end if;
      return From_Domain
             (  Domain,
                Mod_Square (Domain, To_Domain (Domain, Left))
             );
   end Square;

   procedure Swap (Left, Right : in out Montgomery_Number) is
      Length : constant Digit_Offset := Left.Length;
      Value  : constant Vector_Ptr   := Left.Value;
   begin
      Left.Length  := Right.Length;
      Left.Value   := Right.Value;
      Right.Length := Length;
      Right.Value  := Value;
   end Swap;

   procedure Swap_Unchecked
             (  Left  : in out Unbounded_Unsigned;
                Right : in out Montgomery_Number
             )  is
      Length : constant Digit_Offset := Left.Length;
      Value  : constant Vector_Ptr   := Left.Value;
   begin
      Left.Length  := Right.Length;
      Left.Value   := Right.Value;
      Right.Length := Length;
      Right.Value  := Value;
   end Swap_Unchecked;

   procedure To_Domain
             (  Domain : Montgomery_Domain;
                Value  : Unbounded_Unsigned;
                Result : out Montgomery_Number
             )  is
      Temp : Unbounded_Unsigned;
   begin
      Mul (Value, Domain.Squared_Reducer, Temp);
      Reduce (Domain, Temp, Unbounded_Unsigned (Result));
   end To_Domain;

   procedure To_Domain
             (  Domain : Montgomery_Domain;
                Value  : Half_Word;
                Result : out Montgomery_Number
             )  is
      Temp : Unbounded_Unsigned;
   begin
      Mul (Domain.Squared_Reducer, Value, Temp);
      Reduce (Domain, Temp, Unbounded_Unsigned (Result));
   end To_Domain;

   function To_Domain
            (  Domain : Montgomery_Domain;
               Value  : Unbounded_Unsigned
            )  return Montgomery_Number is
      Temp   : Unbounded_Unsigned;
      Result : Montgomery_Number;
   begin
      Mul (Value, Domain.Squared_Reducer, Temp);
      Reduce (Domain, Temp, Unbounded_Unsigned (Result));
      return Result;
   end To_Domain;

   function To_Domain
            (  Domain : Montgomery_Domain;
               Value  : Half_Word
            )  return Montgomery_Number is
      Temp   : Unbounded_Unsigned;
      Result : Montgomery_Number;
   begin
      Mul (Domain.Squared_Reducer, Value, Temp);
      Reduce (Domain, Temp, Unbounded_Unsigned (Result));
      return Result;
   end To_Domain;
--------------------------------------------------------------------------------
   function "+" (Left, Right : Montgomery_Number)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Unbounded_Unsigned (Left) + Unbounded_Unsigned (Right)
         );
   end "+";

   function "+" (Left : Half_Word; Right : Montgomery_Number)
      return Montgomery_Number is
   begin
      return From_Unbounded_Unsigned (Left + Unbounded_Unsigned (Right));
   end "+";

   function "-" (Left, Right : Montgomery_Number)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Unbounded_Unsigned (Left) - Unbounded_Unsigned (Right)
         );
   end "-";

   function "*" (Left, Right : Montgomery_Number)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Unbounded_Unsigned (Left) * Unbounded_Unsigned (Right)
         );
   end "*";

   function "*" (Left : Half_Word; Right : Montgomery_Number)
      return Montgomery_Number is
   begin
      return From_Unbounded_Unsigned (Left * Unbounded_Unsigned (Right));
   end "*";

   function "/" (Left, Right : Montgomery_Number)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Unbounded_Unsigned (Left) / Unbounded_Unsigned (Right)
         );
   end "/";

   function "mod" (Left, Right : Montgomery_Number)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Unbounded_Unsigned (Left) mod Unbounded_Unsigned (Right)
         );
   end "mod";

   function "mod" (Left : Half_Word; Right : Montgomery_Number)
      return Montgomery_Number is
   begin
      return From_Unbounded_Unsigned (Left mod Unbounded_Unsigned (Right));
   end "mod";

   function "rem" (Left, Right : Montgomery_Number)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Unbounded_Unsigned (Left) rem Unbounded_Unsigned (Right)
         );
   end "rem";

   function "rem" (Left : Half_Word; Right : Montgomery_Number)
      return Montgomery_Number is
   begin
      return From_Unbounded_Unsigned (Left rem Unbounded_Unsigned (Right));
   end "rem";

   function "**" (Left : Montgomery_Number; Right : Bit_Count)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Unbounded_Unsigned (Left) ** Right
         );
   end "**";

   function Max (Left, Right : Montgomery_Number)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Max (Unbounded_Unsigned (Left), Unbounded_Unsigned (Right))
         );
   end Max;

   function Min (Left, Right : Montgomery_Number)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Min (Unbounded_Unsigned (Left), Unbounded_Unsigned (Right))
         );
   end Min;

   function "+" (Left : Montgomery_Number; Right : Half_Word)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned (Unbounded_Unsigned (Left) + Right);
   end "+";

   function "-" (Left : Montgomery_Number; Right : Half_Word)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned (Unbounded_Unsigned (Left) - Right);
   end "-";

   function "*" (Left : Montgomery_Number; Right : Half_Word)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned (Unbounded_Unsigned (Left) * Right);
   end "*";

   function "/" (Left : Montgomery_Number; Right : Half_Word)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned (Unbounded_Unsigned (Left) / Right);
   end "/";

   function "="  (Left, Right : Montgomery_Number) return Boolean is
   begin
      return Unbounded_Unsigned (Left) = Unbounded_Unsigned (Right);
   end "=";

   function "<"  (Left, Right : Montgomery_Number) return Boolean is
   begin
      return Unbounded_Unsigned (Left) < Unbounded_Unsigned (Right);
   end "<";

   function "<=" (Left, Right : Montgomery_Number) return Boolean is
   begin
      return Unbounded_Unsigned (Left) <= Unbounded_Unsigned (Right);
   end "<=";

   function ">"  (Left, Right : Montgomery_Number) return Boolean is
   begin
      return Unbounded_Unsigned (Left) > Unbounded_Unsigned (Right);
   end ">";

   function ">=" (Left, Right : Montgomery_Number) return Boolean is
   begin
      return Unbounded_Unsigned (Left) >= Unbounded_Unsigned (Right);
   end ">=";

   function Div_By_Power_of_Two
            (  Dividend : Montgomery_Number;
               Power    : Bit_Count
            )  return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Div_By_Power_of_Two (Unbounded_Unsigned (Dividend), Power)
         );
   end Div_By_Power_of_Two;

   function From_Half_Word (Left : Half_Word)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Unbounded_Unsigneds.From_Half_Word (Left)
         );
   end From_Half_Word;

   function From_Word (Left : Word) return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Unbounded_Unsigneds.From_Word (Left)
         );
   end From_Word;

   function From_Unbounded_Unsigned (Value : Unbounded_Unsigned)
      return Montgomery_Number is
      Result : Montgomery_Number;
   begin
      Unbounded_Unsigned (Result) := Value;
      return Result;
   end From_Unbounded_Unsigned;

   function Greatest_Common_Divisor (Left, Right : Montgomery_Number)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Greatest_Common_Divisor
            (  Unbounded_Unsigned (Left),
               Unbounded_Unsigned (Right)
         )  );
   end Greatest_Common_Divisor;

   function Get_Slice
            (  Left : Montgomery_Number;
               From : Bit_Position;
               To   : Bit_Position
            )  return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Get_Slice (Unbounded_Unsigned (Left), From, To)
         );
   end Get_Slice;

   function Inverse
            (  Left  : Montgomery_Number;
               Count : Digit_Count
            )  return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Inverse (Unbounded_Unsigned (Left), Count)
         );
   end Inverse;

   function Modulo
            (  Left  : Montgomery_Number;
               Power : Digit_Count
            )  return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Modulo (Unbounded_Unsigned (Left), Power)
         );
   end Modulo;

   function Modulo_By_Power_Of_Two
            (  Left  : Montgomery_Number;
               Right : Bit_Count
            )  return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Modulo_By_Power_Of_Two (Unbounded_Unsigned (Left), Right)
         );
   end Modulo_By_Power_Of_Two;

   function Mod_Inv
            (  Left    : Montgomery_Number;
               Modulus : Montgomery_Number
            )  return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Mod_Inv
            (  Unbounded_Unsigned (Left),
               Unbounded_Unsigned (Modulus)
         )  );
   end Mod_Inv;

   function Mod_Inv_In_Power_Of_Two
            (  Left   : Montgomery_Number;
               Power  : Bit_Position
            )  return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Mod_Inv_In_Power_Of_Two (Unbounded_Unsigned (Left), Power)
         );
   end Mod_Inv_In_Power_Of_Two;

   function Mod_Pow (Left, Right, Modulus : Montgomery_Number)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Mod_Pow
            (  Unbounded_Unsigned (Left),
               Unbounded_Unsigned (Right),
               Unbounded_Unsigned (Modulus)
         )  );
   end Mod_Pow;

   function Mod_Pow
            (  Left    : Montgomery_Number;
               Right   : Half_Word;
               Modulus : Montgomery_Number
            )  return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Mod_Pow
            (  Unbounded_Unsigned (Left),
               Right,
               Unbounded_Unsigned (Modulus)
         )  );
   end Mod_Pow;

   function Mod_Pow_By_Power_Of_Two
            (  Left, Right : Montgomery_Number;
               Modulus     : Bit_Count
            )  return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Mod_Pow_By_Power_Of_Two
            (  Unbounded_Unsigned (Left),
               Unbounded_Unsigned (Right),
               Modulus
         )  );
   end Mod_Pow_By_Power_Of_Two;

   function Mod_Pow_By_Power_Of_Two
            (  Left    : Montgomery_Number;
               Right   : Half_Word;
               Modulus : Bit_Count
            )  return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Mod_Pow_By_Power_Of_Two
            (  Unbounded_Unsigned (Left),
               Right,
               Modulus
         )  );
   end Mod_Pow_By_Power_Of_Two;

   function Mod_Pow_Of_Two
            (  Power   : Montgomery_Number;
               Modulus : Montgomery_Number
            )  return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Mod_Pow_Of_Two
            (  Unbounded_Unsigned (Power),
               Unbounded_Unsigned (Modulus)
         )  );
   end Mod_Pow_Of_Two;

   function Mul_By_Power_of_Two
            (  Multiplicand : Montgomery_Number;
               Power        : Bit_Count
            )  return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Mul_By_Power_of_Two
            (  Unbounded_Unsigned (Multiplicand),
               Power
         )  );
   end Mul_By_Power_of_Two;

   function Mul_Classroom (Left, Right : Montgomery_Number)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Mul_Classroom
            (  Unbounded_Unsigned (Left),
               Unbounded_Unsigned (Right)
         )  );
   end Mul_Classroom;

   function Mul_Karatsuba
            (  Left, Right : Montgomery_Number;
               Threshold   : Digit_Count
            )  return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Mul_Karatsuba
            (  Unbounded_Unsigned (Left),
               Unbounded_Unsigned (Right),
               Threshold
         )  );
   end Mul_Karatsuba;

   function Phi (Left : Montgomery_Number) return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned (Phi (Unbounded_Unsigned (Left)));
   end Phi;

   function Power_of_Two (Power : Bit_Count) return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Unbounded_Unsigneds.Power_of_Two (Power)
         );
   end Power_of_Two;

   procedure Set_One
             (  Domain      : Montgomery_Domain;
                Destination : out Montgomery_Number
             )  is
   begin
      Set (Unbounded_Unsigned (Destination), Domain.Reducer); -- 1'
   end Set_One;

   function Sqrt (Left : Montgomery_Number) return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Sqrt (Unbounded_Unsigned (Left))
         );
   end Sqrt;

   function Square (Left : Montgomery_Number)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Square (Unbounded_Unsigned (Left))
         );
   end Square;

   function Square_Classroom (Left : Montgomery_Number)
      return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Square_Classroom (Unbounded_Unsigned (Left))
         );
   end Square_Classroom;

   function Square_Karatsuba
            (  Left      : Montgomery_Number;
               Threshold : Digit_Count
            )  return Montgomery_Number is
   begin
      return
         From_Unbounded_Unsigned
         (  Square_Karatsuba (Unbounded_Unsigned (Left), Threshold)
         );
   end Square_Karatsuba;

   function To_Unbounded_Unsigned (Value : Montgomery_Number)
      return Unbounded_Unsigned is
   begin
      return Unbounded_Unsigned (Value);
   end To_Unbounded_Unsigned;

end Unbounded_Unsigneds.Montgomery;
