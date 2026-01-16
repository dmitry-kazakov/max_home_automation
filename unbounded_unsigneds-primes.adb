--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Unbounded_Unsigneds.Primes                  Luebeck            --
--  Implementation                                 Winter, 2024       --
--                                                                    --
--                                Last revision :  11:09 18 Jan 2025  --
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

with Ada.Numerics.Discrete_Random;

package body Unbounded_Unsigneds.Primes is

   package Dice is new Ada.Numerics.Discrete_Random (Half_Word);
   Source : Dice.Generator;

   function Fermat_Probably_Prime
            (  N      : Unbounded_Unsigned;
               Trials : Positive
            )  return Primality_Test_Outcome is
   begin
      if Is_Two (N) then
         return Prime;
      elsif Is_Even (N) then
         return Composite;
      elsif Compare (N, 1) = Equal then
         return Composite;
      elsif Compare (N, 7) /= Greater then
         return Prime;
      end if;
      declare
         Random   : Unbounded_Unsigned;
         Withness : Unbounded_Unsigned;
         N_1, X   : Unbounded_Unsigned;
      begin
         Random.Value := new Vector (N.Length); -- As long as N
         Set (N_1, N);
         Sub (N_1, 1);
         for Trial in 1..Trials loop
            --
            -- Generate a random number 2..N-2
            --
            declare
               Data   : Half_Word_Array renames Random.Value.Data;
               Length : Digit_Offset    renames Random.Length;
            begin
               loop
                  for Index in Data'Range loop
                     Data (Index) := Dice.Random (Source);
                  end loop;
                  Length := Data'Last;
                  while Length > 0 and then Data (Length) = 0 loop
                     Length := Length - 1;
                  end loop;
                  Set (Withness, Random);
                  Modulo (Withness, N_1);
                  exit when Compare (Withness, 1) = Greater;
               end loop;
            end;
            --
            -- Check the withness
            --
            Square (Withness, X);
            Modulo (X, N);
            if Is_One (X) then
               return Composite;
            end if;
            if not Is_One (Greatest_Common_Divisor (Withness, N)) then
               return Composite;
            end if;
            Mod_Pow (Withness, N_1, N, X);
            if not Is_One (X) then
               return Composite;
            end if;
         end loop;
         return Undecided;
      end;
   end Fermat_Probably_Prime;

   function Fermat_Probably_Prime
            (  Domain : Montgomery_Domain;
               Trials : Positive
            )  return Primality_Test_Outcome is
   begin
      if Compare (Domain.Modulus, 7) /= Greater then
         return Prime;
      end if;
      declare
         Random   : Unbounded_Unsigned;
         Withness : Unbounded_Unsigned;
         N_1      : Unbounded_Unsigned;
         X, Y     : Montgomery_Number;
      begin
         Random.Value := new Vector (Domain.Modulus.Length); -- N's
         Set (N_1, Domain.Modulus);
         Sub (N_1, 1);
         for Trial in 1..Trials loop
            --
            -- Generate a random number 2..N-2
            --
            declare
               Data   : Half_Word_Array renames Random.Value.Data;
               Length : Digit_Offset    renames Random.Length;
            begin
               loop
                  for Index in Data'Range loop
                     Data (Index) := Dice.Random (Source);
                  end loop;
                  Length := Data'Last;
                  while Length > 0 and then Data (Length) = 0 loop
                     Length := Length - 1;
                  end loop;
                  Set (Withness, Random);
                  Modulo (Withness, N_1);
                  exit when Compare (Withness, 1) = Greater;
               end loop;
            end;
            --
            -- Check the withness
            --
            To_Domain (Domain, Withness, Y);
            Mod_Square (Domain, Y, X);
            if Is_One (Domain, X) then
               return Composite;
            end if;
            if not Is_One
                   (  Greatest_Common_Divisor (Withness, Domain.Modulus)
                   )  then
               return Composite;
            end if;
            Mod_Pow (Domain, Y, N_1, X);
            if not Is_One (Domain, X) then
               return Composite;
            end if;
         end loop;
         return Undecided;
      end;
   end Fermat_Probably_Prime;

   function Fibonacci (N : Natural) return Unbounded_Unsigned is
      --
      -- F(2k)   = F(k)(2F(k+1)) - F(k))
      -- F(2k+1) = F(k+1)*F(k+1) + F(k)*F(k)
      --
      A, B, D, E : Unbounded_Unsigned;
      M          : Natural := N;
      Bit        : Natural := 1;
   begin
      while M > 0 loop
         Bit := Bit * 2;
         M   := M / 2;
      end loop;
      Set (B, 1);
      while Bit > 1 loop
         Square (A, E);    -- A = Fm, B = Fm+1
         Square (B, D);
         Add (E, D);
         Set (D, B);
         Mul_By_Power_Of_Two (D, 1);
         Sub (D, A);
         Mul (D, A);
         Swap (A, D);
         Swap (B, E);
         M := M * 2;       -- A = Fm, B = Fm+1
         Bit := Bit / 2;
         if (N / Bit) mod 2 /= 0 then
            Set (D, B);
            Add (D, A);
            Swap (A, B);
            Swap (D, B);
            M := M + 1;    -- A = Fm, B = Fm+1
        end if;
      end loop;
      return A;
   end Fibonacci;

   function Fibonacci
            (  N       : Unbounded_Unsigned;
               Modulus : Unbounded_Unsigned
            )  return Unbounded_Unsigned is
      A, B, D, E : Signed;
      M          : Unbounded_Unsigned;
   begin
      if Is_Zero (Modulus) then
         raise Constraint_Error;
      end if;
      Set (B, 1);
      for Bit in reverse 1..Get_MSB (N) loop
         Square (A, E);              -- A = Fm, B = Fm+1
         Square (B, D);
         Add (E, D);                 -- A ** 2 + B ** 2
         Set (D, B);
         Mul_By_Power_Of_Two (D, 1); -- 2 * B
         Sub (D, A);                 -- 2 * B - A
         Mul (D, A);                 -- A * (2 * B - A)
         Swap (A, D);                -- A' = A * (2 * B - A)
         Swap (B, E);                -- B' = A ** 2 + B ** 2
         Modulo (A.Value, Modulus);
         Modulo (B.Value, Modulus);
         Mul_By_Power_Of_Two (M, 1); -- A = Fm, B = Fm+1
         if Get_Bit (N, Bit) then
            Set (D, B);
            Add (D, A);              -- A + B
            Swap (A, B);             -- A' = B
            Swap (D, B);             -- B' = A + B
            Modulo (A.Value, Modulus);
            Modulo (B.Value, Modulus);
            Add (M, 1);              -- A = Fm, B = Fm+1
        end if;
      end loop;
      if A.Sign and then A.Value.Length > 0 then
         Sub_2 (Modulus, A.Value);
      end if;
      return A.Value;
   end Fibonacci;

    function Fibonacci
             (  Domain : Montgomery_Domain;
                N      : Unbounded_Unsigned
             )  return Montgomery_Number is
      A, B, D, E : Montgomery_Number;
      M          : Unbounded_Unsigned;
   begin
      B := From_Unbounded_Unsigned (Domain.Reducer); -- 1'
      for Bit in reverse 1..Get_MSB (N) loop
         Mod_Square (Domain, A, E);    -- A = Fm, B = Fm+1
         Mod_Square (Domain, B, D);
         Mod_Add    (Domain, E, D);    -- A ** 2 + B ** 2
         Set (D, B);
         Mod_Double (Domain, D);       -- 2 * B
         Mod_Sub (Domain, D, A);       -- 2 * B - A
         Mod_Mul (Domain, D, A, B);    -- A * (2 * B - A)
         Swap (A, B);                  -- A' = A * (2 * B - A)
         Swap (B, E);                  -- B' = A ** 2 + B ** 2
         Mul_By_Power_Of_Two (M, 1);   -- A = Fm, B = Fm+1
         if Get_Bit (N, Bit) then
            Set (D, B);
            Mod_Add (Domain, D, A);    -- A + B
            Swap (A, B);               -- A' = B
            Swap (D, B);               -- B' = A + B
            Add  (M, 1);               -- A = Fm, B = Fm+1
        end if;
      end loop;
      return A;
   end Fibonacci;

   function Fibonacci_Probably_Prime (N : Unbounded_Unsigned)
      return Primality_Test_Outcome is
   begin
      if Is_Two (N) then
         return Prime;
      elsif Is_Even (N) then
         return Composite;
      elsif Compare (N, 1) = Equal then
         return Composite;
      elsif Compare (N, 7) /= Greater then
         return Prime;
      end if;
      declare
         V : Unbounded_Unsigned;
         J : constant Integer := Jacobi_Symbol (Five, N);
      begin
         Set (V, N);
         if J >= 0 then
            Sub (V, Half_Word (J));
         else
            Add (V, Half_Word (-J));
         end if;
         if Is_Zero (Fibonacci (V, N)) then
            return Undecided;
         else
            return Composite;
         end if;
      end;
   end Fibonacci_Probably_Prime;

   function Fibonacci_Probably_Prime (Domain : Montgomery_Domain)
      return Primality_Test_Outcome is
   begin
      if Compare (Domain.Modulus, 7) /= Greater then
         return Prime;
      end if;
      declare
         N : Unbounded_Unsigned;
         J : constant Integer := Jacobi_Symbol (Five, Domain.Modulus);
      begin
         Set (N, Domain.Modulus);
         if J >= 0 then
            Sub (N, Half_Word (J));
         else
            Add (N, Half_Word (-J));
         end if;
         if Is_Zero (Fibonacci (Domain, N)) then
            return Undecided;
         else
            return Composite;
         end if;
      end;
   end Fibonacci_Probably_Prime;

   function Is_Prime (N : Half_Word)
      return Certain_Primality_Test_Outcome is
      T : constant Word := Word (N);
      S : Word;
      D : Word;
   begin
      if N <= 3 then
         if N = 0 then
            return Composite;
         else
            return Prime;
         end if;
      elsif N mod 2 = 0 then
         return Composite;
      elsif N <= 7 then
         return Prime;
      end if;
      for I in Primes'Range loop
         if N mod Half_Word (Primes (I)) = 0 then
            if N = Half_Word (Primes (I)) then
               return Prime;
            else
               return Composite;
            end if;
         end if;
      end loop;
      Sqrt (T, S, D);
      if S * S < T then
         S := S + 1;
      end if;
      D := Word (Primes (Primes'Last)) + 2;
      while D <= S loop
         if T mod D = 0 then
            return Composite;
         end if;
         D := D + 2;
      end loop;
      return Prime;
   end Is_Prime;

   function Is_Prime
            (  N            : Unbounded_Unsigned;
               Trials       : Positive;
               Extended     : Natural := 0;
               Trial_Primes : Natural := 1_800
            )  return Primality_Test_Outcome is
   begin
      if Is_Two (N) then
         return Prime;
      elsif Is_Even (N) then
         return Composite;
      elsif Compare (N, 1) = Equal then
         return Composite;
      elsif Compare (N, 7) /= Greater then
         return Prime;
      end if;
      case Compare (N, 1_000_000) is
         when Less | Equal =>
            return Is_Prime (To_Half_Word (N));
         when Greater =>
            for I in Primes'First
                  .. Positive'Min (Primes'Last, Trial_Primes) loop
               if N mod Half_Word (Primes (I)) = 0 then
                  if Compare (N, Half_Word (Primes (I))) = Equal then
                     return Prime;
                  else
                     return Composite;
                  end if;
               end if;
            end loop;
      end case;
      declare
         Domain   : constant Montgomery_Domain := Create (N);
         Withness : Unbounded_Unsigned;
         Random   : Unbounded_Unsigned;
         D, X     : Unbounded_Unsigned;
         N_1      : Unbounded_Unsigned;
         S        : Half_Word := 0;

         function Is_SPRP_2 return Boolean is
         begin
            Pow_Of_Two (Domain, D, Withness);
            if Is_One (Withness) or else
               Compare (Withness, N_1) = Equal then
               return True;
            end if;
            for I in 1..S loop
               Swap (X, Withness);
               Square (Domain, X, Withness);
               if Compare (Withness, N_1) = Equal then
                  return True;
               end if;
            end loop;
            return False;
         end Is_SPRP_2;

         function Is_SPRP (A : Unbounded_Unsigned) return Boolean is
         begin
            if Compare (A, Domain.Modulus) /= Less then
               return True;
            end if;
            Pow (Domain, A, D, Withness);
            if Is_One (Withness) or else
               Compare (Withness, N_1) = Equal then
               return True;
            end if;
            for I in 1..S loop
               Swap (X, Withness);
               Square (Domain, X, Withness);
               if Compare (Withness, N_1) = Equal then
                  return True;
               end if;
            end loop;
            return False;
         end Is_SPRP;
      begin
         --
         -- Find S, D such that N = 2 ** S * D + 1
         --
         Set (N_1, N);
         Sub (N_1, 1);
         D := N_1;
         Truncate (D, Bit_Count (S));
         --
         -- Running strong probable primality test for 2
         --
         if not Is_SPRP_2 then
            return Composite;
         end if;
         if Extended > 0 then
            --
            -- Running some more strong probable primality tests
            --
            for I in 1..Positive'Min (SPRP_Base'Last, Extended) loop
               if not Is_SPRP (From_Half_Word (SPRP_Base (I))) then
                  return Composite;
               end if;
            end loop;
         end if;

         Random.Value := new Vector (N.Length); -- As long as N
         --
         -- Running Miller-Rabin test
         --
         for Trial in 1..Trials loop
            --
            -- Generate a random number 2..N-1
            --
            declare
               Data   : Half_Word_Array renames Random.Value.Data;
               Length : Digit_Offset    renames Random.Length;
            begin
               loop
                  for Index in Data'Range loop
                     Data (Index) := Dice.Random (Source);
                  end loop;
                  Length := Data'Last;
                  while Length > 0 and then Data (Length) = 0 loop
                     Length := Length - 1;
                  end loop;
                  Set (Withness, Random);
                  Modulo (Withness, N_1);
                  exit when Compare (Withness, 1) = Greater;
               end loop;
            end;
            --
            -- Check the withness
            --
            Pow (Domain, Withness, D, X);
            if Is_One (X) or else X = N_1 then
               return Prime; -- No witness found
            end if;
            for I in 1..S loop
               Swap (Withness, X);
               Square (Domain, Withness, X);
               if Is_One (X) then
                  return Composite; -- Composite number
               elsif Compare (X, N_1) = Equal then
                  return Prime;  -- Prime number
               end if;
            end loop;
         end loop;
         return Undecided; -- No witness found after trials
      end;
   end Is_Prime;

   procedure Jacobi_Symbol
             (  N, K   : in out Unbounded_Unsigned;
                Symbol : out Integer
             )  is
      type Ptr is access all Unbounded_Unsigned;
      R : Half_Word;
      T : Integer := 1;
      N_Ptr : Ptr := N'Access;
      K_Ptr : Ptr := K'Access;
      Temp  : Ptr;
   begin
      if Is_Even (K) then
         raise Constraint_Error;
      end if;
      Modulo (N_Ptr.all, K_Ptr.all);
      T := 1;
      while not Is_Zero (N_Ptr.all) loop
         while Is_Even (N_Ptr.all) loop
            Div (N_Ptr.all, 2, R);
            R := K_Ptr.all mod 8;
            if R = 3 or else R = 5 then
               T := -T;
            end if;
         end loop;
         Temp  := N_Ptr;
         N_Ptr := K_Ptr;
         K_Ptr := Temp;
         if N_Ptr.all mod 4 = 3 and then K_Ptr.all mod 4 = 3 then
            T := -T;
         end if;
         Modulo (N_Ptr.all, K_Ptr.all);
      end loop;
      if Is_One (K_Ptr.all) then
         Symbol := T;
      else
         Symbol := 0;
      end if;
   end Jacobi_Symbol;

   function Jacobi_Symbol (X, M : Unbounded_Unsigned) return Integer is
      N : Unbounded_Unsigned := X;
      K : Unbounded_Unsigned := M;
      Result : Integer;
   begin
      Jacobi_Symbol (N, K, Result);
      return Result;
   end Jacobi_Symbol;

   function Lucas_Lehmer (N : Unbounded_Unsigned)
      return Primality_Test_Outcome is
   begin
      if Is_One (N) then
         return Composite;
      elsif Is_Two (N) then
         return Prime;
      elsif Is_Even (N) then
         return Composite;
      end if;
   --
   -- Checking if all bits are set, i.e. X = 2 ** K - 1
   --
      declare
         Data : Half_Word_Array renames N.Value.Data;
      begin
         if N.Length = 1 and then Data (1) = 3 then
            return Prime; -- Special case
         end if;
         for I in 1..N.Length - 1 loop
            if Data (I) /= Half_Word'Last then
               return Undecided;
            end if;
         end loop;
         declare
            Highest : Half_Word := Data (N.Length);
         begin
            if Highest < Half_Word'Last then
               for Power in 1..Bit_Width - 1 loop
                  if (Highest and 1) = 0 then
                     exit when Shift_Right (Highest, 1) = 0;
                     return Undecided;
                  end if;
                  Highest := Shift_Right (Highest, 1);
               end loop;
            end if;
         end;
      end;
      declare
         S : Unbounded_Unsigned := From_Half_Word (4); --S(1) = 4;
         X : Unbounded_Unsigned;
      begin
         for I in 1..Get_MSB (N) - 2 loop
            Square (S, X);
            Sub (X, 2);
            Swap (X, S);
            Modulo (S, N);
         end loop;
         if Is_Zero (S) then
            return Prime;
         else
            return Composite;
         end if;
      end;
   end Lucas_Lehmer;

   function Lucas_Lehmer (Domain : Montgomery_Domain)
      return Primality_Test_Outcome is
   begin
   --
   -- Checking if all bits are set, i.e. X = 2 ** K - 1
   --
      declare
         Data : Half_Word_Array renames Domain.Modulus.Value.Data;
      begin
         if Domain.Modulus.Length = 1 and then Data (1) = 3 then
            return Prime; -- Special case
         end if;
         for I in 1..Domain.Modulus.Length - 1 loop
            if Data (I) /= Half_Word'Last then
               return Undecided;
            end if;
         end loop;
         declare
            Highest : Half_Word := Data (Domain.Modulus.Length);
         begin
            if Highest < Half_Word'Last then
               for Power in 1..Bit_Width - 1 loop
                  if (Highest and 1) = 0 then
                     exit when Shift_Right (Highest, 1) = 0;
                     return Undecided;
                  end if;
                  Highest := Shift_Right (Highest, 1);
               end loop;
            end if;
         end;
      end;
      declare
         S, X : Montgomery_Number;
         Two  : Montgomery_Number;
      begin
         Two := From_Unbounded_Unsigned (Domain.Reducer);
         Mod_Double (Domain, Two);
         Set (S, Two);
         Mod_Double (Domain, S);  --S(1) = 4
         for I in 1..Get_MSB (Domain.Modulus) - 2 loop
            Mod_Square (Domain, S, X);
            Mod_Sub (Domain, X, Two);
            Swap (X, S);
         end loop;
         if Is_Zero (S) then
            return Prime;
         else
            return Composite;
         end if;
      end;
   end Lucas_Lehmer;

   function Lucas_Probably_Prime (N : Unbounded_Unsigned)
      return Primality_Test_Outcome is
      X         : Unbounded_Unsigned;
      Root      : Unbounded_Unsigned;
      Remainder : Unbounded_Unsigned;
      P         : Half_Word := 3;
   begin
      if Is_Two (N) then
         return Prime;
      elsif Is_Even (N) then
         return Composite;
      elsif Compare (N, 1) = Equal then
         return Composite;
      elsif Compare (N, 7) /= Greater then
         return Prime;
      end if;
      loop
         Set (X, P * P - 4);
         case Jacobi_Symbol (X, N) is
            when -1 =>
               exit;
            when  0 =>
               case Compare (N, P + 2) is
                  when Equal =>
                     return Prime;
                  when Less | Greater =>
                     return Composite;
               end case;
            when others =>
               P := P + 1;
               if P = 41 then
                  Set (X, N);
                  Sqrt (X, Root, Remainder);
                  if Is_Zero (Remainder) then
                     return Composite;
                  end if;
               elsif P = 1000 then -- Impossible
                  return Undecided;
               end if;
         end case;
      end loop;
      declare
         N_2   : Unbounded_Unsigned := N;              -- N - 2
         N_P   : Unbounded_Unsigned := N;              -- N - P
         VK    : Unbounded_Unsigned renames Root;      -- Reuse
         VK_1  : Unbounded_Unsigned renames Remainder; -- Reuse
         Power : Bit_Count;
      begin
         Set (X, N);
         Add (X, 1);
         Truncate (X, Power);
         Sub (N_2, 2);
         Sub (N_P, P);
         --
         -- V0 = 2
         -- V1 = P
         -- Vk = P * Vk-1 - Vk-2
         --
         Set (VK, 2);
         Set (VK_1, P);
         --
         -- V2k   = Vk * Vk - 2
         -- V2k+1 = Vk * Vk+1 - P
         --
         for Index in reverse 1..Get_MSB (X) loop
            if Get_Bit (X, Index) then
               -- k' = 2k+1
               -- V(k') = V(2k+1) = V(k) * V(k+1) - P
               Mul (VK, VK_1);
               case Compare (VK, P) is
                  when Less =>
                     Add (VK, N_P);
                  when Equal =>
                     Erase (VK);
                  when Greater =>
                     Sub (VK, P);
               end case;
               Modulo (VK, N);
               -- V(k'+1) = V(2k+2) = V(k+1) ** 2 - 2
               VK_1 := Square (VK_1);
               case Compare (VK_1, 2) is
                  when Less =>
                     Add (VK_1, N_2);
                  when Equal =>
                     Erase (VK_1);
                  when Greater =>
                     Sub (VK_1, 2);
               end case;
               Modulo (VK_1, N);
            else
               -- k' = 2k
               -- V(k'+1) = V(2k+1) = V(k) * V(k+1) - P
               Mul (VK_1, VK);
               case Compare (VK_1, P) is
                  when Less =>
                     Add (VK_1, N_P);
                  when Equal =>
                     Erase (VK_1);
                  when Greater =>
                     Sub (VK_1, P);
               end case;
               Modulo (VK_1, N);
               -- V(k') = V(2k) = V(k) ** 2 - 2
               VK := Square (VK);
               case Compare (VK, 2) is
                  when Less =>
                     Add (VK, N_2);
                  when Equal =>
                     Erase (VK);
                  when Greater =>
                     Sub (VK, 2);
               end case;
               Modulo (VK, N);
            end if;
         end loop;
         --
         -- Now K = S, so Vk = Vs. Checking Vs = 2 or N-2 (mod N)
         --
         if VK = 2 or else Compare (VK, N_2) = Equal then
         --
         -- Check U(s) = 0
         -- (Jacobsen, apply Crandall and Pomerance)
         --
         --	U(k) = D ** -1 * (2 * Vk+1 - P * Vk)
         --
         --    2 * Vk+1 = P * Vk (mod N)
         -- or
         --    P * Vk - 2 * Vk+1 = 0 (mod N)
         --
            Set (X, VK);
            Mul (X, P);
            Mul (VK_1, 2);
            case Compare (X, VK_1) is
               when Equal =>
                  return Undecided;
               when Less  =>
                  Sub (VK_1, X);
                  Modulo (VK_1, N);
                  if Is_Zero (VK_1) then
                     return Undecided;
                  end if;
               when Greater =>
                  Sub (X, VK_1);
                  Modulo (X, N);
                  if Is_Zero (X) then
                     return Undecided;
                  end if;
            end case;
         end if;
         --
         -- Check V (2 ** T s) mod N = 0
         --
         for Index in 1..Power loop
            if Is_Zero (VK) then
               return Undecided;
            end if;
            --
            -- Optimization: Vk = 2 is a fixed point for
            -- Vk' = Vk ** 2 - 2,
            -- If Vk = 2, we can stop: we  will never find a Vk = 0
            --
            if Compare (VK, 2) = Equal then
               return Composite;
            end if;
            -- k' = 2k
            -- V(k') = V(2k) = V(k) ** 2 - 2
            VK := Square (VK);
            Sub (VK, 2);
            Modulo (VK, N);
         end loop;
         return Composite;
      end;
   end Lucas_Probably_Prime;

   function Lucas_Probably_Prime (Domain : Montgomery_Domain)
      return Primality_Test_Outcome is
      VK    : Montgomery_Number;
      VK_1  : Montgomery_Number;
      X     : Montgomery_Number;
      P     : Half_Word := 3;
      Exp   : Unbounded_Unsigned;
      Power : Bit_Count;
   begin
      if Compare (Domain.Modulus, 7) /= Greater then
         return Prime;
      end if;
      declare
         Root      : Unbounded_Unsigned;
         Remainder : Unbounded_Unsigned;
         T         : Unbounded_Unsigned;
      begin
         loop
            Set (T, P * P - 4);
            case Jacobi_Symbol (T, Domain.Modulus) is
               when -1 =>
                  exit;
               when  0 =>
                  case Compare (Domain.Modulus, P + 2) is
                     when Equal =>
                        return Prime;
                     when Less | Greater =>
                        return Composite;
                  end case;
               when others =>
                  P := P + 1;
                  if P = 41 then
                     Set (T, Domain.Modulus);
                     Sqrt (T, Root, Remainder);
                     if Is_Zero (Remainder) then
                        return Composite;
                     end if;
                  elsif P = 1000 then -- Impossible
                     return Undecided;
                  end if;
            end case;
         end loop;
         Set (Exp, Domain.Modulus);
         Add (Exp, 1);
         Truncate (Exp, Power);

         X    := From_Unbounded_Unsigned (T);          -- Reuse
         VK   := From_Unbounded_Unsigned (Root);       -- Reuse
         VK_1 := From_Unbounded_Unsigned (Remainder);  -- Reuse
      end;
      declare
         D_2 : constant Montgomery_Number := To_Domain (Domain, 2);
         D_P : constant Montgomery_Number := To_Domain (Domain, P);
         N_2 : Montgomery_Number; -- -2'
         N_P : Montgomery_Number; -- -P'
      begin
         Mod_Sub (Domain, N_2, D_2);
         Mod_Sub (Domain, N_P, D_P);
         --
         -- V0 = 2
         -- V1 = P
         -- Vk = P * Vk-1 - Vk-2
         --
         Set (VK, D_2);
         Set (VK_1, D_P);
         --
         -- V2k   = Vk * Vk - 2
         -- V2k+1 = Vk * Vk+1 - P
         --
         for Index in reverse 1..Get_MSB (Exp) loop
            if Get_Bit (Exp, Index) then
               -- k' = 2k+1
               -- V(k') = V(2k+1) = V(k) * V(k+1) - P
               Mod_Mul (Domain, VK, VK_1, X);
               Mod_Sub (Domain, X, D_P, VK);
               -- V(k'+1) = V(2k+2) = V(k+1) ** 2 - 2
               Mod_Square (Domain, VK_1, X);
               Mod_Sub    (Domain, X, D_2, VK_1);
            else
               -- k' = 2k
               -- V(k'+1) = V(2k+1) = V(k) * V(k+1) - P
               Mod_Mul (Domain, VK_1, VK, X);
               Mod_Sub (Domain, X, D_P, VK_1);
               -- V(k') = V(2k) = V(k) ** 2 - 2
               Mod_Square (Domain, VK, X);
               Mod_Sub    (Domain, X, D_2, VK);
            end if;
         end loop;
         --
         -- Now K = S, so Vk = Vs. Checking Vs = 2 or N-2 (mod N)
         --
         if Compare (VK, D_2) = Equal or else
            Compare (VK, N_2) = Equal then
         --
         -- Check U(s) = 0
         -- (Jacobsen, apply Crandall and Pomerance)
         --
         --	U(k) = D ** -1 * (2 * Vk+1 - P * Vk)
         --
         --    2 * Vk+1 = P * Vk (mod N)  <=>
         --    P * Vk - 2 * Vk+1 = 0 (mod N)
         --
            Mod_Double (Domain, VK_1);        -- Vk+1 = Vk+1 * 2
            Mod_Mul (Domain, VK, D_P, X);     -- X    = Vk * P
            case Compare (X, VK_1) is
               when Equal =>
                  return Undecided;
               when Less | Greater =>
                  Mod_Sub (Domain, VK_1, X);
                  if Is_Zero (VK_1) then
                     return Undecided;
                  end if;
            end case;
         end if;
         --
         -- Check V (2 ** T s) mod N = 0
         --
         for Index in 1..Power loop
            if Is_Zero (VK) then
               return Undecided;
            end if;
            --
            -- Optimization: Vk = 2 is a fixed point for
            -- Vk' = Vk ** 2 - 2,
            -- If Vk = 2, we can stop: we  will never find a Vk = 0
            --
            if Compare (VK, D_2) = Equal then
               return Composite;
            end if;
            -- k' = 2k
            -- V(k') = V(2k) = V(k) ** 2 - 2
            Mod_Square (Domain, VK, X);
            Mod_Sub    (Domain, X, D_2, VK);
         end loop;
         return Composite;
      end;
   end Lucas_Probably_Prime;

end Unbounded_Unsigneds.Primes;
