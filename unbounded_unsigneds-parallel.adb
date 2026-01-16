--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Unbounded_Unsigneds.Parallel                Luebeck            --
--  Implementation                                 Winter, 2024       --
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

package body Unbounded_Unsigneds.Parallel is

   procedure Execute
             (  Job    : in out Mul_Job;
                Server : in out Job_Server'Class
             )  is
      Left  : Half_Word_Array renames
                 Job.Left.Data (Job.Left_First..Job.Left_Last);
      Right : Half_Word_Array renames
                 Job.Right.Value.Data (1..Job.Right.Length);
   begin
      Mul_Classroom (Left, Right, Job.Result.all);
   end Execute;

   procedure Execute
             (  Job    : in out Mul_Karatsuba_Job;
                Server : in out Job_Server'Class
             )  is
      Left  : Half_Word_Array renames
                 Job.Left.Data (Job.Left_First..Job.Left_Last);
      Right : Half_Word_Array renames
                 Job.Right.Data (Job.Right_First..Job.Right_Last);
      L : constant Digit_Offset :=
                   Digit_Offset'Max (Left'Length, Right'Length);
      N : constant Digit_Offset := L / 2;
   begin
      if (  Left'Length + Right'Length <= 2 * Job.Threshold
         or else
            Left'Length  <= Digit_Offset'Max (N, Job.Threshold)
         or else
            Right'Length <= Digit_Offset'Max (N, Job.Threshold)
         )  then
         Mul_Classroom (Left, Right, Job.Result);
      else
         declare
            J_1 : Mul_Karatsuba_Job;
            J_2 : Mul_Karatsuba_Job;
            J_3 : Mul_Karatsuba_Job;

            Z0 : Unbounded_Unsigned renames J_2.Result;
            Z1 : Unbounded_Unsigned renames J_1.Result;
            Z2 : Unbounded_Unsigned renames J_3.Result;
            S1 : Unbounded_Unsigned renames Job.Result; -- L1 + H1
            S2 : Unbounded_Unsigned;                    -- L2 + H2
            L1 : Half_Word_Array renames
                    Left (Left'First ..Left'First + N - 1);
            H1 : Half_Word_Array renames
                    Left (Left'First + N..Left'Last);
            L2 : Half_Word_Array renames
                    Right (Right'First..Right'First + N - 1);
            H2 : Half_Word_Array renames
                    Right (Right'First + N..Right'Last);
         begin
            J_2.Left := Job.Left;
            J_2.Left_First := Job.Left_First;
            J_2.Left_Last  := Job.Left_First + N - 1;
            J_2.Right := Job.Right;
            J_2.Right_First := Job.Right_First;
            J_2.Right_Last  := Job.Right_First + N - 1;

            J_3.Left := Job.Left;
            J_3.Left_First := Job.Left_First;
            J_3.Left_Last  := Job.Left_First + N - 1;
            J_3.Right := Job.Right;
            J_3.Right_First := Job.Right_First;
            J_3.Right_Last  := Job.Right_First + N - 1;

            Set (S1, L1);
            Add (S1, H1);                               -- L1 + H1
            Set (S2, L2);
            Add (S2, H2);                               -- L2 + H2
            if S1.Length > 0 and then S1.Length > 0 then
               J_1.Left := S1.Value;                    -- S1
               J_1.Left_First := 1;
               J_1.Left_Last  := S1.Length;
               J_1.Right := S2.Value;                   -- S2
               J_1.Right_First := 1;
               J_1.Right_Last  := S2.Length;
               J_1.Threshold := Job.Threshold;
               Enqueue (Server, J_1);                   -- S1 * S2 -> Z1
            end if;
            J_2.Left := Job.Left;                       -- L1
            J_2.Left_First := Job.Left_First;
            J_2.Left_Last  := Job.Left_First + N - 1;
            J_2.Right := Job.Right;                     -- L2
            J_2.Right_First := Job.Right_First;
            J_2.Right_Last  := Job.Right_First + N - 1;
            J_2.Threshold   := Job.Threshold;
            Enqueue (Server, J_2);                      -- L1 * L2 -> Z0

            J_3.Left := Job.Left;                       -- H1
            J_3.Left_First := Job.Left_First + N;
            J_3.Left_Last  := Job.Left_Last;
            J_3.Right := Job.Right;                     -- H2
            J_3.Right_First := Job.Right_First + N;
            J_3.Right_Last  := Job.Right_Last;
            J_3.Threshold   := Job.Threshold;
            Enqueue (Server, J_3);                      -- H1 * H2 -> Z2

            Wait (J_1);
            Wait (J_2);
            Wait (J_3);

            Sub (Z1, Z0);
            Sub (Z1, Z2);
            Add (Z0, Z1, N);
            Add (Z0, Z2, N * 2);
            Job.Result := Z0;
         end;
      end if;
   end Execute;

   procedure Execute
             (  Job    : in out Square_Karatsuba_Job;
                Server : in out Job_Server'Class
             )  is
      Left : Half_Word_Array renames
                Job.Left.Data (Job.Left_First..Job.Left_Last);
   begin
      if Left'Length <= Digit_Offset'Max (1, Job.Threshold) then
         Square_Classroom (Left, Job.Result);
      else
         declare
            J_1 : Square_Karatsuba_Job;
            J_2 : Square_Karatsuba_Job;
            J_3 : Square_Karatsuba_Job;

            N  : constant Digit_Offset := Left'Length / 2;
            Z0 : Unbounded_Unsigned renames J_2.Result;
            Z1 : Unbounded_Unsigned renames J_1.Result;
            Z2 : Unbounded_Unsigned renames J_3.Result;
            S  : Unbounded_Unsigned renames Job.Result; -- L + H
            L  : Half_Word_Array renames
                    Left (Left'First ..Left'First + N - 1);
            H  : Half_Word_Array renames
                    Left (Left'First + N..Left'Last);
         begin
            J_2.Left := Job.Left;
            J_2.Left_First := Job.Left_First;
            J_2.Left_Last  := Job.Left_First + N - 1;

            J_3.Left := Job.Left;
            J_3.Left_First := Job.Left_First;
            J_3.Left_Last  := Job.Left_First + N - 1;

            Set (S, L);
            Add (S, H);                                  -- L + H
            if S.Length > 0 then
               J_1.Left := S.Value;                     -- S
               J_1.Left_First := 1;
               J_1.Left_Last  := S.Length;
               J_1.Threshold := Job.Threshold;
               Enqueue (Server, J_1);                    -- S ** 2 -> Z1
            end if;
            J_2.Left := Job.Left;                        -- L
            J_2.Left_First := Job.Left_First;
            J_2.Left_Last  := Job.Left_First + N - 1;
            J_2.Threshold   := Job.Threshold;
            Enqueue (Server, J_2);                       -- L ** 2 -> Z0

            J_3.Left := Job.Left;                        -- H
            J_3.Left_First := Job.Left_First + N;
            J_3.Left_Last  := Job.Left_Last;
            J_3.Threshold   := Job.Threshold;
            Enqueue (Server, J_3);                       -- H ** 2 -> Z2

            Wait (J_1);
            Wait (J_2);
            Wait (J_3);

            Sub (Z1, Z0);
            Sub (Z1, Z2);
            Add (Z0, Z1, N);
            Add (Z0, Z2, N * 2);
            Job.Result := Z0;
         end;
      end if;
   end Execute;

   procedure Mod_Mul
             (  Domain    : Montgomery_Domain;
                Left      : Montgomery_Number;
                Right     : Montgomery_Number;
                Server    : in out Job_Server'Class;
                Cache     : in out Unbounded_Unsigned_Array;
                Threshold : Digit_Count;
                Result    : out Montgomery_Number
             )  is
   begin
      if Cache'Length > Server.Workers_Count + 3 then
         declare
            T : Unbounded_Unsigned renames Cache (Cache'Last - 2);
            U : Unbounded_Unsigned renames Cache (Cache'Last - 1);
            S : Unbounded_Unsigned renames Cache (Cache'Last);
         begin
            Mul
            (  To_Unbounded_Unsigned (Left),
               To_Unbounded_Unsigned (Right),
               Server,
               Cache (1..Cache'Last - 3),
               Threshold,
               T
            );
            Mul
            (  T,
               Domain.Modulus_Inverse,
               Server,
               Cache (1..Cache'Last - 3),
               Threshold,
               U
            );
            Get_Slice (U, 1, Domain.Bits);
            Mul
            (  U,
               Domain.Modulus,
               Server,
               Cache (1..Cache'Last - 3),
               Threshold,
               S
            );
            Add (T, S);
            Shift_Right (T, Domain.Half_Words);
            if Compare (T, Domain.Modulus) = Greater then
               Sub (T, Domain.Modulus);
            end if;
            Swap_Unchecked (T, Result);
         end;
      else
         declare
            T, U, S : Unbounded_Unsigned;
         begin
            Mul
            (  To_Unbounded_Unsigned (Left),
               To_Unbounded_Unsigned (Right),
               Server,
               Cache,
               Threshold,
               T
            );
            Mul
            (  T,
               Domain.Modulus_Inverse,
               Server,
               Cache,
               Threshold,
               U
            );
            Get_Slice (U, 1, Domain.Bits);
            Mul
            (  U,
               Domain.Modulus,
               Server,
               Cache,
               Threshold,
               S
            );
            Add (T, S);
            Shift_Right (T, Domain.Half_Words);
            if Compare (T, Domain.Modulus) = Greater then
               Sub (T, Domain.Modulus);
            end if;
            Swap_Unchecked (T, Result);
         end;
      end if;
   end Mod_Mul;

   procedure Mod_Pow
             (  Domain    : Montgomery_Domain;
                Left      : Montgomery_Number;
                Right     : Unbounded_Unsigned;
                Server    : in out Job_Server'Class;
                Cache     : in out Unbounded_Unsigned_Array;
                Threshold : Digit_Count;
                Result    : out Montgomery_Number
             )  is
   begin
      if Is_Zero (Left) then
         if Right.Length = 0 then
            Set_One (Domain, Result); -- 1'
         else
            Erase (Result); -- 0' = 0
         end if;
         return;
      elsif Is_One (Domain, Left) or else Right.Length = 0 then
         Set_One (Domain, Result); -- 1'
         return;
      end if;
      declare
         Base : Montgomery_Number;
         X    : Montgomery_Number;
      begin
         Set (Base, Left);
         if Is_Odd (Right) then -- Reducing the first step of the loop
            Set_One (Domain, X);
            Mod_Mul (Domain, Base, X, Result); -- Base * 1'
         else
            Set_One (Domain, Result); -- 1'
         end if;
         for Bit in 2..Get_MSB (Right) loop
            Mod_Mul (Domain, Base, Base, Server, Cache, Threshold, X);
            Swap (X, Base);
            if Get_Bit (Right, Bit) then
               Mod_Mul
               (  Domain,
                  Result,
                  Base,
                  Server,
                  Cache,
                  Threshold,
                  X
               );
               Swap (X, Result);
            end if;
         end loop;
      end;
   end Mod_Pow;

   procedure Mod_Pow_Karatsuba
             (  Left      : Unbounded_Unsigned;
                Right     : Unbounded_Unsigned;
                Modulus   : Unbounded_Unsigned;
                Server    : in out Job_Server'Class;
                Threshold : Digit_Count;
                Result    : out Unbounded_Unsigned
             )  is
   begin
      if Right.Length <= 1 then
         Mod_Pow (Left, To_Half_Word (Right), Modulus, Result);
         return;
      end if;
      declare
         Base : Unbounded_Unsigned;
         X    : Unbounded_Unsigned;
      begin
         Set (Base, Left);
         Modulo (Base, Modulus);
         if Is_Zero (Base) then
            Erase (Result);
            return;
         end if;
         if Get_Bit (Right, 1) then
            Set (Result, Base);
         else
            Set (Result, 1);
         end if;
         for Bit in 2..Get_MSB (Right) loop
            Square_Karatsuba (Base, Server, Threshold, X);
            Swap (Base, X);
            Modulo (Base, Modulus);
            if Get_Bit (Right, Bit) then
               Mul_Karatsuba (Result, Base, Server, Threshold, X);
               Swap (Result, X);
               Modulo (Result, Modulus);
            end if;
         end loop;
      end;
   end Mod_Pow_Karatsuba;

   procedure Mul
             (  Left, Right : Unbounded_Unsigned;
                Server      : in out Job_Server'Class;
                Cache       : in out Unbounded_Unsigned_Array;
                Threshold   : Digit_Count;
                Result      : out Unbounded_Unsigned
             )  is
      procedure Multiply (Left, Right : Unbounded_Unsigned) is
         Size  : constant Digit_Count :=
                    Digit_Offset'Max
                    (  Left.Length / Digit_Count (Server.Workers_Count),
                       Threshold
                    );
         Count : constant Digit_Count :=
                    Digit_Offset'Min
                    (  Left.Length / Size,
                       Cache'Length
                    );
         Jobs  : array (1..Natural (Count)) of Mul_Job;
         From  : Digit_Count := 1;
         To    : constant Digit_Count := (Left.Length / Size) * Size;
         Index : Positive := Cache'First;
         Shift : Digit_Offset := 0;
      begin
         Erase (Result);
         loop
            while Index - Cache'First < Natural (Cache'Length) loop
               declare
                  Job : Mul_Job renames Jobs (Index);
               begin
                  Job.Left := Left.Value;
                  Job.Left_First := From;
                  Job.Left_Last  := From + Size - 1;
                  From := Job.Left_Last + 1;
                  Job.Right  := Right'Unchecked_Access;
                  Job.Result := Cache (Index)'Unchecked_Access;
                  Execute (Job, Server);
               end;
               Index := Index + 1;
               exit when From > To;
            end loop;
            for Job_No in Cache'First..Index - 1 loop
               declare
                  Job : Mul_Job renames Jobs (Job_No);
               begin
                  Wait (Job);
                  Add (Result, Cache (Positive (Job_No)), Shift);
                  Shift := Shift + Size;
               end;
            end loop;
            exit when From > To;
            Index := Cache'First;
         end loop;
         if From <= Left.Length then
            Mul_Classroom
            (  Left.Value.Data  (From..Left.Length),
               Right.Value.Data (1..Right.Length),
               Cache (Cache'First)
            );
            Shift_Left (Cache (Cache'First), Shift);
            Add (Result, CAche (Cache'First));
         end if;
      end Multiply;
   begin
      if Left.Length = 0 or Right.Length = 0 then
         Erase (Result);
      elsif Left.Length  < 2 * Threshold or else
            Right.Length < 2 * Threshold then
         Mul_Classroom (Left, Right, Result);
      else
         if Left.Length >= Right.Length then
            Multiply (Left => Left, Right => Right);
         else
            Multiply (Left => Right, Right => Left);
         end if;
      end if;
   end Mul;

   procedure Mul
             (  Domain    : Montgomery_Domain;
                Left      : Unbounded_Unsigned;
                Right     : Unbounded_Unsigned;
                Server    : in out Job_Server'Class;
                Cache     : in out Unbounded_Unsigned_Array;
                Threshold : Digit_Count;
                Result    : out Unbounded_Unsigned
             )  is
      X : Montgomery_Number;
   begin
      Mod_Mul
      (  Domain,
         To_Domain (Domain, Left),
         To_Domain (Domain, Right),
         Server,
         Cache,
         Threshold,
         X
      );
      From_Domain (Domain, X, Result);
   end Mul;

   procedure Mul_Karatsuba
             (  Left, Right : Unbounded_Unsigned;
                Server      : in out Job_Server'Class;
                Threshold   : Digit_Count;
                Result      : out Unbounded_Unsigned
             )  is
   begin
      if Left.Length = 0 or else Right.Length = 0 then
         Erase (Result);
      elsif Left.Length + Right.Length <= 2 * Threshold     or
            Left.Length  <= Digit_Offset'Max (1, Threshold) or
            Right.Length <= Digit_Offset'Max (1, Threshold) then
         Mul_Classroom (Left, Right, Result);
      else
         declare
            Job : aliased Mul_Karatsuba_Job;
         begin
            Job.Left := Left.Value;
            Job.Left_First := 1;
            Job.Left_Last  := Left.Length;
            Job.Right := Right.Value;
            Job.Right_First := 1;
            Job.Right_Last  := Right.Length;
            Job.Threshold   := Threshold;
            Execute (Job, Server);
            Result := Job.Result;
         end;
      end if;
   end Mul_Karatsuba;

   procedure Pow
             (  Domain    : Montgomery_Domain;
                Left      : Unbounded_Unsigned;
                Right     : Unbounded_Unsigned;
                Server    : in out Job_Server'Class;
                Cache     : in out Unbounded_Unsigned_Array;
                Threshold : Digit_Count;
                Result    : out Unbounded_Unsigned
             )  is
      X : Montgomery_Number;
   begin
      Mod_Pow
      (  Domain,
         To_Domain (Domain, Left),
         Right,
         Server,
         Cache,
         Threshold,
         X
      );
      From_Domain (Domain, X, Result);
   end Pow;

   procedure Square_Karatsuba
             (  Left      : Unbounded_Unsigned;
                Server    : in out Job_Server'Class;
                Threshold : Digit_Count;
                Result    : out Unbounded_Unsigned
             )  is
   begin
      if Left.Length = 0 then
         Erase (Result);
      elsif Left.Length <= Digit_Offset'Max (1, Threshold) then
         Square_Classroom (Left, Result);
      else
         declare
            Job : aliased Square_Karatsuba_Job;
         begin
            Job.Left := Left.Value;
            Job.Left_First := 1;
            Job.Left_Last  := Left.Length;
            Job.Threshold  := Threshold;
            Execute (Job, Server);
            Result := Job.Result;
         end;
      end if;
   end Square_Karatsuba;

end Unbounded_Unsigneds.Parallel;
