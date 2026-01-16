--                                                                    --
--  package Generic_FFT             Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2025       --
--                                                                    --
--                                Last revision :  14:35 11 Mar 2025  --
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

package body Generic_FFT is
   Pi : constant :=
      3.1415_9265_3589_7932_3846_2643_3832_7950_2884_1971_6939_9375_106;

   use Elementary_Functions;
   subtype Complex is Complex_Types.Complex;

   type Bit_Type is mod (Integer'Last + 1) * 2;

   procedure FFT
             (  Vector : in out Complex_Vector;
                Invert : Boolean := False
             )  is
      function Log2 return Bit_Type is
         Count  : Bit_Type := Vector'Length;
         Result : Bit_Type := 0;
      begin
         if Count = 0 then
            raise Constraint_Error;
         end if;
         while (Count and 1) = 0 loop
            Result := Result + 1;
            Count  := Count / 2;
         end loop;
         Count := Count / 2;
         if Count /= 0 then
            raise Constraint_Error;
         end if;
         return Result;
      end Log2;

      Size : constant Bit_Type := Log2;
      S    : Real'Base := -2.0;
   begin
      if Vector'Length = 1 then
         return;
      end if;
      if Invert then
         S := 2.0;
      end if;
      for I in Bit_Type range 1..Vector'Length - 1 loop
         declare -- Reverse Size bits of I
            Bits : Bit_Type := Bit_Type (I) / 2;
            J    : Bit_Type := Bit_Type (I) and 1;
         begin
            for Bit in 2..Size loop
               J := J * 2;
               J := J or (Bits and 1);
               Bits := Bits / 2;
            end loop;
            if J < I then
               declare
                  X : Complex renames
                         Vector (Vector'First + Index_Type'Base (I));
                  Y : Complex renames
                         Vector (Vector'First + Index_Type'Base (J));
                  T : constant Complex := X;
               begin
                  X := Y;
                  Y := T;
               end;
            end if;
         end;
      end loop;
      declare
         Length : Index_Type'Base := 2;
      begin
         while Length <= Vector'Length loop
            declare
               Angle : constant Real'Base :=
                                PI * S / Real'Base (Length);
               Exp   : constant Complex := (cos (Angle), sin (Angle));
               I     : Index_Type'Base := 0;
            begin
               while I < Vector'Length loop
                  declare
                     W : Complex := (1.0, 0.0);
                     L : constant Index_Type'Base := Length / 2;
                  begin
                     for J in 0..L - 1 loop
                        declare
                           Index : constant Index_Type :=
                                            Vector'First + I + J;
                           X : Complex renames Vector (Index);
                           Y : Complex renames Vector (Index + L);
                           U : constant Complex := X;
                           V : constant Complex := Y * W;
                        begin
                           X := U + V;
                           Y := U - V;
                           W := W * Exp;
                        end;
                     end loop;
                  end;
                  I := I + Length;
               end loop;
            end;
            Length := Length * 2;
         end loop;
      end;
      if Invert then
         declare
            N : constant Real'Base := Real'Base (Vector'Length);
         begin
            for I in Vector'Range loop
               Vector (I) := Vector (I) / N;
            end loop;
         end;
      end if;
   end FFT;

   procedure FFT
             (  Vector      : in out Complex_Vector;
                Permutation : Permutation_Array;
                Factors     : Complex_Vector;
                Invert      : Boolean := False
             )  is
      Size : constant Bit_Type := Vector'Length;
   begin
      if Size = 0 or 0 /= ((Size - 1) and Size) then
         raise Constraint_Error; -- Vector'Length is not a power of two
      elsif 2 ** Factors'Length /= Vector'Length then
         raise Constraint_Error;
      end if;
      if Vector'Length = 1 then
         return;
      end if;
      for Pair in Permutation'Range loop
         declare
            X : Complex renames
                   Vector (Vector'First + Permutation (Pair).I);
            Y : Complex renames
                   Vector (Vector'First + Permutation (Pair).J);
            T : constant Complex := X;
         begin
            X := Y;
            Y := T;
         end;
      end loop;
      declare
         Length : Index_Type'Base := 2;
      begin
         for Angle in Factors'Range loop
            declare
               I : Index_Type'Base := 0;
            begin
               while I < Vector'Length loop
                  declare
                     W : Complex := (1.0, 0.0);
                     L : constant Index_Type'Base := Length / 2;
                  begin
                     for J in 0..L - 1 loop
                        declare
                           Index : constant Index_Type :=
                                            Vector'First + I + J;
                           X : Complex renames Vector (Index);
                           Y : Complex renames Vector (Index + L);
                           U : constant Complex := X;
                           V : constant Complex := Y * W;
                        begin
                           X := U + V;
                           Y := U - V;
                           W := W * Factors (Angle);
                        end;
                     end loop;
                  end;
                  I := I + Length;
               end loop;
            end;
            Length := Length * 2;
         end loop;
      end;
      if Invert then
         declare
            N : constant Real'Base := Real'Base (Vector'Length);
         begin
            for I in Vector'Range loop
               Vector (I) := Vector (I) / N;
            end loop;
         end;
      end if;
   end FFT;

   function FFT
            (  Vector : Complex_Vector;
               Invert : Boolean := False
            )  return Complex_Vector is
      Count : Bit_Type := Vector'Length;
      Size  : Bit_Type := 0;
   begin
      if Vector'Length = 0 then
         raise Constraint_Error;
      end if;
      while (Count and 1) = 0 loop
         Size  := Size + 1;
         Count := Count / 2;
      end loop;
      Count := Count / 2;
      if Count = 0 then
         declare
            Result : Complex_Vector := Vector;
         begin
            FFT (Result, Invert);
            return Result;
         end;
      end if;
      Size := Size + 1;
      loop
         Size  := Size + 1;
         Count := Count / 2;
         exit when Count = 0;
      end loop;
      declare
         Result : Complex_Vector
                  (  Vector'First
                  .. Vector'First + 2 ** Natural (Size) - 1
                  );
      begin
         Result (Vector'Range) := Vector;
         for Index in Vector'Last + 1..Result'Last loop
            Result (Index) := (0.0, 0.0);
         end loop;
         FFT (Result, Invert);
         return Result;
      end;
   end FFT;

   function FFT_Factors
            (  N      : Positive;
               Invert : Boolean := False
            )  return Complex_Vector is
      Result : Complex_Vector
               (  Index_Type'First
               .. Index_Type'First + Index_Type (N) - 1
               );
      Angle  : Real;
   begin
      if Invert then
         Angle := Pi;
      else
         Angle :=-Pi;
      end if;
      for I in Result'Range loop
         Result (I) := (cos (Angle), sin (Angle));
         Angle := Angle / 2.0;
      end loop;
      return Result;
   end FFT_Factors;

   function FFT_Permutation (N : Positive) return Permutation_Array is
      Result : Permutation_Array (1..2 ** N);
      Last   : Natural := 0;
   begin
      if N > 1 then
         for I in Bit_Type range 1..2 ** N - 1 loop
            declare -- Reverse Size bits of I
               Bits : Bit_Type := Bit_Type (I) / 2;
               J    : Bit_Type := Bit_Type (I) and 1;
            begin
               for Bit in 2..Bit_Type (N) loop
                  J := J * 2;
                  J := J or (Bits and 1);
                  Bits := Bits / 2;
               end loop;
               if I < J then
                  Last := Last + 1;
                  Result (Last) :=
                     (  I => Index_Type'Base (I),
                        J => Index_Type'Base (J)
                     );
               end if;
            end;
         end loop;
      end if;
      return Result (1..Last);
   end FFT_Permutation;

end Generic_FFT;
