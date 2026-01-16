--                                                                    --
--  package Unbounded_Rationals.    Copyright (c)  Dmitry A. Kazakov  --
--             Elementary_Functions                Luebeck            --
--  Implementation                                 Spring, 2025       --
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

with Generic_Segmented_Stack;

package body Unbounded_Rationals.Elementary_Functions is

   Pi_2_High : constant Unbounded_Rational :=
                        To_Unbounded_Rational (573204) / 364913;
   Pi_2_Low  : constant Unbounded_Rational :=
                        To_Unbounded_Rational (50399) / 32085;
   V_10005   : constant Unbounded_Rational :=
                        To_Unbounded_Rational (10005);
   One_Point_Seven : constant Unbounded_Rational :=
                              (  Numerator   => From_Half_Word (7),
                                 Denominator => From_Half_Word (10),
                                 Sign        => False
                              );
   V_10939058860032000 : constant Unbounded_Unsigned :=
      (From_Half_Word (109390) * 1000000 + 588600) * 100000 + 32000;
------------------------------------------------------------------------
-- Reduced range functions
--
-- Arcsin_Reduced -- Range -0.7..0.7
--
--            (2 * N)! * Y ** (2 * N + 1)
--    Sum  -------------------------------- =
--   N = 0  4 ** N * N! * N! * (2 * N + 1)
--
--             U (N)
-- =  Sum   ----------
--   N = 0   2 * N + 1
--
--  U (0) = 1
--                      (2 * N + 1) * (2 * N + 2) * Y ** 2
--  U (N + 1) = U (N) * ----------------------------------
--                            4 * (N + 1) * (N + 1)
--
   function Arcsin_Reduced (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
      Y : Unbounded_Rational := abs X;
   begin
      if Is_Zero (Y) then
         return Zero;
      end if;
      declare
         S  : Unbounded_Rational := Y;
         U  : Unbounded_Rational := Y;
         T  : Unbounded_Rational;
         N  : Half_Word := 1;
         N2 : Half_Word := 2;
      begin
         Y := Square (Y);
         loop
            U  := U * Y * ((N2 - 1) * N2) / (4 * N * N);
            T  := U / (N2 + 1);
            S  := S + T;
            exit when
               not Unbounded_Rationals.Error (T, Zero, Error - 2);
            N  := N + 1;
            N2 := N2 + 2;
         end loop;
         Round (S, -Integer'Min (0, Error));
         if X.Sign then
            return -S;
         else
            return S;
         end if;
      end;
   end Arcsin_Reduced;
--
-- Arctan_Reduced -- Range -1/2..1/2
--
--         (-1)**N * Y**(2N+1)
--    Sum  -------------------
--   N = 0      2 * N + 1
--
--             U (N)
-- =  Sum   ----------
--   N = 0   2 * N + 1
--
--  U (0)     = Y
--  U (N + 1) = - U (N) * Y**2
--
   function Arctan_Reduced (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
      Y : Unbounded_Rational := abs X;
   begin
      if Is_Zero (Y) then
         return Zero;
      end if;
      declare
         S : Unbounded_Rational := Y;
         U : Unbounded_Rational := Y;
         T : Unbounded_Rational;
         N : Half_Word := 3;
         Negative : Boolean := True;
      begin
         Y := Square (Y);
         loop
            U  := U * Y;
            T  := U / N;
            if Negative then
               S  := S - T;
            else
               S  := S + T;
            end if;
            exit when
               not Unbounded_Rationals.Error (T, Zero, Error - 2);
            N := N + 2;
            Negative := not Negative;
         end loop;
         Round (S, -Integer'Min (0, Error));
         if X.Sign then
            return -S;
         else
            return S;
         end if;
      end;
   end Arctan_Reduced;
--
-- Cos_Reduced -- Cos for range -Pi/2..Pi/2
--
   function Cos_Reduced (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
      Sum  : Unbounded_Rational := One;
      S    : constant Unbounded_Rational := Square  (X);
      U    : Unbounded_Rational := One;
      Sign : Boolean            := True;
      N    : Half_Word := 2;
   begin
      if Is_Zero (X) then
         return One;
      end if;
      loop
         U := (U * S) / (N * (N - 1));
         exit when not Unbounded_Rationals.Error (U, Zero, Error);
         if Sign then
            Sum  := Sum - U;
            Sign := False;
         else
            Sum  := Sum + U;
            Sign := True;
         end if;
         N := N + 2;
      end loop;
      return Sum;
   end Cos_Reduced;
--
-- Cos_Half_Pi -- Range -Pi/2..Pi/2
--
--    Cos (2 * Y) = 2 * Cos (X) ** 2 - 1
--
   function Cos_Half_Pi (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
      Y : Unbounded_Rational := abs X;
   begin
      Y := Cos_Reduced (Y / 2, Error - 1);
      Y := Square (Y) * 2 - 1;
      return Y;
   end Cos_Half_Pi;
--
-- Exp_Reduced -- For range 0..1
--
   function Exp_Reduced (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
      Sum : Unbounded_Rational := X + 1;
      U   : Unbounded_Rational := X;
      N   : Half_Word := 2;
   begin
      loop
         U := U * X / N;
         Sum := Sum + U;
         exit when not Unbounded_Rationals.Error (U, Zero, Error);
         N := N + 1;
      end loop;
      return Sum;
   end Exp_Reduced;
--
-- Ln_Reduced -- For 1/2..1
--
   function Ln_Reduced (Left : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
      R    : Unbounded_Rational := (-Left + 1) / (Left + 1);
      Sum  : Unbounded_Rational := R;
      U, F : Unbounded_Rational := R;
      K    : Half_Word := 3;
   begin
      R := Square (R);
      loop
         F := F * R;
         U := F / K;
         Sum := Sum + U;
         exit when not Unbounded_Rationals.Error (U, Zero, Error + 2);
         K := K + 2;
      end loop;
      return -Sum * 2;
   end Ln_Reduced;
--
-- Sin_Reduced -- Sin for range 0..Pi/4
--
   function Sin_Reduced (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
      Sum  : Unbounded_Rational := X;
      S    : constant Unbounded_Rational := Square  (X);
      U    : Unbounded_Rational := X;
      Sign : Boolean            := True;
      N    : Half_Word := 2;
   begin
      if Is_Zero (X) then
         return X;
      end if;
      loop
         U := (U * S) / (N * (N + 1));
         exit when not Unbounded_Rationals.Error (U, Zero, Error);
         if Sign then
            Sum  := Sum - U;
            Sign := False;
         else
            Sum  := Sum + U;
            Sign := True;
         end if;
         N := N + 2;
      end loop;
      return Sum;
   end Sin_Reduced;
--
-- Sin_Half_Pi -- Range -Pi/2..Pi/2
--
--    Sin (3 * Y) = 3 * Sin (X) - 4 * Sin (X) ** 3
--
   function Sin_Half_Pi (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
      Y : Unbounded_Rational := abs X;
   begin
      Y := Sin_Reduced (Y / 3, Error - 2);
      Y := Y * (Square (Y) * 4 - 3);
      if X.Sign then
         return Y;
      else
         return -Y;
      end if;
   end Sin_Half_Pi;
------------------------------------------------------------------------
--
-- Reduce_To_Pi_2 -- Range reduction to 0..Pi/2
--
--    Y        - The argument
--    Error    - The error 2**Error
--    Reduced  - The result
--    Quadrant - The quadrant
--
   procedure Reduce_To_Pi_2
             (  X        : Unbounded_Rational;
                Error    : Integer;
                Reduced  : out Unbounded_Rational;
                Quadrant : out Quadrant_Type
             )  is
      Y : Unbounded_Rational := abs X;
   begin
      if Y < Pi_2_Low then
         if X.Sign then
            Quadrant := IV;
         else
            Quadrant := I;
         end if;
         Reduced := Y;
      else
         declare
            N    : Half_Word := Half_Word (To_Integer (Y / Pi_2_High));
            Pi_2 : Unbounded_Rational; -- A more precise value
         begin
            Pi_2 := Pi_Chudnovsky (Error - Integer (Log2 (N)) - 2) / 2;
            Reduced := X - Pi_2 * N;
            while Reduced >= Pi_2 loop
               Reduced := Reduced - Pi_2;
               N := N + 1;
            end loop;
            Quadrant := Quadrant_Type'Val (N mod 4);
            if X.Sign then
               case Quadrant is
                  when I   => Quadrant := IV;
                  when II  => Quadrant := III;
                  when III => Quadrant := II;
                  when IV  => Quadrant := I;
               end case;
            end if;
         end;
      end if;
   end Reduce_To_Pi_2;

   function Arccos (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
      --
      -- arccos (Y) = Pi/2 - arcsin (x)
      --
   begin
      if Greater_Than_One (X)  then
         raise Constraint_Error;
      end if;
      declare
         Z : Unbounded_Rational := abs X;
      begin
         if Z >= One_Point_Seven then
            --
            -- Reduction:
            --             _________
            --    arcsin (V 1 - Y**2)
            --
            Z := Sqrt (-Square (X) + 1, Error - 2);
            Z := Arcsin_Reduced (Z, Error - 1);
         else
            Z := Pi_Chudnovsky (Error - 2) / 2;
            Z := Z - Arcsin_Reduced (X, Error);
         end if;
         Round (Z, -Integer'Min (0, Error));
         return Z;
      end;
   end Arccos;

   function Arccosh (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      case Compare (X, 1) is
         when Less =>
            raise Constraint_Error;
         when Equal =>
            return Zero;
         when Greater =>
            return ln (X + Sqrt (Square (X) - 1, Error - 1), Error);
      end case;
   end Arccosh;

   function Arcsin (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      if Greater_Than_One (X)  then
         raise Constraint_Error;
      end if;
      declare
         Y : Unbounded_Rational := abs X;
      begin
         if Y >= One_Point_Seven then
            --
            -- Reduction:
            --                    _________
            --    Pi/2 - arcsin (V 1 - Y**2)
            --
            Y := Sqrt (-Square (X) + 1, Error - 2);
            Y := Pi_Chudnovsky (Error - 2) / 2
               - Arcsin_Reduced (Y, Error - 1);
            Round (Y, -Integer'Min (0, Error));
            return Y;
         else
            return Arcsin_Reduced (X, Error);
         end if;
      end;
   end Arcsin;

   function Arcsinh (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      if Is_Zero (X) then
         return Zero;
      else
         return ln (X + Sqrt (1 + Square (X), Error - 1), Error);
      end if;
   end Arcsinh;
--
-- Arctan -- Implementation
--
--    arctan (Y) = Pi / 2 - arctan (1/x)
--
--    arctan (Y) = 2 arctan --------------
--                               _________
--                          1 + V 1 + Y**2
--                                                __
   function Arctan (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
      Y   : Unbounded_Rational := abs X;
      Err : Integer := Error;
      Inv : Boolean := False;
   begin
      if Is_Zero (Y) then
         return Y;
      elsif Greater_Than_One (Y) then
         Invert (Y);
         Err := Err - 2;
         Inv := True;
      end if;
      Y := Y / (Sqrt (Square (Y) + 1, Err - 4) + 1);
      Y := Arctan_Reduced (Y, Err) * 2;
      if Inv then
         Y := Pi_Chudnovsky (Err - 1) / 2 - Y;
      end if;
      Round (Y, -Integer'Min (0, Error));
      if X.Sign then
         return -Y;
      else
         return Y;
      end if;
   end Arctan;

   function Arctan (Y, X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
      Z : Unbounded_Rational;
   begin
      if Is_Zero (X) and then Is_Zero (Y) then
         raise Constraint_Error;
      end if;
      case Compare (abs X, abs Y) is
         when Less =>
            if X.Sign xor Y.Sign then
               Z := Arctan (abs (X / Y), Error - 2)
                  - Pi_Chudnovsky (Error) / 2;
            else
               Z := Pi_Chudnovsky (Error) / 2
                  - Arctan (abs (X / Y), Error - 2);
            end if;
         when Equal =>
            if X.Sign xor Y.Sign then
               Z := -Pi_Chudnovsky (Error) / 4;
            else
               Z := Pi_Chudnovsky (Error) / 4;
            end if;
         when Greater =>
            Z := Arctan (Y / X, Error);
      end case;
      Round (Z, -Integer'Min (0, Error));
      return Z;
   end Arctan;

   function Arctanh (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      if Is_Zero (X) then
         return Zero;
      elsif Less_Than_One (X) then
         return ln ((1 + X) / (1 - X), Error) / 2;
      else
         raise Constraint_Error;
      end if;
   end Arctanh;
--
-- Cos -- Implementation
--
--    cos (Y + N * Pi/2) =
--       N mod 4
--          0 =>  cos (Y)
--          1 => -sin (Y)
--          2 => -cos (Y)
--          3 =>  sin (Y)
--
--       cos (Y) - cos (X + E) =
--     = cos (Y) - cos (X) * cos (E) + sin (X) * sin (E) <=
--    <= cos (Y) - cos (X) + sin (X) * sin (E) =
--     = sin (Y) * sin (E) <=
--    <= sin (Y) * E <= E
--
   function Cos (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      if Is_Zero (X) then
         return One;
      end if;
      declare
         Y : Unbounded_Rational;
         Q : Quadrant_Type;
      begin
         Reduce_To_Pi_2 (abs X, Error, Y, Q);
         case (Q) is
            when I =>
               Y := Cos_Half_Pi (Y, Error);
            when II =>
               Y := -Sin_Half_Pi (Y, Error);
            when III =>
               Y := -Cos_Half_Pi (Y, Error);
            when IV =>
               Y := Sin_Half_Pi (Y, Error);
         end case;
         Round (Y, -Integer'Min (0, Error));
         return Y;
      end;
   end Cos;

   function Cosh (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      if Is_Zero (X) then
         return One;
      end if;
      declare
         Sum  : Unbounded_Rational := One;
         S    : constant Unbounded_Rational := Square  (X);
         U    : Unbounded_Rational := One;
         N    : Half_Word := 1;
      begin
         loop
            U   := (U * S) / (N * (N + 1));
            Sum := Sum + U;
            exit when
               not Unbounded_Rationals.Error (U * 2 / 3, Zero, Error);
            N := N + 2;
         end loop;
         Round (Sum, -Integer'Min (0, Error));
         return Sum;
      end;
   end Cosh;
--
-- Cot -- Implementation
--                         1    cos (X)
--    cos (X) / sin (X) = --- * -------
--                         X    sin (X)
--                              -------
--                                 X
   function Cot (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      if Is_Zero (X) or else X > Pi_2_High or else X < -Pi_2_High then
         raise Constraint_Error;
      end if;
      declare
         Y   : Unbounded_Rational;
         Err : Integer;
      begin
         Err := Error - To_Integer (Log2 (abs Invert (X), 0)) - 2;
         Y := Sin_Div_X (X, Err);
         Y := Cos_Half_Pi (X, Err) / Y / X;
         Round (Y, -Integer'Min (0, Error));
         return Y;
      end;
   end Cot;

   function Coth (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      if Is_Zero (X) then
         raise Constraint_Error;
      end if;
      declare
         Cosh : Unbounded_Rational := One;
         Sinh : Unbounded_Rational := Zero;
         U    : Unbounded_Rational := One;
         N    : Half_Word := 1;
         T_0  : Unbounded_Rational;
         T_1  : Unbounded_Rational;
      begin
         loop
            U    := U * X / N;
            Sinh := Sinh + U;
            N    := N + 1;
            U    := U * X / N;
            Cosh := Cosh + U;
            T_1  := Cosh / sinh;
            exit when not Unbounded_Rationals.Error (T_0, T_1, Error);
            N    := N + 1;
            Swap (T_0, T_1);
         end loop;
         Round (T_1, -Integer'Min (0, Error));
         return T_1;
      end;
   end Coth;
--
-- Exp -- Implementation
--
--    X = N * ln (2) + R
--
--    exp (X) = exp (N * ln 2 + R) = 2 ** N * exp (R)
--
   function Exp (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      if Is_Zero (X) then
         return Zero;
      end if;
      declare
         L : Unbounded_Rational;
      begin
         if X >= 1 then
            declare
               N : Integer;
               R : Unbounded_Rational := X;
            begin
               N := To_Integer (X / 693147 * 10000);
               L := Ln_2 (Error * 28 / 10 - N);
               R := X - L * Half_Word (N);
               while R >= 1 loop
                  R := R - L;
                  N := N + 1;
               end loop;
               L := Exp_Reduced (R, Error - N - 1)
                  * Unbounded_Unsigned'(Power_Of_Two (Bit_Count (N)));
            end;
         elsif X.Sign then
            declare
               N : Integer;
               R : Unbounded_Rational := X;
            begin
               N := To_Integer (X / 693147 * 10000);
               L := Ln_2 (Error * 28 / 10 - N);
               R := X - L * Half_Word (N);
               while R.Sign loop
                  R := R + L;
                  N := N - 1;
               end loop;
               L := Exp_Reduced (R, Error - N + 1)
                  / Unbounded_Unsigned'(Power_Of_Two (Bit_Count (-N)));
            end;
         else
            L := Exp_Reduced (X, Error);
         end if;
         Round (L, -Integer'Min (0, Error));
         return L;
      end;
   end Exp;
--
-- Ln -- Implementation
--
--      ln (X)                                   = (X = Y * 2**N)
--    = ln (Y * 2**N)                            =
--    = ln (Y) + ln (2**N)                       =
--    = ln (Y) + log2 (2**N) / log2 (e)          =
--    = ln (Y) + log2 (2**N) / (ln (e) / ln (2)) =
--    = ln (Y) + N * ln (2)
--
   function Ln (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      if X.Sign or else Is_Zero (X) then
         raise Constraint_Error;
      end if;
      declare
         Y : Unbounded_Rational := X;
         N : Half_Word;
         L : Unbounded_Rational;
      begin
         if X >= 1 then
            N := Half_Word (Log2 (X.Numerator) - Log2 (X.Denominator));
            Y := Y / Unbounded_Unsigned'(Power_Of_Two (Bit_Count (N)));
            while Y >= 1 loop
               Y := Y / 2;
               N := N + 1;
            end loop;
            while Y < Half loop
               Y := Y * 2;
               N := N - 1;
            end loop;
            L := Ln_2 (Error - 2 - Log2 (N)) * N;
            Y := L + Ln_Reduced (Y, Error - 2);
         elsif X < Half then
            N := Half_Word (Log2 (X.Denominator) - Log2 (X.Numerator));
            Y := Y * Unbounded_Unsigned'(Power_Of_Two (Bit_Count (N)));
            while Y >= 1 loop
               Y := Y / 2;
               N := N - 1;
            end loop;
            while Y < Half loop
               Y := Y * 2;
               N := N + 1;
            end loop;
            L := Ln_2 (Error - 2 - Log2 (N)) * N;
            Y := Ln_Reduced (Y, Error - 2) - L;
         else
            Y := Ln_Reduced (X, Error);
         end if;
         Round (Y, -Integer'Min (0, Error));
         return Y;
      end;
   end Ln;

   function Ln_2 (Error : Integer) return Unbounded_Rational is
      Sign        : Boolean;
      Denominator : Unbounded_Unsigned;
      Sum         : Unbounded_Rational;
      Term        : Unbounded_Rational;
      N           : Half_Word;
      Err         : constant Integer := Error - 2;
   begin
      N    := 2;
      Sign := True;
      Sum  := (  Sign        => False,
                 Numerator   => Unbounded_Unsigneds.One,
                 Denominator => Unbounded_Unsigneds.Two
              );
      Set (Denominator, 2);
      loop -- At least one iteration
         Mul (Denominator, 2);
         Term := (  Sign        => False,
                    Numerator   => Unbounded_Unsigneds.One,
                    Denominator => Denominator * N
                 );
         exit when not Unbounded_Rationals.Error (Term, Zero, Err);
         if Sign then
            Sum := Sum - Term;
         else
            Sum := Sum + Term;
         end if;
         Sign := not Sign;
         N    := N + 1;
      end loop;
      N    := 2;
      Sign := True;
      Sum  := Sum + (  Sign        => False,
                       Numerator   => Unbounded_Unsigneds.One,
                       Denominator => Unbounded_Unsigneds.Three
                    );
      Set (Denominator, 3);
      loop -- At least one iteration
         Mul (Denominator, 3);
         Term := (  Sign        => False,
                    Numerator   => Unbounded_Unsigneds.One,
                    Denominator => Denominator * N
                 );
         exit when not Unbounded_Rationals.Error (Term, Zero, Err);
         if Sign then
            Sum := Sum - Term;
         else
            Sum := Sum + Term;
         end if;
         Sign := not Sign;
         N    := N + 1;
      end loop;
      Round (Sum, -Integer'Min (0, Error));
      return Sum;
   end Ln_2;

   package Bit_Offset_Stacks is
       new Generic_Segmented_Stack
           (  Index_Type   => Positive,
              Object_Type  => Bit_Offset,
              Null_Element => Bit_Offset'First
           );

   function Log2 (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
      use Bit_Offset_Stacks.Segmented_Stack;
      N, M  : Bit_Offset;
      Sum_M : Integer := 0;
      Y     : Unbounded_Rational;
      List  : Stack;
   begin
      if X = Zero or else X.Sign then
         raise Constraint_Error;
      end if;
      Y := X;
      if Y >= Two then
         N := Bit_Offset (Log2 (Y.Numerator) - Log2 (Y.Denominator));
         Y := Y / Unbounded_Unsigned'(Power_Of_Two (N));
      elsif Less_Than_One (Y) then
         N := Bit_Offset (Log2 (Y.Denominator) - Log2 (Y.Numerator));
         Y := Y * Unbounded_Unsigned'(Power_Of_Two (N));
         N := -N;
      else
         return Zero;
      end if;
      while Y >= Two loop
         Y := Y / 2;
         N := N + 1;
      end loop;
      while Less_Than_One (Y) loop
         Y := Y * 2;
         N := N - 1;
      end loop;
      while Y /= One loop
         M := 0;
         while Y < Two loop
            Y := Square (Y);
            Round (Y, -Integer'Min (0, Error * 2));
            M := M + 1;
            exit when Y = One;
         end loop;
         exit when Y = One;
         Push (List, M);
         Y := Y / 2;
         Sum_M := Sum_M - Integer (M);
         exit when Sum_M <= Error - 2;
      end loop;
      Y := Zero;
      if not Is_Empty (List) then
         loop
            Y := Y / Unbounded_Unsigned'(Power_Of_Two (Top (List)));
            Pop (List);
            exit when Is_Empty (List);
            Y := Y + 1;
         end loop;
      end if;
      if N > 0 then
         Y := Y + Half_Word (N);
      else
         Y := Y - Half_Word (-N);
      end if;
      Round (Y, -Integer'Min (0, Error));
      return Y;
   end Log2;

   procedure Mod_Half_Pi
             (  X        : Unbounded_Rational;
                Error    : Integer;
                Reduced  : out Unbounded_Rational;
                Quadrant : out Quadrant_Type;
                Half_Pi  : out Unbounded_Rational
             )  is
      Y : Unbounded_Rational := abs X;
   begin
      if Y <= Pi_2_Low then
         if X.Sign then
            Quadrant := IV;
         else
            Quadrant := I;
         end if;
         Reduced := Y;
         Half_Pi := Pi_Chudnovsky (Error) / 2;
      else
         declare
            N : Half_Word := Half_Word (To_Integer (Y / Pi_2_High));
         begin
            Half_Pi :=
               Pi_Chudnovsky (Error - Integer (Log2 (N)) - 2) / 2;
            Reduced := Y - Half_Pi * N;
            while Reduced >= Half_Pi loop
               Reduced := Reduced - Half_Pi;
               N := N + 1;
            end loop;
            Quadrant := Quadrant_Type'Val (N mod 4);
            if X.Sign then
               case Quadrant is
                  when I   => Quadrant := IV;
                  when II  => Quadrant := III;
                  when III => Quadrant := II;
                  when IV  => Quadrant := I;
               end case;
            end if;
         end;
      end if;
   end Mod_Half_Pi;

   function Pi_Nilkantha (Error : Integer) return Unbounded_Rational is
      Result : Unbounded_Rational;
      Term   : Unbounded_Rational;
      N      : Unbounded_Unsigned;
      N_1    : Unbounded_Unsigned;
      N_2    : Unbounded_Unsigned;
      Sign   : Boolean := False;
   begin
      Set (N,   2);
      Set (N_1, 3);
      Set (N_2, 4);
      loop
         Set (Term.Denominator, N);
         Mul (Term.Denominator, N_1);
         Mul (Term.Denominator, N_2);
         Set (Term.Numerator, 4); -- Not normalized
         exit when not Unbounded_Rationals.Error (Term, Zero, Error);
         if Sign then
            Result := Result - Term;
         else
            Result := Result + Term;
         end if;
         Swap (N,   N_2);
         Add  (N_1, 2);
         Add  (N_2, 4);   -- N_2 is N now
         Sign := not Sign;
      end loop;
      Result := Result + 3;
      Round (Result, -Integer'Min (0, Error));
      return Result;
   end Pi_Nilkantha;

   function Pi_Chudnovsky (Error : Integer)
      return Unbounded_Rational is
      -- 4.18 decimal digits per iteration 14 binary
      procedure Binary_Split
                (  A, B    : Half_Word;
                   P, Q, R : out Unbounded_Integer
                )  is
         Value : Unbounded_Unsigned;
      begin
         if B = A + 1 then
            Set (Value, 6 * A - 1);
            Mul (Value, 2 * A - 1);
            Mul (Value, 6 * A - 5);
            P := Compose (Value, True);
            Set (Value, A);
            Mul (Value, A);
            Mul (Value, A);
            Mul (Value, V_10939058860032000);
            Q := Compose (Value, False);
            Set (Value, 545140134);
            Mul (Value, A);
            Add (Value, 13591409);
            R := P * Compose (Value, False);
            return;
         end if;
         declare
            M : constant Half_Word := (A + B) / 2;
            P_1, Q_1, R_1 : Unbounded_Integer;
            P_2, Q_2, R_2 : Unbounded_Integer;
         begin
            Binary_Split (A, M, P_1, Q_1, R_1);
            Binary_Split (M, B, P_2, Q_2, R_2);

            P := P_1 * P_2;
            Q := Q_1 * Q_2;
            R := Q_2 * R_1 + P_1 * R_2;
         end;
      end Binary_Split;

      Result  : Unbounded_Rational;
      P, Q, R : Unbounded_Integer;
      N       : Half_Word;
   begin
      if Error > -28 then
         N := 2;
      else
         N := Half_Word (-Error) / 14;
      end if;
      Binary_Split (1, N, P, Q, R);
      Result :=
         Sqrt (V_10005, Error - 2) * 426880  * Q / (R + Q * 13591409);
      Round (Result, -Integer'Min (0, Error));
      return Result;
   end Pi_Chudnovsky;

   function Power (Left, Right : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
      --
      -- Left ** Right = Left ** N * exp (Fraction * ln (Left))
      --
      --           Right = N + A/B,  0 <= A/B < 1
      --
      --   Left ** Right =                          B __________
      -- = Left ** N * Left ** (A / B) = Left ** N * V Left ** A
      --
   begin
      if Left.Sign then
         raise Constraint_Error;
      elsif Is_Zero (Left) then
         return Zero;
      elsif Is_Zero (Right) then
         return One;
      end if;
      declare
         function To_Integer (X : Unbounded_Unsigned) return Integer is
         begin
            return Integer (To_Half_Word (X));
         end To_Integer;
         Result   : Unbounded_Rational;
         Fraction : Unbounded_Rational;
         Int      : Unbounded_Integer;
         Power    : Half_Word;
      begin
         Split (Right, Int, Fraction);
         Power := To_Half_Word (Get_Mantissa (Int));
         if Right.Sign then
            Result := Left ** Integer (Power);
            Result := Invert (Result)
                    * Root
                      (  Left ** To_Integer (Fraction.Numerator),
                         To_Integer (Fraction.Denominator),
                         Error - 4 - Log2 (Result) * 2
                      );
         else
            Result := Left ** Integer (Power);
            Result := Result
                    * Root
                      (  Left ** To_Integer (Fraction.Numerator),
                         To_Integer (Fraction.Denominator),
                         Error - 1 - Log2 (Result)
                      );
         end if;
         Round (Result, -Integer'Min (0, Error));
         return Result;
      end;
   end Power;
--
-- Root_Init -- Initial value for Newton's iteration
--
--    X - The value (must be positive)
--    N - The root's degree
--
--       N __    1/N * log2 X
--        V X = 2
--
-- Returns :
--
--    The initial value to start iterations
--
   function Root_Init
            (  X : Unbounded_Rational;
               N : Positive
            )  return Unbounded_Rational is
      Power : Bit_Count;
   begin
      case Compare (X, 1) is
         when Greater =>
            Power :=
               (  Get_MSB
                  (  Get_Mantissa (To_Unbounded_Integer (X))
                  )
               /  Bit_Count (N)
               );
             return (  Sign        => False,
                       Numerator   => Power_Of_Two (Power),
                       Denominator => Unbounded_Unsigneds.One
                    );
         when Equal =>
            return X;
         when Less =>
            Power :=
               (  Get_MSB
                  (  Get_Mantissa (To_Unbounded_Integer (Invert (X)))
                  )
               /  Bit_Count (N)
               );
             return (  Sign        => False,
                       Numerator   => Unbounded_Unsigneds.One,
                       Denominator => Power_Of_Two (Power)
                    );
      end case;
   end Root_Init;

   function Root
            (  X    : Unbounded_Rational;
               N    : Positive;
               Error : Integer
            )  return Unbounded_Rational is
   begin
      if X.Sign and then (N mod 2) = 0 then
         raise Constraint_Error;
      elsif N = 1 then
         return X;
      elsif Is_Zero (X) then
         return Zero;
      elsif Is_One (X) then
         return One;
      elsif N = 2 then
         return Sqrt (X, Error);
      end if;
      declare
         V : constant Unbounded_Rational := abs X;
         Z : Unbounded_Rational := Root_Init (V, N);
         Y : Unbounded_Rational := Z;
         N1 : constant Integer  := N - 1;
      begin
         loop
            Y := ((Z * Half_Word (N1)) + V / Z ** N1) / Half_Word (N);
            Round (Y, -Integer'Min (0, Error));
            exit when not Unbounded_Rationals.Error (Z, Y, Error);
            Swap (Z, Y);
         end loop;
         if X.Sign then
            return -Y;
         else
            return Y;
         end if;
      end;
   end Root;
--
-- Sin -- Implementation
--
--    sin (Y + N * Pi/2) =
--       N mod 4
--          0 =>  sin (Y)
--          1 =>  cos (Y)
--          2 => -sin (Y)
--          3 => -cos (Y)
--
--      sin (Y + E) - sin (X) =
--    = sin (Y) * cos (E) + cos (X) * sin (E) - sin (X) <=
--   <= sin (Y) + cos (X) * sin (E) - sin (X) =
--    = cos (Y) * sin (E) <=
--   <= cos (Y) * E <= E
--
   function Sin (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      if Is_Zero (X) then
         return Zero;
      end if;
      declare
         Y : Unbounded_Rational;
         Q : Quadrant_Type;
      begin
         Reduce_To_Pi_2 (abs X, Error, Y, Q);
         case (Q) is
            when I =>
               Y := Sin_Half_Pi (Y, Error);
            when II =>
               Y := Cos_Half_Pi (Y, Error);
            when III =>
               Y := -Sin_Half_Pi (Y, Error);
            when IV =>
               Y := -Cos_Half_Pi (Y, Error);
         end case;
         Round (Y, -Integer'Min (0, Error));
         if X.Sign then
            Y := -Y;
         end if;
         return Y;
      end;
   end Sin;
--              Sin (X)
-- Sin_Div_X -- -------
--                 X
--
   function Sin_Div_X (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      if Is_Zero (X) then
         return One;
      elsif abs X > Pi_2_High then
         declare
            Y : Unbounded_Rational := Sin (X, Error + Log2 (X)) / X;
         begin
            Round (Y, -Integer'Min (0, Error));
            return Y;
         end;
      else
         declare
            Sum  : Unbounded_Rational := One;
            S    : constant Unbounded_Rational := Square  (X);
            U    : Unbounded_Rational := One;
            Sign : Boolean            := True;
            N    : Half_Word := 2;
         begin
            loop
               U := (U * S) / (N * (N + 1));
               exit when not Unbounded_Rationals.Error (U, Zero, Error);
               if Sign then
                  Sum  := Sum - U;
                  Sign := False;
               else
                  Sum  := Sum + U;
                  Sign := True;
               end if;
               N := N + 2;
            end loop;
            Round (Sum, -Integer'Min (0, Error));
            return Sum;
         end;
      end if;
   end Sin_Div_X;

   function Sinh (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      if Is_Zero (X) then
         return Zero;
      end if;
      declare
         Sum  : Unbounded_Rational := X;
         S    : constant Unbounded_Rational := Square  (X);
         U    : Unbounded_Rational := X;
         N    : Half_Word := 2;
      begin
         loop
            U   := (U * S) / (N * (N + 1));
            Sum := Sum + U;
            exit when
               not Unbounded_Rationals.Error (U / 3, Zero, Error);
            N := N + 2;
         end loop;
         Round (Sum, -Integer'Min (0, Error));
         return Sum;
      end;
   end Sinh;

   function Sqrt (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      if X.Sign then
         raise Constraint_Error;
      elsif Is_Zero (X) then
         return Zero;
      end if;
      declare
         Z : Unbounded_Rational := Root_Init (X, 2);
         Y : Unbounded_Rational := Z;
      begin
         loop
            Y := (Z + (X / Z)) / 2;
            exit when not Unbounded_Rationals.Error (Z, Y, Error);
            Swap (Z, Y);
         end loop;
         Round (Y, -Integer'Min (0, Error));
         return Y;
      end;
   end Sqrt;

   function Tan (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      if Is_Zero (X) then
         return Zero;
      elsif X > Pi_2_High or else X < -Pi_2_High then
         raise Constraint_Error;
      end if;
      declare
         Cos : Unbounded_Rational := One;
         Sin : Unbounded_Rational := Zero;
         U   : Unbounded_Rational := One;
         N   : Half_Word := 1;
         T_0 : Unbounded_Rational;
         T_1 : Unbounded_Rational;
         Negative : Boolean := True;
      begin
         loop
            U  := U * X / N;
            if Negative then
               Sin := Sin + U;
               N   := N + 1;
               U   := U * X / N;
               Cos := Cos - U;
            else
               Sin := Sin - U;
               N   := N + 1;
               U   := U * X / N;
               Cos := Cos + U;
            end if;
            T_1  := Sin / Cos;
            exit when not Unbounded_Rationals.Error (T_0, T_1, Error);
            Negative := not Negative;
            N := N + 1;
            Swap (T_0, T_1);
         end loop;
         Round (T_1, -Integer'Min (0, Error));
         return T_1;
      end;
   end Tan;

   function Tan_Continued_Fraction
            (  X : Unbounded_Rational;
               N : Positive
            )  return Unbounded_Rational is
   begin
      if Is_Zero (X) then
         return Zero;
      end if;
      declare
         X2 : constant Unbounded_Rational := Square (X);
         N2 : Half_Word := Half_Word (N) * 2 + 1;
         Y  : Unbounded_Rational := X2 / N2;
      begin
         loop
            N2 := N2 - 2;
            Y  := N2 - X2 / Y;
            exit when N2 = 1;
         end loop;
         Y := X / Y;
         return Y;
      end;
   end Tan_Continued_Fraction;

   function Tanh (X : Unbounded_Rational; Error : Integer)
      return Unbounded_Rational is
   begin
      if Is_Zero (X) then
         return Zero;
      end if;
      declare
         Cosh : Unbounded_Rational := One;
         Sinh : Unbounded_Rational := Zero;
         U    : Unbounded_Rational := One;
         N    : Half_Word := 1;
         T_0  : Unbounded_Rational;
         T_1  : Unbounded_Rational;
      begin
         loop
            U    := U * X / N;
            Sinh := Sinh + U;
            N    := N + 1;
            U    := U * X / N;
            Cosh := Cosh + U;
            T_1  := Sinh / Cosh;
            exit when not Unbounded_Rationals.Error (T_0, T_1, Error);
            N    := N + 1;
            Swap (T_0, T_1);
         end loop;
         Round (T_1, -Integer'Min (0, Error));
         return T_1;
      end;
   end Tanh;

   function Tanh_Continued_Fraction
            (  X : Unbounded_Rational;
               N : Positive
            )  return Unbounded_Rational is
   begin
      if Is_Zero (X) then
         return Zero;
      elsif N = 1 then
         return X;
      end if;
      declare
         X2 : constant Unbounded_Rational := Square (X);
         N2 : Half_Word := Half_Word (N) * 2 - 1;
         Y  : Unbounded_Rational :=
                 (False, From_Half_Word (N2), Unbounded_Unsigneds.One);
      begin
         while N2 > 1 loop
            N2 := N2 - 2;
            Y  := N2 + X2 / Y;
         end loop;
         return X / Y;
      end;
   end Tanh_Continued_Fraction;

end Unbounded_Rationals.Elementary_Functions;
