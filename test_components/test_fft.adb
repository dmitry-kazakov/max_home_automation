--                                                                    --
--  package Test_FFT                Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Winder, 2025       --
--                                                                    --
--                                Last revision :  10:38 30 Jan 2025  --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Text_IO;            use Ada.Text_IO;
with Test_Float_FFT;         use Test_Float_FFT;
with Strings_Edit.Floats;    use Strings_Edit.Floats;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

procedure Test_FFT is

   procedure Put (X : Complex_Array; Small : Integer := -5) is
      use Strings_Edit;
      Line    : String (1..80);
      Pointer : Integer := 1;
   begin
      for Index in X'Range loop
         Put (Line, Pointer, "   ");
         Put (Line, Pointer, "(");
         Put
         (  Destination => Line,
            Pointer     => Pointer,
            Value       => X (Index).Re,
            AbsSmall    => Small,
            Field       => 9,
            Justify     => Right,
            PutPlus     => True
         );
         Put (Line, Pointer, ", ");
         Put
         (  Destination => Line,
            Pointer     => Pointer,
            Value       => X (Index).Im,
            AbsSmall    => Small,
            Field       => 9,
            Justify     => Left,
            PutPlus     => True
         );
         Put (Line, Pointer, ")");
         if Index /= X'Last then
            Put (Line, Pointer, ",");
         end if;
         Put_Line (Line (1..Pointer - 1));
         Pointer := 1;
      end loop;
   end Put;
   use Float_FFT;

   procedure Put (P : Permutation_Array) is
      use Strings_Edit;
      Line    : String (1..80);
      Pointer : Integer := 1;
   begin
      for Index in P'Range loop
         Put
         (  Destination => Line,
            Pointer     => Pointer,
            Value       => P (Index).I,
            Field       => 2,
            Justify     => Right
         );
         Put (Line, Pointer, " <-> ");
         Put (Line, Pointer, P (Index).J);
         Put_Line (Line (1..Pointer - 1));
         Pointer := 1;
      end loop;
   end Put;

   procedure Check (X, Y : Complex_Array; Eps : Float := 0.0005) is
   begin
      for I in X'Range loop
         if abs (X (I).Re - Y (I).Re) > Eps then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Real part differs in " & Image (Integer (I))
               &  " |"
               &  Image (X (I).Re)
               &  " - "
               &  Image (Y (I).Re)
               &  " | > "
               &  Image (Eps)
            )  );
         elsif abs (X (I).Im - Y (I).Im) > Eps then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Imaginary part differs in "
               &  Image (Integer (I))
               &  " |"
               &  Image (X (I).Im)
               &  " - "
               &  Image (Y (I).Im)
               &  " | > "
               &  Image (Eps)
            )  );
         end if;
      end loop;
   end Check;
begin
   Put_Line ("FFT permuntation 3:");
   Put (FFT_Permutation (3));
   Put_Line ("FFT permuntation 4:");
   Put (FFT_Permutation (4));
   Put_Line ("FFT permuntation 5:");
   Put (FFT_Permutation (5));
   Put_Line ("FFT permuntation 6:");
   Put (FFT_Permutation (6));
   declare
      X : Complex_Array :=
             (  (1.0, 0.0), (1.0, 0.0), (1.0, 0.0), (1.0, 0.0),
                (0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0)
             );
   begin
      Put_Line ("FFT procedure:");
      FFT (X);
      Put (X);
      Check
      (  X,
         (  (4.0, 0.0), (1.0, -2.41421), (0.0, 0.0), (1.0, -0.41421),
            (0.0, 0.0), (1.0,  0.41421), (0.0, 0.0), (1.0,  2.41421)
      )  );
      Put_Line ("Inverse FFT procedure:");
      FFT (X, True);
      Put (X);
      Check
      (  X,
         (  (1.0, 0.0), (1.0, 0.0), (1.0, 0.0), (1.0, 0.0),
            (0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0)
      )  );
   end;
   declare
      X : Complex_Array :=
             (  (1.0, 0.0), (1.0, 0.0), (1.0, 0.0), (1.0, 0.0),
                (0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0)
             );
   begin
      Put_Line ("FFT (rectangle) procedure with precomputed cos/sin:");
      FFT (X, FFT_Permutation (3), FFT_Factors (3));
      Put (X);
      Check
      (  X,
         (  (4.0, 0.0), (1.0, -2.41421), (0.0, 0.0), (1.0, -0.41421),
            (0.0, 0.0), (1.0,  0.41421), (0.0, 0.0), (1.0,  2.41421)
      )  );
      Put_Line ("FFT (rectangle) inverse procedure:");
      FFT (X, FFT_Permutation (3), FFT_Factors (3, True), True);
      Put (X);
      Check
      (  X,
         (  (1.0, 0.0), (1.0, 0.0), (1.0, 0.0), (1.0, 0.0),
            (0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0)
      )  );
   end;
   declare
      X : Complex_Array :=
             (  (0.0, 0.0), (1.0, 0.0), (2.0, 0.0), (3.0, 0.0),
                (4.0, 0.0), (5.0, 0.0), (6.0, 0.0), (7.0, 0.0)
             );
   begin
      Put_Line ("FFT (ramp) procedure with precomputed cos/sin:");
      FFT (X, FFT_Permutation (3), FFT_Factors (3));
      Put (X);
      Check
      (  X,
         (  (28.0, 0.0), (-4.0, 9.65685), (-4.0, 4.0), (-4.0, 1.65685),
            (-4.0, 0.0), (-4.0,-1.65685), (-4.0,-4.0), (-4.0,-9.65685)
      )  );
      Put_Line ("FFT inverse procedure:");
      FFT (X, FFT_Permutation (3), FFT_Factors (3, True), True);
      Put (X);
      Check
      (  X,
         (  (0.0, 0.0), (1.0, 0.0), (2.0, 0.0), (3.0, 0.0),
            (4.0, 0.0), (5.0, 0.0), (6.0, 0.0), (7.0, 0.0)
      )  );
   end;
   Put_Line ("FFT function:");
   declare
      X : constant Complex_Array :=
             (  FFT
                (  (  (1.0, 0.0), (1.0, 0.0), (1.0, 0.0), (1.0, 0.0),
                      (0.0, 0.0)
             )  )  );
   begin
      Put (X);
      Check
      (  X,
         (  (4.0, 0.0), (1.0, -2.41421), (0.0, 0.0), (1.0, -0.41421),
            (0.0, 0.0), (1.0,  0.41421), (0.0, 0.0), (1.0,  2.41421)
      )  );
   end;
   declare
      use Elementary_Functions;
      Pi : constant := 3.1415_9265_3589_7932_3846_2643;
      X  : Complex_Array (1..64);
   begin
      for I in X'Range loop
         X (I) := (cos (2.0 * Pi * Float (I - 1) * 0.25), 0.0);
      end loop;
      Put_Line ("FFT procedure:");
      FFT (X, FFT_Permutation (6), FFT_Factors (6));
      Put (X, -3);
      Check
      (  X,
         (  ( 0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0),
            ( 0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0),
            ( 0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0),
            ( 0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0),
            (32.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0),
            ( 0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0),
            ( 0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0),
            ( 0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0),
            ( 0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0),
            ( 0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0),
            ( 0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0),
            ( 0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0),
            (32.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0),
            ( 0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0),
            ( 0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0),
            ( 0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0)
      )  );
   end;
end Test_FFT;
