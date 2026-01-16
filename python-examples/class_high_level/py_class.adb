--                                                                    --
--  package Py_Class                Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2025       --
--                                                                    --
--                                Last revision :  11:08 04 Dec 2025  --
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

with Ada.Exceptions;  use Ada.Exceptions;
with Interfaces.C;    use Interfaces.C;

with Ada.Numerics.Elementary_Functions;
use  Ada.Numerics.Elementary_Functions;

with Py.Generic_Class.Generic_Member;
with Py.Generic_Class.Generic_Keyed_Method;
with Py.Generic_Class.Generic_No_Argument_Method;
with Py.Generic_Class.Generic_One_Argument_Method;
with Py.Generic_Class.Generic_Positional_Method;

package body Py_Class is

   function "<" (Left, Right : Point) return Boolean is
   begin
      Raise_Exception (Constraint_Error'Identity, "Incomparable");
      return False;
   end "<";

   function "<" (Left, Right : Point_Ex) return Boolean is
   begin
      return Left.Width < Right.Width;
   end "<";

   function "<" (Left, Right : List_Ex) return Boolean is
   begin
      return Left.State < Right.State;
   end "<";

   procedure Add
             (  Data      : in out Point;
                Arguments : Actual_Argument_List;
                Result    : out Handle
             )  is
   begin
      for Index in Arguments'Range loop
         declare
            This : constant Point :=
                            Point_Class.Get (Arguments (Index));
         begin
            Data.X := Data.X + This.X;
            Data.Y := Data.Y + This.Y;
         end;
      end loop;
      Result := No_Value;
   end Add;

   package Point_Add is
      new Point_Class.Generic_Positional_Method
          (  "add",
             "add up vectors",
             Add
          );

   function Create_Point_Type return Handle is
   begin
      return Point_Class.Create
             (  "point",
                "cartesian point",
                - "x" - "y"
             );
   end Create_Point_Type;

   function Create_Point_Ex_Type (Base : Handle) return Handle is
   begin
      return Point_Class_Ex.Create
             (  "point_ex",
                "cartesian point with width",
                - "x" - "y" - "width",
                Base
             );
   end Create_Point_Ex_Type;

   function Create_List_Type return Handle is
   begin
      return List_Class.Extend
             (  "list_ex",
                "extended list",
                List_Type,
                - "list" - "state",
                (True, False)
             );
   end Create_List_Type;

   function Create (Arguments : Actual_Argument_List) return Point is
      Result : Point := (0.0, 0.0);
   begin
      if Arguments (1) /= No_Value then
         Result.X := Float (Float_AsDouble (Arguments (1)));
      end if;
      if Arguments (2) /= No_Value then
         Result.Y := Float (Float_AsDouble (Arguments (2)));
      end if;
      return Result;
   end Create;

   function Create (Arguments : Actual_Argument_List) return Point_Ex is
      Result : Point_Ex := (0.0, 0.0, 1.0);
   begin
      if Arguments (1) /= No_Value then
         Result.X := Float (Float_AsDouble (Arguments (1)));
      end if;
      if Arguments (2) /= No_Value then
         Result.Y := Float (Float_AsDouble (Arguments (2)));
      end if;
      if Arguments (3) /= No_Value then
         Result.Width := Float (Float_AsDouble (Arguments (3)));
      end if;
      return Result;
   end Create;

   function Create (Arguments : Actual_Argument_List) return List_Ex is
      Result : List_Ex := (State => 0);
   begin
      if Arguments (2) /= No_Value then
         Result.State := Natural (Long_AsUnsignedLong (Arguments (2)));
      end if;
      return Result;
   end Create;

   function Get_X (Data : Point) return Handle is
   begin
      return Float_FromDouble (double (Data.X));
   exception
      when Constraint_Error =>
         Throw_ValueError ("Invalid x");
         return Null_Handle;
   end Get_X;

   procedure Set_X (Data : in out Point; Value : Handle) is
   begin
      Data.X := Float (Float_AsDouble (Value));
   exception
      when Constraint_Error =>
         Throw_ValueError ("Invalid x");
   end Set_X;

   package Point_X is
      new Point_Class.Generic_Member
          (  "x",
             "x co-cordinate",
             Get_X,
             Set_X
          );

   function Get_Y (Data : Point) return Handle is
   begin
      return Float_FromDouble (double (Data.Y));
   exception
      when Constraint_Error =>
         Throw_ValueError ("Invalid y");
         return Null_Handle;
   end Get_Y;

   procedure Set_Y (Data : in out Point; Value : Handle) is
   begin
      Data.Y := Float (Float_AsDouble (Value));
   exception
      when Constraint_Error =>
         Throw_ValueError ("Invalid y");
   end Set_Y;

   package Point_Y is
      new Point_Class.Generic_Member
          (  "y",
             "y co-cordinate",
             Get_Y,
             Set_Y
          );

   function Get_Width (Data : Point_Ex) return Handle is
   begin
      return Float_FromDouble (double (Data.Width));
   exception
      when Constraint_Error =>
         Throw_ValueError ("Invalid width");
         return Null_Handle;
   end Get_Width;

   procedure Set_Width (Data : in out Point_Ex; Value : Handle) is
   begin
      Data.Width := Float (Float_AsDouble (Value));
   exception
      when Constraint_Error =>
         Throw_ValueError ("Invalid width");
   end Set_Width;

   package Point_Ex_Width is
      new Point_Class_Ex.Generic_Member
          (  "width",
             "vector width",
             Get_Width,
             Set_Width
          );

   function Get_State (Data : List_Ex) return Handle is
   begin
      return Long_FromUnsignedLong (unsigned_long (Data.State));
   exception
      when Constraint_Error =>
         Throw_ValueError ("Invalid state");
         return Null_Handle;
   end Get_State;

   procedure Set_State (Data : in out List_Ex; Value : Handle) is
   begin
      Data.State := Natural (Long_AsUnsignedLong (Value));
   exception
      when Constraint_Error =>
         Throw_ValueError ("Invalid state");
   end Set_State;

   package Listt_Ex_State is
      new List_Class.Generic_Member
          (  "state",
             "list state",
             Get_State,
             Set_State
          );

   function Image (Data : Point) return String is
   begin
      return "(x =>"              &
             Float'Image (Data.X) &
             ", y =>"             &
             Float'Image (Data.Y) &
             ")";
   end Image;

   function Image  (Data : Point_Ex) return String is
   begin
      return "(x =>"                  &
             Float'Image (Data.X)     &
             ", y =>"                 &
             Float'Image (Data.Y)     &
             ", width =>"             &
             Float'Image (Data.Width) &
             ")";
   end Image;

   function Image  (Data : List_Ex) return String is
   begin
      return " with state =>" &
             Natural'Image (Data.State);
   end Image;

   procedure Length_Of (Data : in out Point; Result : out Handle) is
   begin
      Result :=
         Float_FromDouble (double (Sqrt (Data.X**2 + Data.Y**2)));
   end Length_Of;

   package Point_Length is
      new Point_Class.Generic_No_Argument_Method
          (  "length",
             "vector length",
             Length_Of
          );

   procedure Rotate
             (  Data   : in out Point;
                Angle  : Handle;
                Result : out Handle
             )  is
      C, S : Float;
      X, Y : Float;
   begin
      C := Cos (Float (Float_AsDouble (Angle)));
      S := Sin (Float (Float_AsDouble (Angle)));
      X := Data.X * C - Data.Y * S;
      Y := Data.X * S + Data.Y * C;
      Data   := (X, Y);
      Result := No_Value;
   end Rotate;
   package Point_Rotate is
      new Point_Class.Generic_One_Argument_Method
          (  "rotate",
             "rotate vector",
             Rotate
          );

   procedure Transform
             (  Data      : in out Point;
                Arguments : Actual_Argument_List;
                Result    : out Handle
             )  is
      A : Float;
      B : Point;
   begin
      A := Float (Float_AsDouble (Arguments (1)));
      B := Point_Class.Get (Arguments (2));
      Result := Point_Class.Create
                (  (  X => Data.X * A + B.X,
                      Y => Data.Y * A + B.Y
                )  );
   exception
      when Constraint_Error =>
         Throw_ValueError ("Invalid angle");
         Result := Null_Handle;
   end Transform;
   package Point_Transform is
      new Point_Class.Generic_Keyed_Method
          (  "transform",
             "affine transformation",
             "a" + "b",
             Transform
          );
end Py_Class;
