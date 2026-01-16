--                                                                    --
--  package Py_Class                Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
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
--
-- Python class example.  This package provides a class point wrapped in
-- the module point. So the type is referenced as point.point.  The type
-- instances contain Ada record type Point.  The type has two attributes
-- x and y.  And method length() that calculates the vector length.  The
-- object constructor takes two keyed parameters x and y.
--
with Py.Generic_Class;

package Py_Class is
   use Py;
--
-- Point -- Ada type
--
   type Point is tagged record
      X : Float;
      Y : Float;
   end record;
   function Create_Point_Type return Handle;

   function Create (Arguments : Actual_Argument_List) return Point;
   function Image  (Data : Point) return String;
   function "<" (Left, Right : Point) return Boolean;
   package Point_Class is new Py.Generic_Class (Point);
--
-- Point_Ex -- An Ada extension of Point
--
   type Point_Ex is new Point with record
      Width : Float;
   end record;
   function Create_Point_Ex_Type (Base : Handle) return Handle;

   function Create (Arguments : Actual_Argument_List) return Point_Ex;
   function Image  (Data : Point_Ex) return String;
   function "<" (Left, Right : Point_Ex) return Boolean;
   package Point_Class_Ex is new Py.Generic_Class (Point_Ex);
--
-- List_Ex -- Ada extension of Python list
--
   type List_Ex is tagged record
      State : Natural;
   end record;
   function Create_List_Type return Handle;

   function Create (Arguments : Actual_Argument_List) return List_Ex;
   function Image  (Data : List_Ex) return String;
   function "<" (Left, Right : List_Ex) return Boolean;
   package List_Class is new Py.Generic_Class (List_Ex);

end Py_Class;
