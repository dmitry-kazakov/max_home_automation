--                                                                    --
--  procedure Test_Class            Copyright (c)  Dmitry A. Kazakov  --
--  test                                           Luebeck            --
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

with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;
with Interfaces.C;            use Interfaces.C;

with Py_Class;
with Py.Load_Python_Library;
with System;

procedure Test_Class is
begin
   Py.Load
   (  Py.Load_Python_Library.Get_Python_Path
   &  Py.Load_Python_Library.Get_Default_Name
   );
   Py.Initialize;
   declare
      use Py;
      GIL      : Global_Interpreter_Lock;
      Hello    : Handle;
      Args     : Handle;
      Result   : Handle;
      Class    : Handle;
      Derived  : Handle;
      Extended : Handle;
   begin
      Class    := Py_Class.Create_Point_Type;
      Derived  := Py_Class.Create_Point_Ex_Type (Class);
      Extended := Py_Class.Create_List_Type;
      Hello :=
         Compile
         (  "import point"                                  & LF &
            "import point_ex"                               & LF &
            "import list_ex"                                & LF &
            "def Test():"                                   & LF &
            "   x = point.point(x=1.0,y=2.0)"               & LF &
            "   y = point.point()"                          & LF &
            "   print (x.x)"                                & LF &
            "   y.y = 100.0"                                & LF &
            "   print (y.y)"                                & LF &
            "   print (""y length=""+str(y.length()))"      & LF &
            "   x.rotate(1.57079632679)"                    & LF &
            "   print (x)"                                  & LF &
            "   x.add (point.point(1,1),point.point(2,1))"  & LF &
            "   print (x)"                                  & LF &
            "   x = x.transform(b=point.point(1,1),a=2)"    & LF &
            "   print (x)"                                  & LF &
            "   x = point_ex.point_ex(width=2.0,y=1.0)"     & LF &
            "   print (x.y)"                                & LF &
            "   print (x<point_ex.point_ex(width=1.0))"     & LF &
            "   print (point.point.__doc__)"                & LF &
            "   z = list_ex.list_ex((1,2,3))"               & LF &
            "   z.append (4)"                               & LF &
            "   z.state = 41"                               & LF &
            "   print (z.state)"                            & LF &
            "   print (z < [2])"                            & LF &
            "   print (z)",
            "point.py"
         );
      Args   := Tuple_New (0);
      Result := Object_CallObject (Hello, Args, True);
   end;
   if Py.FinalizeEx < 0 then
      Put_Line ("Python finalization error");
   end if;
   Put_Line ("Exiting");
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Class;
