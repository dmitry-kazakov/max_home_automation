--                                                                    --
--  package Py_Module               Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2025       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2022  --
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

with Ada.Text_IO;   use Ada.Text_IO;
with Interfaces.C;  use Interfaces.C;
with Py;            use Py;

package body Py_Module is

   Module : Python_Module;

   procedure Keyed_Operation
             (  Module    : in out Python_Module'Class;
                Arguments : Actual_Argument_List;
                Result    : out Handle
             )  is
   begin
      Put_Line ("A =" & long'image (Long_AsLong (Arguments (1))));
      if Arguments (2) /= No_Value then
         Put_Line ("B = " & As_String (Arguments (2)));
      end if;
      if Arguments (3) /= No_Value then
         Put_Line
         (  "C ="
         &  double'Image (Float_AsDouble (Arguments (3)))
         );
      end if;
      Result := Unicode_FromString ("OK");
   end Keyed_Operation;

   procedure Positional_Operation
             (  Module    : in out Python_Module'Class;
                Arguments : Actual_Argument_List;
                Result    : out Handle
             )  is
   begin
      for Index in Arguments'Range loop
         Put_Line
         (  "Argument"
         &  Argument_Position'Image (Index)
         &  " = "
         &  Object_Str (Arguments (Index))
         );
      end loop;
      Result := No_Value;
   end Positional_Operation;

   procedure One_Argument_Operation
             (  Module   : in out Python_Module'Class;
                Argument : Handle;
                Result   : out Handle
             )  is
   begin
      Result := Float_FromDouble (Float_AsDouble (Argument) ** 2);
   end One_Argument_Operation;

   procedure No_Argument_Operation
             (  Module : in out Python_Module'Class;
                Result : out Handle
             )  is
   begin
      Put_Line ("Bye!");
      Result := No_Value;
   end No_Argument_Operation;

   procedure Create is
   begin
      Create
      (  Module => Module,
         Name   => "sample_module",
         Doc    => "sample Python module"
      );
      Add_Function
      (  Module  => Module,
         Name    => "operation",
         Doc     => "Test keyed operation",
         Profile => "a" - "b" - "c",
         Operation => Keyed_Operation'Access
      );
      Add_Function
      (  Module    => Module,
         Name      => "list",
         Doc       => "Test variable argument list operation",
         Operation => Positional_Operation'Access
      );
      Add_Function
      (  Module    => Module,
         Name      => "square",
         Doc       => "Test one argument list operation",
         Operation => One_Argument_Operation'Access
      );
      Add_Function
      (  Module    => Module,
         Name      => "say_bye",
         Doc       => "Test no argument list operation",
         Operation => No_Argument_Operation'Access
      );
   end Create;

end Py_Module;
