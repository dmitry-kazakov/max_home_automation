--                                                                    --
--  package Test_String_Graph       Copyright (c)  Dmitry A. Kazakov  --
--  Test instantiation                             Luebeck            --
--                                                 Winter, 2009       --
--                                                                    --
--                                Last revision :  12:14 29 Mar 2026  --
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

with Generic_Directed_Graph;
with Generic_Undirected_Graph;
with Generic_Unbounded_Array;
with Generic_Address_Order;

package Test_String_Graph is
   type Default is access String;
   package Order is new Generic_Address_Order (String);
   use Order;
   package Directed is
      new Generic_Directed_Graph (String, Default'Storage_Pool);

   package Undirected is
      new Generic_Undirected_Graph (String, Default'Storage_Pool);

   type Node_Set_Array is
      array (Positive range <>) of Undirected.Node_Sets.Set;

   package Node_Set_Arrays is
      new Generic_Unbounded_Array
          (  Index_Type        => Positive,
             Object_Type       => Undirected.Node_Sets.Set,
             Object_Array_Type => Node_Set_Array,
             Null_Element      => Undirected.Node_Sets.Create
          );
end Test_String_Graph;
