--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Stack_Storage.Text_IO                       Luebeck            --
--  Implementation                                 Spring, 2026       --
--                                                                    --
--                                Last revision :  10:48 02 Jul 2026  --
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

package body Stack_Storage.Text_IO is

   procedure Put_Statistics (Stack : Pool) is
   begin
      Put_Statistics (Standard_Output, Stack);
   end Put_Statistics;

   procedure Put_Statistics (File : File_Type; Stack : Pool) is
      Size  : Storage_Count;
      Used  : Storage_Count;
      Start : Address;
   begin
      Put_Line (File, "Stack sorage pool segments:" &
                           Integer'Image (Get_Segments_Number (Stack)));
      Put_Line (File, "             used segments:" &
                              Integer'Image (Get_Last_Segment (Stack)));
      for Index in 1..Get_Segments_Number (Stack) loop
         Get_Segment_Data (Stack, Index, Size, Used, Start);
         Put_Line (File, "   Segment:" & Integer'Image (Index));
         Put_Line (File, "      Size:" & Storage_Count'Image (Size));
         Put_Line (File, "      Used:" & Storage_Count'Image (Used));
         Put_Line (File, "        At:" &
                            Integer_Address'Image (To_Integer (Start)));

      end loop;
   end Put_Statistics;

end Stack_Storage.Text_IO;
