--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Generic_Dot                              Summer, 2025       --
--  Interface                                                                  --
--                                Last revision :  11:48 10 Aug 2025  --
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
--  This  package  generates  DOT (Graph description  language)  from  a
--  syntax tree.
--
with Ada.Streams;  use Ada.Streams;
with Ada.Text_IO;  use Ada.Text_IO;

with Ada.Streams.Stream_IO;

generic
package Parsers.Generic_Ada_Parser.Generic_Dot is
--
-- Put -- Write syntax tree into a file in the DOT format
--
--    Tree           - The syntax tree
--    Output         - The file or stream to write into
--    Show_Locations - Show source locations in the nodes
--
   procedure Put
             (  Tree           : Tokens.Argument_Token;
                Output         : Stream_IO.File_Type;
                Show_Locations : Boolean := True
             );
   procedure Put
             (  Tree           : Tokens.Argument_Token;
                Output         : File_Type;
                Show_Locations : Boolean := True
             );
   procedure Put
             (  Tree           : Tokens.Argument_Token;
                Output         : access Root_Stream_Type'Class;
                Show_Locations : Boolean := True
             );
--
-- Put -- Write syntax tree into a file
--
--    Tree           - The syntax tree
--    Output         - The file name to write into
--    Show_Locations - Show source locations in the nodes
--
   procedure Put
             (  Tree           : Tokens.Argument_Token;
                Name           : String;
                Show_Locations : Boolean := True
             );

end Parsers.Generic_Ada_Parser.Generic_Dot;
