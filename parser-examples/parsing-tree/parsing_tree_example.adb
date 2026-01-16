--                                                                    --
--  procedure Parsing_Tree_Example  Copyright (c)  Dmitry A. Kazakov  --
--  Example                                        Luebeck            --
--                                                 Summer, 2025       --
--                                                                    --
--                                Last revision :  11:57 10 Aug 2025  --
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
with Parsers.String_Source;  use Parsers.String_Source;

with Parsers.Generic_Ada_Parser.Generic_Dot;

procedure Parsing_Tree_Example is
   package Ada_Parsers is
      new Parsers.Generic_Ada_Parser (Parsers.String_Source.Code);
   package Dot is new Ada_Parsers.Generic_Dot;

   Text  : aliased String := "A + B + C * D + E / 1.2";
   Input : aliased Source (Text'Access);

   Parser : Ada_Parsers.Ada_Expression;
   Result : Ada_Parsers.Tokens.Argument_Token;
   Stub   : Ada_Parsers.Node_Ptr;
begin
   Stub := new Ada_Parsers.Mark; -- Mark the stack
   Ada_Parsers.Lexers.Parse (Parser, Input, Result);
   Dot.Put (Result, "tree.dot", False);
   Ada_Parsers.Free (Stub);      -- Release the tree on the stack
exception
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
end Parsing_Tree_Example;
