--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Get_Identifier                           Winter, 2025       --
--  Separate body implementation                                      --
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

separate (Parsers.Generic_Ada_Parser) 
   procedure Get_Identifier
             (  Code     : in out Lexers.Lexer_Source_Type;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is
   Index     : Integer := Pointer;
   Start     : Integer;
   Malformed : Boolean := False;
   Underline : Boolean := False;
   Symbol    : UTF8_Code_Point;
begin
   while Index <= Line'Last loop
      Start := Index;
      Get (Line, Index, Symbol);
      case Category (Symbol) is
         when Mn | Mc | Nd | Cf | Letter | Nl =>
            Underline := False;
         when Pc =>
            Malformed := Malformed or Underline;
            Underline := True;
         when others =>
            Index := Start;
            exit;
      end case;
   end loop;
   Malformed := Malformed or Underline;
   Set_Pointer (Code, Index);
   Argument.Location := Link (Code);
   Argument.Value := new Identifier (Index - Pointer);
   declare
      This : Identifier renames Identifier (Argument.Value.all);
   begin
      This.Malformed := Malformed;
      This.Value     := Line (Pointer..Index - 1);
   end;
exception
   when Data_Error =>
      Set_Pointer (Code, Index);
      Set_Pointer (Code, Index);
      Raise_Exception
      (  Parsers.Syntax_Error'Identity,
         Encoding_Error & Image (Link (Code))
      );
end Get_Identifier;
