--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Get_Character_Literal                    Winter, 2004       --
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
   procedure Get_Character_Literal
             (  Code     : in out Lexers.Lexer_Source_Type;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is
   Symbol : UTF8_Code_Point;
   Index  : Integer := Pointer + 1;
begin
   if Index < Line'Last then
      Get (Line, Index, Symbol);
      if Index <= Line'Last and then ''' = Line (Index) then
         Set_Pointer (Code, Index + 1);
         Argument.Location := Link (Code);
         Argument.Value := new Character_Literal (Index - Pointer - 1);
         Character_Literal (Argument.Value.all).Value :=
            Line (Pointer + 1..Index - 1);
         return;
      end if;
   end if;
   Set_Pointer (Code, Pointer + 1);
   Raise_Exception
   (  Parsers.Syntax_Error'Identity,
      (  "Missing ' in the character literal at "
      &  Image (Link (Code))
   )  );
exception
   when Data_Error =>
      Set_Pointer (Code, Index);
      Set_Pointer (Code, Index);
      Raise_Exception
      (  Syntax_Error'Identity,
         Encoding_Error & Image (Link (Code))
      );
end Get_Character_Literal;
