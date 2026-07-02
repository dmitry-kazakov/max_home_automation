--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Get_Simple_Name                          Summer, 2025       --
--  Separate body implementation                                      --
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

separate (Parsers.Generic_Ada_Parser) 
   procedure Get_Simple_Name
             (  Context    : in out Ada_Expression;
                Code       : in out Lexers.Lexer_Source_Type;
                No_Strings : Boolean;
                Argument   : out Tokens.Argument_Token
             )  is
   use Operator_Tables;
   Line    : Line_Ptr_Type;
   Pointer : Integer;
   Last    : Integer;
begin
   Get_Blank (Context, Code);
   Get_Line (Code, Line, Pointer, Last);
   if Pointer > Last then
      Raise_Exception
      (  Parsers.Syntax_Error'Identity,
         "Identifier is expected at " & Image (Link (Code))
      );
   end if;
   if Line (Pointer) = '"' then
      Set_Pointer (Code, Pointer);
      Get_String_Literal
      (  Context,
         Code,
         Line (Pointer..Last),
         Pointer,
         Argument
      );
      if No_Strings then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Identifier is expected at " & Image (Argument.Location)
         );
      elsif Locate
            (  Operators,
               Text_Literal (Argument.Value.all).Value
            )
         =  0
      then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Invalid operator symbol '"             &
            Text_Literal (Argument.Value.all).Value &
            "' at "                                 &
            Image (Argument.Location)
         );
      end if;
   else
      declare
         Index  : Integer := Pointer;
         Symbol : UTF8_Code_Point;
      begin
         Get (Line (Pointer..Last), Index, Symbol);
         if not Is_Identifier_Start (Symbol) then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               "Identifier is expected at " &
               Image (Link (Code))
            );
         end if;
      exception
         when Data_Error =>
            Set_Pointer (Code, Pointer);
            Set_Pointer (Code, Pointer);
            Raise_Exception
            (  Syntax_Error'Identity,
               Encoding_Error & Image (Link (Code))
            );
      end;
      Get_Identifier
      (  Context,
         Code,
         Line (Pointer..Last),
         Pointer,
         Argument
      );
      declare
         This : Identifier renames Identifier (Argument.Value.all);
      begin
         if Reserved_Words_Tables.IsIn (Reserved_Words, This.Value) then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "A reserved word '"
               &  This.Value
               &  "' cannot be used as an identifier at "
               &  Image (Link (Code))
            )  );
         end if;
      end;
   end if;
end Get_Simple_Name;
