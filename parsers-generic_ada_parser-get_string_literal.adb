--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Get_String_Literal                       Winter, 2004       --
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
   procedure Get_String_Literal
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is
   Length : Natural := 0;
   Index  : Integer := Pointer + 1;
begin
   while Index <= Line'Last loop
      if Line (Index) = '"' then
         Index := Index + 1;
         if Index > Line'Last or else Line (Index) /= '"' then
            --
             -- End of literal reached, validate string
            --
            declare
               Start : Integer := Pointer + 1;
            begin
               while Start < Index loop
                  Skip (Line, Start);
               end loop;
            exception
               when Data_Error =>
                  Set_Pointer (Code, Start);
                  Set_Pointer (Code, Start);
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     Encoding_Error & Image (Link (Code))
                  );
            end;
            Set_Pointer (Code, Index);
            declare
               type Arena_Ptr is access String_Literal;
               for Arena_Ptr'Storage_Pool use Context.Pool.all;
               Result : constant Arena_Ptr :=
                             new String_Literal (Length);
               This   : String_Literal renames Result.all;
            begin
               Index := Index - 2;
               for Target in reverse This.Value'Range loop
                  This.Value (Target) := Line (Index);
                  if '"' = This.Value (Target) then
                     Index := Index - 2;
                  else
                     Index := Index - 1;
                  end if;
               end loop;
               Argument.Value    := This'Unchecked_Access;
               Argument.Location := Link (Code);
            end;
            return;
         end if;
      end if;
      Index  := Index  + 1;
      Length := Length + 1;
   end loop;
   Set_Pointer (Code, Index);
   Raise_Exception
   (  Parsers.Syntax_Error'Identity,
      (  "Missing "" closing the string literal at "
      &  Image (Link (Code))
   )  );
end Get_String_Literal;
