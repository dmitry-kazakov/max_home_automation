--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Get_Raise                                Summer, 2025       --
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
   procedure Get_Raise
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Enclosed : Boolean;
                Left     : Location_Type
             )  is
   Has_Message : Boolean := False;
   Got_It      : Boolean;
   Where       : constant Location_Type := Link (Code);
   Message     : Tokens.Argument_Token;
   Name        : Tokens.Argument_Token;

   procedure Get (Text : String; Delimited : Boolean := True) is
   begin
      Get_Delimited (Code, Text, Delimited, Got_It);
   end;
begin
   Get_Blank (Context, Code);
   Lexers.Parse (Context, Code, Name);
   Get_Blank (Context, Code);
   Get ("with");
   if Got_It then
      Has_Message := True;
      Get_Blank (Context, Code);
      Lexers.Parse (Context, Code, Message);
      Get_Blank (Context, Code);
   end if;
   if Enclosed and then not Has_Bracket (Code) then
      Raise_Exception
      (  Parsers.Syntax_Error'Identity,
         (  "The raise expression's parenthesis '(' at "
         &  Image (Left)
         &  " is expected to be closed by ')' at "
         &  Image (Link (Code))
      )  );
   end if;
   declare
      type Arena_Ptr is access Raise_Expression;
      for Arena_Ptr'Storage_Pool use Context.Pool.all;
      Result : constant Arena_Ptr := new Raise_Expression (Has_Message);
      This   : Raise_Expression renames Result.all;
   begin
      This.Name := Name;
      if Has_Message then
         This.Message := Message;
      end if;
      Argument.Value    := This'Unchecked_Access;
      Argument.Location := Where & Link (Code);
   end;
end Get_Raise;
