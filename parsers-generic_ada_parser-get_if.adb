--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Get_If                                   Summer, 2025       --
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
   procedure Get_If
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Left     : Location_Type
             )  is
   Has_Else : Boolean;
   Got_It   : Boolean;
   Where    : Location_Type := Link (Code);
   Default  : Tokens.Argument_Token;

   procedure Get (Text : String; Delimited : Boolean := True) is
   begin
      Get_Delimited (Code, Text, Delimited, Got_It);
   end;
begin
   for Count in Positive'Range loop
      Get_Blank (Context, Code);
      Lexers.Parse (Context, Code, Argument);
      Push (Context, Argument);
      Get_Blank (Context, Code);
      Set_Pointer (Code, Get_Pointer (Code));
      Get ("then");
      if not Got_It then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "'then' is expected at " & Image (Link (Code))
         );
      end if;
      Get_Blank (Context, Code);
      Lexers.Parse (Context, Code, Argument);
      Push (Context, Argument);
      Get_Blank (Context, Code);
      Set_Pointer (Code, Get_Pointer (Code));
      Get (",", False);
      if Got_It then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            (  "Unexpected comma (',') in if expression at "
            &  Image (Link (Code))
         )  );
      end if;        
      Get ("elsif");
      if not Got_It then
         Get ("else");
         Has_Else := Got_It;
         if Has_Else then
            Get_Blank (Context, Code);
            Lexers.Parse (Context, Code, Default);
            Where := Where & Default.Location;
         end if;
         Get_Blank (Context, Code);
         declare
            Line    : Line_Ptr_Type;
            Pointer : Integer;
            Last    : Integer;
         begin
            Get_Line (Code, Line, Pointer, Last);
            if Pointer > Last or else Line (Pointer) /= ')' then
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  (  "Bracket closing if expression's left bracket at "
                  &  Image (Left)
                  &  " is expected at "
                  &  Image (Link (Code))
               )  );
            end if;
         end;
         Argument.Value := new If_Expression (Count, Got_It);
         declare
            Item : Tokens.Arguments.Frame (1..2);
            This : If_Expression'Class renames
                   If_Expression'Class (Argument.Value.all);
         begin
            for Index in reverse This.Alternatives'Range loop
               declare
                  Pair : Alternative_Pair renames
                         This.Alternatives (Index);
               begin
                  Pop (Context, Item);
                  Pair.Guard := Item (1);
                  Pair.Value := Item (2);
               end;
            end loop;
            if Has_Else then
               This.Else_Alternative := Default;
            end if;
            Argument.Location := Where;
            return;
         end;
      end if;
   end loop;
end Get_If;
