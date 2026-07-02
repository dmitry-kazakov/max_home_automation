--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser                  Luebeck            --
--        Get_Case                                 Summer, 2025       --
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
   procedure Get_Case
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Left     : Location_Type
             )  is
   use Case_Alternatives_Stack;

   type Case_Expression_Ptr is access Case_Expression;
   for Case_Expression_Ptr'Storage_Pool use Context.Pool.all;

   Where    : constant Location_Type := Link (Code);
   Selector : Argument_Token;
   Pair     : Case_Alternative;

   function Get (Text : String; Delimited : Boolean := True)
      return Boolean is
      Got_It : Boolean;
   begin
      Get_Delimited (Code, Text, Delimited, Got_It);
      return Got_It;
   end;
begin
   Get_Blank (Context, Code);
   Lexers.Parse (Context, Code, Selector);
   Get_Blank (Context, Code);
   if not Get ("is") then
      Raise_Exception
      (  Parsers.Syntax_Error'Identity,
         "'is' is expected at " & Image (Link (Code))
      );
   end if;
   for Index in Positive'Range loop
      Get_Blank (Context, Code);
      if not Get ("when") then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "'when' is expected at " & Image (Link (Code))
         );
      elsif Get ("others") then
         Get_Blank (Context, Code);
         if not Get ("=>", False) then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               "'=>' is expected after 'others' at " &
               Image (Link (Code))
            );
         end if;
         Lexers.Parse (Context, Code, Argument);
         Get_Blank (Context, Code);
         if not Has_Bracket (Code) then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "Parenthesis closing the case expression starting at "
               &  Image (Left)
               &  " is expected at "
               &  Image (Link (Code))
            )  );
         end if;
         declare
            Result : constant Case_Expression_Ptr :=
                          new Case_Expression (Index - 1, True);
            This   : Case_Expression renames Result.all;
         begin
            This.Selector := Selector;
            This.Others_Alternative := Argument;
            for Index in reverse This.Alternatives'Range loop
               Pop (Context, This.Alternatives (Index));
            end loop;
            Argument.Value    := This'Unchecked_Access;
            Argument.Location := Where & Argument.Location;
            return;
         end;
      end if;
      Get_Discrete_Choice_List (Context, Code, Pair.Choice);
      Lexers.Parse (Context, Code, Pair.Value);
      Push (Context, Pair);
      if not Get (",", False) then
         if not Has_Bracket (Code) then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "Parenthesis closing the case expression starting at "
               &  Image (Left)
               &  " is expected at "
               &  Image (Link (Code))
            )  );
         end if;
         declare
            Result : constant Case_Expression_Ptr :=
                          new Case_Expression (Index, False);
            This   : Case_Expression renames Result.all;
         begin
            This.Selector := Selector;
            for Index in reverse This.Alternatives'Range loop
               Pop (Context, This.Alternatives (Index));
            end loop;
            Argument.Value    := This'Unchecked_Access;
            Argument.Location := Where & Argument.Location;
            return;
         end;
      end if;
   end loop;
end Get_Case;
