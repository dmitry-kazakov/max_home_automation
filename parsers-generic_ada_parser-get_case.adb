--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser                  Luebeck            --
--        Get_Case                                 Summer, 2025       --
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
   procedure Get_Case
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Left     : Location_Type
             )  is
   Count      : Natural       := 0;
   Where      : Location_Type := Link (Code);
   Others_At  : Location_Type := Where;
   Got_Others : Boolean       := False;
   Got_It     : Boolean;
   Selector   : Argument_Token;
   Default    : Argument_Token;

   procedure Get (Text : String; Delimited : Boolean := True) is
   begin
      Get_Delimited (Code, Text, Delimited, Got_It);
   end;
begin
   Get_Blank (Context, Code);
   Lexers.Parse (Context, Code, Selector);
   Get_Blank (Context, Code);
   Set_Pointer (Code, Get_Pointer (Code));
   Get ("is");
   if not Got_It then
      Raise_Exception
      (  Parsers.Syntax_Error'Identity,
         "'is' is expected at " & Image (Link (Code))
      );
   end if;
   for Index in Positive'Range loop
      Get_Blank (Context, Code);
      Set_Pointer (Code, Get_Pointer (Code));
      Get ("when");
      if not Got_It then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "'when' is expected at " & Image (Link (Code))
         );
      end if;
      Get_Blank (Context, Code);
      Set_Pointer (Code, Get_Pointer (Code));
      Get ("others");
      if Got_It then
         if Index = 1 then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "'others' choice is the first variant specified at "
               &  Image (Link (Code))
            )  );
         elsif Got_Others then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "'others' choice appeared at "
               &  Image (Others_At)
               & " is duplicated at "
               &  Image (Link (Code))
            )  );
         end if;
         Get_Blank (Context, Code);
         Set_Pointer (Code, Get_Pointer (Code));
         Get ("=>", False);
         if not Got_It then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "'=>' is expected after 'others' at "
               &  Image (Link (Code))
            )  );
         end if;
         Got_Others := True;
         Others_At  := Link (Code);
         Get_Blank (Context, Code);
         Lexers.Parse (Context, Code, Default);
         Where := Where & Default.Location;
      else
         if Got_Others then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "'others' choice at "
               &  Image (Others_At)
               & " is followed by another variant at "
               &  Image (Link (Code))
            )  );
         end if;
         Count := Count + 1;
         Get_Blank (Context, Code);
         Lexers.Parse (Context, Code, Argument);
         Push (Context, Argument);
         Get_Blank (Context, Code);
         Set_Pointer (Code, Get_Pointer (Code));
         Get ("=>", False);
         if not Got_It then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               "'=>' is expected at " & Image (Link (Code))
            );
         end if;
         Get_Blank (Context, Code);
         Lexers.Parse (Context, Code, Argument);
         Push (Context, Argument);
      end if;
      Get (",", False);
      exit when not Got_It;
   end loop;
   declare
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
   begin
      Get_Line (Code, Line, Pointer, Last);
      if Pointer > Last or else Line (Pointer) /= ')' then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            (  "Bracket closing case expression's left bracket at "
            &  Image (Left)
            &  " is expected at "
            &  Image (Link (Code))
         )  );
      end if;
   end;
   Argument.Value := new Case_Expression (Count, Got_Others);
   declare
      Item : Tokens.Arguments.Frame (1..2);
      This : Case_Expression'Class renames
             Case_Expression'Class (Argument.Value.all);
   begin
      This.Selector := Selector;
      if Got_Others then
         This.Others_Alternative := Default;
      end if;
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
      Argument.Location :=
         Where & This.Alternatives (Count).Value.Location;
   end;
end Get_Case;
