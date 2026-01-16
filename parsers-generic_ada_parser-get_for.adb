--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Get_For                                  Summer, 2025       --
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
   procedure Get_For
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Left     : Descriptors.Operation_Type;
                Par      : Boolean
             )  is
   Where      : constant Location_Type := Link (Code);
   Got_It     : Boolean;
   For_Type   : For_Qualifier := For_Any;
   Options    : For_Optional  := 0;
   Count      : Natural       := 0;
   Chunk      : Tokens.Argument_Token;
   Range_Type : Tokens.Argument_Token;
   Expression : Tokens.Argument_Token;
   Identifier : Tokens.Argument_Token;
   Iterator   : Tokens.Argument_Token;
   Condition  : Tokens.Argument_Token;
   Key        : Tokens.Argument_Token;

   procedure Get (Text : String; Delimited : Boolean := True) is
   begin
      Get_Delimited (Code, Text, Delimited, Got_It);
   end;
begin
   if Par then
      Options := Options or For_Parallel;
      Get_Blank (Context, Code);
      Get ("(", False);
      if Got_It then
         Options := Options or For_Chunk;
         declare
            Left_At : constant Location_Type := Link (Code);
         begin 
            Get_Blank (Context, Code);
            Lexers.Parse (Context, Code, Chunk);
            Get_Blank (Context, Code);
            Get (")", False);
            if not Got_It then
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  (  "The right bracket closing the left bracket at "
                  &  Image (Left_At)
                  &  " expected at "
                  & Image (Link (Code))
               )  );
            end if;
            Get_Blank (Context, Code);
         end;
      end if;
      Get ("with");
      if Got_It then
         loop
            Count := Count + 1;
            Get_Blank (Context, Code);
            Lexers.Parse (Context, Code, Argument);
            Push (Context, Argument);
            Get_Blank (Context, Code);
            Get ("=>");
            if not Got_It then
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "'=>' is expected at " & Image (Link (Code))
               );
            end if;
            Get_Blank (Context, Code);
            Lexers.Parse (Context, Code, Argument);
            Push (Context, Argument);
            Get_Blank (Context, Code);
            Get (",");
            exit when not Got_It;
         end loop;
      end if;
      Get ("for");
      if not Got_It then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "'for' is expected at " & Image (Link (Code))
         );
      end if;
   end if;
   Get_Blank (Context, Code);
   Get ("all");
   if Got_It then
      For_Type := For_All;
   else
      Get ("some");
      if Got_It then
         For_Type := For_Some;
      end if;
   end if;
   Get_Blank (Context, Code);
   declare
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Index   : Integer;
      Last    : Integer;
      Symbol  : UTF8_Code_Point;
   begin
      Get_Line (Code, Line, Pointer, Last);
      Index := Pointer;
      begin
         Get (Line (Pointer..Last), Index, Symbol);
      exception
         when Data_Error =>
            Set_Pointer (Code, Pointer);
            Set_Pointer (Code, Pointer);
            Raise_Exception
            (  Syntax_Error'Identity,
               Encoding_Error & Image (Link (Code))
            );
      end;
      if not Is_Identifier_Start (Symbol) then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Identifier is expected at " & Image (Link (Code))
         );
      end if;
      Get_Identifier
      (  Code,
         Line (Pointer..Last),
         Pointer,
         Identifier
      );
   end;
   Get_Blank (Context, Code);
   Get ("in");
   if Got_It then
      Get_Blank (Context, Code);
      Get ("reverse");
      if Got_It then
         Options := Options or For_Reverse;
         Get_Blank (Context, Code);
      end if;
      Lexers.Parse (Context, Code, Iterator);
      Get_Blank (Context, Code);
      Get ("range");
      if Got_It then
         Options := Options or For_Range;
         Get_Blank (Context, Code);
         Range_Type := Iterator;
         Lexers.Parse (Context, Code, Iterator);
      end if;
   else
      Get ("of");
      if Got_It then
         Get_Blank (Context, Code);
         Get ("reverse");
         if Got_It then
            Options := Options or For_Reverse;
            Get_Blank (Context, Code);
         end if;
         Get_Blank (Context, Code);
         Lexers.Parse (Context, Code, Iterator);
      else
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "'in' or 'of' is expected at " & Image (Link (Code))
         );
      end if;
      Options := Options or For_Of;
   end if;
   Get_Blank (Context, Code);
   Get ("use");
   if Got_It then
      Options := Options or For_Key;
      Get_Blank (Context, Code);
      Lexers.Parse (Context, Code, Key);
      Get_Blank (Context, Code);
   end if;
   Get_Blank (Context, Code);
   Get ("when");
   if Got_It then
      Options := Options or For_Condition;
      Get_Blank (Context, Code);
      Lexers.Parse (Context, Code, Condition);
      Get_Blank (Context, Code);
   end if;
   Get ("=>", False);
   if not Got_It then
      Raise_Exception
      (  Parsers.Syntax_Error'Identity,
         "'=>' is expected at " & Image (Link (Code))
      );
   end if;
   Get_Blank (Context, Code);
   Lexers.Parse (Context, Code, Expression);
   declare
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
   begin
      if Left.Operation = Left_Square_Bracket then
         Options := Options or For_Container;
      end if;
      Get_Line (Code, Line, Pointer, Last);
      Argument.Value := new For_Expression (Count, Options, For_Type);
      declare
         Item : Tokens.Arguments.Frame (1..2);
         This : For_Expression'Class renames
                For_Expression'Class (Argument.Value.all);
      begin
         for Index in reverse This.Aspects'Range loop
            declare
               Pair : Alternative_Pair renames This.Aspects (Index);
            begin
               Pop (Context, Item);
               Pair.Guard := Item (1);
               Pair.Value := Item (2);
            end;
         end loop;
         This.Identifier := Identifier;
         This.Iterator   := Iterator;
         This.Expression := Expression;
         if 0 /= (Options and For_Range) then
            This.Range_Type := Range_Type;
         end if;
         if 0 /= (Options and For_Chunk) then
            This.Chunk := Chunk;
         end if;
         if 0 /= (Options and For_Condition) then
            This.Condition := Condition;
         end if;
         if 0 /= (Options and For_Key) then
            This.Key := Key;
         end if;
         Argument.Location := Where & Link (Code);
         return;
      end;
   end;
end Get_For;
