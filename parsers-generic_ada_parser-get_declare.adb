--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Get_Declare                              Summer, 2025       --
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
   procedure Get_Declare
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Left     : Location_Type
             )  is
   Got_It : Boolean;
   Count  : Natural := 0;
   Where  : constant Location_Type := Link (Code);
   Name   : Tokens.Argument_Token;
   Object : Tokens.Argument_Token;
   Value  : Tokens.Argument_Token;

   procedure Get (Text : String; Delimited : Boolean := True) is
   begin
      Get_Delimited (Code, Text, Delimited, Got_It);
   end;
begin
   for Index in Positive'Range loop
      Get_Blank (Context, Code);
      Get ("begin");
      exit when Got_It;
      Count := Count + 1;
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
            Name
         );
      end;
      Get_Blank (Context, Code);
      Get (":", False);
      if Got_It then
         Get_Blank (Context, Code);
         Get ("constant");
         if Got_It then
            Get_Blank (Context, Code);
            Lexers.Parse (Context, Code, Object);
            Get_Blank (Context, Code);
            Get (":=", False);
            if not Got_It then
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "':=' is expected at " & Image (Link (Code))
               );
            end if;
            Get_Blank (Context, Code);
            Lexers.Parse (Context, Code, Value);
            Argument.Value := new Declare_Object_Item (Immutable);
         else
            Lexers.Parse (Context, Code, Object);
            Get_Blank (Context, Code);
            Get (":=", False);
            if Got_It then
               Get_Blank (Context, Code);
               Lexers.Parse (Context, Code, Value);
               Argument.Value := new Declare_Object_Item (Initialized);
            else
               Argument.Value := new Declare_Object_Item (Uninitialized);
            end if;
         end if;
         declare
            This : Declare_Object_Item renames
                   Declare_Object_Item (Argument.Value.all);
         begin
            This.Name   := Name;
            This.Object := Object;
            if This.Kind_Of in Immutable..Initialized then
               This.Value := Value;
            end if;
         end;
      else
         Get ("renames");
         if not Got_It then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "Colon ':' or 'renames' is expected at "
               &  Image (Link (Code))
            )  );
         end if;
         Get_Blank (Context, Code);
         Lexers.Parse (Context, Code, Object);
         Argument.Value := new Declare_Renaming_Item;
         declare
            This : Declare_Renaming_Item renames
                   Declare_Renaming_Item (Argument.Value.all);
         begin
            This.Name   := Name;
            This.Object := Object;
         end;
         Argument.Location := Name.Location & Link (Code);
      end if;
      Push (Context, Argument);
      Get_Blank (Context, Code);
      Get (";", False);
      if not Got_It then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            (  "Semicolon ';' or 'renames' is expected at "
            &  Image (Link (Code))
         )  );
      end if;
   end loop;
   Get_Blank (Context, Code);
   Lexers.Parse (Context, Code, Value);
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
            (  "Bracket closing declare expression's left bracket at "
            &  Image (Left)
            &  " is expected at "
            &  Image (Link (Code))
         )  );
      end if;
      Argument.Value := new Declare_Expression (Count);
      declare
         This : Declare_Expression'Class renames
                Declare_Expression'Class (Argument.Value.all);
      begin
         This.Expression := Value;
         for Index in reverse This.Items'Range loop
            declare
               Token : Tokens.Arguments.Frame (1..1);
               Item  : Declare_Token renames This.Items (Index);
            begin
               Pop (Context, Token);
               declare
                  Value : Tokens.Argument_Token renames Token (1);
               begin
                  Item.Location := Value.Location;
                  Item.Value :=
                     Declare_Item'Class (Value.Value.all)'Access;
               end;
            end;
         end loop;
      end;
      Argument.Location := Where & Link (Code);
   end;
end Get_Declare;
