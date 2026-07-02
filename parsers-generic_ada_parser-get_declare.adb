--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Get_Declare                              Summer, 2025       --
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
   procedure Get_Declare
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Left     : Location_Type
             )  is
   Got_It : Boolean;
   Count  : Natural := 0;
   Where  : constant Location_Type := Link (Code);
   Length : Positive;
   Object : Tokens.Argument_Token;

   procedure Get (Text : String; Delimited : Boolean := True) is
   begin
      Get_Delimited (Code, Text, Delimited, Got_It);
   end;

   function Get_Aspects_Count return Natural is
      Count : Natural := 0;
   begin
      Get ("with");
      if Got_It then
         Get_Aspect (Context, Code, Count);
      end if;
      return Count;
   end Get_Aspects_Count;
begin
   for Index in Positive'Range loop
      Get_Blank (Context, Code);
      Get ("begin");
      exit when Got_It;
      Count := Count + 1;
      Get_Names_List (Context, Code, Length);
      Get_Blank (Context, Code);
      Get (":", False);
      if Got_It then
         Get_Blank (Context, Code);
         Get ("constant");
         if Got_It then
            Get_Blank (Context, Code);
            Get ("access");
            if Got_It then
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "Anonymous access type is not allowed in " &
                  "a declare expression as found at "        &
                  Image (Link (Code))
               );
            end if;
            Get ("array");
            declare
               type Arena_Ptr is access Declare_Object_Item;
               for Arena_Ptr'Storage_Pool use Context.Pool.all;
               Result : Arena_Ptr;
            begin
               if Got_It then
                  declare
                     Definition : Array_Type_Definition_Ptr;
                  begin
                     Get_Blank (Context, Code);
                     Get_Array_Type_Definition
                     (  Context  => Context,
                        Code     => Code,
                        Argument => Definition
                     );
                     Result :=
                        new Declare_Object_Item
                            (  Names_Count   => Length,
                               Array_Object  => True,
                               Aspects_Count => Get_Aspects_Count
                            );
                     Result.Definition := Definition;
                  end;
               else
                  Get_Blank (Context, Code);
                  Lexers.Parse (Context, Code, Object);
                  Result :=
                     new Declare_Object_Item
                         (  Names_Count   => Length,
                            Array_Object  => False,
                            Aspects_Count => Get_Aspects_Count
                         );
                  Result.Object := Object;
               end if;
               Pop (Context, Result.Aspects);
               Pop (Context, Result.Names);
               Get_Blank (Context, Code);
               Get (":=", False);
               if not Got_It then
                  Raise_Exception
                  (  Parsers.Syntax_Error'Identity,
                     "':=' is expected at " & Image (Link (Code))
                  );
               end if;
               Get_Blank (Context, Code);
               Lexers.Parse (Context, Code, Result.Value);
               Argument.Value := Result.all'Unchecked_Access;
               Argument.Location :=
                  Result.Names (1).Location & Link (Code);
            end;
         else
            declare
               Not_Null : Boolean;
               Mark     : Subtype_Mark;
            begin
               Get_Not_Null (Context, Code, Not_Null);
               Get_Subtype_Mark
               (  Context  => Context,
                  Code     => Code,
                  No_Range => True,
                  Not_Null => Not_Null,
                  Mark     => Mark
               );
               Get ("renames");
               if not Got_It then
                  Raise_Exception
                  (  Parsers.Syntax_Error'Identity,
                     "Object in declare statement must be constant " &
                     "at "                                           &
                     Image (Link (Code))
                  );
               end if;
               if Length > 1 then
                  Raise_Exception
                  (  Parsers.Syntax_Error'Identity,
                     "Renaming cannot involve several names " &
                     "as found at "                           &
                     Image (Link (Code))
                  );
               end if;
               Get_Blank (Context, Code);
               Lexers.Parse (Context, Code, Object);             
               declare
                  type Arena_Ptr is access Declare_Renaming_Item;
                  for Arena_Ptr'Storage_Pool use Context.Pool.all;
                  Result : constant Arena_Ptr :=
                                new Declare_Renaming_Item
                                    (  True,
                                       Get_Aspects_Count
                                    );
                  This   : Declare_Renaming_Item renames Result.all;
                  Token  : Tokens.Arguments.Frame (1..1);
               begin
                  Argument.Value := This'Unchecked_Access;
                  This.Mark      := Mark;
                  This.Object    := Object;
                  Pop (Context, This.Aspects);
                  Pop (Context, Token);
                  This.Name  := Token (1);
                  Argument.Location := This.Name.Location & Link (Code);
               end;
            end;
         end if;
      else
         Get ("renames");
         if not Got_It then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "Colon ':' or 'renames' is expected at "
               &  Image (Link (Code))
            )  );
         end if;
         if Length > 1 then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               "Renaming cannot involve several names as found at " &
               Image (Link (Code))
            );
         end if;
         Get_Blank (Context, Code);
         Lexers.Parse (Context, Code, Object);
         Get_Blank (Context, Code);
         declare
            type Arena_Ptr is access Declare_Renaming_Item;
            for Arena_Ptr'Storage_Pool use Context.Pool.all;
            Result : constant Arena_Ptr :=
                          new Declare_Renaming_Item
                              (  False,
                                 Get_Aspects_Count
                              );
            This  : Declare_Renaming_Item renames Result.all;
            Token : Tokens.Arguments.Frame (1..1);
         begin
            Argument.Value := This'Unchecked_Access;
            This.Object    := Object;
            Pop (Context, This.Aspects);
            Pop (Context, Token);
            This.Name := Token (1);
            Argument.Location := This.Name.Location & Link (Code);
         end;
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
   declare
      type Arena_Ptr is access Declare_Expression;
      for Arena_Ptr'Storage_Pool use Context.Pool.all;
      Result : constant Arena_Ptr := new Declare_Expression (Count);
      This   : Declare_Expression renames Result.all;
   begin
      Argument.Value := This'Unchecked_Access;
      Get_Blank (Context, Code);
      Lexers.Parse (Context, Code, This.Expression);
      Get_Blank (Context, Code);
      Get (")", False);
      if not Got_It then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Parenthesis closing the declare expression's left " &
            "parenthesis at "                                    &
            Image (Left)                                         &
            " is expected at "                                   &
            Image (Link (Code))
         );
      end if;
      Reset_Pointer (Code);
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
                  Abstract_Declare_Item'Class (Value.Value.all)'Access;
            end;
         end;
      end loop;
   end;
   Argument.Location := Where & Link (Code);
end Get_Declare;
