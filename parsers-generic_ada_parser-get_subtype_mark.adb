--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                Luebeck            --
--        Get_Subtype_Mark                         Spring, 2026       --
--  Separate body implementation                                                    --
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
   procedure Get_Subtype_Mark
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                No_Range : Boolean;
                Not_Null : Boolean;
                Mark     : out Subtype_Mark
             )  is
   Got_It : Boolean;
   Name   : Tokens.Argument_Token;
begin
   Get_Expanded_Name (Context, Code, True, Name);
   Get_Delimited (Code, "'", False, Got_It);
   if not Got_It then
      Mark := (  Attribute => No_Attribute,
                 Name      => Name
              );
      return;
   end if;
   Get_Blank (Context, Code);
   Get_Delimited (Code, "base", True, Got_It);
   if Got_It then
      if Not_Null then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Null exclusion does not apply to 'base' " &
            "attribute at "                            &
            Image (Link (Code))
         );
      end if;
      loop
         Get_Blank (Context, Code);
         Get_Delimited (Code, "'", False, Got_It);
         exit when not Got_It;
         Get_Blank (Context, Code);
         Get_Delimited (Code, "base", True, Got_It);
         if not Got_It then
            if No_Range then
               Get_Delimited (Code, "range", True, Got_It);
               if Got_It then
                  Mark := (  Attribute => Base_Range_Attribute,
                             Name      => Name
                          );
                  return;
               end if;
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "Attribute name 'base' or 'range' is expected at " &
                  Image (Link (Code))
               );
            else
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "Attribute name 'base' is expected at " &
                  Image (Link (Code))
               );
            end if;
         end if;
      end loop;
      Mark := (  Attribute => Base_Attribute,
                 Name      => Name
              );
      return;
   end if;
   Get_Delimited (Code, "class", True, Got_It);
   if Got_It then
      if Not_Null then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Null exclusion does not apply to 'class' " &
            "attribute at "                             &
            Image (Link (Code))
         );
      end if;
      Mark := (  Attribute => Class_Attribute,
                 Name      => Name
              );
      return;
   end if;
   Get_Delimited (Code, "range", True, Got_It);
   if Got_It then
      if Not_Null then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Null exclusion does not apply to 'range' " &
            "attribute at "                             &
            Image (Link (Code))
         );
      end if;
      Get_Blank (Context, Code);
      Get_Delimited (Code, "(", False, Got_It);
      if Got_It then
         declare
            Open_At   : constant Location_Type := Link (Code);
            Dimension : Tokens.Argument_Token;
         begin
            Lexers.Parse (Context, Code, Dimension);
            Get_Blank (Context, Code);
            Get_Delimited (Code, ")", False, Got_It);
            if not Got_It then
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "')' corresponding to '(' at " &
                  Image (Open_At)                &
                  "is expected at "              &
                  Image (Link (Code))
               );
            end if;
            Mark := (  Attribute => Dimension_Range_Attribute,
                       Name      => Name,
                       Dimension => Dimension
                    );
            return;
         end;
      else
         Mark := (  Attribute => Range_Attribute,
                    Name      => Name
                 );
         return;
      end if;
   end if;
   if No_Range then
      Raise_Exception
      (  Parsers.Syntax_Error'Identity,
         "Attribute name 'class' or 'base' is expected at " &
         Image (Link (Code))
      );
   else
      Raise_Exception
      (  Parsers.Syntax_Error'Identity,
         "Attribute name 'class', 'base' or 'range' is expected at " &
         Image (Link (Code))
      );
   end if;
end Get_Subtype_Mark;
