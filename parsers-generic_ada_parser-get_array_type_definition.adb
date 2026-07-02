--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                Luebeck            --
--        Get_Array_Type_Definition                Spring, 2026       --
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
   procedure Get_Array_Type_Definition
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Array_Type_Definition_Ptr
             )  is
   type Array_Type_Definition_Ref is access Array_Type_Definition;
   for Array_Type_Definition_Ref'Storage_Pool use Context.Pool.all;

   function Get (Text : String; Delimited : Boolean := True)
      return Boolean is
      pragma Inline (Get);
      Got_It : Boolean;
   begin
      Get_Blank (Context, Code);
      Get_Delimited (Code, Text, Delimited, Got_It);
      return Got_It;
   end Get;

   Dimension : Positive;
   Not_Null  : Boolean;
begin
   Get_Ranges_List (Context, Code, False, "(", ",", ")", Dimension);
   if not Get ("of") then
      Raise_Exception
      (  Parsers.Syntax_Error'Identity,
         "'of' is expected at " & Image (Link (Code))
      );
   end if;
   if Get ("aliased") then
      Raise_Exception
      (  Parsers.Syntax_Error'Identity,
         "Array objects in declare statements cannot have " &
         "aliased components as found at "                  &
         Image (Link (Code))
      );
   end if;
   Get_Not_Null (Context, Code, Not_Null);
   if Get ("access") then
      Raise_Exception
      (  Parsers.Syntax_Error'Identity,
         "Array objects in declare statements cannot have " &
         "accesss components as found at "                  &
         Image (Link (Code))
      );
   end if;
   declare
      Result : constant Array_Type_Definition_Ref :=
                    new Array_Type_Definition
                        (  Dimension         => Dimension,
                           Aliased_Component => False
                        );
      This   : Array_Type_Definition renames Result.all;
   begin
      Get_Subtype_Indication
      (  Context   => Context,
         Code      => Code,
         Composite => True,
         Not_Null  => Not_Null,
         Argument  => This.Component
      );
      Pop (Context, This.Indices);
      Argument := This'Unchecked_Access;
   end;
end Get_Array_Type_Definition;
