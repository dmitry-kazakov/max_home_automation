--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Generic_Unary_Folder                     Summer, 2026       --
--                                                                    --
--  Separate generic body         Last revision :  10:48 02 Jul 2026  --
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
   function Generic_Unary_Folder
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
   Left : Tokens.Argument_Token := List (List'First);
begin
   if Left.Value.all in Universal_Integer then
      declare
         First : Universal_Integer renames
                 Universal_Integer (Ref (Left.Value).all);
      begin
         if First.Value /= null then
            First.Value.all := Int_Op (First.Value.all);
            Left.Location   := Left.Location & Operation.Location;
            return Left;
         end if;
      exception
         when others => -- No folding on errors
            null;
      end;
   elsif Left.Value.all in Universal_Real then
      declare
         First : Universal_Real renames
                 Universal_Real (Ref (Left.Value).all);
      begin
         if First.Value /= null then
            First.Value.all := Real_Op (First.Value.all);
            Left.Location := Left.Location & Operation.Location;
            return Left;
         end if;
      exception
         when others => -- No folding on errors
            null;
      end;
   end if;
   declare
      type Arena_Ptr is access Expression;
      for Arena_Ptr'Storage_Pool use Context.Pool.all;
      Result : constant Arena_Ptr :=
                    new Expression (1, Operation.Operation);
      This   : Expression renames Result.all;
   begin
      This.Location := Operation.Location;
      Store (Context.all, Left);
      This.Operands (1) := Left;
      return
      (  This'Unchecked_Access,
         Left.Location & Operation.Location
      );
   end;
end Generic_Unary_Folder;
