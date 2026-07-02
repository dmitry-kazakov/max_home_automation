--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Fold_Or_Else                             Summer, 2026       --
--                                                                    --
--  Separate body                 Last revision :  10:48 02 Jul 2026  --
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
   function Fold_Or_Else
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
   Count : Natural := 0;
   Left  : Tokens.Argument_Token := List (List'First);

   procedure Fold (Right : Tokens.Argument_Token) is
   begin
      if Left.Value.all in Universal_Boolean then
         declare
            First : Universal_Boolean renames
                    Universal_Boolean (Ref (Left.Value).all);
         begin
            if First.Value then -- True or else Right = True
               Free (Right);
            else                -- False or else Right = Right
               Left.Value := Right.Value;
            end if;
            Left.Location := Left.Location & Right.Location;
            return;
         end;
      end if;
      Push (Context.all, Left);
      Count  := Count + 1;
      Left   := Right;
   end Fold;
begin
   for Index in List'First + 1..List'Last loop
      Fold (List (Index));
   end loop;
   if Count = 0 then -- All operands folded
      return Left;
   end if;
   declare
      type Arena_Ptr is access Expression;
      for Arena_Ptr'Storage_Pool use Context.Pool.all;
      Result : constant Arena_Ptr :=
                    new Expression (Count + 1, Operation.Operation);
      This   : Expression renames Result.all;
   begin
      This.Location := Operation.Location;
      Pop (Context.all, This.Operands (1..Count));
      This.Operands (Count + 1) := Left;
      Store (Context.all, This.Operands);
      return
      (  This'Unchecked_Access,
         Operation.Location & Link (List)
      );
   end;
end Fold_Or_Else;
