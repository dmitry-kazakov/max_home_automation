--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Fold_Inverse                             Summer, 2026       --
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
   function Fold_Inverse
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
   Left : constant Tokens.Argument_Token := List (List'First);

   function Fold return Boolean is
   begin
      if Left.Value.all in Universal_Integer then
         declare
            First : Universal_Integer renames
                    Universal_Integer (Ref (Left.Value).all);
         begin
            if First.Value /= null then
               First.Negative := not First.Negative;
               return True;
            end if;
         end;
      elsif Left.Value.all in Universal_Real then
         declare
            First : Universal_Real renames
                    Universal_Real (Ref (Left.Value).all);
         begin
            if First.Value /= null then
               Invert (First.Value.all);
            end if;
         exception
            when others => -- No folding on errors
               null;
         end;
      end if;
      return False;
   end Fold;
begin
   if Fold then
      return Left;
   end if;
   declare
      type Arena_Ptr is access Expression;
      for Arena_Ptr'Storage_Pool use Context.Pool.all;
      Result : constant Arena_Ptr :=
                    new Expression (List'Length, Operation.Operation);
      This   : Expression renames Result.all;
   begin
      This.Location := Operation.Location;
      for Argument in List'Range loop
         This.Operands (Integer (Argument)) := List (Argument);
      end loop;
      Store (Context.all, This.Operands);
      return
      (  This'Unchecked_Access,
         Operation.Location & Link (List)
      );
   end;
end Fold_Inverse;
