--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Generic_Boolean_Folder                   Summer, 2026       --
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
   function Generic_Boolean_Folder
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
   First  : Tokens.Argument_Token;
   Count  : Natural := 0;
   Folded : Boolean := False;
   Value  : Boolean := False;
begin
   for Index in List'Range loop
      if List (Index).Value.all in Universal_Boolean then
         declare
            This : Universal_Boolean renames
                   Universal_Boolean (List (Index).Value.all);
         begin
            if Folded then
               Value := Op (Value, This.Value);
            else
               Folded := True;
               First  := List (Index);
               Value  := This.Value;
            end if;
         end;
      else
         Count := Count + 1;
         Push (Context.all, List (Index));
      end if;
   end loop;
   if Count = 0 then -- All operands folded
      Universal_Boolean (Ref (First.Value).all).Value := Value;
      First.Location := Operation.Location & Link (List);
      return First;
   end if;
   if Folded then -- There is a folded value
      if Has_Null and then Value = Null_Value then -- Drop folded
         if Count = 1 then -- Folded to one operand
            declare
               Result : Tokens.Arguments.Frame (1..1);
            begin
               Pop (Context.all, Result);
               First := Result (1);
               First.Location := Operation.Location & Link (List);
               return First;
            end;
         end if;
         Folded := False;
      else
         Universal_Boolean (Ref (First.Value).all).Value := Value;
         Count := Count + 1;
      end if;
   end if;
   declare
      type Arena_Ptr is access Expression;
      for Arena_Ptr'Storage_Pool use Context.Pool.all;
      Result : constant Arena_Ptr :=
                    new Expression (Count, Operation.Operation);
      This   : Expression renames Result.all;
   begin
      This.Location := Operation.Location;
      if Folded then
         Pop (Context.all,   This.Operands (1..Count - 1));
         Store (Context.all, This.Operands (1..Count - 1));
         This.Operands (Count) := First; -- Add folded value to the end
      else
         Pop (Context.all, This.Operands);
         Store (Context.all, This.Operands);
      end if;
      return
      (  This'Unchecked_Access,
         Operation.Location & Link (List)
      );
   end;
end Generic_Boolean_Folder;
