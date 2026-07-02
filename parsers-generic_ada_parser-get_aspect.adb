--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Get_Aspect                               Spring, 2026       --
--  Separate body implementation                                      --
--                                Last revision :  15:25 02 Jul 2026  --
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
   procedure Get_Aspect
             (  Context : in out Ada_Expression;
                Code    : in out Lexers.Lexer_Source_Type;
                Count   : out Positive
             )  is
   type Item_Ptr is access Aspect_Specification_Item;
   for Item_Ptr'Storage_Pool use Context.Pool.all;
   function From_Item is
      new Ada.Unchecked_Conversion (Item_Ptr, Node_Ptr);
   function To_Item is
      new Ada.Unchecked_Conversion (Node_Ptr, Item_Ptr);

   type Element_Ptr is access Global_Aspect_Element;
   for Element_Ptr'Storage_Pool use Context.Pool.all;
   function From_Element is
      new Ada.Unchecked_Conversion (Element_Ptr, Node_Ptr);
   function To_Element is
      new Ada.Unchecked_Conversion (Node_Ptr, Element_Ptr);

   procedure Pop (List : out Global_Aspect_Element_Ptr_Array) is
      Tokens : Frame (1..1);
   begin
      for Item in reverse List'Range loop
         Pop (Context, Tokens);
         List (Item) :=
            To_Element (Tokens (1).Value).all'Unchecked_Access;
      end loop;
   end Pop;

   Got_It   : Boolean;
   Mark     : Tokens.Argument_Token;
   Item     : Tokens.Argument_Token;
   Basic    : Basic_Global_Mode;
   Kind_Of  : Global_Designator_Mode;
   Extended : Boolean;
   Global   : Boolean := False;

   function Get_Basic_Mode return Boolean is
   begin
       Get_Delimited (Code, "in", True, Got_It);
       if Got_It then
          Get_Blank (Context, Code);
          Get_Delimited (Code, "out", True, Got_It);
          if Got_It then
             Basic := Inout_Global_Mode;
          else
             Basic := In_Global_Mode;
          end if;
       else
          Get_Delimited (Code, "out", True, Got_It);
          if Got_It then
             Basic := Out_Global_Mode;
          else
             return False;
          end if;
       end if;
       return True;
   end Get_Basic_Mode;

   procedure Get_Designator_Mode is
   begin
       Get_Delimited (Code, "all", True, Got_It);
       if Got_It then
          Kind_Of := All_Designator_Mode;
       else
          Get_Delimited (Code, "synchronized", True, Got_It);
          if Got_It then
             Kind_Of := Synchronized_Designator_Mode;
          else
             Kind_Of := Global_Name_Designator_Mode;
          end if;
       end if;
   end Get_Designator_Mode;

begin
   Count := 1;
   loop
      Lexers.Parse (Context, Code, Mark);
      declare
         This : Node'Class renames Mark.Value.all;
      begin
         if This not in Identifier                     and then
            This not in Expression                     and then
            Expression (This).Operation /= Attribute   and then
            not Is_Class (Expression (This).Operands (2))
         then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               "Aspect mark is not an identifier or class-wide at " &
               Image (Mark.Location)
            );
         end if;
      end;
      Get_Blank (Context, Code);
      Get_Delimited (Code, "=>", False, Got_It);
      Item.Location := Link (Code);
      if not Got_It then
         Item.Value :=
            From_Item
            (  new Aspect_Specification_Item (No_Designator, 1)
            );
      else
         Get_Blank (Context, Code);
         Get_Delimited (Code, "(", False, Got_It);
         if Got_It then
            declare
               Open_At     : constant Location_Type := Link (Code);
               Name        : Tokens.Argument_Token;
               Names_Count : Positive := 1;
            begin
               for Elements_Count in Positive'Range loop
                  Get_Blank (Context, Code);
                  Get_Delimited (Code, "overriding", True, Extended);
                  Get_Blank (Context, Code);
                  if Get_Basic_Mode then
                     Global := True;
                  elsif Extended or else Elements_Count > 1 then
                     Raise_Exception
                     (  Parsers.Syntax_Error'Identity,
                        "Aspect basic global mode 'in', 'in out' " &
                        "or 'out' is expected at "                 &
                        Image (Link (Code))
                     );
                  else
                     Global := Extended;
                  end if;
                  if not Global then
                     Lexers.Parse
                     (  Context => Context,
                        Code    => Code,
                        Left    => (Left_Bracket, Open_At),
                        Result  => Name
                     );
                     Item.Value :=
                        From_Item
                        (  new Aspect_Specification_Item
                               (  Value_Designator,
                                  1
                        )      );
                     declare
                        This : Aspect_Specification_Item renames
                               To_Item (Item.Value).all;
                     begin
                        This.Value := Name;
                     end;
                     exit;
                  end if;
                  Get_Blank (Context, Code);
                  Get_Designator_Mode;
                  if Kind_Of = Global_Name_Designator_Mode then
                     loop
                        Lexers.Parse (Context, Code, Name);
                        Push (Context, Name);
                        Get_Blank (Context, Code);
                        Get_Delimited (Code, ",", False, Got_It);
                        exit when not Got_It;
                        Names_Count := Names_Count + 1;
                     end loop;
                  end if;
                  Item.Value :=
                     From_Element
                     (  new Global_Aspect_Element
                            (  Kind_Of     => Kind_Of,
                               Mode        => Basic,
                               Extended    => Extended,
                               Names_Count => Names_Count
                     )      );
                  if Kind_Of = Synchronized_Designator_Mode then
                     declare
                        This : Global_Aspect_Element renames
                               To_Element (Item.Value).all;
                     begin
                        Pop (Context, This.List);
                     end;
                  end if;
                  Push (Context, Item);
                  Get_Blank (Context, Code);
                  Get_Delimited (Code, ";", False, Got_It);
                  if not Got_It then
                     Get_Delimited (Code, ")", False, Got_It);
                     if not Got_It then
                        Raise_Exception
                        (  Parsers.Syntax_Error'Identity,
                           "';' or ')' is expected at " &
                           Image (Link (Code))
                        );
                     end if;
                     Item.Value :=
                        From_Item
                        (  new Aspect_Specification_Item
                           (  Global_Aspect_Elements_List_Designator,
                              Elements_Count
                        )  );
                     declare
                        This : Aspect_Specification_Item renames
                               To_Item (Item.Value).all;
                     begin
                        Pop (This.List);
                     end;
                     exit;
                  end if;
               end loop;
               Item.Location := Open_At & Link (Code);
            end;
         else
            Get_Delimited (Code, "null", True, Got_It);
            if Got_It then
               Item.Value :=
                 From_Item
                 (  new Aspect_Specification_Item
                        (  Null_Designator,
                           1
                 )      );
            else
               Get_Delimited (Code, "unspecified", True, Got_It);
               if Got_It then
                  Item.Value :=
                    From_Item
                    (  new Aspect_Specification_Item
                           (  Unspecified_Designator,
                              1
                    )      );
               else
                  Get_Delimited (Code, "overriding", True, Extended);
                  Get_Blank (Context, Code);
                  if Extended or else Get_Basic_Mode then
                     if Extended and then not Get_Basic_Mode then
                        Raise_Exception
                        (  Parsers.Syntax_Error'Identity,
                           "Aspect basic global mode 'in', 'in out' " &
                           "or 'out' is expected at "                 &
                           Image (Link (Code))
                        );
                     end if;
                     Get_Blank (Context, Code);
                     Get_Designator_Mode;
                     Item.Value :=
                        From_Item
                        (  new Aspect_Specification_Item
                               (  Global_Designator,
                                  1
                        )      );
                     declare
                        This : Aspect_Specification_Item renames
                               To_Item (Item.Value).all;
                        Designator : constant Element_Ptr :=
                                     new Global_Aspect_Element
                                         (  Kind_Of     => Kind_Of,
                                            Mode        => Basic,
                                            Extended    => Extended,
                                            Names_Count => 1
                                         );
                     begin
                        This.Designator :=
                           Designator.all'Unchecked_Access;
                        if Kind_Of = Global_Name_Designator_Mode then
                           Lexers.Parse
                           (  Context => Context,
                              Code    => Code,
                              Result  => Designator.List (1)
                           );
                        end if;
                     end;
                  else
                     Item.Value :=
                        From_Item
                        (  new Aspect_Specification_Item
                               (  Value_Designator,
                                  1
                        )      );
                     declare
                        This : Aspect_Specification_Item renames
                               To_Item (Item.Value).all;
                     begin
                        Lexers.Parse (Context, Code, This.Value);
                     end;
                  end if;
               end if;
            end if;
         end if;
      end if;
      declare
         This : Aspect_Specification_Item renames
                To_Item (Item.Value).all;
      begin
         This.Mark := Mark;
      end;
      Item.Location := Item.Location & Link (Code);
      Push (Context, Item);
      Get_Blank (Context, Code);
      Get_Delimited (Code, ",", False, Got_It);
      exit when not Got_It;
      Count := Count + 1;
   end loop;
end Get_Aspect;
