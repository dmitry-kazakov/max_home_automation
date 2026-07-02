--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser                  Luebeck            --
--  Implementation                                 Winter, 2004       --
--                                                                    --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Strings_Edit.Integers;    use Strings_Edit.Integers;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;
with Strings_Edit.UTF8;        use Strings_Edit.UTF8;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Maps.Constants;
with Strings_Edit.Unbounded_Integer_Edit;
with Strings_Edit.Unbounded_Rational_Edit;
with Strings_Edit.Unbounded_Unsigned_Edit;
with Strings_Edit.UTF8.Mapping;
with System;

with Strings_Edit.UTF8.Categorization;
use  Strings_Edit.UTF8.Categorization;

package body Parsers.Generic_Ada_Parser is
   use Sources;
   use Tokens.Arguments;

   Image_Length   : constant := 2048;
   Fraction       : constant := 15;
   Keywords       : Keyword_Tables.Dictionary;
   Operators      : Operator_Tables.Dictionary;
   Reserved_Words : Reserved_Words_Tables.Dictionary;

   Logical_Priority : constant := 1;

   type Node_Ref is access all Node'Class;
   function Ref is new Ada.Unchecked_Conversion (Node_Ptr, Node_Ref);

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Unbounded_Integer,
             Unbounded_Integer_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Unbounded_Rational,
             Unbounded_Rational_Ptr
          );
   function Fold_And_Then
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is separate;
   function Fold_Inverse
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is separate;
   function Fold_Not
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is separate;
   function Fold_Or_Else
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is separate;
   function Fold_Pow
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is separate;
   function Generic_Boolean_Folder
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is separate;
   function Generic_Dyadic_Folder
            (  Context     : access Ada_Expression'Class;
               Operation   : Tokens.Operation_Token;
               List        : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is separate;
   function Generic_Integer_Folder
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is separate;
   function Generic_Logical_Folder
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is separate;
   function Generic_Unary_Folder
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is separate;

   package body Generic_Stack_Access is
      function From_Pointer is
         new Ada.Unchecked_Conversion (Pointer_Type, Node_Ptr);
      function To_Pointer is
         new Ada.Unchecked_Conversion (Node_Ptr, Pointer_Type);

      procedure Pop
                (  Context  : in out Ada_Expression'Class;
                   Pointer  : out Pointer_Type;
                   Location : out Location_Type
                )  is
         Token : Frame (1..1);
      begin
         Pop (Context, Token);
         Pointer  := To_Pointer (Token (1).Value);
         Location := Token (1).Location;
      end Pop;

      procedure Push
                (  Context  : in out Ada_Expression'Class;
                   Pointer  : Pointer_Type;
                   Location : Location_Type
                )  is
      begin
         Push
         (  Context,
            Tokens.Argument_Token'
            (  Value    => From_Pointer (Pointer),
               Location => Location
         )  );
      end Push;
   end Generic_Stack_Access;

   package body Generic_Stack_Object is
      Token_Size : constant Argument_No :=
                            Frame'Component_Size / System.Storage_Unit;
      Item_Size  : constant Argument_No :=
                   (  (  Object_Type'Max_Size_In_Storage_Elements
                      +  Token_Size
                      -  1
                      )
                      / Token_Size
                   );
      subtype Chunk_Frame is Tokens.Arguments.Frame (1..Item_Size);
      pragma Assert
             (  Item_Size
             <= Chunk_Frame'Max_Size_In_Storage_Elements
             );

      function From_Object is
         new Ada.Unchecked_Conversion (Object_Type, Chunk_Frame);

      function To_Object is
         new Ada.Unchecked_Conversion (Chunk_Frame, Object_Type);

      procedure Pop
                (  Context : in out Ada_Expression'Class;
                   Object  : out Object_Type
                )  is
         Chunk : Chunk_Frame;
      begin
         Pop (Context, Chunk);
         Object := To_Object (Chunk);
      end Pop;

      procedure Push
                (  Context : in out Ada_Expression'Class;
                   Object  : Object_Type
                )  is
         Chunk : Chunk_Frame;
      begin
         Chunk := From_Object (Object);
         for Index in Chunk'Range loop
            Push (Context, Chunk (Index));
         end loop;
      end Push;

   end Generic_Stack_Object;

   package Stubtype_Indication_Stack is
      new Generic_Stack_Object (Subtype_Indication);

   package Case_Alternatives_Stack is
      new Generic_Stack_Object (Case_Alternative);

   function Compare (Left, Right : String) return Precedence is
      use Strings_Edit.UTF8.Mapping;
      I    : Integer := Left'First;
      J    : Integer := Right'First;
      This : UTF8_Code_Point;
      That : UTF8_Code_Point;
   begin
      loop
         Get (Left, I, This);
         This := To_Lowercase (This);
         Get (Right, J, That);
         That := To_Lowercase (That);
         if This = That then
            exit when I > Left'Last;
            if J > Right'Last then
               return Greater;
            end if;
         elsif This < That then
            return Less;
         else
            return Greater;
         end if;
      end loop;
      if J <= Right'Last then
         return Less;
      else
         return Equal;
      end if;
   end Compare;

   function Compare (Left, Right : Node'Class) return Precedence is
   begin
      if Left in Identifier then
         if Right in Identifier then
            return Compare
                   (  Identifier (Left).Value,
                      Identifier (Right).Value
                   );
         else
            return Less;
         end if;
      elsif Left in Text_Literal'Class then
         if Right in Text_Literal'Class then
            return Compare
                   (  Text_Literal'Class (Left).Value,
                      Text_Literal'Class (Right).Value
                   );
         else
            return Greater;
         end if;
      else
         Raise_Exception
         (  Program_Error'Identity,
            "Invalid name comparison"
         );
      end if;
   end Compare;

   function Compare (Left, Right : Argument_List) return Precedence is
      I : Integer := Left'First;
      J : Integer := Right'First;
   begin
      loop
         case Compare (Left (I).Value.all, Right (J).Value.all) is
            when Less =>
               return Less;
            when Greater =>
               return Greater;
            when Equal =>
               if I = Left'Last then
                  if J <= Right'Last then
                     return Less;
                  else
                     return Equal;
                  end if;
               elsif J = Right'Last then
                  return Greater;
               end if;
         end case;
         I := I + 1;
         J := J + 1;
      end loop;
   end Compare;

   procedure Free (Item : Tokens.Argument_Token) is
      Tail : Node'Class renames Item.Value.all;
   begin
      if Tail in Universal_Integer then
         declare
            Second : Universal_Integer renames
                     Universal_Integer (Ref (Item.Value).all);
         begin
            Free (Second.Value);
         end;
      elsif Tail in Universal_Real then
         declare
            Second : Universal_Real renames
                     Universal_Real (Ref (Item.Value).all);
         begin
            Free (Second.Value);
         end;
      end if;
   end Free;

   function Get_Constant_Folding (Context : Ada_Expression)
      return Boolean is
   begin
      return Context.Fold;
   end Get_Constant_Folding;

   procedure Get_Delimited
             (  Code      : in out Lexers.Lexer_Source_Type;
                Text      : String;
                Delimited : Boolean;
                Got_It    : out Boolean
             )  is
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
   begin
      Get_Line (Code, Line, Pointer, Last);
      if Last - Pointer < Text'Length - 1 then
         Got_It := False;
         return;
      end if;
      for Index in Text'Range loop
         if To_Lower (Line (Pointer)) /= Text (Index) then
             Got_It := False;
             return;
         end if;
         Pointer := Pointer + 1;
      end loop;
      if Pointer <= Last and then Delimited then
         declare
            Symbol : UTF8_Code_Point;
            Index  : Integer := Pointer;
         begin
            Get (Line (Pointer..Last), Index, Symbol);
            if Is_Alphanumeric (Symbol) then
               Got_It := False;
               return;
            end if;
         exception
            when Data_Error | End_Error =>
               Set_Pointer (Code, Pointer);
               Set_Pointer (Code, Pointer);
               Raise_Exception
               (  Syntax_Error'Identity,
                  Encoding_Error & Image (Link (Code))
               );
         end;
      end if;
      Got_It := True;
      Set_Pointer (Code, Pointer);
   end Get_Delimited;

   function Get_Operator
            (  Item : Tokens.Argument_Token
            )  return Operations is
      use Operator_Tables;
      Offset : Natural;
   begin
      if Item.Value.all in String_Literal then
         Offset :=
            Locate (Operators, String_Literal (Item.Value.all).Value);
      elsif Item.Value.all in Expression then
         declare
            This : Expression renames Expression (Item.Value.all);
         begin
            if This.Operation = Component then
               declare
                  Last : Node'Class renames
                         This.Operands (This.Operands'Last).Value.all;
               begin
                  if Last in String_Literal then
                     Offset :=
                        Locate (Operators, String_Literal (Last).Value);
                  else
                     return Reserved;
                  end if;
               end;
            end if;
         end;
      else
         return Reserved;
      end if;
      return GetTag (Operators, Offset);
   end Get_Operator;

   function "and" (Left, Right : Operations) return Boolean is
   begin
      case Right is
         when Logical_And =>
            case Left is
               when Logical_Or | Logical_Xor | And_Then | Or_Else =>
                  return False;
               when others =>
                  return True;
            end case;
         when Logical_Or =>
            case Left is
               when Logical_And | Logical_Xor | And_Then | Or_Else =>
                  return False;
               when others =>
                  return True;
            end case;
         when Logical_Xor =>
            case Left is
               when Logical_And | Logical_Or | And_Then | Or_Else =>
                  return False;
               when others =>
                  return True;
            end case;
         when And_Then =>
            case Left is
               when Logical_And | Logical_Or | Logical_Xor | Or_Else =>
                  return False;
               when others =>
                  return True;
            end case;
         when Or_Else =>
            case Left is
               when Logical_And | Logical_Or | Logical_Xor | And_Then =>
                  return False;
               when others =>
                  return True;
            end case;
         when Right_Bracket =>
            case Left is
               when Left_Bracket | Left_Index =>
                  return True;
               when others =>
                  return True;
            end case;
         when Relational =>
            case Left is
               when Relational =>
                  return False;
               when others =>
                  return True;
            end case;
         when Right_Square_Bracket =>
            return Left = Left_Square_Bracket;
         when Unary =>
            case Left is
               when Additive | Unary | Multiplying | Highest =>
                  return False;
               when Attribute =>
                  return False;
               when others =>
                  return True;
            end case;
         when Highest =>
            case Left is
               when Highest =>
                  return False;
               when others =>
                  return True;
            end case;
         when Extend =>
            return Left /= Left_Index;
         when others =>
            return True;
      end case;
   end "and";

   function Is_Commutative (Left, Right : Operations) return Boolean is
   begin
      case Left is
         when Logical_And =>
            return Right = Logical_And;
         when Logical_Or =>
            return Right = Logical_Or;
         when Logical_Xor =>
            return Right = Logical_Xor;
         when Add | Sub =>
            case Right is
              when Add | Sub =>
                 return True;
              when others =>
                 return False;
            end case;
         when Mul | Div =>
            case Right is
              when Mul | Div =>
                 return True;
              when others =>
                 return False;
            end case;
         when Alternative | Associate | Component | Concatenate =>
            return Left = Right;
         when And_Then =>
            return Right = And_Then;
         when Or_Else =>
            return Right = Or_Else;
         when others =>
            return False;
      end case;
   end Is_Commutative;

   function Is_Inverse (Operation : Operations) return Boolean is
   begin
      case Operation is
         when Sub | Div =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Inverse;

   function Is_Range (Item : Tokens.Argument_Token) return Boolean is
      This : Node'Class renames Item.Value.all;
   begin
      return This in Identifier and then
             Equal = Compare (Identifier (This).Value, "range");
   end Is_Range;

   function Group_Inverse (Operation : Operations) return Operations is
   begin
      case Operation is
         when Add | Sub =>
            return Add_Inv;
         when Mul | Div =>
            return Mul_Inv;
         when others =>
            raise Program_Error;
      end case;
   end Group_Inverse;

   procedure Check_Spelling (Name : String) is
   begin
      null;
   end Check_Spelling;

   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean is
   begin
      if not Is_Alphanumeric (Source (Pointer - 1)) then
         return True;
      end if;
      declare
         Symbol : UTF8_Code_Point;
         Index  : Integer := Pointer;
      begin
         Get (Source, Index, Symbol);
         case Category (Symbol) is
            when Mn | Mc | Nd | Cf | Letter | Nl | Pc =>
               return False;
            when others =>
               return True;
         end case;
      end;
   exception
      when Data_Error =>
         return True;
   end Check_Matched;

   function Inequal (Left, Right : Unbounded_Integer) return Boolean is
   begin
      return Left /= Right;
   end Inequal;

   function Inequal (Left, Right : Unbounded_Rational) return Boolean is
   begin
      return Left /= Right;
   end Inequal;

   function Fold_Abs is new Generic_Unary_Folder
                            (  Unbounded_Integers."abs",
                               Unbounded_Rationals."abs"
                            );
   function Fold_Add is new Generic_Dyadic_Folder
                            (  Int_Op      => Unbounded_Integers."+",
                               Int_Rev_Op  => Unbounded_Integers."+",
                               Real_Op     => Unbounded_Rationals."+",
                               Real_Rev_Op => Unbounded_Rationals."+"
                            );
   function Fold_And is new Generic_Boolean_Folder ("and", True, True);
   function Fold_Div is new Generic_Dyadic_Folder
                            (  Int_Op      => Unbounded_Integers."/",
                               Int_Rev_Op  => Unbounded_Integers."*",
                               Real_Op     => Unbounded_Rationals."/",
                               Real_Rev_Op => Unbounded_Rationals."*"
                            );
   function Fold_EQ is new Generic_Logical_Folder
                           (  Unbounded_Integers."=",
                              Unbounded_Rationals."="
                           );
   function Fold_GE is new Generic_Logical_Folder
                           (  Unbounded_Integers.">",
                              Unbounded_Rationals.">"
                           );
   function Fold_GT is new Generic_Logical_Folder
                           (  Unbounded_Integers.">=",
                              Unbounded_Rationals.">="
                           );
   function Fold_LE is new Generic_Logical_Folder
                           (  Unbounded_Integers."<",
                              Unbounded_Rationals."<"
                           );
   function Fold_LT is new Generic_Logical_Folder
                           (  Unbounded_Integers."<=",
                              Unbounded_Rationals."<="
                           );
   function Fold_Minus is new Generic_Unary_Folder
                              (  Unbounded_Integers."-",
                                 Unbounded_Rationals."-"
                              );
   function Fold_Mod is new Generic_Integer_Folder
                            (  Unbounded_Integers."mod"
                            );
   function Fold_Mul is new Generic_Dyadic_Folder
                            (  Int_Op      => Unbounded_Integers."*",
                               Int_Rev_Op  => Unbounded_Integers."*",
                               Real_Op     => Unbounded_Rationals."*",
                               Real_Rev_Op => Unbounded_Rationals."*"
                            );
   function Fold_NE is new Generic_Logical_Folder (Inequal, Inequal);
   function Fold_Or is new Generic_Boolean_Folder ("or", True, False);
   function Fold_Plus is new Generic_Unary_Folder
                             (  Unbounded_Integers."+",
                                Unbounded_Rationals."+"
                             );
   function Fold_Rem is new Generic_Integer_Folder
                            (  Unbounded_Integers."rem"
                            );
   function Fold_Sub is new Generic_Dyadic_Folder
                            (  Int_Op      => Unbounded_Integers."-",
                               Int_Rev_Op  => Unbounded_Integers."+",
                               Real_Op     => Unbounded_Rationals."-",
                               Real_Rev_Op => Unbounded_Rationals."+"
                            );
   function Fold_Xor is new Generic_Boolean_Folder ("xor");

   function Call
            (  Context   : access Ada_Expression;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
      type Arena_Ptr is access Expression;
      for Arena_Ptr'Storage_Pool use Context.Pool.all;
   begin
      if Context.Fold then
         case Operation.Operation is
            when Abs_Value =>
               return Fold_Abs      (Context, Operation, List);
            when Add =>
               return Fold_Add      (Context, Operation, List);
            when And_Then =>
               return Fold_And_Then (Context, Operation, List);
            when Div =>
               return Fold_Div      (Context, Operation, List);
            when EQ =>
               return Fold_EQ       (Context, Operation, List);
            when GE =>
               return Fold_GE       (Context, Operation, List);
            when GT =>
               return Fold_GT       (Context, Operation, List);
            when LE =>
               return Fold_LE       (Context, Operation, List);
            when Logical_And =>
               return Fold_And      (Context, Operation, List);
            when Logical_Not =>
               return Fold_Not      (Context, Operation, List);
            when Logical_Or =>
               return Fold_Or       (Context, Operation, List);
            when Logical_Xor =>
               return Fold_Xor      (Context, Operation, List);
            when LT =>
               return Fold_LT       (Context, Operation, List);
            when Modulus =>
               return Fold_Mod      (Context, Operation, List);
            when Minus | Add_Inv =>
               return Fold_Minus    (Context, Operation, List);
            when Mul =>
               return Fold_Mul      (Context, Operation, List);
            when Mul_Inv =>
               return Fold_Inverse  (Context, Operation, List);
            when NE =>
               return Fold_NE       (Context, Operation, List);
            when Or_Else =>
               return Fold_Or_Else  (Context, Operation, List);
            when Plus =>
               return Fold_Plus     (Context, Operation, List);
            when Pow =>
               return Fold_Pow      (Context, Operation, List);
            when Remainder =>
               return Fold_Rem      (Context, Operation, List);
            when Sub =>
               return Fold_Sub      (Context, Operation, List);
            when others =>
               null;
            end case;
      end if;
      if Operation.Operation = Allocator and then
         List'Length = 1                     then
         --
         -- Reducing new (<subpool>) ... to new (Subpool ...)
         --
         declare
            Second : constant Node_Ptr := List (List'First).Value;
         begin
            if Second.all in Expression then
               declare
                  use Descriptors;
                  Tail : Expression renames
                         Expression (Second.all);
               begin
                  if Tail.Operation = Left_Bracket and then
                     Tail.Count = 1 then
                     Lexers.Lexical_Descriptors.Operation.Push
                     (  Container => Context.all,
                        Item      =>
                           Descriptor'
                           (  Class     => Operator,
                              Operation =>
                                 (  Allocator_Subpool,
                                    Operation.Location
                                 ),
                              Count     => 1,
                              Right     => Priority_Type'Last
                     )     );
                     return Tail.Operands (Tail.Operands'First);
                  end if;
               end;
            end if;
         end;
      elsif Operation.Operation = Attribute and then
            List'Length = 2                     then
         --
         -- Reducing Value'(...) to '(Value,...)
         --
         declare
            Second : constant Node_Ptr := List (List'Last).Value;
         begin
            if Second.all in Expression then
               declare
                  Tail : Expression renames
                         Expression (Second.all);
               begin
                  if Tail.Operation = Left_Bracket then
                     declare
                        Result : constant Arena_Ptr :=
                           new Expression (Tail.Count + 1, Attribute);
                        This   : Expression renames Result.all;
                     begin
                        This.Location  := Operation.Location;
                        This.Operands (1) := List (List'First);
                        This.Operands (2..This.Count) := Tail.Operands;
                        return
                        (  This'Unchecked_Access,
                           Operation.Location & Link (List)
                        );
                     end;
                  end if;
               end;
            end if;
         end;
      end if;
      declare
         Result : constant Arena_Ptr :=
                  new Expression (List'Length, Operation.Operation);
         This   : Expression renames Result.all;
      begin
         This.Location := Operation.Location;
         for Argument in List'Range loop
            This.Operands (Integer (Argument)) := List (Argument);
         end loop;
         if Context.Fold then
            Store (Context.all, This.Operands);
         end if;
         return
         (  This'Unchecked_Access,
            Operation.Location & Link (List)
         );
      end;
   end Call;

   function Enclose
            (  Context : access Ada_Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
      procedure Check_Aggregate is
         Have_Keyed : Boolean := False;
      begin
         for Index in List'First..List'Last loop
            declare
               This : Tokens.Argument_Token renames List (Index);
            begin
               if This.Value.all in Expression and then
                  Expression (This.Value.all).Operation = Associate
               then
                  Have_Keyed := True;
               elsif Have_Keyed then
                  Raise_Exception
                  (  Parsers.Syntax_Error'Identity,
                     "A positional association appears after a " &
                     "named one at "                             &
                     Image (This.Location)
                  );
               end if;
            end;
         end loop;
      end Check_Aggregate;
   begin
      if List'Length = 1 then
         case Left.Operation is
            when Left_Bracket => -- Check if we can drop the brackets
               Check_Aggregate;
               declare
                  use Descriptors;
                  This : Node_Ptr renames List (List'First).Value;
                  Operation : constant Descriptor := Top (Context.all);
               begin
                  if Operation.Class /= Stub then
                     declare -- Left prefix operation
                        use Lexers.Lexical_Descriptors.Operation;
                        Prefix : constant Descriptor :=
                                          Get (Context.all, 1);
                     begin
                        if Prefix.Class = Operator            and then
                           Prefix.Operation.Operation = Allocator then
                           Pop (Context.all);
                           Replace
                           (  Context.all,
                              Descriptor'
                              (  Class     => Operator,
                                 Operation =>
                                    (  Allocator_Subpool,
                                       Prefix.Operation.Location
                                    ),
                                 Count     => 2,
                                 Right     => Priority_Type'Last
                           )  );
                           Push
                           (  Context.all,
                              Descriptor'(Class => Switch)
                           );
                           Push (Context.all, Operation); -- Push back
                           return
                           (  This,
                              (  Left.Location
                              &  Right.Location
                              &  Link (List)
                           )  );
                        end if;
                     end;
                  end if;
                  if This.all in Expression then
                     --  if Operation.Class = Operator and then
                     --     Operation.Operation.Operation = Attribute
                     --  then
                     --     return -- X'(...) case
                     --     (  This,
                     --        Left.Location & Right.Location & Link (List)
                     --     );
                     --  end if;
                     case Expression (This.all).Operation is
                        when Associate    | Alternative | Attribute |
                             Component    | Extend      | Ellipsis  |
                             Extend_Delta =>
                           goto Keep;  -- (X => Y) or (X with Y)
                        when others =>
                           return
                           (  This,
                              (  Left.Location
                              &  Right.Location
                              &  Link (List)
                           )  );
                     end case;
                  elsif This.all not in For_Expression then
                     return -- (X)
                     (  This,
                        Left.Location & Right.Location & Link (List)
                     );
                  end if;
               end;
            when Left_Square_Bracket =>
               if List (List'First).Value.all in
                  Null_Aggregate'Class then
                  return
                  (  List (List'First).Value,
                     Left.Location & List (List'First).Location
                  );
               end if;
               Check_Aggregate;
            when others =>
               null;
         end case;
      elsif Left.Operation = Left_Index then
         declare
            Have_Keyed : Boolean := False;
         begin
            for Index in List'First + 1..List'Last loop
               declare
                  This : Tokens.Argument_Token renames List (Index);
               begin
                  if This.Value.all in Expression and then
                     Expression (This.Value.all).Operation = Associate
                  then
                     Have_Keyed := True;
                  elsif Have_Keyed then
                     Raise_Exception
                     (  Parsers.Syntax_Error'Identity,
                        "A positional parameter appears after a " &
                        "named one at "                           &
                        Image (This.Location)
                     );
                  end if;
               end;
            end loop;
         end;
      end if;
<<Keep>>
      declare
         type Arena_Ptr is access Expression;
         for Arena_Ptr'Storage_Pool use Context.Pool.all;
         Result : constant Arena_Ptr :=
                       new Expression (List'Length, Left.Operation);
         This   : Expression renames Result.all;
      begin
         This.Location := Left.Location & Right.Location;
         for Argument in List'Range loop
            This.Operands (Integer (Argument)) := List (Argument);
         end loop;
         if Context.Fold then
            Store (Context.all, This.Operands);
         end if;
         return
         (  This'Unchecked_Access,
            Left.Location & Right.Location & Link (List)
         );
      end;
   end Enclose;

   procedure Free
             (  Context : Ada_Expression;
                Pointer : in out Node_Ptr
             )  is
      type Arena_Ptr is access Node'Class;
      for Arena_Ptr'Storage_Pool use Context.Pool.all;
      procedure Free_Arena is
         new Ada.Unchecked_Deallocation (Node'Class, Arena_Ptr);
      function To_Arena_Ptr is
         new Ada.Unchecked_Conversion (Node_Ptr, Arena_Ptr);
      Ptr : Arena_Ptr := To_Arena_Ptr (Pointer);
   begin
      Free_Arena (Ptr);
      Pointer := null;
   end Free;

   function Generic_Image
            (  Item       : Item_Type;
               Max_Length : Output_Length := Output_Length'Last
            )  return String is
      Length : Integer := Image_Length;
   begin
      loop
         declare
            Result  : String (1..Length);
            Pointer : Integer := 1;
         begin
            Put (Result, Pointer, Item);
            Pointer := Pointer - 1;
            if Pointer <= Max_Length then
               return Result (1..Pointer);
            else
               declare
                  Half : constant Integer := (Max_Length - 3) / 2;
               begin
                  return Result (1..Max_Length - Half - 3) &
                         "..."                             &
                         Result (Pointer - Half + 1..Pointer);
               end;
            end if;
         exception
            when Layout_Error =>
               Length := Length * 3 / 2;
         end;
      end loop;
   end Generic_Image;

   procedure Get_Array_Type_Definition
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Array_Type_Definition_Ptr
             )  is separate;

   function Get_Class (Item : Case_Expression) return Node_Class is
   begin
      return Case_Node;
   end Get_Class;

   function Get_Class (Item : Declare_Expression) return Node_Class is
   begin
      return Declare_Node;
   end Get_Class;

   function Get_Class (Item : Abstract_Declare_Item)
      return Node_Class is
   begin
      return Declare_Item_Node;
   end Get_Class;

   function Get_Class (Item : Expression) return Node_Class is
   begin
      return Expression_Node;
   end Get_Class;

   function Get_Class (Item : For_Expression) return Node_Class is
   begin
      return For_Node;
   end Get_Class;

   function Get_Class (Item : If_Expression) return Node_Class is
   begin
      return If_Node;
   end Get_Class;

   function Get_Class (Item : Mark) return Node_Class is
   begin
      return Term_Node;
   end Get_Class;

   function Get_Class (Item : Raise_Expression) return Node_Class is
   begin
      return Raise_Node;
   end Get_Class;

   function Get_Class (Item : Term) return Node_Class is
   begin
      return Term_Node;
   end Get_Class;

   procedure Get_Aspect
             (  Context : in out Ada_Expression;
                Code    : in out Lexers.Lexer_Source_Type;
                Count   : out Positive
             )  is separate;

   procedure Get_Case
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Left     : Location_Type
             )  is separate;

   procedure Get_Character_Literal
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is separate;

   procedure Get_Identifier
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is separate;

   procedure Get_Declare
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Left     : Location_Type
             )  is separate;

   procedure Get_Discrete_Choice_List
             (  Context : in out Ada_Expression;
                Code    : in out Lexers.Lexer_Source_Type;
                List    : out Subtype_Indication_Array_Ptr
             )  is
      type Indication_Array_Ref is access Subtype_Indication_Array;
      for Indication_Array_Ref'Storage_Pool use Context.Pool.all;
      Count : Positive;
      Ref   : Indication_Array_Ref;
   begin
      Get_Ranges_List (Context, Code, True, "", "|", "=>", Count);
      Ref := new Subtype_Indication_Array (1..Count);
      Pop (Context, Ref.all);
      if Count > 1 then
         for Index in Ref'Range loop
            if Is_Name (Ref (Index).Mark.Name, "others") then
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  (  "'others' is not the single choice in "
                  &  "the list at "
                  &  Image (Ref (Index).Mark.Name.Location)
               )  );
            end if;
         end loop;
      end if;
      List := Ref.all'Unchecked_Access;
   end Get_Discrete_Choice_List;

   procedure Get_Expanded_Name
             (  Context    : in out Ada_Expression;
                Code       : in out Lexers.Lexer_Source_Type;
                No_Strings : Boolean;
                Argument   : out Tokens.Argument_Token
             )  is
      Got_It : Boolean;
      Item   : Tokens.Argument_Token;
      Count  : Positive := 1;
   begin
      loop
         Get_Simple_Name (Context, Code, False, Item);
         if No_Strings and then Item.Value.all not in Identifier then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               "Identifier is expected at " & Image (Item.Location)
            );
         end if;
         Get_Blank (Context, Code);
         Get_Delimited (Code, ".", False, Got_It);
         exit when not Got_It;
         Push (Context, Item);
         Count := Count + 1;
      end loop;
      if Count > 1 then
         declare
            type Arena_Ptr is access Expression;
            for Arena_Ptr'Storage_Pool use Context.Pool.all;
            Result : constant Arena_Ptr :=
                          new Expression
                              (  Count     => Count,
                                 Operation => Component
                              );
            This   : Expression renames Result.all;
            List   : Frame (1..1);
         begin
            Argument.Value := This'Unchecked_Access;
            This.Operands (Count) := Item;
            for Index in reverse 1..Count - 1 loop
               Pop (Context, List);
               if List (1).Value.all not in Identifier then
                  Raise_Exception
                  (  Parsers.Syntax_Error'Identity,
                     "Identifier is expected at " &
                     Image (List (1).Location)
                  );
               end if;
               This.Operands (Index) := List (1);
            end loop;
            This.Location :=
               This.Operands (1).Location & Item.Location;
            Argument.Location := This.Location;
         end;
      else
         Argument := Item;
      end if;
   end Get_Expanded_Name;

   procedure Get_For
             (  Context   : in out Ada_Expression;
                Code      : in out Lexers.Lexer_Source_Type;
                Argument  : out Tokens.Argument_Token;
                Left      : Descriptors.Operation_Type;
                Par       : Boolean
             )  is separate;

   procedure Get_If
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Left     : Location_Type
             )  is separate;

   procedure Get_Names_List
             (  Context : in out Ada_Expression;
                Code    : in out Lexers.Lexer_Source_Type;
                Count   : out Positive
             )  is
      Name   : Tokens.Argument_Token;
      Got_It : Boolean;
   begin
      Count := 1;
      loop
         Get_Simple_Name (Context, Code, True, Name);
         Push (Context, Name);
         Get_Blank (Context, Code);
         Get_Delimited (Code, ",", False, Got_It);
         exit when not Got_It;
         Count := Count + 1;
      end loop;
   end Get_Names_List;

   procedure Get_Not_Null
             (  Context: in out Ada_Expression;
                Code   : in out Lexers.Lexer_Source_Type;
                Got_It : out Boolean
             )  is
   begin
      Get_Blank (Context, Code);
      Get_Delimited (Code, "not", True, Got_It);
      if Got_It then
         Get_Blank (Context, Code);
         Get_Delimited (Code, "null", True, Got_It);
         if not Got_It then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               "'null' is expected after 'not' at " &
               Image (Link (Code))
            );
         end if;
      end if;
   end Get_Not_Null;

   procedure Get_Numeric_Literal
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is separate;

   procedure Get_Range
             (  Context     : in out Ada_Expression;
                Code        : in out Lexers.Lexer_Source_Type;
                Constrained : Boolean;
                Any_Name    : Boolean;
                Argument    : out Subtype_Indication
             )  is separate;

   procedure Get_Raise
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Enclosed : Boolean;
                Left     : Location_Type
             )  is separate;

   procedure Get_Operand
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Got_It   : out Boolean
             )  is
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
   begin
      Get_Line (Code, Line, Pointer, Last);
      case Line (Pointer) is
         when '"' =>
            Get_String_Literal
            (  Context,
               Code,
               Line (Pointer..Last),
               Pointer,
               Argument
            );
            Got_It := True;
         when ''' =>
            Get_Character_Literal
            (  Context,
               Code,
               Line (Pointer..Last),
               Pointer,
               Argument
            );
            Got_It := True;
         when '0'..'9' =>
            Get_Numeric_Literal
            (  Context,
               Code,
               Line (Pointer..Last),
               Pointer,
               Argument
            );
            Got_It := True;
         when ']' =>
            if Pointer - 1 >= Line'First and then
               Line (Pointer - 1) = '['      then
               Argument.Location :=
                  Direct_Link (Code, Pointer, Pointer) & Link (Code);
               declare
                  type Arena_Ptr is access Null_Aggregate;
                  for Arena_Ptr'Storage_Pool use Context.Pool.all;
                  Result : constant Arena_Ptr := new Null_Aggregate;
               begin
                  Argument.Value := Result.all'Unchecked_Access;
               end;
               Got_It := True;
            else
               Got_It := False;
            end if;
         when '@' =>
            declare
               type Arena_Ptr is access Target_Name;
               for Arena_Ptr'Storage_Pool use Context.Pool.all;
               Result : constant Arena_Ptr := new Target_Name;
            begin
               Argument.Value := Result.all'Unchecked_Access;
            end;
            Argument.Location := Link (Code);
            Set_Pointer (Code, Pointer + 1);
            Got_It := True;
         when others =>
            declare
               use Descriptors;
               Operation : constant Descriptors.Descriptor :=
                                    Top (Context);
               Index     : Integer := Pointer;
               Symbol    : UTF8_Code_Point;
            begin
               if Operation.Class = Tuple then
                  declare
                     Kind_Of : constant Operations :=
                                        Operation.Operation.Operation;
                     Where   : Location_Type;
                     Keyword : Keyword_Type;
                  begin
                     if Kind_Of = Left_Bracket then
                        Where := Link (Code);
                        Get_Keyword (Code, Keywords, Keyword, Got_It);
                        if Got_It then
                           case Keyword is
                              when Case_Keyword =>
                                 Get_Case
                                 (  Context,
                                    Code,
                                    Argument,
                                    Where
                                 );
                                 return;
                              when Declare_Keyword =>
                                 Get_Declare
                                 (  Context,
                                    Code,
                                    Argument,
                                    Where
                                 );
                                 return;
                              when For_Keyword =>
                                 Get_For
                                 (  Context,
                                    Code,
                                    Argument,
                                    Operation.Operation,
                                    False
                                 );
                                 return;
                              when If_Keyword =>
                                 Get_If
                                 (  Context,
                                    Code,
                                    Argument,
                                    Where
                                 );
                                 return;
                              when Parallel_Keyword =>
                                 Get_For
                                 (  Context,
                                    Code,
                                    Argument,
                                    Operation.Operation,
                                    True
                                 );
                                 return;
                              when Raise_Keyword =>
                                 Get_Raise
                                 (  Context,
                                    Code,
                                    Argument,
                                    True,
                                    Where
                                 );
                                 return;
                           end case;
                        end if;
                     end if;
                     if Kind_Of = Left_Square_Bracket then
                        Where := Link (Code);
                        Get_Keyword (Code, Keywords, Keyword, Got_It);
                        if Got_It then
                           case Keyword is
                              when For_Keyword =>
                                 Get_For
                                 (  Context,
                                    Code,
                                    Argument,
                                    Operation.Operation,
                                    False
                                 );
                                 return;
                              when Parallel_Keyword =>
                                 Get_For
                                 (  Context,
                                    Code,
                                    Argument,
                                    Operation.Operation,
                                    True
                                 );
                                 return;
                              when others =>
                                 Reset_Pointer (Code);
                           end case;
                        end if;
                     end if;
                  end;
               end if;
               if Pointer < Last                 and then
                  Line (Pointer..Pointer + 1) = "<>" then
                  Set_Pointer (Code, Pointer + 2);
                  Argument.Location := Link (Code);
                  declare
                     type Arena_Ptr is access Box_Choice;
                     for Arena_Ptr'Storage_Pool
                        use Context.Pool.all;
                     Result : constant Arena_Ptr := new Box_Choice;
                  begin
                     Argument.Value := Result.all'Unchecked_Access;
                  end;
                  Got_It := True;
               elsif Strings_Edit.Is_Prefix
                     (  "raise",
                        Line (Pointer..Last),
                        Ada.Strings.Maps.Constants.Lower_Case_Map
                     )  then
                  Set_Pointer (Code, Pointer + 5);
                  Get_Raise
                  (  Context,
                     Code,
                     Argument,
                     False,
                     Link (Code)
                  );
                  Got_It := True;
               else
                  Get (Line (Pointer..Last), Index, Symbol);
                  if Is_Identifier_Start (Symbol) then
                     Get_Identifier
                     (  Context,
                        Code,
                        Line (Pointer..Last),
                        Pointer,
                        Argument
                     );
                     Got_It := True;
                  else
                     Got_It := False;
                  end if;
               end if;
            exception
               when Data_Error =>
                  Set_Pointer (Code, Index);
                  Set_Pointer (Code, Index);
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     Encoding_Error & Image (Link (Code))
                  );
            end;
      end case;
   exception
      when End_Error =>
         Got_It := False;
   end Get_Operand;

   procedure Get_Ranges_List
             (  Context   : in out Ada_Expression;
                Code      : in out Lexers.Lexer_Source_Type;
                Any_Name  : Boolean;
                Prefix    : String;
                Delimiter : String;
                Suffix    : String;
                Count     : out Positive
             )  is
      Got_It : Boolean;
   begin
      if Prefix'Length > 0 then
         Get_Blank (Context, Code);
         Get_Delimited (Code, Prefix, False, Got_It);
         if not Got_It then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               "'"                 &
               Prefix              &
               "' is expected at " &
               Image (Link (Code))
            );
         end if;
      end if;
      declare
         Item   : Subtype_Indication;
         Result : Natural := 0;
      begin
         loop
            Get_Range (Context, Code, False, Any_Name, Item);
            if Item.Mark.Name.Value.all in Expression then
               declare
                  This : Expression renames
                         Expression (Item.Mark.Name.Value.all);
               begin
                  if This.Operation = Alternative then
                     for Index in This.Operands'First
                               .. This.Operands'Last - 1
                     loop
                        Push
                        (  Context,
                           Subtype_Indication'
                           (  Mode       => Subtype_Mode,
                              Not_Null   => False,
                              Mark       => (  No_Attribute,
                                               This.Operands (Index)
                                            ),
                              Constraint => (Mode => No_Constraint)
                        )  );
                        Result := Result + 1;
                     end loop;
                     Item.Mark.Name :=
                        This.Operands (This.Operands'Last);
                     Push (Context, Item);
                     Result := Result + 1;
                  else
                     Push (Context, Item);
                     Result := Result + 1;
                  end if;
               end;
            else
               Push (Context, Item);
               Result := Result + 1;
            end if;
            Get_Blank (Context, Code);
            Get_Delimited (Code, Suffix, False, Got_It);
            exit when Got_It;
            Get_Delimited (Code, Delimiter, False, Got_It);
            if not Got_It then
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "'"                 &
                  Delimiter           &
                  "' or '"            &
                  Suffix              &
                  "' is expected at " &
                  Image (Link (Code))
               );
            end if;
         end loop;
         Count := Result;
      end;
   end Get_Ranges_List;

   procedure Get_Reserved_Word
             (  Code : in out Lexers.Lexer_Source_Type;
                Word : out Reserved_Word
             )  is
      use Reserved_Words_Tables;
      Got_It  : Boolean;
      Line    : Line_Ptr_Type;
      Last    : Integer;
      Pointer : Integer;
   begin
      Get_Line (Code, Line, Pointer, Last);
      Get (Line (Pointer..Last), Pointer, Reserved_Words, Word, Got_It);
      if not Got_It then
         Word := No_Reserved_Word;
      end if;
      Set_Pointer (Code, Pointer);
   end Get_Reserved_Word;

   procedure Get_String_Literal
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is separate;

   procedure Get_Simple_Name
             (  Context    : in out Ada_Expression;
                Code       : in out Lexers.Lexer_Source_Type;
                No_Strings : Boolean;
                Argument   : out Tokens.Argument_Token
             )  is separate;

   procedure Get_Subtype_Mark
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                No_Range : Boolean;
                Not_Null : Boolean;
                Mark     : out Subtype_Mark
             )  is separate;

   procedure Get_Subtype_Indication
             (  Context   : in out Ada_Expression;
                Code      : in out Lexers.Lexer_Source_Type;
                Composite : Boolean;
                Not_Null  : in out Boolean;
                Argument  : out Subtype_Indication
             )  is separate;

   function Get_Value (Item : Half_Word_Array)
      return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      for Digit in Item'Range loop
         Shift_Left (Result, 1);
         Add (Result, Item (Digit));
      end loop;
      return Result;
   end Get_Value;

   function Get_Value (Item : Universal_Integer)
      return Unbounded_Integer is
   begin
      if Item.Value = null then
         if Item.Data = null then
            return Unbounded_Integers.Zero;
         else
            return Compose
                   (  Get_Value (Item.Data.all),
                      Item.Negative
                   );
         end if;
      else
         return Item.Value.all;
      end if;
   end Get_Value;

   function Get_Value (Item : Universal_Real)
      return Unbounded_Rational is
   begin
      if Item.Value = null then
         if Item.Numerator = null then
            return Unbounded_Rationals.Zero;
         else
            return Compose
                   (  Get_Value (Item.Numerator.all),
                      Get_Value (Item.Denominator.all),
                      Item.Negative
                   );
         end if;
      else
         return Item.Value.all;
      end if;
   end Get_Value;

   function Image (Key : Reserved_Word) return String is
      use Reserved_Words_Tables;
   begin
      for Index in 1..GetSize (Reserved_Words) loop
         if GetTag (Reserved_Words, Index) = Key then
            return GetName (Reserved_Words, Index);
         end if;
      end loop;
      Raise_Exception
      (  Program_Error'Identity,
         "Keyword " & Reserved_Word'Image (Key) & " is not recognized"
      );
   end Image;

   function Aspect_Specification_Item_Image is
      new Generic_Image (Aspect_Specification_Item);

   function Image
            (  Item       : Aspect_Specification_Item;
               Max_Length : Output_Length := Output_Length'Last
            )  return String renames Aspect_Specification_Item_Image;

   procedure Put_Argument_List
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Argument_List
             )  is
   begin
      Put (Destination, Pointer, Item, ", ");
   end Put_Argument_List;

   function Image (Mode : Basic_Global_Mode) return String is
   begin
      case Mode is
         when In_Global_Mode =>
            return "in";
         when Inout_Global_Mode =>
            return "in out";
         when Out_Global_Mode =>
            return "out";
      end case;
   end Image;

   function Argument_List_Image is
      new Generic_Image (Argument_List, Put_Argument_List);
   function Image
            (  Item       : Argument_List;
               Max_Length : Output_Length := Output_Length'Last
            )  return String renames Argument_List_Image;

   function Image
            (  Item       : Node'Class;
               Max_Length : Output_Length := Output_Length'Last
            )  return String is
      Length : Integer := Image_Length;
   begin
      loop
         declare
            Result  : String (1..Length);
            Pointer : Integer := 1;
         begin
            Put (Result, Pointer, Item);
            Pointer := Pointer - 1;
            if Pointer <= Max_Length then
               return Result (1..Pointer);
            else
               declare
                  Half : constant Integer := (Max_Length - 3) / 2;
               begin
                  return Result (1..Max_Length - Half - 3) &
                         "..."                             &
                         Result (Pointer - Half + 1..Pointer);
               end;
            end if;
         exception
            when Layout_Error =>
               Length := Length * 3 / 2;
         end;
      end loop;
   end Image;

   function Image (Operation : Operations) return String is
      use Strings_Edit;
   begin
      case Operation is
         when Logical_And          => return "and";
         when Logical_Or           => return "or";
         when Logical_Xor          => return "xor";
         when And_Then             => return "and then";
         when Or_Else              => return "or else";
         when EQ                   => return "=";
         when NE                   => return "/=";
         when LT                   => return "<";
         when LE                   => return "<=";
         when GE                   => return ">=";
         when GT                   => return ">";
         when Member               => return "in";
         when Not_Member           => return "not in";
         when Add                  => return "+";
         when Sub                  => return "-";
         when Concatenate          => return "&";
         when Plus                 => return "+";
         when Minus                => return "-";
         when Mul                  => return "*";
         when Div                  => return "/";
         when Modulus              => return "mod";
         when Remainder            => return "rem";
         when Pow                  => return "**";
         when Abs_Value            => return "abs";
         when Logical_Not          => return "not";
         when Allocator            => return "new";
         when Allocator_Subpool    => return "new";
         when Alternative          => return "|";
         when Attribute            => return "'";
         when Ellipsis             => return "..";
         when Component            => return ".";
         when Left_Bracket         => return "()";
         when Right_Bracket        => return ")";
         when Left_Square_Bracket  => return "[]";
         when Right_Square_Bracket => return "]";
         when Left_Index           => return "*()";
         when Comma                => return ",";
         when Associate            => return "=>";
         when Extend               => return "with";
         when Extend_Delta         => return "with delta";
         when Add_Inv              => return "0-";
         when Mul_Inv              => return UTF8.Image (16#215F#);
         when Keyword_Delta        => return "delta";
         when Keyword_Record       => return "record";
         when Reserved             => return "reserved";
      end case;
   end Image;

   function Subtype_Indication_Image is
      new Generic_Image (Subtype_Indication);
   function Image
            (  Item       : Subtype_Indication;
               Max_Length : Output_Length := Output_Length'Last
            )  return String renames Subtype_Indication_Image;

   function Subtype_Mark_Image is
      new Generic_Image (Subtype_Mark);
   function Image
            (  Item       : Subtype_Mark;
               Max_Length : Output_Length := Output_Length'Last
            )  return String renames Subtype_Mark_Image;

   function Has_Bracket (Code : Lexers.Lexer_Source_Type)
      return Boolean is
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
   begin
      Get_Line (Code, Line, Pointer, Last);
      return Pointer <= Last and then Line (Pointer) = ')';
   end Has_Bracket;

   function Is_Class (Item : Tokens.Argument_Token) return Boolean is
      This : Node'Class renames Item.Value.all;
   begin
      return This in Identifier and then
             Equal = Compare (Identifier (This).Value, "class");
   end Is_Class;

   function Is_Defining_Identifier (Item : Tokens.Argument_Token)
      return Boolean is
   begin
      return Item.Value.all in Identifier;
   end Is_Defining_Identifier;

   function Is_Defining_Operator (Item : Tokens.Argument_Token)
      return Boolean is
      use Operator_Tables;
   begin
      return Item.Value.all in Text_Literal and then
             IsIn (Operators, Text_Literal (Item.Value.all).Value);
   end Is_Defining_Operator;

   function Is_Name
            (  Item : Tokens.Argument_Token;
               Name : String
            )  return Boolean is
      This : Node'Class renames Item.Value.all;
   begin
      return This in Identifier and then
             Equal = Compare (Identifier (This).Value, Name);
   end Is_Name;

   procedure On_Association_Error
             (  Context : in out Ada_Expression;
                Code    : in out Lexers.Lexer_Source_Type;
                Left    : in out Operation_Token;
                Right   : in out Operation_Token
             )  is
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Parentheses required. '"
         &  Image (Left.Operation)
         &  "' at "
         &  Image (Left.Location)
         &  " cannot be associated with '"
         &  Image (Right.Operation)
         &  "' at "
         &  Sources.Image (Right.Location)
      )  );
   end On_Association_Error;

   procedure On_Missing_Operation
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Modifier : Tokens.Operation_Token;
                Token    : out Lexers.Token_Lexer.Implementation.
                                  Lexical_Token;
                Got_It   : out Boolean
             )  is
   begin
      case Modifier.Operation is
         when Logical_And | Logical_Or =>
            Token  :=
               (Operator, Modifier, Logical_Priority, Logical_Priority);
            Got_It := True;
         when Logical_Not =>
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "'in' is expected at "
               &  Image (Link (Code))
               &  " after 'not' at "
               &  Image (Modifier.Location)
            )  );
         when others =>
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "Unknown error after a modifier at "
               &  Image (Modifier.Location)
            )  );
      end case;
   end On_Missing_Operation;

   procedure On_Missing_Right_Bracket
             (  Context : in out Ada_Expression;
                Code    : in out Lexers.Lexer_Source_Type;
                Left    : in out Operation_Token;
                Right   : out Operation_Token
             )  is
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Closing parenthesis ')' matching the opening one '(' at "
         &  Sources.Image (Left.Location)
         &  " is expected at "
         &  Sources.Image (Sources.Link (Code))
      )  );
   end On_Missing_Right_Bracket;

   procedure On_Postmodifier
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : in out Tokens.Argument_Token;
                Modifier : Tokens.Operation_Token;
                Got_It   : out Boolean
             )  is
   begin
      case Modifier.Operation is
         when Keyword_Record =>
            if (  Argument.Value.all in Identifier'Class
               and then
                  Identifier'Class (Argument.Value.all).Value = "null"
               )
            then
               Free (Context, Argument.Value);
               declare
                  type Arena_Ptr is access Identifier;
                  for Arena_Ptr'Storage_Pool use Context.Pool.all;
                  Result : constant Arena_Ptr := new Identifier (11);
               begin
                  Result.Value   := "null record";
                  Argument.Value := Result.all'Unchecked_Access;
               end;
               Argument.Location :=
                  Argument.Location & Modifier.Location;
               Got_It := True;
               return;
            end if;
         when others =>
            null;
      end case;
      Reset_Pointer (Code);
      Got_It := False;
   end On_Postmodifier;

   procedure On_Postmodifier
             (  Context   : in out Ada_Expression;
                Code      : in out Lexers.Lexer_Source_Type;
                Operation : in out Tokens.Operation_Token;
                Modifier  : Tokens.Operation_Token;
                Got_It    : out Boolean
             )  is
   begin
      case Modifier.Operation is
         when Keyword_Delta =>
            case Operation.Operation is
               when Extend =>
                  Operation :=
                     (  Extend_Delta,
                        Operation.Location & Modifier.Location
                     );
                  Got_It := True;
                  return;
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
      Reset_Pointer (Code);
      Got_It := False;
   end On_Postmodifier;

   procedure On_Premodifier
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Token    : in out Lexers.Token_Lexer.Implementation.
                                     Lexical_Token;
                Modifier : Tokens.Operation_Token;
                Got_It   : out Boolean
             )  is
   begin
      case Modifier.Operation is
         when Logical_And =>
            if (  Token.Class = Postmodifier
               and then
                  Token.Operation.Operation = And_Then
               )
            then
               Token :=
                  (  Operator,
                     (  And_Then,
                        Token.Operation.Location & Modifier.Location
                     ),
                     Logical_Priority,
                     Logical_Priority
                  );
               Got_It := True;
               return;
            else
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  (  "'then' is expected at "
                  &  Image (Token.Operation.Location)
                  &  " after 'and' at "
                  &  Image (Modifier.Location)
               )  );
            end if;
         when Logical_Or =>
            if (  Token.Class = Postmodifier
               and then
                  Token.Operation.Operation = Or_Else
               )
            then
               Token :=
                  (  Operator,
                     (  Or_Else,
                        Token.Operation.Location & Modifier.Location
                     ),
                     Logical_Priority,
                     Logical_Priority
                  );
               Got_It := True;
               return;
            else
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  (  "'else' is expected at "
                  &  Image (Token.Operation.Location)
                  &  " after 'or' at "
                  &  Image (Modifier.Location)
               )  );
            end if;
         when Logical_Not =>
            if (  Token.Class = Operator
               and then
                  Token.Operation.Operation = Member
               )
            then
               Token :=
                  (  Operator,
                     (  Not_Member,
                        Token.Operation.Location & Modifier.Location
                     ),
                     1,
                     1
                  );
               Got_It := True;
               return;
            else
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  (  "'in' is expected at "
                  &  Image (Token.Operation.Location)
                  &  " after 'not' at "
                  &  Image (Modifier.Location)
               )  );
            end if;
         when others =>
            null;
      end case;
      Reset_Pointer (Code);
      Got_It := False;
   end On_Premodifier;

   procedure On_Success
             (  Context : in out Ada_Expression;
                Code    : in out Lexers.Lexer_Source_Type;
                Result  : in out Tokens.Argument_Token
             )  is
   begin
      Store (Context, Result);
   end On_Success;

   procedure Pop
             (  Context : in out Ada_Expression;
                List    : out Argument_List
             )  is
      Tokens : Frame (1..1);
   begin
      for Item in reverse List'Range loop
         Pop (Context, Tokens);
         List (Item) := Tokens (1);
      end loop;
   end Pop;

   procedure Pop
             (  Context : in out Ada_Expression;
                List    : out Aspect_Items_Array
             )  is
      Tokens : Frame (1..1);
      type Item_Ptr is access Aspect_Specification_Item;
      for Item_Ptr'Storage_Pool use Context.Pool.all;
      function To_Item is
         new Ada.Unchecked_Conversion (Node_Ptr, Item_Ptr);
   begin
      for Index in reverse List'Range loop
         Pop (Context, Tokens);
         List (Index) :=
            To_Item (Tokens (1).Value).all'Unchecked_Access;
      end loop;
   end Pop;

   procedure Pop
             (  Context : in out Ada_Expression;
                List    : out Subtype_Indication_Array
             )  is
   begin
      for Index in reverse List'Range loop
         Stubtype_Indication_Stack.Pop (Context, List (Index));
      end loop;
   end Pop;

   procedure Push
             (  Context : in out Ada_Expression;
                Item    : Subtype_Indication
             )  is
   begin
      Stubtype_Indication_Stack.Push (Context, Item);
   end Push;

   procedure Push_Stub
             (  Context : in out Ada_Expression;
                Stub    : out Node_Ptr
             )  is
      type Arena_Ptr is access Mark;
      for Arena_Ptr'Storage_Pool use Context.Pool.all;
      Result : constant Arena_Ptr := new Mark;
   begin
      Stub := Result.all'Unchecked_Access;
   end Push_Stub;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                List        : Argument_List;
                Delimiter   : String := " "
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      for Item in List'Range loop
         if Item > List'First then
            if List (Item).Value.all in Expression and then
               Expression (List (Item).Value.all).Operation = Extend
            then
               Put (Destination, Index, " ");
            else
               Put (Destination, Index, Delimiter);
            end if;
         end if;
         Put (Destination, Index, List (Item));
      end loop;
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Argument_Token
             )  is
   begin
      Put (Destination, Pointer, Item.Value.all);
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Array_Type_Definition
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      Put (Destination, Index, "array ");
      Put (Destination, Index, Item.Indices);
      Put (Destination, Index, " of ");
      if Item.Aliased_Component then
         Put (Destination, Index, "aliased ");
      end if;
      Put (Destination, Index, Item.Component);
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                List        : Aspect_Items_Array;
                Prefix      : String := " with "
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      if List'Length = 0 then
         return;
      end if;
      Put (Destination, Index, Prefix);
      for Item in List'Range loop
         if Item > List'First then
            Put (Destination, Index, ", ");
         end if;
         Put (Destination, Index, List (Item).all);
      end loop;
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Aspect_Specification_Item
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      Put (Destination, Index, Item.Mark);
      case Item.Mode is
         when No_Designator =>
            null;
         when Null_Designator =>
            Put (Destination, Index, " => null");
         when Unspecified_Designator =>
            Put (Destination, Index, " => unspecified");
         when Global_Designator =>
            Put (Destination, Index, " => ");
            Put (Destination, Index, Item.Designator.all);
         when Global_Aspect_Elements_List_Designator =>
            Put (Destination, Index, " => (");
            declare
               List : Global_Aspect_Element_Ptr_Array renames Item.List;
            begin
               for Item in List'Range loop
                  if Item > List'First then
                     Put (Destination, Index, "; ");
                  end if;
                  Put (Destination, Index, List (Item).all);
               end loop;
            end;
            Put (Destination, Index, ")");
         when Value_Designator =>
            Put (Destination, Index, " => ");
            Put (Destination, Index, Item.Value);
      end case;
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Box_Choice
             )  is
      use Strings_Edit;
   begin
      Put (Destination, Pointer, "<>");
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                List        : Case_Alternatives_Array;
                Prefix      : String := "";
                Delimiter   : String := ", ";
                Ligature    : String := " => ";
                Suffix      : String := ""
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      if List'Length = 0 then
         return;
      end if;
      Put (Destination, Index, Prefix);
      for Item in List'Range loop
         declare
            This : Case_Alternative renames List (Item);
         begin
            if Item > 1 then
               Put (Destination, Index, Delimiter);
            end if;
            for Item in This.Choice'Range loop
               if Item > This.Choice'First then
                  Put (Destination, Index, " | ");
               end if;
               Put (Destination, Index, This.Choice (Item));
            end loop;
            Put (Destination, Index, Ligature);
            Put (Destination, Index, This.Value);
         end;
      end loop;
      Put (Destination, Index, Suffix);
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Case_Expression
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      Put (Destination, Index, "(case ");
      Put (Destination, Index, Item.Selector);
      Put (Destination, Index, " is ");
      Put
      (  Destination => Destination,
         Pointer     => Index,
         List        => Item.Alternatives,
         Prefix      => "when ",
         Delimiter   => ", when ",
         Ligature    => " => "
      );
      if Item.Has_Others then
         Put (Destination, Index, ", when others => ");
         Put (Destination, Index, Item.Others_Alternative);
      end if;
      Put (Destination, Index, ")");
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Character_Literal
             )  is
      use Strings_Edit;
   begin
      Put (Destination, Pointer, ''' & Item.Value & ''');
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Declare_Expression
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
      List  : Declare_Item_Array renames Item.Items;
   begin
      Put (Destination, Index, "(declare");
      for Item in List'Range loop
         Put (Destination, Index, " ");
         Put (Destination, Index, List (Item).Value.all);
         Put (Destination, Index, ";");
      end loop;
      Put (Destination, Index, " begin ");
      Put (Destination, Index, Item.Expression);
      Put (Destination, Index, ")");
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Declare_Object_Item
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
       Put (Destination, Index, Item.Names, ", ");
       Put (Destination, Index, " : constant ");
       if Item.Array_Object then
          Put (Destination, Index, Item.Definition.all);
       else
          Put (Destination, Index, Item.Object);
       end if;
       Put (Destination, Index, " := ");
       Put (Destination, Index, Item.Value);
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Declare_Renaming_Item
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      Put (Destination, Index, Item.Name);
      if Item.Has_Mark then
         Put (Destination, Index, " : ");
         Put (Destination, Index, Item.Mark);
      end if;
      Put (Destination, Index, " renames ");
      Put (Destination, Index, Item.Object);
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Expression
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;

      procedure Put_Attribute is
      begin
         Put
         (  Destination => Destination,
            Pointer     => Index,
            Item        => Item.Operands (Item.Operands'First)
         );
         Put (Destination, Index, "'(");
         Put
         (  Destination => Destination,
            Pointer     => Index,
            List        => Item.Operands
                           (  Item.Operands'First + 1
                           .. Item.Operands'Last
                           ),
            Delimiter   => ", "
         );
         Put (Destination, Index, ")");
      end Put_Attribute;
   begin
      case Item.Operation is
         when Logical_And | Logical_Xor | Logical_Or | Concatenate |
              Remainder   | Modulus     | And_Then   | Or_Else     |
              Add  | Sub  | Mul  | Div  | Pow  |
              EQ   | NE   | GT   | GE   | LT   |  LE  =>
            Put (Destination, Index, "(");
            Put
            (  Destination => Destination,
               Pointer     => Index,
               List        => Item.Operands,
               Delimiter   => ' ' & Image (Item.Operation) & ' '
            );
            Put (Destination, Index, ")");
         when Ellipsis | Component =>
            Put
            (  Destination => Destination,
               Pointer     => Index,
               List        => Item.Operands,
               Delimiter   => Image (Item.Operation)
            );
         when Extend =>
            Put (Destination, Index, "with ");
            Put (Destination, Index, Item.Operands, ", ");
         when Extend_Delta =>
            Put (Destination, Index, "with delta ");
            Put (Destination, Index, Item.Operands, ", ");
         when Attribute =>
            if Item.Operands'Length = 2 then
               declare
                  List   : Argument_List renames Item.Operands;
                  Second : Node'Class renames
                           List (List'Last).Value.all;
               begin
                  if Second in Expression'Class then
                     declare
                        This : Expression'Class renames
                               Expression'Class (Second);
                     begin
                        if This.Operation = Left_index then
                           Put
                           (  Destination => Destination,
                              Pointer     => Index,
                              Item        => List (List'First)
                           );
                           Put (Destination, Index, "'");
                           Put
                           (  Destination => Destination,
                              Pointer     => Index,
                              Item        => List (List'Last)
                           );
                        else
                           Put_Attribute;
                        end if;
                     end;
                  elsif Second not in Composite'Class and then
                        Second not in Literal'Class       then
                     Put
                     (  Destination => Destination,
                        Pointer     => Index,
                        Item        => List (List'First)
                     );
                     Put (Destination, Index, "'");
                     Put
                     (  Destination => Destination,
                        Pointer     => Index,
                        Item        => List (List'Last)
                     );
                  else
                     Put_Attribute;
                  end if;
               end;
            else
               Put_Attribute;
            end if;
         when Associate | Alternative | Member | Not_Member =>
            Put
            (  Destination => Destination,
               Pointer     => Index,
               List        => Item.Operands,
               Delimiter   => ' ' & Image (Item.Operation) & ' '
            );
         when Left_Bracket =>
            Put (Destination, Index, "(");
            Put (Destination, Index, Item.Operands, ", ");
            Put (Destination, Index, ")");
         when Left_Square_Bracket =>
            Put (Destination, Index, "[");
            Put (Destination, Index, Item.Operands, ", ");
            Put (Destination, Index, "]");
         when Plus | Minus | Abs_Value | Add_Inv | Mul_Inv =>
            Put (Destination, Index, Image (Item.Operation));
            Put (Destination, Index, "(");
            Put (Destination, Index, Item.Operands, ", ");
            Put (Destination, Index, ")");
         when Logical_Not | Allocator | Allocator_Subpool =>
            Put (Destination, Index, Image (Item.Operation));
            Put (Destination, Index, " (");
            Put (Destination, Index, Item.Operands, ", ");
            Put (Destination, Index, ")");
         when Right_Bracket        |
              Right_Square_Bracket |
              Comma                |
              Keyword_Delta        |
              Keyword_Record       |
              Reserved             =>
            null;
         when Left_Index =>
            Put
            (  Destination => Destination,
               Pointer     => Index,
               Item        => Item.Operands (Item.Operands'First)
            );
            Put (Destination, Index, " (");
            Put
            (  Destination => Destination,
               Pointer     => Index,
               List        => Item.Operands
                              (  Item.Operands'First + 1
                              .. Item.Operands'Last
                              ),
               Delimiter   => ", "
            );
            Put (Destination, Index, ")");
      end case;
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : For_Expression
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      if 0 /= (Item.Options and For_Parallel) then
         Put (Destination, Index, "parallel");
         if 0 /= (Item.Options and For_Chunk) then
            Put (Destination, Index, " (");
            Put (Destination, Index, Item.Chunk);
            Put (Destination, Index, ")");
         end if;
         Put (Destination, Index, Item.Aspects, " with ");
         Put (Destination, Index, " ");
      end if;
      case Item.Qualifier is
         when For_All =>
            Put (Destination, Index, "for all ");
         when For_Some =>
            Put (Destination, Index, "for some ");
         when For_Any =>
            Put (Destination, Index, "for ");
      end case;
      Put (Destination, Index, Item.Identifier);
      if 0 /= (Item.Options and For_Of) then
         Put (Destination, Index, " of ");
         if 0 /= (Item.Options and For_Reverse) then
            Put (Destination, Index, "reverse ");
         end if;
      elsif 0 = (Item.Options and For_Range) then
         Put (Destination, Index, " in ");
         if 0 /= (Item.Options and For_Reverse) then
            Put (Destination, Index, "reverse ");
         end if;
      else
         Put (Destination, Index, " in ");
         Put (Destination, Index, Item.Range_Type);
         Put (Destination, Index, " range ");
      end if;
      Put (Destination, Index, Item.Iterator);
      if 0 /= (Item.Options and For_Key) then
         Put (Destination, Index, " use ");
         Put (Destination, Index, Item.Key);
      end if;
      if 0 /= (Item.Options and For_Condition) then
         Put (Destination, Index, " when ");
         Put (Destination, Index, Item.Condition);
      end if;
      Put (Destination, Index, " => ");
      Put (Destination, Index, Item.Expression);
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Global_Aspect_Element
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      if Item.Extended then
         Put (Destination, Index, "overriding ");
      end if;
      Put (Destination, Index, Image (Item.Mode) & " ");
      case Item.Kind_Of is
         when All_Designator_Mode =>
            Put (Destination, Index, "all");
         when Synchronized_Designator_Mode =>
            Put (Destination, Index, "synchronized");
         when Global_Name_Designator_Mode =>
            Put (Destination, Index, Item.List, ", ");
      end case;
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                List        : Guarded_List;
                Prefix      : String := "";
                Delimiter   : String := ", ";
                Ligature    : String := " => ";
                Suffix      : String := ""
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      if List'Length = 0 then
         return;
      end if;
      Put (Destination, Index, Prefix);
      for Item in List'Range loop
         declare
            This : Alternative_Pair renames List (Item);
         begin
            if Item > 1 then
               Put (Destination, Index, Delimiter);
            end if;
            Put (Destination, Index, This.Guard);
            Put (Destination, Index, Ligature);
            Put (Destination, Index, This.Value);
         end;
      end loop;
      Put (Destination, Index, Suffix);
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : If_Expression
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      Put
      (  Destination => Destination,
         Pointer     => Index,
         List        => Item.Alternatives,
         Prefix      => "(if ",
         Delimiter   => " elsif ",
         Ligature    => " then ",
         Suffix      => ""
      );
      if Item.Has_Else then
         Put (Destination, Index, " else ");
         Put (Destination, Index, Item.Else_Alternative);
      end if;
      Put (Destination, Index, ")");
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Identifier
             )  is
      use Strings_Edit;
   begin
      if Item.Malformed then
         Put (Destination, Pointer, "<malformed>");
      else
         Put (Destination, Pointer, Item.Value);
      end if;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Integer_Literal
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      if Item.Malformed then
         Put (Destination, Index, "<malformed>");
      elsif Item.Exponent = Integer'First then
         Put (Destination, Index, "<underflown>");
      elsif Item.Exponent = Integer'Last then
         Put (Destination, Index, "<overflown>");
      elsif Item.Base = 10 then
         if Item.Exponent = 0 then
            Put (Destination, Index, Item.Value);
         else
            Put (Destination, Index, Item.Value);
            Put (Destination, Index, "E");
            Put (Destination, Index, Item.Exponent);
         end if;
      else
         if Item.Exponent = 0 then
            Put (Destination, Index, Item.Base);
            Put (Destination, Index, "#");
            Put (Destination, Index, Item.Value);
            Put (Destination, Index, "#");
         else
            Put (Destination, Index, Item.Base);
            Put (Destination, Index, "#");
            Put (Destination, Index, Item.Value);
            Put (Destination, Index, "#E");
            Put (Destination, Index, Item.Exponent);
         end if;
      end if;
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Mark
             )  is
      use Strings_Edit;
   begin
      Put (Destination, Pointer, "<stub>");
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Null_Aggregate
             )  is
      use Strings_Edit;
   begin
      Put (Destination, Pointer, "[]");
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Numeric_Literal
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      if Item.Malformed then
         Put (Destination, Index, "<malformed>");
      elsif Item.Exponent = Integer'First then
         Put (Destination, Index, "<underflown>");
      elsif Item.Exponent = Integer'Last then
         Put (Destination, Index, "<overflown>");
      elsif Item.Base = 10 then
         if Item.Exponent = 0 then
            Put (Destination, Index, Item.Value);
         else
            Put (Destination, Index, Item.Value);
            Put (Destination, Index, "E");
            Put (Destination, Index, Item.Exponent);
         end if;
      else
         if Item.Exponent = 0 then
            Put (Destination, Index, Item.Base);
            Put (Destination, Index, "#");
            Put (Destination, Index, Item.Value);
            Put (Destination, Index, "#");
         else
            Put (Destination, Index, Item.Base);
            Put (Destination, Index, "#");
            Put (Destination, Index, Item.Value);
            Put (Destination, Index, "#E");
            Put (Destination, Index, Item.Exponent);
         end if;
      end if;
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Raise_Expression
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      Put (Destination, Index, "(raise ");
      Put (Destination, Index, Item.Name);
      if Item.Has_Message then
         Put (Destination, Index, " with ");
         Put (Destination, Index, Item.Message);
      end if;
      Put (Destination, Index, ")");
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Real_Literal
             )  is
      use Strings_Edit;
      Index    : Integer := Pointer;
      Exponent : Integer := Item.Exponent;

      function "abs" (S : String) return String is
      begin
         if S'Length <= 1 then
            if Exponent = -1 then
               Exponent := 0;
               return "0." & S;
            else
               return S & ".0";
            end if;
         elsif Exponent = 0 then
            return S & ".0";
         elsif Exponent = -S'Length then
            Exponent := 0;
            return "0." & S;
         elsif Exponent < 0 and then Exponent > -S'Length then
            Exponent := 0;
            return S (S'First..S'Last + Item.Exponent) &
                   '.'                                 &
                   S (S'Last + Item.Exponent + 1..S'Last);
         else
            Exponent := Exponent + S'Length - 1;
            return S (S'First) & '.' & S (S'First + 1..S'Last);
         end if;
      end "abs";
   begin
      if Item.Malformed then
         Put (Destination, Index, "<malformed>");
      elsif Item.Exponent = Integer'First then
         Put (Destination, Index, "<underflown>");
      elsif Item.Exponent = Integer'Last then
         Put (Destination, Index, "<overflown>");
      else
         declare
            Value : constant String := abs Item.Value;
         begin
            if Item.Base = 10 then
               if Exponent = 0 then
                  Put (Destination, Index, Value);
               else
                  Put (Destination, Index, Value);
                  Put (Destination, Index, "E");
                  Put (Destination, Index, Exponent);
               end if;
            elsif Exponent = 0 then
               Put (Destination, Index, Item.Base);
               Put (Destination, Index, "#");
               Put (Destination, Index, Value);
               Put (Destination, Index, "#");
            else
               Put (Destination, Index, Item.Base);
               Put (Destination, Index, "#");
               Put (Destination, Index, Value);
               Put (Destination, Index, "#E");
               Put (Destination, Index, Exponent);
            end if;
         end;
      end if;
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : String_Literal
             )  is
      use Strings_Edit;
   begin
      Put (Destination, Pointer, Quote (Item.Value));
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Subtype_Indication
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      if Item.Not_Null then
         Put (Destination, Index, "not null ");
      end if;
      Put (Destination, Index, Item.Mark);
      case Item.Constraint.Mode is
         when No_Constraint =>
            null;
         when Fixed_Point_Constraint =>
            Put (Destination, Index, " delta ");
            Put (Destination, Index, Item.Constraint.Delta_Constraint);
         when Floating_Point_Constraint =>
            Put (Destination, Index, " digits ");
            Put (Destination, Index, Item.Constraint.Digits_Constraint);
         when Index_Constraint | Discriminant_Constraint =>
            Put (Destination, Index, " ");
            Put (Destination, Index, Item.Constraint.Constraint);
      end case;
      case Item.Mode is
         when Subtype_Range_Mode =>
            Put (Destination, Index, " range ");
            Put (Destination, Index, Item.Range_Constraint);
         when Range_Mode | Subtype_Mode =>
            null;
         when Unconstrained_Mode =>
            Put (Destination, Index, " range <>");
      end case;
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                List        : Subtype_Indication_Array;
                Prefix      : String := "(";
                Delimiter   : String := ", ";
                Suffix      : String := ")"
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      if List'Length > 0 then
         Put (Destination, Index, Prefix);
         for Item in List'Range loop
            if Item > List'First then
               Put (Destination, Index, Delimiter);
            end if;
            Put (Destination, Index, List (Item));
         end loop;
         Put (Destination, Index, Suffix);
         Pointer := Index;
      end if;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Subtype_Mark
             )  is
      use Strings_Edit;
      Index : Integer := Pointer;
   begin
      Put (Destination, Index, Item.Name);
      case Item.Attribute is
         when Class_Attribute =>
            Put (Destination, Index, "'Class");
         when No_Attribute =>
            null;
         when Base_Attribute =>
            Put (Destination, Index, "'Base");
         when Base_Range_Attribute =>
            Put (Destination, Index, "'Base'Range");
         when Dimension_Range_Attribute =>
            Put (Destination, Index, "'Range (");
            Put (Destination, Index, Item.Dimension);
            Put (Destination, Index, ")");
         when Range_Attribute =>
            Put (Destination, Index, "'Range");
      end case;
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Target_Name
             )  is
      use Strings_Edit;
   begin
      Put (Destination, Pointer, "@");
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Universal_Boolean
             )  is
      use Strings_Edit;
   begin
      if Item.Value then
         Put (Destination, Pointer, "True");
      else
         Put (Destination, Pointer, "False");
      end if;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Universal_Integer
             )  is
      use Strings_Edit.Unbounded_Integer_Edit;
   begin
      Put (Destination, Pointer, Get_Value (Item));
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Universal_Real
             )  is
      use Strings_Edit.Unbounded_Rational_Edit;
      use Strings_Edit.Unbounded_Unsigned_Edit;
      use Strings_Edit;
      Obelus : constant String := Character'Val (16#C3#) &
                                  Character'Val (16#B7#);
      Value  : constant Unbounded_Rational := Get_Value (Item);
      Index  : Integer := Pointer;
   begin
      if Is_Zero (Value) then
         Put (Destination, Index, "0.0");
      elsif Is_One (Get_Denominator (Value)) then
         if Is_Negative (Value) then
            Put (Destination, Index, "-");
         end if;
         Put (Destination, Index, Get_Numerator (Value));
         Put (Destination, Index, ".0");
      elsif Is_One (Get_Denominator (Value)) then
         Put
         (  Destination => Destination,
            Pointer     => Index,
            Value       => Value,
            Fraction    => Fraction
         );
      else
         declare
            Shifted : constant Unbounded_Rational :=
                           abs Value * Half_Word (10) ** Fraction;
            Rounded : constant Unbounded_Integer := Round (Shifted);
         begin
            if Rounded = Shifted then
               Put
               (  Destination => Destination,
                  Pointer     => Index,
                  Value       => Value,
                  Fraction    => Fraction
               );
               while Destination (Index - 1) = '0' loop
                  Index := Index - 1;
               end loop;
            else
               if Is_Negative (Value) then
                  Put (Destination, Index, "-");
               end if;
               Put (Destination, Index, Get_Numerator (Value));
               Put (Destination, Index, Obelus);
               Put (Destination, Index, Get_Denominator (Value));
            end if;
         end;
      end if;
      Pointer := Index;
   end Put;

   procedure Set_Constant_Folding
             (  Context : in out Ada_Expression;
                Enable  : Boolean
             )  is
   begin
      Context.Fold := Enable;
   end Set_Constant_Folding;

   function Store
            (  Context : access Ada_Expression'Class;
               Value   : Unbounded_Unsigned
            )  return Half_Word_Array_Ptr is
      type Arena_Ptr is access Half_Word_Array;
      for Arena_Ptr'Storage_Pool use Context.Pool.all;
      Data : constant Arena_Ptr :=
                  new Half_Word_Array (1..Get_Length (Value));
   begin
      for Digit in Data'Range loop
         Data (Digit) := Get_Digit (Value, Digit);
      end loop;
      return Data.all'Unchecked_Access;
   end Store;

   procedure Store
             (  Context : in out Ada_Expression;
                Item    : in out Tokens.Argument_Token
             )  is
      This : Node'Class renames Ref (Item.Value).all;
   begin
      if This in Universal_Integer then
         declare
            Object : Universal_Integer renames
                     Universal_Integer (This);
            Inversed : constant Boolean := Object.Negative;
         begin
            if Object.Value /= null then
               Object.Negative := Is_Negative (Object.Value.all);
               Object.Data := Store
                              (  Context'Access,
                                 Get_Mantissa (Object.Value.all)
                              );
               Free (Object.Value);
               if Inversed then -- Inversed value
                  declare
                     type Area_Ptr is access Expression;
                     for Area_Ptr'Storage_Pool use Context.Pool.all;
                     Ptr  : constant Area_Ptr :=
                                 new Expression'
                                     (  Count     => 1,
                                        Location  => Item.Location,
                                        Operation => Mul_Inv,
                                        Operands  => (1 => Item)
                                     );
                  begin
                     Item.Value := Ptr.all'Unchecked_Access;
                  end;
               end if;
            end if;
         end;
      elsif This in Universal_Real then
         declare
            Object : Universal_Real renames Universal_Real (This);
         begin
            if Object.Value /= null then
               Object.Negative := Is_Negative (Object.Value.all);
               Object.Numerator :=
                   Store
                   (  Context'Access,
                      Get_Numerator (Object.Value.all)
                   );
               Object.Denominator :=
                  Store
                  (  Context'Access,
                     Get_Denominator (Object.Value.all)
                  );
               Free (Object.Value);
            end if;
         end;
      end if;
   end Store;

   procedure Store
             (  Context : in out Ada_Expression;
                List    : in out Argument_List
             )  is
   begin
      for Argument in List'Range loop
         Store (Context, List (Argument));
      end loop;
   end Store;

   use Operator_Tables;
   use Reserved_Words_Tables;
begin
   Add_Operator     (Infixes,   "|",         Alternative, 0,  0);
   Add_Operator     (Infixes,   "xor",       Logical_Xor, 1,  1);
   Add_Operator     (Infixes,   "in",        Member,      2,  2);
   Add_Operator     (Infixes,   "..",        Ellipsis,    3,  3);

   Add_Operator     (Infixes,   "=",         EQ,          4,  4);
   Add_Operator     (Infixes,   "/=",        NE,          4,  4);
   Add_Operator     (Infixes,   "<",         LT,          4,  4);
   Add_Operator     (Infixes,   "<=",        LE,          4,  4);
   Add_Operator     (Infixes,   ">",         GT,          4,  4);
   Add_Operator     (Infixes,   ">=",        GE,          4,  4);

   Add_Operator     (Infixes,   "+",         Add,         5,  5);
   Add_Operator     (Infixes,   "-",         Sub,         5,  5);
   Add_Operator     (Infixes,   "&",         Concatenate, 5,  5);

   Add_Operator     (Prefixes,  "+",         Plus,        6,  6);
   Add_Operator     (Prefixes,  "-",         Minus,       6,  6);

   Add_Operator     (Infixes,   "*",         Mul,         7,  7);
   Add_Operator     (Infixes,   "/",         Div,         7,  7);
   Add_Operator     (Infixes,   "mod",       Modulus,     7,  7);
   Add_Operator     (Infixes,   "rem",       Remainder,   7,  7);

   Add_Operator     (Prefixes,  "abs",       Abs_Value,   8,  8);
   Add_Operator     (Prefixes,  "not",       Logical_Not, 8,  8);
   Add_Operator     (Infixes,   "**",        Pow,         8,  8);

   Add_Operator     (Prefixes,  "new",       Allocator,   9,  9);
   Add_Operator     (Infixes,   "'",         Attribute,  10, 10);
   Add_Operator     (Infixes,   ".",         Component,  12, 12);
   Add_Index        (Infixes,   "(",         Left_Index, 11);

   Add_Comma        (Infixes,   ",",         Comma);
   Add_Ligature     (Infixes,   "=>",        Associate);
   Add_Bracket      (Prefixes,  "(",         Left_Bracket);
   Add_Bracket      (Prefixes,  "[",         Left_Square_Bracket);
   Add_Bracket      (Postfixes, ")",         Right_Bracket);
   Add_Bracket      (Postfixes, "]",         Right_Square_Bracket);

   Add_Premodifier  (Postfixes, "and",       Logical_And);
   Add_Premodifier  (Postfixes, "or",        Logical_Or);
   Add_Premodifier  (Postfixes, "not",       Logical_Not);
   Add_Postmodifier (Infixes,   "then",      And_Then);
   Add_Postmodifier (Infixes,   "else",      Or_Else);

   Add_Postmodifier (Prefixes,  "delta",     Keyword_Delta);
   Add_Postmodifier (Postfixes, "record",    Keyword_Record);

   Add_Semicolon    (Infixes,   "with",      Extend,
                                             Parsers.Sublist_Open, 1);

   Add_Postmodifier (Infixes,   "is",        Reserved);
   Add_Postmodifier (Infixes,   "loop",      Reserved);
   Add_Postmodifier (Infixes,   "do",        Reserved);

   Keyword_Tables.Add (Keywords, "case",     Case_Keyword);
   Keyword_Tables.Add (Keywords, "declare",  Declare_Keyword);
   Keyword_Tables.Add (Keywords, "if",       If_Keyword);
   Keyword_Tables.Add (Keywords, "raise",    Raise_Keyword);
   Keyword_Tables.Add (Keywords, "for",      For_Keyword);
   Keyword_Tables.Add (Keywords, "parallel", Parallel_Keyword);

   Add (Operators, "&",   Concatenate);
   Add (Operators, "*",   Mul);
   Add (Operators, "**",  Pow);
   Add (Operators, "+",   Add);
   Add (Operators, "-",   Sub);
   Add (Operators, "/",   Div);
   Add (Operators, "/=",  NE);
   Add (Operators, "<",   LT);
   Add (Operators, "<=",  LE);
   Add (Operators, "=",   EQ);
   Add (Operators, ">",   GT);
   Add (Operators, ">=",  GE);
   Add (Operators, "abs", Abs_Value);
   Add (Operators, "and", Logical_And);
   Add (Operators, "mod", Modulus);
   Add (Operators, "not", Logical_Not);
   Add (Operators, "or",  Logical_Or);
   Add (Operators, "rem", Remainder);
   Add (Operators, "xor", Logical_Xor);

   Add (Reserved_Words, "abort",        Abort_Word);
   Add (Reserved_Words, "abs",          Abs_Word);
   Add (Reserved_Words, "abstract",     Abstract_Word);
   Add (Reserved_Words, "accept",       Accept_Word);
   Add (Reserved_Words, "access",       Access_Word);
   Add (Reserved_Words, "aliased",      Aliased_Word);
   Add (Reserved_Words, "all",          All_Word);
   Add (Reserved_Words, "and",          And_Word);
   Add (Reserved_Words, "array",        Array_Word);
   Add (Reserved_Words, "at",           At_Word);
   Add (Reserved_Words, "begin",        Begin_Word);
   Add (Reserved_Words, "body",         Body_Word);
   Add (Reserved_Words, "case",         Case_Word);
   Add (Reserved_Words, "constant",     Constant_Word);
   Add (Reserved_Words, "declare",      Declare_Word);
   Add (Reserved_Words, "delay",        Delay_Word);
   Add (Reserved_Words, "delta",        Delta_Word);
   Add (Reserved_Words, "digits",       Digits_Word);
   Add (Reserved_Words, "do",           Do_Word);
   Add (Reserved_Words, "else",         Else_Word);
   Add (Reserved_Words, "elsif",        Elsif_Word);
   Add (Reserved_Words, "end",          End_Word);
   Add (Reserved_Words, "entry",        Entry_Word);
   Add (Reserved_Words, "exception",    Exception_Word);
   Add (Reserved_Words, "exit",         Exit_Word);
   Add (Reserved_Words, "for",          For_Word);
   Add (Reserved_Words, "function",     Function_Word);
   Add (Reserved_Words, "generic",      Generic_Word);
   Add (Reserved_Words, "goto",         Goto_Word);
   Add (Reserved_Words, "if",           If_Word);
   Add (Reserved_Words, "in",           In_Word);
   Add (Reserved_Words, "interface",    Interface_Word);
   Add (Reserved_Words, "is",           Is_Word);
   Add (Reserved_Words, "limited",      Limited_Word);
   Add (Reserved_Words, "loop",         Loop_Word);
   Add (Reserved_Words, "mod",          Mod_Word);
   Add (Reserved_Words, "new",          New_Word);
   Add (Reserved_Words, "not",          Not_Word);
   Add (Reserved_Words, "null",         Null_Word);
   Add (Reserved_Words, "of",           Of_Word);
   Add (Reserved_Words, "or",           Or_Word);
   Add (Reserved_Words, "others",       Others_Word);
   Add (Reserved_Words, "out",          Out_Word);
   Add (Reserved_Words, "overriding",   Overriding_Word);
   Add (Reserved_Words, "package",      Package_Word);
   Add (Reserved_Words, "parallel",     Parallel_Word);
   Add (Reserved_Words, "pragma",       Pragma_Word);
   Add (Reserved_Words, "private",      Private_Word);
   Add (Reserved_Words, "procedure",    Procedure_Word);
   Add (Reserved_Words, "protected",    Protected_Word);
   Add (Reserved_Words, "raise",        Raise_Word);
   Add (Reserved_Words, "range",        Range_Word);
   Add (Reserved_Words, "record",       Record_Word);
   Add (Reserved_Words, "rem",          Rem_Word);
   Add (Reserved_Words, "renames",      Renames_Word);
   Add (Reserved_Words, "requeue",      Requeue_Word);
   Add (Reserved_Words, "return",       Return_Word);
   Add (Reserved_Words, "reverse",      Reverse_Word);
   Add (Reserved_Words, "select",       Select_Word);
   Add (Reserved_Words, "separate",     Separate_Word);
   Add (Reserved_Words, "some",         Some_Word);
   Add (Reserved_Words, "subtype",      Subtype_Word);
   Add (Reserved_Words, "synchronized", Synchronized_Word);
   Add (Reserved_Words, "tagged",       Tagged_Word);
   Add (Reserved_Words, "task",         Task_Word);
   Add (Reserved_Words, "terminate",    Terminate_Word);
   Add (Reserved_Words, "then",         Then_Word);
   Add (Reserved_Words, "type",         Type_Word);
   Add (Reserved_Words, "until",        Until_Word);
   Add (Reserved_Words, "use",          Use_Word);
   Add (Reserved_Words, "when",         When_Word);
   Add (Reserved_Words, "while",        While_Word);
   Add (Reserved_Words, "with",         With_Word);
   Add (Reserved_Words, "xor",          Xor_Word);

end Parsers.Generic_Ada_Parser;
