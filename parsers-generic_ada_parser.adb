--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser                  Luebeck            --
--  Implementation                                 Winter, 2004       --
--                                                                    --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Strings_Edit.Integers;    use Strings_Edit.Integers;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;
with Strings_Edit.UTF8;        use Strings_Edit.UTF8;

with Strings_Edit.UTF8.Categorization;
use  Strings_Edit.UTF8.Categorization;

package body Parsers.Generic_Ada_Parser is
   use Sources;

   Keywords : Keyword_Tables.Dictionary;
--
-- Get_Delimited -- Get delimited text
--
--    Code      - The source
--    Text      - Lower case ASCII text
--    Delimited - Check delimiter after the text
--    Got_It    - True if matched
--
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

   function Call
            (  Context   : access Ada_Expression;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
      use Tokens.Arguments;
   begin
      if Operation.Operation = Allocator and then
         List'Length = 1                     then
         --
         -- Reducing new (<subpool>) ... to new (Subpool ...)
         --
         declare
            Second : constant Node_Ptr := List (List'First).Value;
         begin
            if Second.all in Expression'Class then
               declare
                  use Descriptors;
                  Tail : Expression'Class renames
                         Expression'Class (Second.all);
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
            if Second.all in Expression'Class then
               declare
                  Tail : Expression'Class renames
                         Expression'Class (Second.all);
               begin
                  if Tail.Operation = Left_Bracket then
                     declare
                        Result : constant Node_Ptr :=
                                      new Expression (Tail.Count + 1);
                     begin
                        declare
                           This : Expression renames
                                  Expression (Result.all);
                        begin
                           This.Operation := Attribute;
                           This.Location  := Operation.Location;
                           This.Operands (1) := List (List'First);
                           This.Operands (2..This.Count) :=
                              Tail.Operands;
                        end;
                        return
                        (  Result,
                           Operation.Location & Link (List)
                        );
                     end;
                  end if;
               end;
            end if;
         end;
      end if;
      declare
         Result : constant Node_Ptr := new Expression (List'Length);
         This   : Expression renames Expression (Result.all);
      begin
         This.Operation := Operation.Operation;
         This.Location  := Operation.Location;
         for Argument in List'Range loop
            This.Operands (Integer (Argument)) := List (Argument);
         end loop;
         return (Result, Operation.Location & Link (List));
      end;
   end Call;

   function Enclose
            (  Context : access Ada_Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
   begin
      if List'Length = 1 then
         case Left.Operation is
            when Left_Bracket => -- Check if we can drop the brackets
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
                  if This.all in Expression'Class      then
                     if Operation.Class = Operator and then
                        Operation.Operation.Operation = Attribute
                     then
                        return -- X'(...) case
                        (  This,
                           Left.Location & Right.Location & Link (List)
                        );
                     end if;
                     case Expression'Class (This.all).Operation is
                        when Associate | Extend =>
                           goto Keep;  -- (X => Y) or (X with Y)
                        when others =>
                           return
                           (  This,
                              (  Left.Location
                              &  Right.Location
                              &  Link (List)
                           )  );
                     end case;
                  elsif This.all not in For_Expression'Class then
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
            when others =>
               null;
         end case;
      end if;
<<Keep>>
      declare
         Result : constant Node_Ptr := new Expression (List'Length);
      begin
         declare
            This : Expression renames Expression (Result.all);
         begin
            This.Operation := Left.Operation;
            This.Location  := Left.Location & Right.Location;
            for Argument in List'Range loop
               This.Operands (Integer (Argument)) := List (Argument);
            end loop;
         end;
         return
         (  Result,
            Left.Location & Right.Location & Link (List)
         );
      end;
   end Enclose;

   function Get_Class (Item : Case_Expression) return Node_Class is
   begin
      return Case_Node;
   end Get_Class;

   function Get_Class (Item : Declare_Expression) return Node_Class is
   begin
      return Declare_Node;
   end Get_Class;

   function Get_Class (Item : Declare_Item) return Node_Class is
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

   procedure Get_Case
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Left     : Location_Type
             )  is separate;

   procedure Get_Character_Literal
             (  Code     : in out Lexers.Lexer_Source_Type;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is separate;

   procedure Get_Identifier
             (  Code     : in out Lexers.Lexer_Source_Type;
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

   procedure Get_Numeric_Literal
             (  Code     : in out Lexers.Lexer_Source_Type;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is separate;

   procedure Get_Raise
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Left     : Location_Type
             )  is separate;

   procedure Get_String_Literal
             (  Code     : in out Lexers.Lexer_Source_Type;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
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
            (  Code,
               Line (Pointer..Last),
               Pointer,
               Argument
            );
            Got_It := True;
         when ''' =>
            Get_Character_Literal
            (  Code,
               Line (Pointer..Last),
               Pointer,
               Argument
            );
            Got_It := True;
         when '0'..'9' =>
            Get_Numeric_Literal
            (  Code,
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
               Argument.Value := new Null_Aggregate;
               Got_It := True;
            else
               Got_It := False;
            end if;
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
                  Argument.Value := new Box_Choice;
                  Got_It := True;
               else
                  Get (Line (Pointer..Last), Index, Symbol);
                  if Is_Identifier_Start (Symbol) then
                     Get_Identifier
                     (  Code,
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

   function Image (Item : Mark) return String is
   begin
      return "";
   end Image;

   function Image (Item : Numeric_Literal) return String is
   begin
      if Item.Malformed then
         return "<malformed>";
      elsif Item.Exponent = Integer'First then
         return "<underflown>";
      elsif Item.Exponent = Integer'Last then
         return "<overflown>";
      elsif Item.Base = 10 then
         if Item.Exponent = 0 then
            return Item.Value;
         else
            return Item.Value & "E" & Image (Item.Exponent);
         end if;
      else
         if Item.Exponent = 0 then
            return Image (Item.Base) & '#' & Item.Value & '#';
         else
            return
            (  Image (Item.Base)
            &  '#' & Item.Value & '#'
            &  "E" & Image (Item.Exponent)
            );
         end if;
      end if;
   end Image;

   function Image (Item : Integer_Literal) return String is
   begin
      if Item.Malformed then
         return "<malformed>";
      elsif Item.Exponent = Integer'First then
         return "<underflown>";
      elsif Item.Exponent = Integer'Last then
         return "<overflown>";
      elsif Item.Base = 10 then
         if Item.Exponent = 0 then
            return Item.Value;
         else
            return Item.Value & "E" & Image (Item.Exponent);
         end if;
      else
         if Item.Exponent = 0 then
            return Image (Item.Base) & '#' & Item.Value & '#';
         else
            return
            (  Image (Item.Base)
            &  '#' & Item.Value & '#'
            &  "E" & Image (Item.Exponent)
            );
         end if;
      end if;
   end Image;

   function Image (Item : Real_Literal) return String is
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
         elsif Exponent in -1..1 - S'Length then
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
         return "<malformed>";
      elsif Item.Exponent = Integer'First then
         return "<underflown>";
      elsif Item.Exponent = Integer'Last then
         return "<overflown>";
      else
         declare
            Value : constant String := abs Item.Value;
         begin
            if Item.Base = 10 then
               if Exponent = 0 then
                  return Value;
               else
                  return Value & "E" & Image (Exponent);
               end if;
            elsif Exponent = 0 then
               return Image (Item.Base) & '#' & Value & '#';
            else
               return
               (  Image (Item.Base)
               &  '#'
               &  Value
               &  '#'
               &  "E"
               & Image (Exponent)
               );
            end if;
         end;
      end if;
   end Image;

   function Image (Item : String_Literal) return String is
   begin
      return Quote (Item.Value);
   end Image;

   function Image (Item : Case_Expression) return String is
      function Image (List : Guarded_List) return String is
      begin
         if List'Length = 0 then
            return "";
         else
            return
            (  " when "
            &  Image (List (List'First).Guard.Value.all)
            &  " => "
            &  Image (List (List'First).Value.Value.all)
            &  Image (List (List'First + 1..List'Last))
            );
         end if;
      end Image;
   begin
      if Item.Has_Others then
         return
         (  "(case "
         &  Image (Item.Selector.Value.all)
         &  " when "
         &  Image (Item.Alternatives (1).Guard.Value.all)
         &  " => "
         &  Image (Item.Alternatives (1).Value.Value.all)
         &  Image (Item.Alternatives (2..Item.Count))
         &  ", others => "
         &  Image (Item.Others_Alternative.Value.all)
         &  ")"
         );
      else
         return
         (  "(case "
         &  Image (Item.Selector.Value.all)
         &  " when "
         &  Image (Item.Alternatives (1).Guard.Value.all)
         &  " => "
         &  Image (Item.Alternatives (1).Value.Value.all)
         &  Image (Item.Alternatives (2..Item.Count))
         &  ")"
         );
      end if;
   end Image;

   function Image (Item : Declare_Expression) return String is
      function Image (List : Declare_Item_Array) return String is
      begin
         if List'Length = 0 then
            return "";
         else
            return
            (  " "
            &  Image (List (List'First).Value.all)
            &  ";"
            &  Image (List (List'First + 1..List'Last))
            );
         end if;
      end Image;
   begin
      return
      (  "(declare"
      &  Image (Item.Items)
      &  " begin "
      &  Image (Item.Expression.Value.all)
      &  ")"
      );
   end Image;

   function Image (Item : Declare_Object_Item) return String is
   begin
      case Item.Kind_Of is
         when Immutable =>
            return
            (  Image (Item.Name.Value.all)
            &  " : constant "
            &  Image (Item.Object.Value.all)
            &  " := "
            &  Image (Item.Value.Value.all)
            );
         when Initialized =>
            return
            (  Image (Item.Name.Value.all)
            &  " : "
            &  Image (Item.Object.Value.all)
            &  " :=  "
            &  Image (Item.Value.Value.all)
            );
         when Uninitialized =>
            return
            (  Image (Item.Name.Value.all)
            &  " : "
            &  Image (Item.Object.Value.all)
            );
      end case;
   end Image;

   function Image (Item : Declare_Renaming_Item) return String is
   begin
      return
      (  Image (Item.Name.Value.all)
      &  " renames "
      &  Image (Item.Object.Value.all)
      );
   end Image;

   function Image (Item : Character_Literal) return String is
   begin
      return ''' & Item.Value & ''';
   end Image;

   function Image (Item : Identifier) return String is
   begin
      if Item.Malformed then
         return "<malformed>";
      else
         return Item.Value;
      end if;
   end Image;

   function Image (Item : For_Expression) return String is

      function Do_For return String is
      begin
         case Item.Qualifier is
            when For_All =>
               return "for all ";
            when For_Some =>
               return "for some ";
            when For_Any =>
               return "for ";
         end case;
      end Do_For;

      function Do_In return String is
         function Do_Reverse return String is
         begin
            if 0 = (Item.Options and For_Reverse) then
               return "";
            else
               return "reverse ";
            end if;
         end Do_Reverse;
      begin
         if 0 /= (Item.Options and For_Of) then
            return " of " & Do_Reverse;
         elsif 0 = (Item.Options and For_Range) then
            return " in " & Do_Reverse;
         else
            return  " in "    &  Image (Item.Range_Type.Value.all) &
                    " range " & Do_Reverse;
         end if;
      end Do_In;

      function Par return String is
         function Aspect return String is
            function Image (List : Guarded_List; First : Boolean)
               return String is
            begin
               if List'Length = 0 then
                  return "";
               elsif First then
                  return
                  (  Image (List (List'First).Guard.Value.all)
                  &  " => "
                  &  Image (List (List'First).Value.Value.all)
                  &  Image (List (List'First + 1..List'Last), False)
                  );
               else
                  return
                  (  ", "
                  &  Image (List (List'First).Guard.Value.all)
                  &  " => "
                  &  Image (List (List'First).Value.Value.all)
                  &  Image (List (List'First + 1..List'Last), False)
                  );
               end if;
            end Image;
         begin
            if Item.Count = 0 then
               return "";
            else
               return " with " & Image (Item.Aspects, True);
            end if;
         end Aspect;

         function Chunk return String is
         begin
            if 0 = (Item.Options and For_Chunk) then
               return "";
            else
               return " (" & Image (Item.Chunk.Value.all) & ")";
            end if;
         end Chunk;
      begin
         if 0 = (Item.Options and For_Parallel) then
            return "";
         else
            return "parallel" & Chunk & Aspect & " ";
         end if;
      end Par;

      function Key return String is
      begin
         if 0 = (Item.Options and For_Key) then
            return "";
         else
            return " use " & Image (Item.Key.Value.all);
         end if;
      end Key;

      function Condition return String is
      begin
         if 0 = (Item.Options and For_Condition) then
            return "";
         else
            return " when " & Image (Item.Condition.Value.all);
         end if;
      end Condition;

   begin
      return
      (  Par
      &  Do_For
      &  Image (Item.Identifier.Value.all)
      &  Do_In
      &  Image (Item.Iterator.Value.all)
      &  Key
      &  Condition
      &  " => "
      &  Image (Item.Expression.Value.all)
      );
   end Image;

   function Image (Item : If_Expression) return String is
      function Image (List : Guarded_List; First : Boolean)
         return String is
      begin
         if List'Length = 0 then
            return "";
         elsif First then
            return
            (  "if "
            &  Image (List (List'First).Guard.Value.all)
            &  " then "
            &  Image (List (List'First).Value.Value.all)
            &  Image (List (List'First + 1..List'Last), False)
            );
         else
            return
            (  " elsif "
            &  Image (List (List'First).Guard.Value.all)
            &  " then "
            &  Image (List (List'First).Value.Value.all)
            &  Image (List (List'First + 1..List'Last), False)
            );
         end if;
      end Image;
   begin
      if Item.Has_Else then
         return
         (  "("
         &  Image (Item.Alternatives, True)
         &  " else "
         &  Image (Item.Else_Alternative.Value.all)
         &  ")"
         );
      else
         return "(" & Image (Item.Alternatives, True) & ")";
      end if;
   end Image;

   function Image (Item : Raise_Expression) return String is
   begin
      if Item.Has_Message then
         return
         (  "(raise "
         &  Image (Item.Name.Value.all)
         &  " with "
         &  Image (Item.Message.Value.all)
         &  ")"
         );
      else
         return "(raise " & Image (Item.Name.Value.all) & ")";
      end if;
   end Image;

   function Image (Item : Missing_Operand) return String is
   begin
      return "<missing>";
   end Image;

   function Image (Item : Null_Aggregate) return String is
   begin
      return "[]";
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

   function Image
            (  List      : Argument_List;
               Operation : Operations;
               Spacer    : String := " "
            )  return String is
   begin
      case List'Length is
         when 1 =>
            return Image (Operation)                   &
                   " "                                 &
                   Image (List (List'First).Value.all);
         when 2 =>
            return Image (List (List'First).Value.all) &
                   Spacer                              &
                   Image (Operation)                   &
                   Spacer                              &
                   Image (List (List'Last).Value.all);
         when others =>
            return Image (List (List'First).Value.all) &
                   Spacer                              &
                   Image (Operation)                   &
                   Spacer                              &
                   Image
                   (  List (List'First + 1..List'Last),
                      Operation,
                      Spacer
                   );
      end case;
   end Image;

   function Image
            (  List  : Argument_List;
               First : Boolean := True
            )  return String is
   begin
      if List'Length = 0 then
         return "";
      elsif List (List'First).Value.all in Expression'Class and then
            (  Expression'Class (List (List'First).Value.all).Operation
            =  Extend
            )  then
         return
         (  " "
         &  Image (List (List'First).Value.all)
         &  Image (List (List'First + 1..List'Last), False)
         );
      else
         if First then
            return
            (  Image (List (List'First).Value.all)
            &  Image (List (List'First + 1..List'Last), False)
            );
         else
            return
            (  ", "
            &  Image (List (List'First).Value.all)
            &  Image (List (List'First + 1..List'Last), False)
            );
         end if;
      end if;
   end Image;

   function Image (Item : Box_Choice) return String is
   begin
      return "<>";
   end Image;

   function Image (Item : Expression) return String is
   begin
      case Item.Operation is
         when Logical_And | Logical_Xor | Logical_Or | Concatenate |
              Remainder   | Modulus     | And_Then   | Or_Else     |
              Add  | Sub  | Mul  | Div  | Pow  |
              EQ   | NE   | GT   | GE   | LT   |  LE  =>
            return "(" & Image (Item.Operands, Item.Operation) & ")";
         when Ellipsis | Component =>
            return Image (Item.Operands, Item.Operation, "");
         when Extend =>
            return "with " & Image (Item.Operands);
         when Extend_Delta =>
            return "with delta " & Image (Item.Operands);
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
                           return
                           (  Image (List (List'First).Value.all)
                           &  "'"
                           &  Image (List (List'Last).Value.all)
                           );
                        end if;
                     end;
                  elsif Second not in Composite'Class and then
                        Second not in Literal'Class       then
                     return
                     (  Image (List (List'First).Value.all)
                     &  "'"
                     &  Image (List (List'Last).Value.all)
                     );
                  end if;
               end;
            end if;
            return
            (  Image (Item.Operands (Item.Operands'First).Value.all)
            &  "'("
            &  Image
               (  Item.Operands
                  (  Item.Operands'First + 1
                  .. Item.Operands'Last
               )  )
            &  ")"
            );
         when Associate   | Alternative | Member | Not_Member =>
            return Image (Item.Operands, Item.Operation);
         when Left_Bracket =>
            return "(" & Image (Item.Operands) & ")";
         when Left_Square_Bracket =>
            return "[" & Image (Item.Operands) & "]";
         when Plus | Minus | Abs_Value | Add_Inv | Mul_Inv =>
            return
            (  Image (Item.Operation)
            &  "("
            &  Image (Item.Operands (1).Value.all)
            &  Image (Item.Operands (2..Item.Count))
            &  ")"
            );
         when Logical_Not | Allocator | Allocator_Subpool =>
            return
            (  Image (Item.Operation)
            &  " ("
            &  Image (Item.Operands)
            &  ")"
            );
         when Right_Bracket        |
              Right_Square_Bracket |
              Comma                |
              Keyword_Delta        |
              Keyword_Record       |
              Reserved             =>
            return "";
         when Left_Index =>
            return
            (  Image (Item.Operands (Item.Operands'First).Value.all)
            &  " ("
            &  Image
               (  Item.Operands
                  (  Item.Operands'First + 1
                  .. Item.Operands'Last
               )  )
            &  ")"
            );
      end case;
   end Image;

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
            Token  := (Operator, Modifier, 2, 2);
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
               Free (Argument.Value);
               Argument.Value := new Identifier (11);
               Identifier (Argument.Value.all).Value := "null record";
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
                     3,
                     3
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
                     3,
                     3
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

begin
   Add_Operator     (Infixes,   "|",         Alternative, 0,  0);
   Add_Operator     (Infixes,   "in",        Member,      1,  1);
   Add_Operator     (Infixes,   "..",        Ellipsis,    2,  2);
   Add_Operator     (Infixes,   "xor",       Logical_Xor, 3,  3);

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

end Parsers.Generic_Ada_Parser;
