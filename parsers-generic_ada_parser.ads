--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser                  Luebeck            --
--  Interface                                      Winter, 2004       --
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
--
--  This package  provides a full Ada  expression parser.  The result of
--  parsing is stored in a parsing tree allocated on a stack pool.
--
--  (o)  Initial_Size, - Of the stack segments
--  (o)  Items_Number, - The number of items in a segment
--  (o)  Argument_Frame_Segment_Size,
--  (o)  Argument_Frame_Minimal_Size,
--  (o)  Argument_Frame_Increment,
--  (o)  Argument_Stub_Minimal_Size,
--  (o)  Argument_Stub_Increment,
--  (o)  Operation_Segment_Size,
--  (o)  Operation_Minimal_Size,
--  (o)  Operation_Increment.
--
--  The  formal  parameters  Initial_Size  and  Items_Number control the
--  stack  pool  allocation.  The  stack  pool  consists  of   segments.
--  Initial_Size  determines  the  initial  default  size  of  a   newly
--  allocated segment. If this size is less than the size of the  object
--  being  allocated  the  default  size  is  set  to  the  object  size
--  multiplied to Items_Number. This value will  then  be  used  as  the
--  default size for all further segments.
--
--  The formal parameters Argument_* control argument  stack  allocation
--  policy,  see  Parsers.Generic_Argument.Segmented_Stack.  The  formal
--  parameters Operation_* control operation  stack  allocation  policy,
--  see Parsers.Generic_Operation.Segmented_Stack.
--
with Ada.Unchecked_Deallocation;
with Parsers.Generic_Lexer.Ada_2005_Blanks;
with Parsers.Generic_Source;
with Parsers.Generic_Token.Segmented_Lexer;
with Stack_Storage;
with Tables.UTF8_Names;
with Parsers.Generic_Source.Get_Token;
with System.Storage_Elements;

generic
   with package Sources is new Parsers.Generic_Source (<>);
   Initial_Size : System.Storage_Elements.Storage_Count := 2048;
   Items_Number                : Positive := 128;
   Argument_Frame_Segment_Size : Positive := 128;
   Argument_Frame_Minimal_Size : Positive := 64;
   Argument_Frame_Increment    : Natural  := 50;
   Argument_Stub_Minimal_Size  : Positive := 64;
   Argument_Stub_Increment     : Natural  := 50;
   Operation_Segment_Size      : Positive := 128;
   Operation_Minimal_Size      : Positive := 64;
   Operation_Increment         : Natural  := 50;
package Parsers.Generic_Ada_Parser is
--
-- Operations -- All the operations supported
--
   type Operations is
        (     -- Operators according to ARM 4.5
           Logical_And, Logical_Or, Logical_Xor, -- Logical operators
           And_Then, Or_Else,                    -- Short-circuit
           EQ, NE, LT, LE, GE, GT,               -- Relational
           Member, Not_Member,                   -- Membership tests
           Add, Sub, Concatenate,                -- Binary adding
           Plus, Minus,                          -- Unary adding
           Mul, Div, Modulus, Remainder,         -- Multiplying
           Pow, Abs_Value, Logical_Not,          -- Highest precedence
              -- Hard-wired operators
           Allocator,           -- Allocator "new"
           Allocator_Subpool,   -- Allocator "new (<subpool>)"
           Alternative,         -- Alternative separator "|"
           Attribute,           -- Attribute specification "'"
           Ellipsis,            -- Range ".."
           Component,           -- Component extraction "."
              -- Order and aggregate brackets
           Left_Bracket,        Right_Bracket,    -- Brackets ()
              -- Container aggregate brackets
           Left_Square_Bracket, Right_Square_Bracket,
              -- Index brackets
           Left_Index,                           -- Brackets f()
              -- Commas and ligatures
           Comma, Associate, Extend,             -- ",", "=>", "with"
           Extend_Delta,                         -- "with delta"
              -- Inverses
           Add_Inv, Mul_Inv,                     -- 0-x, 1/x
              -- Keywords
           Keyword_Delta,                        -- "delta"
           Keyword_Record,                       -- "record"
           Reserved                              -- "is", "loop", "do"
        );
   function Image (Operation : Operations) return String;
   subtype Logical     is Operations range Logical_And..Or_Else;
   subtype Relational  is Operations range EQ..Not_Member;
   subtype Additive    is Operations range Add..Concatenate;
   subtype Unary       is Operations range Plus..Minus;
   subtype Multiplying is Operations range Mul..Remainder;
   subtype Highest     is Operations range Pow..Logical_Not;
--
-- "and" -- Checks operation associations
--
--     Left  - The operation on the left
--     Right - The operation on the right
--
-- Returns :
--
--     True if Left is compatible with Right
--
   function "and" (Left, Right : Operations) return Boolean;
--
-- Is_Commutative -- Commutative operations
--
--     Left  - The operation on the left
--     Right - The operation on the right
--
-- Commutative groups:
--
--     {+, -}, {*, /}, {and}, {or}, {xor}, {.}, {|}, {&}, {and then},
--     {or else}
--
-- Though  A.B and some  other operations are not commutative,  it makes
-- sense to  treat  it  as if  it were  commutative  to parse  A.B.C  as
-- "."(A,B,C).
--
-- Returns :
--
--     True if Left and Right are from a group
--
   function Is_Commutative (Left, Right : Operations) return Boolean;
--
-- Is_Inverse -- Of a group
--
--     Operation - To be tested
--
-- Returns :
--
--     True if - or /
--
   function Is_Inverse (Operation : Operations) return Boolean;
--
-- Group_Inverse -- Of a group
--
--     Operation - An operation of either {+, -} or {*, /}
--
-- Returns :
--
--     Add_Inv for + or -
--     Mul_Inv for * or /
--
   function Group_Inverse (Operation : Operations) return Operations;
--
-- Priorities -- The levels of association
--
   type Priorities is mod 20;
--
-- Parsing  tree.  To  make  it  efficient  the  nodes  of  the tree are
-- allocated  on  a  stack.  The stack is provided by a stack pool. This
-- allows  to  remove the whole tree by deallocating its first allocated
-- node or any other pool object allocated before it. Tree_Pool  is  the
-- stack storage pool used for this.
--
   Tree_Pool : Stack_Storage.Pool (Initial_Size, Items_Number);
--
-- Node -- Of a parsing tree
--
   type Node is abstract tagged limited null record;
--
-- Node_Class -- The classes of nodes used to avoid class-wide testing
--
   type Node_Class is
        (  Term_Node,         -- Expresson terms
           Expression_Node,   -- Expression'Class
           If_Node,           -- If_Expression'Class
           Case_Node,         -- Case_Expression'Class
           Declare_Node,      -- Declare_Expression'Class
           Declare_Item_Node, -- Declare_Item'Class
           For_Node,          -- For_Expression'Class
           Raise_Node         -- Rase_Expression'Class
       );
--
-- Get_Class -- The node class
--
--    Item - The node
--
-- Returns :
--
--    The string representation of the node
--
   function Get_Class (Item : Node) return Node_Class is abstract;
--
-- Image -- To be used for tree output
--
--    Item - The node
--
-- Returns :
--
--    The string representation of the node
--
   function Image (Item : Node) return String is abstract;
--
-- Node_Ptr -- Pointer to a node, class-wide, Tree_Pool specific
--
   type Node_Ptr is access Node'Class;
   for Node_Ptr'Storage_Pool use Tree_Pool;
   procedure Free is
      new Standard.Ada.Unchecked_Deallocation (Node'Class, Node_Ptr);
--
-- Tokens -- The lexical tokens
--
   package Tokens is
      new Parsers.Generic_Token
          (  Operation_Type => Operations,
             Argument_Type  => Node_Ptr,
             Priority_Type  => Priorities,
             Sources        => Sources
          );
   use Tokens;
   --
   -- Mark -- Marks the pool state for quick tree removal
   --
   type Mark is new Node with null record;
   function Get_Class (Item : Mark) return Node_Class;
   function Image (Item : Mark) return String;
   --
   -- Term -- Expression term, abstract base type
   --
   type Term is abstract new Node with null record;
   function Get_Class (Item : Term) return Node_Class;
   --
   -- Literal -- Expression literal, abstract base type
   --
   type Literal (Length : Natural) is abstract new Term with record
      Value : String (1..Length);
   end record;
   --
   -- Numeric_Literal -- A numeric literal, abstract base type
   --
   -- The  field  Malformed is set to true to indicate a syntax error in
   -- the  literal, which was detected and corrected. The field Exponent
   -- is set to Integer'First or Integer'Last  when  the  exponent  part
   -- cannot be represented (it is too big). The  field  Value  contains
   -- the mantissa, which is always whole.
   --
   subtype Number_Base is Integer range 2..16;
   type Numeric_Literal is abstract new Literal with record
      Malformed : Boolean     := False;
      Base      : Number_Base := 10;
      Exponent  : Integer;
   end record;
   function Image (Item : Numeric_Literal) return String;
   --
   -- Integer_Literal -- Represents integer literals
   --
   type Integer_Literal is new Numeric_Literal with null record;
   function Image (Item : Integer_Literal) return String;
   --
   -- Real_Literal -- Represents real literals
   --
   type Real_Literal is new Numeric_Literal with null record;
   function Image (Item : Real_Literal) return String;
   --
   -- String_Literal -- Represents string literals
   --
   type Text_Literal is abstract new Literal with null record;
   type String_Literal is new Text_Literal with null record;
   function Image (Item : String_Literal) return String;
   --
   -- Character_Literal -- Represents character literals
   --
   type Character_Literal is new Text_Literal with null record;
   function Image (Item : Character_Literal) return String;
   --
   -- Identifier -- Represents identifiers
   --
   type Identifier (Length : Positive) is new Term with record
      Malformed : Boolean := False;
      Value     : String (1..Length);
   end record;
   function Image (Item : Identifier) return String;
   --
   -- Missing operand -- Represents an assumed operand
   --
   type Missing_Operand is new Term with null record;
   function Image (Item : Missing_Operand) return String;
   --
   -- Box_Choice -- The box choice <>
   --
   type Box_Choice is new Term with null record;
   function Image (Item : Box_Choice) return String;
   --
   -- Null_Aggregate -- Null container aggregate
   --
   type Null_Aggregate is new Term with null record;
   function Image (Item : Null_Aggregate) return String;
   --
   -- Expression -- Non-terminal node
   --
   type Composite is abstract new Node with null record;
   type Argument_List is array (Positive range <>) of Argument_Token;
   type Expression (Count : Positive) is new Composite with record
      Operation : Operations;
      Location  : Sources.Location_Type;
      Operands  : Argument_List (1..Count);
   end record;
   function Get_Class (Item : Expression) return Node_Class;
   function Image (Item : Expression) return String;
   --
   -- Case_Expression -- The case-expression
   --
   type Alternative_Pair is record
      Guard : Argument_Token;
      Value : Argument_Token;
   end record;
   type Guarded_List is array (Positive range <>) of Alternative_Pair;
   type Case_Expression
        (  Count      : Positive;
           Has_Others : Boolean
        )  is new Composite with
   record
      Selector     : Argument_Token;
      Alternatives : Guarded_List (1..Count);
      case Has_Others is
         when True =>
            Others_Alternative : Argument_Token;
         when False =>
            null;
      end case;
   end record;
   function Get_Class (Item : Case_Expression) return Node_Class;
   function Image (Item : Case_Expression) return String;

   type Declare_Item is abstract new Composite with record
      Name : Argument_Token;
   end record;
   type Declare_Item_Ptr is access constant Declare_Item'Class;
   function Get_Class (Item : Declare_Item) return Node_Class;

   type Declare_Renaming_Item is new Declare_Item with record
      Object : Argument_Token;
   end record;
   function Image (Item : Declare_Renaming_Item) return String;

   type Declare_Object_Type is (Immutable, Initialized, Uninitialized);
   type Declare_Object_Item
        (  Kind_Of : Declare_Object_Type
        )  is new Declare_Item with
   record
      Object : Argument_Token;
      case Kind_Of is
         when Immutable | Initialized =>
            Value : Argument_Token;
         when Uninitialized =>
            null;
      end case;
   end record;
   function Image (Item : Declare_Object_Item) return String;

   type Declare_Token is record
      Value    : Declare_Item_Ptr;
      Location : Sources.Location_Type;
   end record;
   type Declare_Item_Array is
      array (Positive range <>) of Declare_Token;
   --
   -- Declare_Expression -- The declare-expression
   --
   type Declare_Expression (Count : Natural) is
      new Composite with
   record
      Expression : Argument_Token;
      Items      : Declare_Item_Array (1..Count);
   end record;
   function Get_Class (Item : Declare_Expression) return Node_Class;
   function Image (Item : Declare_Expression) return String;
   --
   -- For_Expression -- The for-expression
   --
   type For_Qualifier is (For_All, For_Some, For_Any);
   type For_Optional is mod 2**8;
   For_Parallel  : constant For_Optional := 2**0;
   For_Chunk     : constant For_Optional := 2**1;
   For_Key       : constant For_Optional := 2**2;
   For_Condition : constant For_Optional := 2**3;
   For_Container : constant For_Optional := 2**4;
   For_Of        : constant For_Optional := 2**5;
   For_Reverse   : constant For_Optional := 2**6;
   For_Range     : constant For_Optional := 2**7;
   type For_Expression
        (  Count     : Natural;
           Options   : For_Optional;
           Qualifier : For_Qualifier
        )  is new Composite with
   record
      Identifier : Argument_Token;
      Range_Type : Argument_Token;          -- If For_Range
      Iterator   : Argument_Token;
      Expression : Argument_Token;
      Condition  : Argument_Token;          -- For_Condition
      Key        : Argument_Token;
      Chunk      : Argument_Token;          -- If For_Parallel
      Aspects    : Guarded_List (1..Count); -- If For_Parallel
   end record;
   function Get_Class (Item : For_Expression) return Node_Class;
   function Image (Item : For_Expression) return String;
   --
   -- If_Expression -- The if-expression
   --
   type If_Expression
        (  Count    : Positive;
           Has_Else : Boolean
        )  is new Composite with
   record
      Alternatives : Guarded_List (1..Count);
      case Has_Else is
         when True =>
            Else_Alternative : Argument_Token;
         when False =>
            null;
      end case;
   end record;
   function Get_Class (Item : If_Expression) return Node_Class;
   function Image (Item : If_Expression) return String;
   --
   -- Raise_Expression -- The raise-expression
   --
   type Raise_Expression (Has_Message : Boolean) is
      new Composite with
   record
      Name : Argument_Token;
      case Has_Message is
         when True =>
            Message : Argument_Token;
         when False =>
            null;
      end case;
   end record;
   function Get_Class (Item : Raise_Expression) return Node_Class;
   function Image (Item : Raise_Expression) return String;
--
-- Check_Spelling -- Of a name, no checks
--
   procedure Check_Spelling (Name : String);
--
-- Check_Matched -- Check if no broken keyword matched
--
   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean;
--
-- Token_Tables -- Case-insensitive tables of tokens
--
   package Token_Tables is new Tokens.Vocabulary.UTF8_Names;
--
-- The tables of prefix, infix and postfix operations
--
   Prefixes  : aliased Token_Tables.Dictionary;
   Infixes   : aliased Token_Tables.Dictionary;
   Postfixes : aliased Token_Tables.Dictionary;
--
-- Lexers -- Table driven lexers
--
   package Lexers is
      new Tokens.Segmented_Lexer
           (  Argument_Frame_Segment_Size,
              Argument_Frame_Minimal_Size,
              Argument_Frame_Increment,
              Argument_Stub_Minimal_Size,
              Argument_Stub_Increment,
              Operation_Segment_Size,
              Operation_Minimal_Size,
              Operation_Increment
          );
--
-- Blank_Skipping_Lexers -- Ones that skip blanks
--
   package Blank_Skipping_Lexers is
      new Lexers.Token_Lexer.Implementation.
          Ada_2005_Blanks (Lexers.Lexer);
--
-- Ada_Expression -- The lexer using our tables
--
   type Ada_Expression is
      new Blank_Skipping_Lexers.Lexer
          (  Prefixes  => Prefixes'Access,
             Infixes   => Infixes'Access,
             Postfixes => Postfixes'Access
          )  with null record;
private
--
-- Call -- Evaluates an operator
--
   function Call
            (  Context   : access Ada_Expression;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token;
--
-- Enclose -- Evaluates an expression in brackets
--
   function Enclose
            (  Context : access Ada_Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token;
--
-- Get_Operand -- Recognizes an operand (float number)
--
   procedure Get_Operand
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Got_It   : out Boolean
             );
--
-- On_Postmodifier -- Overrides the default handling of modifiers
--
   procedure On_Postmodifier
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : in out Tokens.Argument_Token;
                Modifier : Tokens.Operation_Token;
                Got_It   : out Boolean
             );
   procedure On_Postmodifier
             (  Context   : in out Ada_Expression;
                Code      : in out Lexers.Lexer_Source_Type;
                Operation : in out Tokens.Operation_Token;
                Modifier  : Tokens.Operation_Token;
                Got_It    : out Boolean
             );
--
-- On_Premodifier -- Overrides the default handling of modifiers
--
   procedure On_Premodifier
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Token    : in out Lexers.Token_Lexer.Implementation.
                                     Lexical_Token;
                Modifier : Tokens.Operation_Token;
                Got_It   : out Boolean
             );
--
-- On_Missing_Operation -- To deal with "and", "or" etc
--
   procedure On_Missing_Operation
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Modifier : Tokens.Operation_Token;
                Token    : out Lexers.Token_Lexer.Implementation.
                                  Lexical_Token;
                Got_It   : out Boolean
             );
--
-- Keywords -- Keyword tables
--
   type Keyword_Type is
        (  Case_Keyword,
           Declare_Keyword,
           If_Keyword,
           Raise_Keyword,
           For_Keyword,
           Parallel_Keyword
        );
   package Keyword_Raw_Tables is new Tables (Keyword_Type);
   package Keyword_Tables is
      new Keyword_Raw_Tables.UTF8_Names;
   procedure Get_Keyword is
      new Sources.Get_Token (Keyword_Raw_Tables);
end Parsers.Generic_Ada_Parser;
