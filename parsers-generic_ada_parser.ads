--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser                  Luebeck            --
--  Interface                                      Winter, 2004       --
--                                                                    --
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
with Parsers.Generic_Lexer.Ada_2005_Blanks;
with Parsers.Generic_Source;
with Parsers.Generic_Token.Segmented_Lexer;
with Tables.Names;
with Tables.UTF8_Names;
with Parsers.Generic_Source.Get_Token;
with Unbounded_Unsigneds;

with System.Storage_Pools;  use System.Storage_Pools;
with Unbounded_Integers;    use Unbounded_Integers;
with Unbounded_Rationals;   use Unbounded_Rationals;

with Strings_Edit.Lexicographical_Order;
use  Strings_Edit.Lexicographical_Order;

generic
   with package Sources is new Parsers.Generic_Source (<>);
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
   subtype Relational  is Operations range EQ..GT;
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
-- Node -- Of an expression parsing tree
--
   type Node is abstract tagged null record;
--
-- Node_Class -- The classes of nodes used to avoid class-wide testing
--
   type Node_Class is
        (  Term_Node,         -- Expresson terms
           Expression_Node,   -- Expression'Class
           If_Node,           -- If_Expression'Class
           Case_Node,         -- Case_Expression'Class
           Declare_Node,      -- Declare_Expression'Class
           Declare_Item_Node, -- Abstract_Declare_Item'Class
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
--    Item       - The node
--    Max_Length - The maximum output length before truncated
--
-- Returns :
--
--    The string representation of the node
--
   subtype Output_Length is Integer range 5..Integer'Last;
   function Image
            (  Item       : Node'Class;
               Max_Length : Output_Length := Output_Length'Last
            )  return String;
--
-- Put -- Textual representation into a string
--
--    Destination - The string to put textual representation of a node
--    Pointer     - The string location to start at
--    Item        - The node
--
-- This procedure puts a string representation of the node Item starting
-- at Pointer. Pointer is advanced after successful completion.
--
-- Exceptions :
--
--    Layout_Error - Pointer is out of range
--                   Destination'First..Destination'Last + 1
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Node
             )  is abstract;
--
-- Node_Ptr -- Pointer to an expression node, class-wide, pool specific
--
   type Node_Ptr is access constant Node'Class;
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

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Argument_Token
             );
--
-- Mark -- Marks the pool state for quick tree removal
--
   type Mark is new Node with null record;
   function Get_Class (Item : Mark) return Node_Class;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Mark
             );
------------------------------------------------------------------------
-- Attribute_Type -- Language defined attributes
--
   type Attribute_Type is
        (  Class_Attribute,
           No_Attribute,              -- No attribute
           Base_Attribute,

           Dimension_Range_Attribute, -- 'Range (<expression>)
           Base_Range_Attribute,      -- 'Base{'Base}'Range
           Range_Attribute,           -- 'Range

           Access_Attribute,             Adjacent_Attribute,
           Address_Attribute,            Aft,
           Alignment_Attribute,

           Bit_Order_Attribute,          Body_Version_Attribute,

           Callable_Attribute,           Caller_Attribute,
           Ceiling_Attribute,            Component_Size_Attribute,
           Compose_Attribute,            Constrained_Attribute,
           Copy_Sign_Attribute,          Count_Attribute,

           Define_Attribute,             Delta_Attribute,
           Denorn_Attribute,             Digits_Attribute,

           Enum_Rep_Attribute,           Enum_Val_Attribute,
           Exponent_Attribute,           External_Tag_Attribute,

           First_Attribute,              First_Bit_Attribute,
           First_Valid_Attribute,        Floor_Attribute,
           Fore_Attribute,               Fraction_Attribute,

           Has_Same_Storage_Attribute,

           Identity_Attribute,           Image_Attribute,
           Index_Attribute,              Input_Attribute,

           Last_Attribute,               Last_Bit_Attribute,
           Last_Valid_Attribute,         Leading_Part_Attribute,
           Length_Attribute,

           Machine_Attribute,            Machine_Emax_Attribute,
           Machine_Emin_Attribute,       Machine_Mantissa_Attribute,
           Machine_Overflow_Attribute,   Machine_Radix_Attribute,
           Machine_Rounding_Attribute,   Machine_Rounds_Attribute,
           Max_Attribute,
           Max_Alignment_For_Allocation_Attribute,
           Max_Size_In_Storage_Elements_Attribute,
           Min_Attribute,                Mod_Attribute,
           Model_Attribute,              Model_Emin_Attribute,
           Model_Epsilon_Attribute,      Model_Mantissa_Attribute,
           Model_Small_Attribute,        Modulus_Attribute,

           Object_Size_Attribute,        Old_Attribute,
           Output_Attribute,             Overlaps_Storage_Attribute,

           Parallel_Reduce_Attribute,    Partition_Id_Attribute,
           Pos_Attribute,                Position_Attribute,
           Pred_Attribute,
           Preelaborable_Initialization_Attribute,
           Priority_Attribute,           Put_Image_Attribute,

           Read_Attribute,               Reduce_Attribute,
           Relative_Deadline_Attribute,  Remainder_Attribute,
           Result_Attribute,             Round_Attribute,
           Rounding_Attribute,

           Safe_First_Attribute,         Safe_Last_Attribute,
           Scale_Attribute,              Scaling_Attribute,
           Signed_Zeros_Attribute,       Size_Attribute,
           Small_Attribute,              Storage_Pool_Attribute,
           Storage_Size_Attribute,       Stream_Size_Attribute,
           Succ_Attribute,

           Tag_Attribute,                Terminated_Attribute,
           Truncation_Attribute,

           Unbiased_Rounding_Attribute,  Unchecked_Access_Attribute,

           Val_Attribute,                Valid_Attribute,
           Version_Attribute,

           Wide_Image_Attribute,         Wide_Value_Attribute,
           Wide_Wide_Image_Attribute,    Wide_Wide_Value_Attribute,
           Wide_Wide_Width_Attribute,    Wide_Width_Attribute,
           Width_Attribute,              Write_Attribute
        );
   subtype Constraint_Attribute is Attribute_Type
      range Class_Attribute..Range_Attribute;
   subtype Subtype_Mark_Attribute is Attribute_Type
      range Class_Attribute..Base_Attribute;
   subtype Scalar_Stubtype_Attribute is Attribute_Type
      range No_Attribute..Range_Attribute;
--
-- Subtype_Mark -- Subtype mark ARM 3.2.2(4).  The subtype mark can have
--                 attributes 'Base, 'Class, 'Range,
--
   type Subtype_Mark
        (  Attribute : Constraint_Attribute := No_Attribute
        )  is
   record
      Name : Argument_Token;
      case Attribute is
         when Dimension_Range_Attribute =>
            Dimension : Argument_Token; -- Name'Range (Dimension)
         when others =>
            null;
      end case;
   end record;
   function Image
            (  Item       : Subtype_Mark;
               Max_Length : Output_Length := Output_Length'Last
            )  return String;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Subtype_Mark
             );
--
-- Subtype_Constraint_Mode -- The constraint added to the subtype mark
--
   type Subtype_Constraint_Mode is
        (  No_Constraint,             -- [<mark>]
           Fixed_Point_Constraint,    -- [<mark>] delta <expression>
           Floating_Point_Constraint, -- [<mark>] digits <expression>
           Index_Constraint,          -- [<mark>] (indices)
           Discriminant_Constraint    -- [<mark>] (discriminants)
        );
   type Subtype_Constraint
        (  Mode : Subtype_Constraint_Mode := No_Constraint
        )  is
   record
      case Mode is
         when No_Constraint =>
            null;
         when Fixed_Point_Constraint =>
            Delta_Constraint  : Argument_Token;
         when Floating_Point_Constraint =>
            Digits_Constraint : Argument_Token;
         when Index_Constraint | Discriminant_Constraint =>
            Constraint        : Argument_Token;
      end case;
   end record;
--
-- Stubtype_Indication_Mode -- Subtype indication mode
--
--    Subtype_Mode       - <subtype>
--    Subtype_Range_Mode - <subtype> range <expression>
--    Range_Mode         - <expression>..<expression>
--    Unconstrained_Mode - <subtype> range <>
--
   type Stubtype_Indication_Mode is
        (  Subtype_Mode,
           Subtype_Range_Mode,
           Range_Mode,
           Unconstrained_Mode
        );
   subtype Constrained_Mode is Stubtype_Indication_Mode
      range Subtype_Mode..Subtype_Range_Mode;
--
-- Subtype_Indication -- A subtype mark with a constraint ARM 3.2.2
--
-- For example:     S delta 0.1 range 0.0..1.0
--                 /  \_______/ \____________/
--              Mark  Constraint  Range_Constraint
--
   type Subtype_Indication
        (  Mode     : Stubtype_Indication_Mode := Subtype_Mode;
           Not_Null : Boolean := False
        )  is
   record
      Mark       : Subtype_Mark;
      Constraint : Subtype_Constraint;
      case Mode is
         when Subtype_Range_Mode =>
            Range_Constraint : Argument_Token;
         when Range_Mode | Subtype_Mode | Unconstrained_Mode =>
            null;
      end case;
   end record;
   function Image
            (  Item       : Subtype_Indication;
               Max_Length : Output_Length := Output_Length'Last
            )  return String;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Subtype_Indication
             );
--
-- Stubtype_Indication_Array -- A list of subtype indications
--
   type Subtype_Indication_Array is
      array (Positive range <>) of Subtype_Indication;
   type Subtype_Indication_Array_Ptr is
      access constant Subtype_Indication_Array;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                List        : Subtype_Indication_Array;
                Prefix      : String := "(";
                Delimiter   : String := ", ";
                Suffix      : String := ")"
             );
--
-- Array_Type_Definition -- Anonymous  array object allowed in a declare
--                          statements.  Such objects  cannot  be access
-- or have aliased components.  The  discriminant  Aliased_Component  is
-- always false when created by parsing an expression.  When the type is
-- used in a wider context it can be set to True.
--
   type Array_Type_Definition
        (  Dimension         : Positive;
           Aliased_Component : Boolean
        )  is
   record
      Component : Subtype_Indication;
      Indices   : Subtype_Indication_Array (1..Dimension);
   end record;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Array_Type_Definition
             );
   type Array_Type_Definition_Ptr is access
      constant Array_Type_Definition;
------------------------------------------------------------------------
-- Specialized expresson tree nodes
--
-- Term -- Expression term, abstract base type
--
   type Term is abstract new Node with null record;
   function Get_Class (Item : Term) return Node_Class;
   --
   -- Target name -- Target name  is the symbol @ used in assignments to
   --                refer the left part of. ARM 5.2.1
   --
   type Target_Name is new Term with null record;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Target_Name
             );
   --
   -- Named_Term -- Named term, abstract base type.
   --
   type Named_Term (Length : Natural) is abstract new Term with record
      Malformed : Boolean := False;
      Value     : String (1..Length);
   end record;
   --
   -- Literal -- Expression literal,  abstract base type.  When constant
   --            folding is enabled literal nodes are not created. Nodes
   -- of Universal_Value'Class are created.
   --
   type Literal is abstract new Named_Term with null record;
--
-- Numeric_Literal -- A numeric literal, abstract base type
--
-- The  field Malformed is set to true to indicate a syntax error in the
-- literal, which was detected and corrected. The field Exponent is  set
-- to Integer'First or Integer'Last when the  exponent  part  cannot  be
-- represented  (it  is too big). The field Value contains the mantissa,
-- which is always whole.
--
   subtype Number_Base is Integer range 2..16;
   type Numeric_Literal is abstract new Literal with record
      Base      : Number_Base := 10;
      Exponent  : Integer;
   end record;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Numeric_Literal
             );
--
-- Integer_Literal -- Represents integer literals
--
   type Integer_Literal is new Numeric_Literal with null record;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Integer_Literal
             );
--
-- Real_Literal -- Represents real literals
--
   type Real_Literal is new Numeric_Literal with null record;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Real_Literal
             );
--
-- String_Literal -- Represents string literals
--
   type Text_Literal is abstract new Literal with null record;
   type String_Literal is new Text_Literal with null record;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : String_Literal
             );
--
-- Character_Literal -- Represents character literals
--
   type Character_Literal is new Text_Literal with null record;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Character_Literal
             );
--
-- Identifier -- Represents identifiers
--
   type Identifier is new Named_Term with null record;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Identifier
             );
--
-- Box_Choice -- The box choice <> ARM 5.5.3
--
   type Box_Choice is new Term with null record;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Box_Choice
             );
--
-- Null_Aggregate -- Null container aggregate ARM 4.3.5
--
   type Null_Aggregate is new Term with null record;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Null_Aggregate
             );
--
-- Universal_Value -- Folder  expression  in universal  types.  When  no
--                    constant   folding   enabled   no    nodes    from
-- Universal_Value'Class are created.
--
   type Universal_Value is abstract new Term with null record;
--
-- Universal_Boolean -- Boolean expression in universal numeric type
--
   type Universal_Boolean is new Universal_Value with record
      Value : Boolean;
   end record;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Universal_Boolean
             );
--
-- Universal_Integer -- Universal integer
--
   type Universal_Integer is new Universal_Value with private;
   function Get_Value (Item : Universal_Integer)
      return Unbounded_Integer;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Universal_Integer
             );
--
-- Universal_Real -- Universal real
--
   type Universal_Real is new Universal_Value with private;
   function Get_Value (Item : Universal_Real)
      return Unbounded_Rational;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Universal_Real
             );
------------------------------------------------------------------------
-- Composite -- Non-terminal node
--
   type Composite is abstract new Node with null record;

   type Argument_List is array (Positive range <>) of Argument_Token;
   function Image
            (  Item       : Argument_List;
               Max_Length : Output_Length := Output_Length'Last
            )  return String;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                List        : Argument_List;
                Delimiter   : String := " "
             );
--
-- Expression -- An expression except {case|for|if|raise}-expressions
--
   type Expression
        (  Count     : Positive;
           Operation : Operations
        )  is new Composite with
   record
      Location : Sources.Location_Type;
      Operands : aliased Argument_List (1..Count);
   end record;
   function Get_Class (Item : Expression) return Node_Class;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Expression
             );
------------------------------------------------------------------------
-- Aspect specification
--
   type Basic_Global_Mode is
        (  In_Global_Mode,
           Inout_Global_Mode,
           Out_Global_Mode
        );
   function Image (Mode : Basic_Global_Mode) return String;
   type Global_Designator_Mode is
        (  All_Designator_Mode,
           Synchronized_Designator_Mode,
           Global_Name_Designator_Mode
        );
   type Global_Aspect_Element
        (  Kind_Of     : Global_Designator_Mode;
           Mode        : Basic_Global_Mode;
           Extended    : Boolean;
           Names_Count : Positive
        )  is
   record
      case Kind_Of is
         when All_Designator_Mode | Synchronized_Designator_Mode =>
            null;
         when Global_Name_Designator_Mode =>
            List : Argument_List (1..Names_Count);
      end case;
   end record;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Global_Aspect_Element
             );

   type Global_Aspect_Element_Ptr is
      access constant Global_Aspect_Element;
   type Global_Aspect_Element_Ptr_Array is
      array (Positive range <>) of Global_Aspect_Element_Ptr;

   type Aspect_Designator_Type is
        (  No_Designator,
           Null_Designator,
           Unspecified_Designator,
           Global_Designator,
           Global_Aspect_Elements_List_Designator,
           Value_Designator
        );
   type Aspect_Specification_Item
        (  Mode           : Aspect_Designator_Type;
           Elements_Count : Positive
        )  is
   record
      Mark : Argument_Token;
      case Mode is
         when Value_Designator =>
            Value : Argument_Token;
         when No_Designator          |
              Null_Designator        |
              Unspecified_Designator =>
            null;
         when Global_Designator =>
            Designator : Global_Aspect_Element_Ptr;
         when Global_Aspect_Elements_List_Designator =>
            List : Global_Aspect_Element_Ptr_Array (1..Elements_Count);
      end case;
   end record;
   function Image
            (  Item       : Aspect_Specification_Item;
               Max_Length : Output_Length := Output_Length'Last
            )  return String;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Aspect_Specification_Item
             );

   type Aspect_Specification_Item_Ptr is
      access constant Aspect_Specification_Item;
   type Aspect_Items_Array is
      array (Positive range <>) of Aspect_Specification_Item_Ptr;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                List        : Aspect_Items_Array;
                Prefix      : String := " with "
             );
------------------------------------------------------------------------
--
-- Case_Alternative -- The case alternative
--
--    Choice - The choice list
--    Value  - The alternative
--
-- The choice list can contain subtype indications and ranges of values.
-- A range is specified in the subtype mark of the indication.
--
   type Case_Alternative is record
      Choice : Subtype_Indication_Array_Ptr;
      Value  : Argument_Token;
   end record;
   type Case_Alternatives_Array is
      array (Positive range <>) of Case_Alternative;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                List        : Case_Alternatives_Array;
                Prefix      : String := "";
                Delimiter   : String := ", ";
                Ligature    : String := " => ";
                Suffix      : String := ""
             );
   type Case_Expression
        (  Count      : Positive;
           Has_Others : Boolean
        )  is new Composite with
   record
      Selector     : Argument_Token;
      Alternatives : Case_Alternatives_Array (1..Count);
      case Has_Others is
         when True =>
            Others_Alternative : Argument_Token;
         when False =>
            null;
      end case;
   end record;
   function Get_Class (Item : Case_Expression) return Node_Class;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Case_Expression
             );
--
-- Abstract_Declare_Item -- A declaration made inside declare statement
--
   type Abstract_Declare_Item (Aspects_Count : Natural) is abstract
      new Term with
   record
      Aspects : Aspect_Items_Array (1..Aspects_Count);
   end record;
   function Get_Class (Item : Abstract_Declare_Item) return Node_Class;
   type Declare_Item_Ptr is access constant Abstract_Declare_Item'Class;

   type Declare_Renaming_Item
        (  Has_Mark      : Boolean;
           Aspects_Count : Natural
        )  is new Abstract_Declare_Item (Aspects_Count) with
   record
      Name    : Argument_Token;
      Object  : Argument_Token;
      case Has_Mark is
         when True =>
            Mark : Subtype_Mark;
         when False =>
            null;
      end case;
   end record;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Declare_Renaming_Item
             );

   type Declare_Object_Item
        (  Names_Count   : Positive;
           Array_Object  : Boolean;
           Aspects_Count : Natural
        )  is new Abstract_Declare_Item (Aspects_Count) with
   record
      Value   : Argument_Token;
      Names   : Argument_List (1..Names_Count);
      case Array_Object is
         when True =>
            Definition : Array_Type_Definition_Ptr;
         when False =>
            Object : Argument_Token;
      end case;
   end record;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Declare_Object_Item
             );

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
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Declare_Expression
             );
--
-- Alternative_Pair -- Execution alternative
--
   type Alternative_Pair is record
      Guard : Argument_Token;
      Value : Argument_Token;
   end record;
   type Guarded_List is array (Positive range <>) of Alternative_Pair;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                List        : Guarded_List;
                Prefix      : String := "";
                Delimiter   : String := ", ";
                Ligature    : String := " => ";
                Suffix      : String := ""
             );
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
      Range_Type : Argument_Token;          -- If For_Range set
      Iterator   : Argument_Token;
      Expression : Argument_Token;
      Condition  : Argument_Token;          -- For_Condition
      Key        : Argument_Token;
      Chunk      : Argument_Token;          -- If For_Parallel set
      Aspects    : Guarded_List (1..Count); -- If For_Parallel set
   end record;
   function Get_Class (Item : For_Expression) return Node_Class;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : For_Expression
             );
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
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : If_Expression
             );
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
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Item        : Raise_Expression
             );
------------------------------------------------------------------------
   type Reserved_Word is
        (  Abort_Word,     Abs_Word,        Abstract_Word,
           Accept_Word,    Access_Word,     Aliased_Word,
           All_Word,       And_Word,        Array_Word,
           At_Word,        Begin_Word,      Body_Word,
           Case_Word,      Constant_Word,   Declare_Word,
           Delay_Word,     Delta_Word,      Digits_Word,
           Do_Word,        Else_Word,       Elsif_Word,
           End_Word,       Entry_Word,      Exception_Word,
           Exit_Word,      For_Word,        Function_Word,
           Generic_Word,   Goto_Word,       If_Word,
           In_Word,        Interface_Word,  Is_Word,
           Limited_Word,   Loop_Word,       Mod_Word,
           New_Word,       Not_Word,        Null_Word,
           Of_Word,        Or_Word,         Others_Word,
           Out_Word,       Overriding_Word, Package_Word,
           Parallel_Word,  Pragma_Word,     Private_Word,
           Procedure_Word, Protected_Word,  Raise_Word,
           Range_Word,     Record_Word,     Rem_Word,
           Renames_Word,   Requeue_Word,    Return_Word,
           Reverse_Word,   Select_Word,     Separate_Word,
           Some_Word,      Subtype_Word,    Synchronized_Word,
           Tagged_Word,    Task_Word,       Terminate_Word,
           Then_Word,      Type_Word,       Until_Word,
           Use_Word,       When_Word,       While_Word,
           With_Word,      Xor_Word,        No_Reserved_Word
       );
   function Image (Key : Reserved_Word) return String;
------------------------------------------------------------------------
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
------------------------------------------------------------------------
-- Ada_Expression -- The lexer using our tables
--
--    Tree_Pool -- The arena pool to allocate AST
--
-- Parsing  tree.  To  make  it  efficient  the  nodes  of  the tree are
-- allocated  on  a  stack.  The stack is provided by a stack pool. This
-- allows  to  remove the whole tree by deallocating its first allocated
-- node or any other pool object allocated before it. Tree_Pool  is  the
-- stack storage pool used for this.
--
   type Ada_Expression
        (  Pool : access Root_Storage_Pool'Class
        )  is new Blank_Skipping_Lexers.Lexer
              (  Prefixes  => Prefixes'Access,
                 Infixes   => Infixes'Access,
                 Postfixes => Postfixes'Access
              )  with private;
--
-- Free -- Free arena
--
--    Context - The context
--    Pointer - The node allocated in the arena
--
   procedure Free
             (  Context : Ada_Expression;
                Pointer : in out Node_Ptr
             );
--
-- Get_Aspect -- Get aspect
--
--    Context - The context
--    Code    - The code
--    Count   - The number of aspects stored on the stack
--
   procedure Get_Aspect
             (  Context : in out Ada_Expression;
                Code    : in out Lexers.Lexer_Source_Type;
                Count   : out Positive
             );
--
-- Get_Discrete_Choice_List -- Get discrete choice list
--
--    Context - The parsing context
--    Code    - The source
--    List    - The result
--
   procedure Get_Discrete_Choice_List
             (  Context : in out Ada_Expression;
                Code    : in out Lexers.Lexer_Source_Type;
                List    : out Subtype_Indication_Array_Ptr
             );
--
-- Get_Constant_Folding -- The constant folding mode
--
--    Context - The context
--
-- Returns :
--
--    The constant folding is enabled if true
--
   function Get_Constant_Folding (Context : Ada_Expression)
      return Boolean;
--
-- Get_Delimited -- Get delimited text
--
--    Code      - The source
--    Text      - Lower case ASCII text
--    Delimited - Check delimiter after the text
--    Got_It    - True if matched (and source is advanced)
--
   procedure Get_Delimited
             (  Code      : in out Lexers.Lexer_Source_Type;
                Text      : String;
                Delimited : Boolean;
                Got_It    : out Boolean
             );
--
-- Get_Expanded_Name -- Get expanded name
--
--    Context    - The parsing context
--    Code       - The source
--    No_Strings - When True only identifiers are accepted
--    Argument   - The result
--
   procedure Get_Expanded_Name
             (  Context    : in out Ada_Expression;
                Code       : in out Lexers.Lexer_Source_Type;
                No_Strings : Boolean;
                Argument   : out Argument_Token
             );
--
-- Get_Identifier -- Get delimited text
--
--    Context  - The parsing context
--    Code     - The source
--    Line     - The string
--    Pointer  - To start at
--    Argument - The result (and source is advanced)
--
   procedure Get_Identifier
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Line     : String;
                Pointer  : Integer;
                Argument : out Argument_Token
             );
--
-- Get_Names_List -- Get comma-separated names list of identifiers
--
--    Context - The parsing context
--    Code    - The source
--    Count   - The number of names pushed on the argument stack
--
   procedure Get_Names_List
             (  Context : in out Ada_Expression;
                Code    : in out Lexers.Lexer_Source_Type;
                Count   : out Positive
             );
--
-- Get_Not_Null -- Get null exclustion
--
--    Context - The parsing context
--    Code    - The source
--    Got_It  - True if 'not null' matched (and source is advanced)
--
   procedure Get_Not_Null
             (  Context: in out Ada_Expression;
                Code   : in out Lexers.Lexer_Source_Type;
                Got_It : out Boolean
             );
--
-- Get_Operator -- Check for an operator
--
--    Item - An argument
--
-- This function checks item for an operator name.  The argument  can be
-- an expression, e.g. a.b."and".
--
-- Returns :
--
--    The operator or else Reserved
--
   function Get_Operator
            (  Item : Argument_Token
            )  return Operations;
--
-- Get_Range -- Get range specification
--
--    Context     - The parsing context
--    Code        - The source
--    Constrained - The range must be definite (no S range <>)
--    Any_Name    - Any name is allowed as a subtype mark
--    Argument    - The result (and source is advanced)
--
   procedure Get_Range
             (  Context     : in out Ada_Expression;
                Code        : in out Lexers.Lexer_Source_Type;
                Constrained : Boolean;
                Any_Name    : Boolean;
                Argument    : out Subtype_Indication
             );
--
-- Get_Ranges_List -- Get comma-separated parenthesed list of ranges
--
--    Context   - The parsing context
--    Code      - The source
--    Any_Name  - Any name is allowed as a subtype mark
--    Prefix    - The list prefix
--    Delimiter - The list delimiter
--    Suffix    - The list suffix
--    Count     - The number of ranges on the stack
--
-- The list can be taken from the stack into a Stubtype_Indication_Array
-- using procedure Pop.
--
   procedure Get_Ranges_List
             (  Context   : in out Ada_Expression;
                Code      : in out Lexers.Lexer_Source_Type;
                Any_Name  : Boolean;
                Prefix    : String;
                Delimiter : String;
                Suffix    : String;
                Count     : out Positive
             );
--
-- Get_Reserved_Word -- Get a reserved word
--
--    Code - The source
--    Word - The result, No_Reserved_Word if unmatched
--
   procedure Get_Reserved_Word
             (  Code : in out Lexers.Lexer_Source_Type;
                Word : out Reserved_Word
             );
--
-- Get_Subtype_Mark -- Get subtype mark
--
--    Context  - The context
--    Code     - The code
--    No_Range - The ranges are not accepted attributes
--    Not_Null - Null exclusion applied
--    Mark     - The subtype mark
--
   procedure Get_Subtype_Mark
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                No_Range : Boolean;
                Not_Null : Boolean;
                Mark     : out Subtype_Mark
             );
--
-- Get_Simple_Name -- Get name such as identifier or operator symbol
--
--    Context    - The context
--    Code       - The source
--    No_Strings - If True, then only identifiers are accepted
--    Argument   - The result
--
   procedure Get_Simple_Name
             (  Context    : in out Ada_Expression;
                Code       : in out Lexers.Lexer_Source_Type;
                No_Strings : Boolean;
                Argument   : out Argument_Token
             );
--
-- Get_Subtype_Indication -- Get subtype indication
--
--    Context   - The context
--    Code      - The code
--    Composite - The composite constraint allowed
--    Not_Null  - Has null exclusion upfront, updated
--    Argument  - The result
--
   procedure Get_Subtype_Indication
             (  Context   : in out Ada_Expression;
                Code      : in out Lexers.Lexer_Source_Type;
                Composite : Boolean;
                Not_Null  : in out Boolean;
                Argument  : out Subtype_Indication
             );
--
-- Get_String_Literal -- Get delimited text
--
--    Context  - The context
--    Code     - The source
--    Line     - The string
--    Pointer  - To start at (after the quotation mark)
--    Argument - The result (and source is advanced)
--
   procedure Get_String_Literal
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Line     : String;
                Pointer  : Integer;
                Argument : out Argument_Token
             );
--
-- Set_Constant_Folding -- Change constant folding mode
--
--    Context - The context
--    Enable  - The mode
--
-- The  constant folding if enabled evaluates universal integer and real
-- expressions   on   literals.   The   syntax   tree    will    contain
-- Universal_Integer and Universal_Real nodes.
--
   procedure Set_Constant_Folding
             (  Context : in out Ada_Expression;
                Enable  : Boolean
             );
--
-- Compare -- Case-insensitive comparison of strings
--
--    Left, Right - Arguments
--
-- Returns :
--
--    The result
--
   function Compare (Left, Right : String       ) return Precedence;
   function Compare (Left, Right : Argument_List) return Precedence;
   function Compare (Left, Right : Node'Class   ) return Precedence;
--
-- Is_Class -- Check if the argument is an identifier 'class'
--
--    Argument - The argument
--
-- Returns :
--
--    True if Argument identifies 'class'
--
   function Is_Class (Item : Argument_Token) return Boolean;
--
-- Is_Defining_Identifier -- Check if the argument is an identifier
--
--    Argument - The argument
--
-- Returns :
--
--    True if Argument is an identifier
--
   function Is_Defining_Identifier (Item : Argument_Token)
      return Boolean;
--
-- Is_Defining_Operator -- Check identifier or operator
--
--    Argument - The argument
--
-- Returns :
--
--    True if Argument is an identifier or operator like ">"
--
   function Is_Defining_Operator (Item : Argument_Token)
      return Boolean;
--
-- Is_Name -- Check identifier
--
--    Item - An argument
--    Name - The name identifier should have
--
-- Returns :
--
--    True if Item is an identifier with Name
--
   function Is_Name
            (  Item : Argument_Token;
               Name : String
            )  return Boolean;
--
-- Pop -- Take lists from the argument stack
--
   procedure Pop
             (  Context : in out Ada_Expression;
                List    : out Argument_List
             );
   procedure Pop
             (  Context : in out Ada_Expression;
                List    : out Aspect_Items_Array
             );
   procedure Pop
             (  Context : in out Ada_Expression;
                List    : out Subtype_Indication_Array
             );
--
-- Push -- Push onto the argument stack
--
   procedure Push
             (  Context : in out Ada_Expression;
                Item    : Subtype_Indication
             );
--
-- Push_Stub -- Push a stub on the stack
--
--    Context - The context
--    Stub    - The stack stub
--
   procedure Push_Stub
             (  Context : in out Ada_Expression;
                Stub    : out Node_Ptr
             );
--
-- Generic_Stack_Access -- Pushing  and popping  access  types  onto the
--                         argument stack
--
   generic
      type Object_Type (<>) is limited private;
      type Pointer_Type is access constant Object_Type;
   package Generic_Stack_Access is
      procedure Push
                (  Context  : in out Ada_Expression'Class;
                   Pointer  : Pointer_Type;
                   Location : Sources.Location_Type
                );
      procedure Pop
                (  Context  : in out Ada_Expression'Class;
                   Pointer  : out Pointer_Type;
                   Location : out Sources.Location_Type
                );
   end Generic_Stack_Access;
--
-- Generic_Stack_Object -- Pushing  and popping object onto the argument
--                         stack
--
   generic
      type Object_Type is private;
   package Generic_Stack_Object is
      procedure Push
                (  Context : in out Ada_Expression'Class;
                   Object  : Object_Type
                );
      procedure Pop
                (  Context : in out Ada_Expression'Class;
                   Object  : out Object_Type
                );
   end Generic_Stack_Object;

   generic
      type Item_Type (<>) is limited private;
      with procedure Put
                     (  Destination : in out String;
                        Pointer     : in out Integer;
                        Item        : Item_Type
                     )  is <>;
   function Generic_Image
            (  Item       : Item_Type;
               Max_Length : Output_Length := Output_Length'Last
            )  return String;

private
   use Unbounded_Unsigneds;

   pragma Inline (Compare);

   type Half_Word_Array is array (Digit_Count range <>) of Half_Word;
   type Half_Word_Array_Ptr is access constant Half_Word_Array;
--
-- Universal_Integer -- During  parsing  the value  is  not  stored  and
--                      contains  the  actual  value  in  Value. Once it
-- appears in the result or else in an expression that cannot be  folded
-- the value is stored into the arena as sign and vector of digits.
--
   type Unbounded_Integer_Ptr is access Unbounded_Integer;
   type Universal_Integer is new Universal_Value with record
      Negative : Boolean; -- inversed if Data = null otherwise
      Data     : Half_Word_Array_Ptr;
      Value    : Unbounded_Integer_Ptr; -- Tagged type reference
   end record;
--
-- Universal_Real -- A folded universal real expression
--
   type Unbounded_Rational_Ptr is access Unbounded_Rational;
   type Universal_Real is new Universal_Value with record
      Negative    : Boolean;
      Numerator   : Half_Word_Array_Ptr;
      Denominator : Half_Word_Array_Ptr;
      Value       : Unbounded_Rational_Ptr;
   end record;

   type Ada_Expression
        (  Pool : access Root_Storage_Pool'Class
        )  is new Blank_Skipping_Lexers.Lexer
                  (  Prefixes  => Prefixes'Access,
                     Infixes   => Infixes'Access,
                     Postfixes => Postfixes'Access
                  )  with
   record
      Fold : Boolean := True;
   end record;
--
-- Call -- Evaluates an operator
--
   function Call
            (  Context   : access Ada_Expression;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Argument_Token;
--
-- Enclose -- Evaluates an expression in brackets
--
   function Enclose
            (  Context : access Ada_Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Argument_Token;
--
-- Get_Operand -- Recognizes an operand (float number)
--
   procedure Get_Operand
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Argument_Token;
                Got_It   : out Boolean
             );
--
-- On_Association_Error -- Change message
--
   procedure On_Association_Error
             (  Context : in out Ada_Expression;
                Code    : in out Lexers.Lexer_Source_Type;
                Left    : in out Operation_Token;
                Right   : in out Operation_Token
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
-- On_Missing_Right_Bracket -- Change message
--
   procedure On_Missing_Right_Bracket
             (  Context : in out Ada_Expression;
                Code    : in out Lexers.Lexer_Source_Type;
                Left    : in out Operation_Token;
                Right   : out Operation_Token
             );
--
-- On_Postmodifier -- Overrides the default handling of modifiers
--
   procedure On_Postmodifier
             (  Context  : in out Ada_Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : in out Argument_Token;
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
-- On_Success -- Store folded result if needed
--
   procedure On_Success
             (  Context : in out Ada_Expression;
                Code    : in out Lexers.Lexer_Source_Type;
                Result  : in out Argument_Token
             );
--
-- Store -- Unversal type expression if not already stored
--
-- The value of an expression is allocated in the pool.  When stored its
-- value  is converted  to a form  allowing  storing  it  in  the arena.
-- Storing  happens  when  no  further  folding  involving  the value is
-- possible or required.
--
   procedure Store
             (  Context : in out Ada_Expression;
                Item    : in out Argument_Token
             );
   procedure Store
             (  Context : in out Ada_Expression;
                List    : in out Argument_List
             );
--
-- Free -- Unversal type expression
--
-- The pool  memory  of the expression is freed when  no more used.  For
-- example when folding ignores the value.
--
   procedure Free (Item : Argument_Token);

   function Has_Bracket (Code : Lexers.Lexer_Source_Type)
      return Boolean;
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

   package Operator_Tables_Raw is new Tables (Operations);
   package Operator_Tables is new Operator_Tables_Raw.Names;

   package Reserved_Words_Tables_Raw is new Tables (Reserved_Word);
   package Reserved_Words_Tables is new Reserved_Words_Tables_Raw.Names;

   generic
      with function Op (Left, Right : Boolean) return Boolean;
      Has_Null   : Boolean := False;
      Null_Value : Boolean := False;
   function Generic_Boolean_Folder
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Argument_Token;
   generic
      with function Int_Op (Left, Right : Unbounded_Integer)
         return Unbounded_Integer;
      with function Int_Rev_Op (Left, Right : Unbounded_Integer)
         return Unbounded_Integer;
      with function Real_Op (Left, Right : Unbounded_Rational)
         return Unbounded_Rational;
      with function Real_Rev_Op (Left, Right : Unbounded_Rational)
         return Unbounded_Rational;
   function Generic_Dyadic_Folder
            (  Context     : access Ada_Expression'Class;
               Operation   : Tokens.Operation_Token;
               List        : Tokens.Arguments.Frame
            )  return Argument_Token;
   generic
      with function Int_Op (Left, Right : Unbounded_Integer)
         return Unbounded_Integer;
   function Generic_Integer_Folder
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Argument_Token;
   generic
      with function Int_Op (Left, Right : Unbounded_Integer)
         return Boolean;
      with function Real_Op (Left, Right : Unbounded_Rational)
         return Boolean;
   function Generic_Logical_Folder
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Argument_Token;

   generic
      with function Int_Op (Left : Unbounded_Integer)
         return Unbounded_Integer;
      with function Real_Op (Left : Unbounded_Rational)
         return Unbounded_Rational;
   function Generic_Unary_Folder
            (  Context   : access Ada_Expression'Class;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Argument_Token;

end Parsers.Generic_Ada_Parser;
