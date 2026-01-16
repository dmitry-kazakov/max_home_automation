--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.Patterns             Luebeck            --
--  Interface                                      Summer, 2025       --
--                                                                    --
--                                Last revision :  12:17 04 Jan 2026  --
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

with Strings_Edit.UTF8.Maps;  use Strings_Edit.UTF8.Maps;

with Ada.Text_IO;
with Object.Handle;

generic
package Parsers.Generic_Source.Patterns is
   subtype Location_Subtype is Location_Type;
   subtype Source_Subtype   is Source_Type;
--
-- Result_Type -- Pattern matching result
--
--    Successful - Successful matching
--    Failed     - Temporal matching failure
--    Aborted    - No continuation possible
--
   type Severity is (Successful, Failed, Aborted);
   subtype Non_Fatal is Severity range Successful..Failed;
   type Result_Type (Outcome : Severity; Length : Natural) is record
      case Outcome is
         when Successful | Failed =>
            null;
         when Aborted =>
            Where   : Location_Type;
            Message : String (1..Length);
      end case;
   end record;
   Matched   : constant Result_Type := (Successful, 0);
   Unmatched : constant Result_Type := (Failed, 0);
--
-- Match_State -- Pattern matching state
--
   type Match_State (Size : Positive) is limited private;
--
-- Get_Tracing -- Tracing state
--
--    State - The state
--
-- Returns :
--
--    True if tracing is on
--
   function Get_Tracing (State : Match_State) return Boolean;
--
-- Set_Tracing -- Set tracing mode
--
--    State - The state
--    On    - The tracing mode
--
   procedure Set_Tracing (State : in out Match_State; On : Boolean);
------------------------------------------------------------------------
--
-- Pattern_Type -- Pattern
--
   type Pattern_Type is private;
--
-- Alphanumeric -- A pattern matching any letter or digit
--
-- Returns :
--
--    The pattern
--
   function Alphanumeric return Pattern_Type;
--
-- Any -- A pattern matching any character
--
-- Returns :
--
--    The pattern
--
   function Any return Pattern_Type;
--
-- Any_Of -- A pattern matching any character from the list
--
--    List - The list of characters
--
-- Returns :
--
--    The pattern
--
-- Exceotions :
--
--    Data_Error - List is not a valid UTF-8 string
--
   function Any_Of (List : String     ) return Pattern_Type;
   function Any_Of (List : Unicode_Set) return Pattern_Type;
--
-- Anything -- Pattern that expands on failure
--
-- This pattern is equivalent to +Any & Fence. It initally matches empty
-- strings. When matching fails it matches one character more. It can be
-- used as followsL
--
--    "a" & Anything & "b"
--
-- With the meaning "a" then anything to "b."
--
-- Returns :
--
--    The pattern
--
   function Anything return Pattern_Type;
--
-- Blank          -- A pattern matching  a non-empty sequence of Unicode
--                   spaces and tabs
-- Blank_Or_Empty -- As above but can be empty
-- Space          -- A single Unicode space or tab character
--
-- Returns :
--
--    The pattern
--
   function Blank          return Pattern_Type;
   function Blank_Or_Empty return Pattern_Type;
   function Space          return Pattern_Type;
--
-- Case_Insensitive_Text -- A pattern matching case insensitive text
--
--    Text - The text
--
-- Returns :
--
--    The pattern
--
   function Case_Insensitive_Text (Text : String) return Pattern_Type;
   function "abs" (Text : String) return Pattern_Type;
--
-- Digit -- A pattern matching one digit 0..9
--
-- Returns :
--
--    The pattern
--
   function Digit return Pattern_Type;
--
-- Def -- A recursively matched pattern
--
--    Pattern - THe pattern
--
-- The pattern matches recursively.  Inside the pattern it is referenced
-- as Self. For example:
--
--    Def ("(" & Self & ")" or "a")
--
-- This pattern matches a, (a), ((a)) etc.
--
-- Returns :
--
--    The pattern
--
-- Exceptions :
--
--    Constraint_Error - There is no unresolved Self inside the pattern
--
   function Def (Pattern : Pattern_Type) return Pattern_Type;
--
-- Empty -- A pattern matching empty string
--
-- Returns :
--
--    The pattern
--
   function Empty return Pattern_Type;
--
-- End_Of_Line -- A pattern matching empty substring at the end of line
--
-- Returns :
--
--    The pattern
--
   function End_Of_Line return Pattern_Type;
--
-- Failure -- End matching with failure
--
--    Message - Error message
--
-- When Message is specified the matching result will be "Aborted."
--
-- Returns :
--
--    The pattern
--
   function Failure return Pattern_Type;
   function Failure (Message : String) return Pattern_Type;
--
-- Fence -- A pattern matching empty substring and disables returns
--
-- The  pattern  is  used  to optimize  matching  process  by  disabling
-- returns. For example:
--
--    Text ("a") & -Any & Text ("b")
--
-- tries to rematch shorter sequences matched by Any. To reduce  useless
-- rematching  Fence forces failure  after  the longest  sequence  of is
-- matched by Any.
--
-- Returns :
--
--    The pattern
--
   function Fence return Pattern_Type;
--
-- Field -- A pattern matching a field
--
--    List - The list of characters
--
-- The field is a non-empty string ending at the line end or else at the
-- the delimiter character from List.
--
-- Returns :
--
--    The pattern
--
-- Exceotions :
--
--    Data_Error - List is not a valid UTF-8 string
--
   function Field (List : String     ) return Pattern_Type;
   function Field (List : Unicode_Set) return Pattern_Type;
--
-- Floating_Point_Number -- A  pattern  that  matches  a  floating-point
--                          number
--
-- Returns :
--
--    A pattern that matches a number
--
   function Floating_Point_Number return Pattern_Type;
--
-- Fixed_Point_Number -- A pattern that matches a fixed-point number
--
--    Base - The number base 2..16
--
-- Returns :
--
--    A pattern that matches a number in the specified base
--
   function Fixed_Point_Number (Base : Strings_Edit.NumberBase := 10)
      return Pattern_Type;
--
-- Image -- A text representation of a pattern
--
--    Pattern - The pattern
--
-- Returns :
--
--    The representation
--
   function Image (Pattern : Pattern_Type) return String;
--
-- Letter -- A pattern matching one letter
--
-- Returns :
--
--    The pattern
--
   function Letter return Pattern_Type;
--
-- Lower_Case_Letter -- A pattern matching one lower case letter
--
-- Returns :
--
--    The pattern
--
   function Lower_Case_Letter return Pattern_Type;
--
-- Match -- Pattern against the source
--
--    Pattern - The pattern
--    Source  - The source to match
--    State   - The matching state
--
-- Returns :
--
--    The matching result
--
   function Match
            (  Pattern : Pattern_Type;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
--
-- Natural_Number -- A pattern that matches a natural number
--
--    Base - The number base 2..16
--
-- Returns :
--
--    A pattern that matches a natural number in the specified base
--
   function Natural_Number (Base : Strings_Edit.NumberBase := 10)
      return Pattern_Type;
--
-- Nil -- A pattern that always fails
--
-- Returns :
--
--    A pattern that always fail to match
--
   function Nil return Pattern_Type;
--
-- NL -- New line pattern
--
-- The new line pattern  cannot be  rolled back  because source does not
-- support going back. On file end matching is aborted.
--
-- Returns :
--
--    A pattern that matches empty string and skips to the next line
--
   function NL return Pattern_Type;
--
-- NL_Or_EOF -- New line or EOF pattern
--
-- This is like NL  but end matching with success on  file end.  The new
-- line pattern  cannot be  rolled back  because source does not support
-- going back.
--
-- Returns :
--
--    A pattern that matches empty string and skips to the next one
--
   function NL_Or_EOF return Pattern_Type;
--
-- Nonempty -- Create a pattern that matched non-empty substrings
--
--    Pattern - The pattern to match
--
-- Returns :
--
--    A pattern that matches Pattern but only if the result is non empty
--
   function Nonempty (Pattern : Pattern_Type) return Pattern_Type;
--
-- Put[_Line] -- Print pattern when matched
--
--  [ File ]       - File
--    Pattern|Text - Pattern
--    Prefix       - To decorate the output
--    Suffix       - To decorate the output
--
-- Returns :
--
--    A pattern that matches Left  and prints matched substring into the
--    file Left
--
   function Put
            (  File    : Ada.Text_IO.File_Access;
               Pattern : Pattern_Type;
               Prefix  : String := "";
               Suffix  : String := ""
            )  return Pattern_Type;
   function Put
            (  File    : Ada.Text_IO.File_Access;
               Text    : String;
               Prefix  : String := "";
               Suffix  : String := ""
            )  return Pattern_Type;
   function Put
            (  Pattern : Pattern_Type;
               Prefix  : String := "";
               Suffix  : String := ""
            )  return Pattern_Type;
   function Put
            (  Text    : String;
               Prefix  : String := "";
               Suffix  : String := ""
            )  return Pattern_Type;
   function Put_Line
            (  File    : Ada.Text_IO.File_Access;
               Pattern : Pattern_Type;
               Prefix  : String := "";
               Suffix  : String := ""
            )  return Pattern_Type;
   function Put_Line
            (  File    : Ada.Text_IO.File_Access;
               Text    : String;
               Prefix  : String := "";
               Suffix  : String := ""
            )  return Pattern_Type;
   function Put_Line
            (  Pattern : Pattern_Type;
               Prefix  : String := "";
               Suffix  : String := ""
            )  return Pattern_Type;
   function Put_Line
            (  Text    : String;
               Prefix  : String := "";
               Suffix  : String := ""
            )  return Pattern_Type;
--
-- Trace -- Print current line when pattern when matched
--
--  [ File ]       - File
--    Pattern|Text - Pattern
--    Prefix       - To decorate the output
--    Suffix       - To decorate the output
--
-- When Pattern matches the current line is printed. The part matched by
-- the pattern is decorated by Prefix and Suffix like:
--
--                   XXXXX|YYYY|ZZZZ
--                      |    |    |
--     matched previously    |    unmatched
--                      matched by the pattern
--
-- Returns :
--
--    The pattern
--
   function Trace
            (  File    : Ada.Text_IO.File_Access;
               Pattern : Pattern_Type;
               Prefix  : String := "|";
               Suffix  : String := "|"
            )  return Pattern_Type;
   function Trace
            (  File    : Ada.Text_IO.File_Access;
               Text    : String;
               Prefix  : String := "|";
               Suffix  : String := "|"
            )  return Pattern_Type;
   function Trace
            (  Pattern : Pattern_Type;
               Prefix  : String := "|";
               Suffix  : String := "|"
            )  return Pattern_Type;
   function Trace
            (  Text    : String;
               Prefix  : String := "|";
               Suffix  : String := "|"
            )  return Pattern_Type;
--
-- Rematch -- Pattern against the source
--
--    Source - The source to match
--    State  - The matching state left from previous matching
--
-- This function  attempts to rematch looking for an alternative path in
-- State.
--
-- Returns :
--
--    The matching result
--
-- Exceptions :
--
--    The pattern has not been matched
--
   function Rematch
            (  Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
--
-- Self -- A recursively matched pattern reference
--
-- This function returns a reference to the enclosing recursive pattern.
-- For example:
--
--    Def ("(" & Self & ")" or "a")
--
-- This pattern matches a, (a), ((a)) etc.
--
-- Returns :
--
--    The pattern
--
   function Self return Pattern_Type;
--
-- Subscript_Digit -- A pattern matching one subscript digit
--
-- Returns :
--
--    The pattern
--
   function Subscript_Digit return Pattern_Type;
--
-- Success -- End matching with success
--
-- Returns :
--
--    The pattern
--
   function Success return Pattern_Type;
--
-- Superscript_Digit -- A pattern matching one superscript digit
--
-- Returns :
--
--    The pattern
--
   function Superscript_Digit return Pattern_Type;
--
-- Text -- A pattern matching case sensitive text
--
--    Text - The text
--
-- Returns :
--
--    The pattern
--
   function Text (Text : String) return Pattern_Type;
--
-- Upper_Case_Letter -- A pattern matching one upper case letter
--
-- Returns :
--
--    The pattern
--
   function Upper_Case_Letter return Pattern_Type;
--
-- Voidable -- A pattern can match empty string
--
--    Pattern - The pattern
--
-- Returns :
--
--    True if Pattern can match an empty string
--
   function Voidable (Pattern : Pattern_Type) return Boolean;
--
-- XML_Literal -- A pattern matching XML literal
--
-- Returns :
--
--    The pattern
--
   function XML_Literal return Pattern_Type;
--
-- & -- Create a sequence pattern
--
--    Left, Right - Patterns
--
-- Returns :
--
--    A pattern that matches Left and then Right
--
   function "&" (Left, Right : Pattern_Type) return Pattern_Type;
   function "&" (Left : String; Right : Pattern_Type)
      return Pattern_Type;
   function "&" (Left : Pattern_Type; Right : String)
      return Pattern_Type;
--
-- or -- Create an alternative pattern
--
--    Left, Right - Patterns
--
-- Returns :
--
--    A pattern that matches Left or Right
--
   function "or" (Left, Right : Pattern_Type) return Pattern_Type;
   function "or" (Left, Right : String)       return Pattern_Type;
   function "or" (Left : String; Right : Pattern_Type)
      return Pattern_Type;
   function "or" (Left : Pattern_Type; Right : String)
      return Pattern_Type;
--
-- * -- Create repeated pattern
--
--    Left  - Repetition count / pattern
--    Right - Pattern / repetition count
--
-- Returns :
--
--    A pattern that matches Left or Right
--
   function "*" (Left : Positive; Right : Pattern_Type)
      return Pattern_Type;
   function "*" (Left : Pattern_Type; Right : Positive)
      return Pattern_Type;
   function "*" (Left : Positive; Right : String) return Pattern_Type;
   function "*" (Left : String; Right : Positive) return Pattern_Type;
--
-- + -- Create a eagerly repeated pattern
--
--    Left - Pattern
--
-- Returns :
--
--    A pattern that matches Left as many times as possible
--
-- Exceptions :
--
--    Constraint_Error - Left can match an empty substring
--
   function "+" (Left : Pattern_Type) return Pattern_Type;
   function "+" (Left : String)       return Pattern_Type;
--
-- - -- Create a lazily repeated pattern
--
--    Left - Pattern
--
-- Returns :
--
--    A pattern that matches Left as many times as possible
--
-- Exceptions :
--
--    Constraint_Error - Left can match an empty substring
--
   function "-" (Left : Pattern_Type) return Pattern_Type;
   function "-" (Left : String      ) return Pattern_Type;
--
-- not -- Create an inversed pattern
--
--    Left - Pattern
--
-- Returns :
--
--    A pattern that matches empty substring when Left does not match
--
   function "not" (Left : Pattern_Type) return Pattern_Type;
   function "not" (Left : String      ) return Pattern_Type;
------------------------------------------------------------------------
--
-- Variable_Type -- Variable that can be assigned upon pattern matching
--
   type Variable_Type is private;
--
-- Append -- A value to the variable
--
--    Variable - The variable to be set
--    Value    - The value to append
--    Where    - The source location of the appended value
--
   procedure Append
             (  Variable : in out Variable_Type;
                Value    : String;
                Where    : Location_Type
             );
--
-- Case_Insensitive_Text -- Pattern matching current variable value
--
--    Variable - The variable
--
-- Returns :
--
--    The pattern matching text of the current variable value
--
   function Case_Insensitive_Text (Variable : Variable_Type)
      return Pattern_Type;
--
-- Defined -- Test
--
--    Variable - The variable
--
-- Returns :
--
--    True if the Variable value is defined
--
   function Defined (Variable : Variable_Type) return Boolean;
--
-- Link -- The value location
--
--    Variable - The variable
--
-- Returns :
--
--    The source location of assigned value
--
-- Exceptions :
--
--    Constraint_Error - The variable is not defined
--
   function Link (Variable : Variable_Type) return Location_Type;
--
-- Reset -- Undefine variable
--
--    Variable - The variable to set undefined
--
   procedure Reset (Variable : in out Variable_Type);
--
-- Set -- Define variable
--
--    Variable - The variable to be set
--    Value    - The value to set
--    Where    - The source location of the value
--
   procedure Set
             (  Variable : in out Variable_Type;
                Value    : String;
                Where    : Location_Type
             );
--
-- Text -- Pattern matching current variable value
--
--    Variable - The variable
--
-- Returns :
--
--    The pattern matching text of the current variable value
--
   function Text (Variable : Variable_Type) return Pattern_Type;
--
-- Truncate -- Delete last appended value
--
--    Variable - The variable to set undefined
--
   procedure Truncate (Variable : in out Variable_Type);
--
-- Value -- The variable value
--
--    Variable - The variable
--
-- Returns :
--
--    The assigned value
--
-- Exceptions :
--
--    Constraint_Error - The variable is not defined
--
   function Value (Variable : Variable_Type) return String;
--
-- <= -- Create an assignment pattern
--
--    Left  - Variable
--    Right - Pattern
--
-- Returns :
--
--    A pattern that assigns Left when the pattern Right matched
--
   function "<="
            (  Left  : Variable_Type;
               Right : Pattern_Type
            )  return Pattern_Type;
   function "<="
            (  Left  : Variable_Type;
               Right : String
            )  return Pattern_Type;
--
-- < -- Create an assignment pattern
--
--    Left  - Variable
--    Right - Pattern
--
-- Returns :
--
--    A pattern that appends to Left when the pattern Right matched
--
   function "<"
            (  Left  : Variable_Type;
               Right : Pattern_Type
            )  return Pattern_Type;
   function "<"
            (  Left  : Variable_Type;
               Right : String
            )  return Pattern_Type;
------------------------------------------------------------------------
--
-- Abstract_User_Variable -- User-defined variable
--
   type Abstract_User_Variable is abstract
      new Object.Entity with null record;
--
-- Add -- Matched text to the variable
--
--    Variable - The variable
--    Source   - The souce
--    Value    - Matched text
--    Where    - The text location in the source
--    Append   - True if the pattern is an operand of <=
--
-- This procedure is called when the pattern is matched.
--
   procedure Add
             (  Variable : in out Abstract_User_Variable;
                Source   : in out Source_Type;
                Value    : String;
                Where    : Location_Type;
                Append   : Boolean
             )  is abstract;
--
-- Delete -- Previously matched text to the variable
--
--    Variable - The variable
--    Source   - The souce
--    Append   - True if the pattern is an operand of <=
--
-- This procedure is called when the pattern is rolled-back.
--
   procedure Delete
             (  Variable : in out Abstract_User_Variable;
                Source   : in out Source_Type;
                Append   : Boolean
             )  is abstract;
--
-- On_Line_Change -- Line changed while matching
--
--    Variable - The variable
--    Source   - The souce
--    Where    - The location in the source
--
-- This function  is called when  the pattern matches text spanning more
-- than one line.
--
-- Returns :
--
--    Aborted - Aborts matching with the result returned
--    Failed  - Aborts matching with multiline error
--    Success - Contunues matching
--
   function On_Line_Change
            (  Variable   : access Abstract_User_Variable;
               Source     : access Source_Type;
               Where      : Location_Type
            )  return Result_Type is abstract;

   type User_Variable_Ptr is access Abstract_User_Variable'Class;
   package User_Variable_Handles is
      new Object.Handle
          (  Abstract_User_Variable,
             User_Variable_Ptr
          );
--
-- <= -- Create an assignment pattern
--
--    Left  - Variable
--    Right - Pattern
--
-- Returns :
--
--    A pattern that assigns Left when the pattern Right matched
--
   function "<="
            (  Left  : User_Variable_Handles.Handle;
               Right : Pattern_Type
            )  return Pattern_Type;
   function "<="
            (  Left  : User_Variable_Handles.Handle;
               Right : String
            )  return Pattern_Type;
--
-- < -- Create an assignment pattern
--
--    Left  - Variable
--    Right - Pattern
--
-- Returns :
--
--    A pattern that appends to Left when the pattern Right matched
--
   function "<"
            (  Left  : User_Variable_Handles.Handle;
               Right : Pattern_Type
            )  return Pattern_Type;
   function "<"
            (  Left  : User_Variable_Handles.Handle;
               Right : String
            )  return Pattern_Type;

private
--
-- Statement_Type -- Statement type, when pattern is a statement
--
   type Statement_Type is
        (  No_Statement,
           Alternate_Statement,
           Anything_Statement,
           Goto_Statement,
           Eager_Statement,
           Fence_Statement,
           Active_Hook_Statement,
           Passive_Hook_Statement,
           Active_Lazy_Statement,
           Passive_Lazy_Statement,
           Return_Statement,
           Next_Line_Statement,
           Next_Line_Or_EOF_Statement,
           Nil_Statement,
           Nonempty_Statement,
           Not_Statement,
           Repeater_Statement,
           Sequence_Statement,
           Stub_Statement
        );
   subtype Hook_Statement is Statement_Type
      range Active_Hook_Statement..Passive_Hook_Statement;
   subtype Lazy_Statement is Statement_Type
      range Active_Lazy_Statement..Passive_Lazy_Statement;

   type Pattern_Object is abstract new Object.Entity with null record;
   type Pattern_Ptr is access Pattern_Object'Class;
   type Pattern_Ref is access constant Pattern_Object'Class;

   function Get_Type (Pattern : Pattern_Object) return Statement_Type;
   function Resolve
            (  Pattern   : access Pattern_Object;
               Reference : Pattern_Ref
            )  return Boolean;
   function Voidable (Pattern : Pattern_Object; Recursive : Boolean)
      return Boolean is abstract;
   function Image (Pattern : Pattern_Object) return String is abstract;
   function Match
            (  Pattern : Pattern_Object;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;

   package Pattern_Handles is
      new Object.Handle (Pattern_Object, Pattern_Ptr);

   type Pattern_Type is new Pattern_Handles.Handle with null record;
   function Ref (Thing : Pattern_Ptr) return Pattern_Type;
   type Pattern_Array is array (Positive range <>) of Pattern_Type;
   function Image
            (  Items     : Pattern_Array;
               Delimiter : String := ", "
            )  return String;

   type Matching_Hook_Type is abstract new Object.Entity with record
      Pattern : Pattern_Type;
   end record;
------------------------------------------------------------------------
--
-- Alphanumeric_Pattern -- Pattern matching any letter or didit
--
   type Alphanumeric_Pattern is new Pattern_Object with null record;
   function Image (Pattern : Alphanumeric_Pattern) return String;
   function Match
            (  Pattern : Alphanumeric_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Alphanumeric_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Alternate_Pattern -- Pattern matching  one alternative  from the list
--                      of patterns
--
   type Alternate_Pattern (Length : Positive) is
      new Pattern_Object with
   record
      Items : Pattern_Array (1..Length);
   end record;
   function Get_Type (Alternation : Alternate_Pattern)
      return Statement_Type;
   function Image (Alternation : Alternate_Pattern) return String;
   function Resolve
            (  Pattern   : access Alternate_Pattern;
               Reference : Pattern_Ref
            )  return Boolean;
   function Voidable
            (  Alternation : Alternate_Pattern;
               Recursive   : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Any_Pattern -- Pattern matching any character
--
   type Any_Pattern is new Pattern_Object with null record;
   function Image (Pattern : Any_Pattern) return String;
   function Match
            (  Pattern : Any_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Any_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Any_Of_Pattern -- Pattern matching any character from the list
--
   type Any_Of_Pattern is new Pattern_Object with record
      Set : Unicode_Set;
   end record;
   function Image (Pattern : Any_Of_Pattern) return String;
   function Match
            (  Pattern : Any_Of_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Any_Of_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Anything -- Pattern matching expanding string
--
   type Anything_Pattern is new Pattern_Object with null record;
   function Get_Type (Pattern : Anything_Pattern) return Statement_Type;
   function Image (Pattern : Anything_Pattern) return String;
   function Voidable
            (  Pattern   : Anything_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- As_Text_Pattern -- Pattern matching current variable value
--
   type As_Text_Pattern is new Pattern_Object with record
      Variable : Variable_Type;
   end record;
   function Image (Pattern : As_Text_Pattern) return String;
   function Match
            (  Pattern : As_Text_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : As_Text_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- As_Case_Insensitive_Text_Pattern -- Pattern matching current variable
--
   type As_Case_Insensitive_Text_Pattern is
      new Pattern_Object with
   record
      Variable : Variable_Type;
   end record;
   function Image (Pattern : As_Case_Insensitive_Text_Pattern)
      return String;
   function Match
            (  Pattern : As_Case_Insensitive_Text_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : As_Case_Insensitive_Text_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Blank_Pattern -- Pattern matching any Unicode space character or tab
--
   type Blank_Mode is (Single, Chain, Non_Empty_Chain);
   type Blank_Pattern is new Pattern_Object with record
      Mode : Blank_Mode;
   end record;
   function Image (Pattern : Blank_Pattern) return String;
   function Match
            (  Pattern : Blank_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Blank_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Case_Insensitive_Text_Pattern -- Pattern matching a text string, case
--                                  insensitive
--
   type Case_Insensitive_Text_Pattern (Length : Natural) is
      new Pattern_Object with
   record
      Text : String (1..Length);
   end record;
   function Image (Text : Case_Insensitive_Text_Pattern) return String;
   function Match
            (  Text   : Case_Insensitive_Text_Pattern;
               Source : access Source_Type;
               State  : access Match_State
            )  return Result_Type;
   function Voidable
            (  Text      : Case_Insensitive_Text_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Digit_Pattern -- Pattern matching one digit 0..9
--
   type Digit_Pattern is new Pattern_Object with null record;
   function Image (Pattern : Digit_Pattern) return String;
   function Match
            (  Pattern : Digit_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Digit_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Eager_Pattern -- Eager pattern repeater
--
   type Eager_Pattern is new Pattern_Object with record
      Pattern : Pattern_Type;
   end record;
   function Get_Type (Repeater : Eager_Pattern) return Statement_Type;
   function Image    (Repeater : Eager_Pattern) return String;
   function Resolve
            (  Pattern   : access Eager_Pattern;
               Reference : Pattern_Ref
            )  return Boolean;
   function Voidable
            (  Repeater  : Eager_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Empty_Pattern -- Eager pattern repeater
--
   type Empty_Pattern is new Pattern_Object with null record;
   function Image    (Pattern : Empty_Pattern) return String;
   function Match
            (  Pattern : Empty_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Empty_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- End_Of_Line_Pattern -- Pattern matching end of line
--
   type End_Of_Line_Pattern is new Pattern_Object with null record;
   function Image (Pattern : End_Of_Line_Pattern) return String;
   function Match
            (  Pattern : End_Of_Line_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : End_Of_Line_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Fence_Pattern -- Fence pattern
--
   type Fence_Pattern is new Pattern_Object with null record;
   function Get_Type (Pattern : Fence_Pattern) return Statement_Type;
   function Image    (Pattern : Fence_Pattern) return String;
   function Voidable
            (  Pattern   : Fence_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Field_Pattern -- Pattern matching any text until a character from the
--                  list
--
   type Field_Pattern is new Pattern_Object with record
      Set : Unicode_Set;
   end record;
   function Image (Pattern : Field_Pattern) return String;
   function Match
            (  Pattern : Field_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Field_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Fixed_Point_Number_Pattern -- Pattern matching a fixed-point number
--
   type Fixed_Point_Number_Pattern
        (  Base : Strings_Edit.NumberBase
        )  is new Pattern_Object with null record;
   function Image (Pattern : Fixed_Point_Number_Pattern) return String;
   function Match
            (  Pattern : Fixed_Point_Number_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Fixed_Point_Number_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Floating_Point_Number_Pattern -- Pattern matching a floating-point
--                                  number
--
   type Floating_Point_Number_Pattern is
      new Pattern_Object with null record;
   function Image (Pattern : Floating_Point_Number_Pattern)
      return String;
   function Match
            (  Pattern : Floating_Point_Number_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Floating_Point_Number_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Goto_Pattern -- Recursive pattern
--
   type Goto_Pattern is new Pattern_Object with record
      Pattern : Pattern_Ref;
   end record;
   function Get_Type (Pattern : Goto_Pattern)
      return Statement_Type;
   function Image (Pattern : Goto_Pattern) return String;
   function Resolve
            (  Pattern   : access Goto_Pattern;
               Reference : Pattern_Ref
            )  return Boolean;
   function Voidable
            (  Pattern   : Goto_Pattern;
               Recursive : Boolean
            ) return Boolean;
------------------------------------------------------------------------
--
-- Hook_Pattern -- Pattern hooking matching its argument pattern
--
   type Hook_Handler is abstract new Object.Entity with null record;
   type Hook_Handler_Ptr is access Hook_Handler'Class;
   function Image (Hook : Hook_Handler) return String is abstract;
   procedure On_Failure
             (  Hook   : in out Hook_Handler;
                Source : in out Source_Type
             )  is abstract;
   function On_Line_Change
            (  Hook   : access Hook_Handler;
               Source : access Source_Type;
               Where  : Location_Type
            )  return Result_Type is abstract;
   procedure On_Success
             (  Hook   : in out Hook_Handler;
                Source : in out Source_Type;
                From   : Integer;
                To     : Integer
             )  is abstract;
   package Hook_Handlers is
      new Object.Handle (Hook_Handler, Hook_Handler_Ptr);

   type Hook_Pattern (Hook : access Hook_Handler'Class) is
      new Pattern_Object with
   record
      Handler : Hook_Handlers.Handle;
      Pattern : Pattern_Type;
   end record;
   function Get_Type (Pattern : Hook_Pattern) return Statement_Type;
   function Image    (Pattern : Hook_Pattern) return String;
   function Resolve
            (  Pattern   : access Hook_Pattern;
               Reference : Pattern_Ref
            )  return Boolean;
   function Voidable
            (  Pattern   : Hook_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Lazy_Pattern -- Lazy pattern repeater
--
   type Lazy_Pattern is new Pattern_Object with record
      Pattern : Pattern_Type;
   end record;
   function Get_Type (Repeater : Lazy_Pattern) return Statement_Type;
   function Image    (Repeater : Lazy_Pattern) return String;
   function Resolve
            (  Pattern   : access Lazy_Pattern;
               Reference : Pattern_Ref
            )  return Boolean;
   function Voidable
            (  Repeater  : Lazy_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Letter_Pattern -- Pattern matching one letter
--
   type Letter_Pattern is new Pattern_Object with null record;
   function Image (Pattern : Letter_Pattern) return String;
   function Match
            (  Pattern : Letter_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Letter_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Lower_Case_Letter_Pattern -- Pattern matching one lower case letter
--
   type Lower_Case_Letter_Pattern is
      new Pattern_Object with null record;
   function Image (Pattern : Lower_Case_Letter_Pattern) return String;
   function Match
            (  Pattern : Lower_Case_Letter_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Lower_Case_Letter_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Natural_Number_Pattern -- Pattern matching a natural number
--
   type Natural_Number_Pattern
        (  Base : Strings_Edit.NumberBase
        )  is new Pattern_Object with null record;
   function Image (Pattern : Natural_Number_Pattern) return String;
   function Match
            (  Pattern : Natural_Number_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Natural_Number_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Next_Line_Pattern -- Pattern matching end of line and skipping it
--
   type Next_Line_Pattern is new Pattern_Object with null record;
   function Get_Type (Pattern : Next_Line_Pattern)
      return Statement_Type;
   function Image (Pattern : Next_Line_Pattern) return String;
   function Voidable
            (  Pattern   : Next_Line_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Next_Line_Or_EOF_Pattern -- End of line or end of file
--
   type Next_Line_Or_EOF_Pattern is new Next_Line_Pattern with
      null record;
   function Get_Type (Pattern : Next_Line_Or_EOF_Pattern)
      return Statement_Type;
   function Image (Pattern : Next_Line_Or_EOF_Pattern) return String;
------------------------------------------------------------------------
--
-- Nil_Pattern -- Pattern that never matches
--
   type Nil_Pattern is new Pattern_Object with null record;
   function Get_Type (Pattern : Nil_Pattern)
      return Statement_Type;
   function Image (Pattern : Nil_Pattern) return String;
   function Voidable
            (  Pattern   : Nil_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Nonempty_Pattern -- Pattern matching only nonempty substring
--
   type Nonempty_Pattern is new Pattern_Object with record
      Pattern : Pattern_Type;
   end record;
   function Get_Type (Pattern : Nonempty_Pattern) return Statement_Type;
   function Image    (Pattern : Nonempty_Pattern) return String;
   function Resolve
            (  Pattern   : access Nonempty_Pattern;
               Reference : Pattern_Ref
            )  return Boolean;
   function Voidable
            (  Pattern   : Nonempty_Pattern;
               Recursive : Boolean
            ) return Boolean;
------------------------------------------------------------------------
--
-- Not_Pattern -- Pattern matching empty substring when argument pattern
--                does match
--
   type Not_Pattern is new Pattern_Object with record
      Pattern : Pattern_Type;
   end record;
   function Get_Type (Pattern : Not_Pattern) return Statement_Type;
   function Image    (Pattern : Not_Pattern) return String;
   function Resolve
            (  Pattern   : access Not_Pattern;
               Reference : Pattern_Ref
            )  return Boolean;
   function Voidable
            (  Pattern   : Not_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Repeater_Pattern -- Pattern repeater
--
   type Repeater_Pattern is new Pattern_Object with record
      Count   : Positive;
      Pattern : Pattern_Type;
   end record;
   function Get_Type (Repeater : Repeater_Pattern)
      return Statement_Type;
   function Image (Repeater : Repeater_Pattern) return String;
   function Resolve
            (  Repeater  : access Repeater_Pattern;
               Reference : Pattern_Ref
            )  return Boolean;
   function Voidable
            (  Repeater  : Repeater_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Return_Pattern -- Matching failure or success
--
   type Return_Pattern (Result : Severity; Length : Natural) is
      new Pattern_Object with
   record
      case Result is
         when Aborted =>
            Message : String (1..Length);
         when Successful | Failed =>
            null;
      end case;
   end record;
   function Get_Type (Pattern : Return_Pattern)
      return Statement_Type;
   function Image (Pattern : Return_Pattern) return String;
   function Voidable
            (  Pattern   : Return_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Sequence_Pattern -- Pattern matching a sequence of patterns
--
   type Sequence_Pattern (Length : Positive) is
      new Pattern_Object with
   record
      Items : Pattern_Array (1..Length);
   end record;
   function Get_Type (Sequence : Sequence_Pattern)
      return Statement_Type;
   function Image (Sequence : Sequence_Pattern) return String;
   function Resolve
            (  Pattern   : access Sequence_Pattern;
               Reference : Pattern_Ref
            )  return Boolean;
   function Voidable
            (  Sequence  : Sequence_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Subscript_Digit_Pattern -- Pattern matching one of subscript digits
--
   type Subscript_Digit_Pattern is new Pattern_Object with null record;
   function Image (Pattern : Subscript_Digit_Pattern) return String;
   function Match
            (  Pattern : Subscript_Digit_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Subscript_Digit_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Superscript_Digit_Pattern -- Pattern   matching  one  of  superscript
--                              digits
--
   type Superscript_Digit_Pattern is
      new Pattern_Object with null record;
   function Image (Pattern : Superscript_Digit_Pattern) return String;
   function Match
            (  Pattern : Superscript_Digit_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Superscript_Digit_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Text_Pattern -- Pattern matching a text string, case sensitive
--
   type Text_Pattern (Length : Natural) is
      new Pattern_Object with
   record
      Text : String (1..Length);
   end record;
   function Image (Text : Text_Pattern) return String;
   function Match
            (  Text   : Text_Pattern;
               Source : access Source_Type;
               State  : access Match_State
            )  return Result_Type;
   function Voidable
            (  Text      : Text_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- Upper_Case_Letter_Pattern -- Pattern matching one upper case letter
--
   type Upper_Case_Letter_Pattern is
      new Pattern_Object with null record;
   function Image (Pattern : Upper_Case_Letter_Pattern) return String;
   function Match
            (  Pattern : Upper_Case_Letter_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : Upper_Case_Letter_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
--
-- XML_Literal_Pattern -- Pattern matching an XML literal
--
   type XML_Literal_Pattern is new Pattern_Object with null record;
   function Image (Pattern : XML_Literal_Pattern) return String;
   function Match
            (  Pattern : XML_Literal_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type;
   function Voidable
            (  Pattern   : XML_Literal_Pattern;
               Recursive : Boolean
            )  return Boolean;
------------------------------------------------------------------------
   type Print_Mode is (Put_Mode, Put_Line_Mode, Trace_Mode);
   type Print_Handler
        (  Mode          : Print_Mode;
           Prefix_Length : Natural;
           Suffix_Length : Natural
        )  is new Hook_Handler with record
      File   : Ada.Text_IO.File_Access;
      Prefix : String (1..Prefix_Length);
      Suffix : String (1..Suffix_Length);
   end record;
   function Image (Hook : Print_Handler) return String;
   procedure On_Failure
             (  Hook   : in out Print_Handler;
                Source : in out Source_Type
             );
   function On_Line_Change
            (  Hook   : access Print_Handler;
               Source : access Source_Type;
               Where  : Location_Type
            )  return Result_Type;
   procedure On_Success
             (  Hook   : in out Print_Handler;
                Source : in out Source_Type;
                From   : Integer;
                To     : Integer
             );
------------------------------------------------------------------------
   type Alternation_Ptr is access constant Alternate_Pattern;
   type Anything_Ptr    is access constant Anything_Pattern;
   type Eager_Ptr       is access constant Eager_Pattern;
   type Fence_Ptr       is access constant Fence_Pattern;
   type Goto_Ptr        is access constant Goto_Pattern;
   type Hook_Ptr        is access constant Hook_Pattern;
   type Lazy_Ptr        is access constant Lazy_Pattern;
   type Next_Line_Ptr   is access constant Next_Line_Pattern;
   type Nil_Ptr         is access constant Nil_Pattern;
   type Nonempty_Ptr    is access constant Nonempty_Pattern;
   type Not_Ptr         is access constant Not_Pattern;
   type Repeater_Ptr    is access constant Repeater_Pattern;
   type Return_Ptr      is access constant Return_Pattern;
   type Sequence_Ptr    is access constant Sequence_Pattern;
   type Statement_Data
        (  Kind_Of : Statement_Type := Sequence_Statement
        )  is
   record
      case Kind_Of is
         when No_Statement =>
            null;
         when Alternate_Statement =>
            Alternate_Statement : Alternation_Ptr;
            Alternate_Current   : Natural;
            Alternate_Pointer   : Integer;
         when Anything_Statement =>
            Anything_Statement  : Anything_Ptr;
            Anything_Pointer    : Integer;
         when Goto_Statement =>
            Goto_Statement      : Goto_Ptr;
            Goto_Target         : Pattern_Ptr;
         when Eager_Statement =>
            Eager_Statement     : Eager_Ptr;
            Eager_Count         : Natural;
         when Fence_Statement =>
            Fence_Statement     : Fence_Ptr;
            Fence_Pointer       : Integer;
         when Hook_Statement =>
            Hook_Statement      : Hook_Ptr;
            Hook_Pointer        : Integer;
            Hook_Stub           : Integer;
         when Lazy_Statement =>
            Lazy_Statement      : Lazy_Ptr;
            Lazy_Pointer        : Integer;
            Lazy_Parent         : Integer;
         when Nil_Statement =>
            Nil_Statement       : Nil_Ptr;
         when Next_Line_Statement | Next_Line_Or_EOF_Statement =>
            Next_Line_Statement : Next_Line_Ptr;
            Next_Line_At        : Location_Type;
         when Nonempty_Statement =>
            Nonempty_Statement  : Nonempty_Ptr;
            Nonempty_Pointer    : Integer;
            Nonempty_Stub       : Integer;
         when Not_Statement =>
            Not_Statement       : Not_Ptr;
            Not_Pointer         : Integer;
            Not_Stub            : Integer;
         when Repeater_Statement =>
            Repeater_Statement  : Repeater_Ptr;
            Repeater_Count      : Natural;
         when Return_Statement =>
            Return_Statement    : Return_Ptr;
         when Sequence_Statement =>
            Sequence_Statement  : Sequence_Ptr;
            Sequence_Current    : Natural;
         when Stub_Statement =>
            Stub_Pointer        : Integer;
            Stub_Data           : Integer := 0;
      end case;
   end record;
   subtype Active_Hook_Data  is Statement_Data (Active_Hook_Statement);
   subtype Active_Lazy_Data  is Statement_Data (Active_Lazy_Statement);
   subtype Alternate_Data    is Statement_Data (Alternate_Statement);
   subtype Anything_Data     is Statement_Data (Anything_Statement);
   subtype Eager_Data        is Statement_Data (Eager_Statement);
   subtype Fence_Data        is Statement_Data (Fence_Statement);
   subtype Goto_Data         is Statement_Data (Goto_Statement);
   subtype Next_Line_Data    is Statement_Data (Next_Line_Statement);
   subtype Nil_Data          is Statement_Data (Nil_Statement);
   subtype Nonempty_Data     is Statement_Data (Nonempty_Statement);
   subtype Not_Data          is Statement_Data (Not_Statement);
   subtype Passive_Hook_Data is Statement_Data (Passive_Hook_Statement);
   subtype Passive_Lazy_Data is Statement_Data (Passive_Lazy_Statement);
   subtype Repeater_Data     is Statement_Data (Repeater_Statement);
   subtype Sequence_Data     is Statement_Data (Sequence_Statement);
   subtype Stub_Data         is Statement_Data (Stub_Statement);
   type Statement_Data_Ptr   is access all Statement_Data;
   type Statement_Data_Array is
      array (Positive range <>) of Statement_Data;

   type Statement_Stack (Size : Positive) is limited record
      Count : Natural := 0;
      Stack : Statement_Data_Array (1..Size);
   end record;
   procedure Dump (Stack : Statement_Stack; Prefix : String := "");
   procedure Pop (Stack : in out Statement_Stack);
   procedure Push
             (  Stack : in out Statement_Stack;
                Data  : Statement_Data
             );
   function Top (Stack : Statement_Stack) return Statement_Data;
   function Top (Stack : Statement_Stack) return Statement_Type;
   pragma Inline (Pop, Push, Top);

   type Match_State (Size : Positive) is limited record
      Trace   : Boolean := False;
      ASS     : Statement_Stack (Size);
      MSS     : Statement_Stack (Size);
      Pattern : Pattern_Type;
   end record;

   Empty_Error       : constant String := "Empty string matched";
   Encoding_Error    : constant String := "UTF-8 encoding error";
   Multiline_Error   : constant String := "Several lines matched";
   End_Of_File_Error : constant String := "End of source reached";
   Return_Error      : constant String := "Attempt to return to the " &
                                          "previous source line";
   Voidable_Error    : constant String := "Pattern can match an "     &
                                          "empty string";
------------------------------------------------------------------------
   String_Increment  : constant := 20;
   Segment_Increment : constant := 10;
   type String_Ptr is access String;
   type Segment is record
      Where  : Location_Type;
      Length : Integer;
   end record;
   type Segment_Array is array (Positive range <>) of Segment;
   type Segment_Ptr   is access Segment_Array;
   type Variable_Object is new Object.Entity with record
      Where   : Location_Type;
      Length  : Integer := -1; -- Undefined
      Depth   : Natural :=  0;
      Value   : String_Ptr;
      History : Segment_Ptr;
   end record;
   type Variable_Ptr is access Variable_Object'Class;
   procedure Finalize (Variable : in out Variable_Object);

   package Variable_Handles is
      new Object.Handle (Variable_Object, Variable_Ptr);
   type Variable_Type is record
      Handle : Variable_Handles.Handle :=
               Variable_Handles.Ref (new Variable_Object);
   end record;

   type Assignment_Handler (Append : Boolean) is
      new Hook_Handler with
   record
      Variable : Variable_Type;
   end record;
   function Image (Hook : Assignment_Handler) return String;
   procedure On_Failure
             (  Hook   : in out Assignment_Handler;
                Source : in out Source_Type
             );
   function On_Line_Change
            (  Hook   : access Assignment_Handler;
               Source : access Source_Type;
               Where  : Location_Type
            )  return Result_Type;
   procedure On_Success
             (  Hook   : in out Assignment_Handler;
                Source : in out Source_Type;
                From   : Integer;
                To     : Integer
             );
------------------------------------------------------------------------
   type User_Assignment_Handler (Append : Boolean) is
      new Hook_Handler with
   record
      Variable : User_Variable_Handles.Handle;
   end record;
   function Image (Hook : User_Assignment_Handler) return String;
   procedure On_Failure
             (  Hook   : in out User_Assignment_Handler;
                Source : in out Source_Type
             );
   function On_Line_Change
            (  Hook   : access User_Assignment_Handler;
               Source : access Source_Type;
               Where  : Location_Type
            )  return Result_Type;
   procedure On_Success
             (  Hook   : in out User_Assignment_Handler;
                Source : in out Source_Type;
                From   : Integer;
                To     : Integer
             );
------------------------------------------------------------------------
--
-- Generic variable instance may be finalizaed prematurely. This variant
-- of  assignment  handler  delegates  primitive  operations  to   plain
-- pointers,  which  can  be  zeroed upon finalization. In that case the
-- hook pattern will become void .
--
   type Variable_Assignment_Handler;

   type On_Failure_Ptr is access procedure
        (  Hook   : in out Variable_Assignment_Handler;
           Source : in out Source_Type
        );
   type On_Line_Change_Ptr is access function
        (  Hook   : access Variable_Assignment_Handler;
           Source : access Source_Type;
           Where  : Location_Type
        )  return Result_Type;
   type On_Success_Ptr is access procedure
        (  Hook   : in out Variable_Assignment_Handler;
           Source : in out Source_Type;
           From   : Integer;
           To     : Integer
        );
   type Variable_Assignment_Handler (Append : Boolean) is
      new Hook_Handler with
   record
      On_Failure     : On_Failure_Ptr;
      On_Line_Change : On_Line_Change_Ptr;
      On_Success     : On_Success_Ptr;
   end record;
   function Image (Hook : Variable_Assignment_Handler) return String;
   procedure On_Failure
             (  Hook   : in out Variable_Assignment_Handler;
                Source : in out Source_Type
             );
   function On_Line_Change
            (  Hook   : access Variable_Assignment_Handler;
               Source : access Source_Type;
               Where  : Location_Type
            )  return Result_Type;
   procedure On_Success
             (  Hook   : in out Variable_Assignment_Handler;
                Source : in out Source_Type;
                From   : Integer;
                To     : Integer
             );
end Parsers.Generic_Source.Patterns;
