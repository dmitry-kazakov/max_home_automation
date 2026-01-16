--                                                                    --
--  procedure Test_Patterns         Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Summer, 2025       --
--                                                                    --
--                                Last revision :  21:31 04 Jan 2026  --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Text_IO;          use Ada.Text_IO;
with Strings_Edit.Quoted;  use Strings_Edit.Quoted;

with Parsers.Multiline_Patterns.Fields;
with Parsers.Multiline_Patterns.Floats;
with Parsers.Multiline_Patterns.Integers;
with Parsers.Multiline_Source.Stream_IO;
with Parsers.String_Patterns;
with Parsers.String_Source;
with Strings_Edit.Floats;
with Strings_Edit.Integers;
with Strings_Edit.Streams;
with Strings_Edit.UTF8;
with Test_Patterns_Instantiations;

procedure Test_Patterns is
   use Test_Patterns_Instantiations;
   Stack_Size : constant := 300;
   Tracing    : constant Boolean  := False;
begin
   Put_Line ("Alternation test 1");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      Line  : aliased String := "abcdef";
      Code  : aliased Source (Line'Access);
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Result : constant Result_Type :=
                  Match
                  (  Text ("abce") or Text ("abcd"),
                     Code'Access,
                     State'Access
                  );
      begin
         if Result.Outcome /= Successful then
            Raise_Exception
            (  Data_Error'Identity,
              "Alternation test 1 ""abcd"" not matched"
            );
         end if;
      end;
   end;
   Put_Line ("Alternation test 2");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      Line    : aliased String := "abcdef";
      Code    : aliased Source (Line'Access);
      State   : aliased Match_State (Stack_Size);
      Pattern : constant Pattern_Type :=
                         Text ("ab") or Text ("abc");
   begin
      Set_Tracing (State, Tracing);
      declare
         Result : constant Result_Type :=
                  Match (Pattern, Code'Access, State'Access);
      begin
         if Result.Outcome /= Successful then
            Raise_Exception
            (  Data_Error'Identity,
              "Alternation test  ""ab"" not matched"
            );
         end if;
      end;
      declare
         Result : constant Result_Type :=
                  Rematch (Code'Access, State'Access);
      begin
         if Result.Outcome /= Successful then
            Raise_Exception
            (  Data_Error'Identity,
              "Alternation test 2 ""abc"" not rematched"
            );
         end if;
      end;
   end;
   Put_Line ("Sequence test 1");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      Line    : aliased String := "aBCDef";
      Code    : aliased Source (Line'Access);
      State   : aliased Match_State (Stack_Size);
      Pattern : constant Pattern_Type :=
                         Case_Insensitive_Text ("aBc") &
                         Case_Insensitive_Text ("DEF");
   begin
      Set_Tracing (State, Tracing);
      declare
         Result : constant Result_Type :=
                  Match (Pattern, Code'Access, State'Access);
      begin
         if Result.Outcome /= Successful then
            Raise_Exception
            (  Data_Error'Identity,
              "Sequence test 1 ""abcdef"" not matched"
            );
         end if;
      end;
   end;
   Put_Line ("Sequence test 2");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State   : aliased Match_State (Stack_Size);
      Pattern : constant Pattern_Type :=
                         (Text ("a") or Text ("b")) &
                         (Text ("c") or Text ("d"));
   begin
      Set_Tracing (State, Tracing);
      declare
         Line : aliased String := "ac";
         Code : aliased Source (Line'Access);
      begin
         declare
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                 "Sequence test 2 ""ac"" not matched"
               );
            end if;
         end;
         Line := "bc";
         Set_Pointer (Code, 1, False);
         declare
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                 "Sequence test 2 ""bc"" not matched"
               );
            end if;
         end;
         Line := "ad";
         Set_Pointer (Code, 1, False);
         declare
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                 "Sequence test 2 ""ad"" not matched"
               );
            end if;
         end;
         Line := "bd";
         Set_Pointer (Code, 1, False);
         declare
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                 "Sequence test 2 ""bd"" not matched"
               );
            end if;
         end;
      end;
   end;
   Put_Line ("Any_Of test");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State   : aliased Match_State (Stack_Size);
      Pattern : constant Pattern_Type := Any_Of ("abc");
   begin
      Set_Tracing (State, Tracing);
      declare
         Line : aliased String := "cdef";
         Code : aliased Source (Line'Access);
      begin
         declare
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Any_Of test ""c"" not matched"
               );
            elsif Get_Pointer (Code) /= 2 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Any_Of test 1 Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 2 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Any test");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State   : aliased Match_State (Stack_Size);
      Pattern : constant Pattern_Type := Any & Any;
   begin
      Set_Tracing (State, Tracing);
      declare
         Line : aliased String := "cdef";
         Code : aliased Source (Line'Access);
      begin
         declare
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Any test ""cd"" not matched"
               );
            elsif Get_Pointer (Code) /= 3 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Any test Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 3 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Blank test");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := Any & Blank & Any;
         Line    : aliased String := "a   b";
         Code    : aliased Source (Line'Access);
      begin
         declare
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Blank test ""a   b"" not matched"
               );
            elsif Get_Pointer (Code) /= 6 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Blank test ""a   b"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 6 (expected)"
               )  );
            end if;
         end;
      end;
      declare
         Pattern : constant Pattern_Type := Any & Space;
         Line    : aliased String := "a   b";
         Code    : aliased Source (Line'Access);
      begin
         declare
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Blank test ""a "" not matched"
               );
            elsif Get_Pointer (Code) /= 3 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Blank test ""a "" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 3 (expected)"
               )  );
            end if;
         end;
      end;
      declare
         Pattern : constant Pattern_Type := Any & Blank_Or_Empty;
         Line    : aliased String := "ab";
         Code    : aliased Source (Line'Access);
      begin
         declare
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Blank test ""a"" not matched"
               );
            elsif Get_Pointer (Code) /= 2 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Blank test ""a"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 2 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Repeater test");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := Text ("ab") * 5;
         Line    : aliased String := "abababababc";
         Code    : aliased Source (Line'Access);
      begin
         declare
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Repeater test ""ababababab"" not matched"
               );
            elsif Get_Pointer (Code) /= 11 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Repeater test ""ababababab"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 11 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Eager repeater test 1");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := +(Text ("a") or Text ("b"));
      begin
         declare
            Line   : aliased String := "ababc";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Eager repeater test 1 ""abab"" not matched"
               );
            elsif Get_Pointer (Code) /= 5 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Eager test 1 ""abab"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 5 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Eager repeater test 2");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type :=
                   +(Text ("a") or Text ("b")) & Text ("c");
      begin
         declare
            Line   : aliased String := "ababc";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Eager repeater test 2 ""ababc"" not matched"
               );
            elsif Get_Pointer (Code) /= 6 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Eager test 2 ""ababc"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 6 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Eager repeater test 3");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := +Any or Text ("c");
      begin
         declare
            Line   : aliased String := "ac";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Eager repeater test 3 ""ac"" not matched"
               );
            elsif Get_Pointer (Code) /= 3 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Eager test 3 ""ac"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 3 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Digit test");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := Digit;
      begin
         declare
            Line   : aliased String := "3";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Digit test ""3"" not matched"
               );
            elsif Get_Pointer (Code) /= 2 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Digit test ""3"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 2 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Subscript digit test");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := Subscript_Digit;
      begin
         declare
            Line   : aliased String := Character'Val (16#E2#) &
                                       Character'Val (16#82#) &
                                       Character'Val (16#83#);
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Subscript digit test "
                  &  Quote (Line)
                  &  " not matched"
               )  );
            elsif Get_Pointer (Code) /= 4 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Subscript digit test "
                  &  Quote (Line)
                  &  " Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 4 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Superscript digit test");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := Superscript_Digit;
      begin
         declare
            Line   : aliased String := Character'Val (16#E2#) &
                                       Character'Val (16#81#) &
                                       Character'Val (16#B4#);
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Superscript digit test "
                  &  Quote (Line)
                  &  " not matched"
               )  );
            elsif Get_Pointer (Code) /= 4 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Superscript digit test "
                  &  Quote (Line)
                  &  " Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 4 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("XML literal test");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := XML_Literal;
      begin
         declare
            Line   : aliased String := "'Smith&amp;Sons'";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "XML literal test "
                  &  Quote (Line)
                  &  " not matched"
               )  );
            elsif Get_Pointer (Code) /= 17 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "XML literal test "
                  &  Quote (Line)
                  &  " Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 17 (expected)"
               )  );
            end if;
         end;
      end;
   end;

   Put_Line ("Letter test");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := Letter;
      begin
         declare
            Line   : aliased String := Character'Val (16#C3#) &
                                       Character'Val (16#A4#);
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Letter test " & Quote (Line) & " not matched"
               );
            elsif Get_Pointer (Code) /= 3 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Letter test " & Quote (Line) & " Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 3 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Alphanumeric test");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := Alphanumeric;
      begin
         declare
            Line   : aliased String := "J";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Alphanumeric test "
                  &  Quote (Line)
                  &  " not matched"
               )  );
            elsif Get_Pointer (Code) /= 2 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Alphanumeric test "
                  &  Quote (Line)
                  &  " Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 2 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Lower case letter test");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := Lower_Case_Letter;
      begin
         declare
            Line   : aliased String := Character'Val (16#C3#) &
                                       Character'Val (16#A4#);
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Lower case letter test "
                  &  Quote (Line)
                  &  " not matched"
               )  );
            elsif Get_Pointer (Code) /= 3 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Lower case letter test "
                  &  Quote (Line)
                  &  " Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 3 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Upper case letter test");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := Upper_Case_Letter;
      begin
         declare
            Line   : aliased String := Character'Val (16#C3#) &
                                       Character'Val (16#84#);
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Upper case letter test "
                  &  Quote (Line)
                  &  " not matched"
               )  );
            elsif Get_Pointer (Code) /= 3 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Upper case letter test "
                  &  Quote (Line)
                  &  " Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 3 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Empty test 1");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := (Text ("a") or Empty);
      begin
         declare
            Line   : aliased String := "a";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Empty test 1 ""a"" not matched"
               );
            elsif Get_Pointer (Code) /= 2 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Empty test 1 ""a"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 2 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Empty test 2");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := (Text ("a") or Empty);
      begin
         declare
            Line   : aliased String := "b";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Empty test 2 """" not matched"
               );
            elsif Get_Pointer (Code) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Empty test 2 """" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 1 (expected)"
               )  );
            end if;
         end;
      end;
   end;
--end if;
   Put_Line ("End of line test 1");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := (Text ("a") & End_Of_Line);
      begin
         declare
            Line   : aliased String := "a";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "End of line test 1 ""a"" not matched"
               );
            elsif Get_Pointer (Code) /= 2 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "End of line test 1 ""a"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 2 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("End of line test 2");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := End_Of_Line;
      begin
         declare
            Line   : aliased String := "b";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome = Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "End of line test 2 """" matched"
               );
            elsif Get_Pointer (Code) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "End of line test 2 """" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 1 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Lazy repeater test 1");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := -(Text ("a") or Text ("b"));
      begin
         declare
            Line   : aliased String := "ababc";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Lazy repeater test 1 """" not matched"
               );
            elsif Get_Pointer (Code) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Lazy test 1 """" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 1 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Lazy repeater test 2");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type :=
                   -(Text ("a") or Text ("b")) & Text ("c");
      begin
         declare
            Line   : aliased String := "ababc";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Lazy repeater test 2 ""ababc"" not matched"
               );
            elsif Get_Pointer (Code) /= 6 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Lazy test 2 ""ababc"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 6 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Lazy repeater test 3");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := -Any & Text ("c");
      begin
         declare
            Line   : aliased String := "ac";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Lazy repeater test 3 ""ac"" not matched"
               );
            elsif Get_Pointer (Code) /= 3 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Lazy test 3 ""ac"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 3 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Lazy repeater test 4");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := -Text ("a") & Text ("b");
      begin
         declare
            Line   : aliased String := "b";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Lazy repeater test 4 ""b"" not matched"
               );
            elsif Get_Pointer (Code) /= 2 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Lazy test 4 ""b"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 2 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Lazy repeater test 5");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := -Text ("a") & Text ("c");
      begin
         declare
            Line   : aliased String := "aac";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Lazy repeater test 5 ""aac"" not matched"
               );
            elsif Get_Pointer (Code) /= 4 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Lazy test 5 ""aac"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 4 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Lazy repeater test 6");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := -Text ("a") & Text ("c");
      begin
         declare
            Line   : aliased String := "aab";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome = Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Lazy repeater test 6 ""aab"" matched"
               );
            elsif Get_Pointer (Code) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Lazy test 6 ""aab"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 1 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Not test 1");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := Text ("a") & not Text ("b");
      begin
         declare
            Line   : aliased String := "ab";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome = Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Not test 1 ""ab"" matched"
               );
            elsif Get_Pointer (Code) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Not test 1 ""ab"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 1 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Not test 2");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := Text ("a") & not Text ("b");
      begin
         declare
            Line   : aliased String := "ac";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Not test 2 ""a"" not matched"
               );
            elsif Get_Pointer (Code) /= 2 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Not test 1 ""a"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 2 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Fence test 1");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type :=
                      Text ("a") & (-Any) & Fence & Text ("b");
      begin
         declare
            Line   : aliased String := "aaab";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Fence test 1 ""aaab"" not matched"
               );
            elsif Get_Pointer (Code) /= 5 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Fence test 1 ""aaab"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 5 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Fence test 2");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type :=
                      Text ("a") & (-Any) & Fence & Text ("b");
      begin
         declare
            Line   : aliased String := "aaac";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome = Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Fence test 2 ""aaac"" matched"
               );
            elsif Get_Pointer (Code) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Fence test 2 ""aaac"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 1 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Variable test 1");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      Variable : Variable_Type;
      Line     : aliased String := "abcdefghij";
      Code     : aliased Source (Line'Access);
   begin
      if Defined (Variable) then
         Raise_Exception
         (  Data_Error'Identity,
            "Variable test 1 defined"
         );
      end if;
      Set (Variable, "abc", Direct_Link (Code, 1, 3));
      if not Defined (Variable) then
         Raise_Exception
         (  Data_Error'Identity,
            "Variable test 1 undefined"
         );
      elsif Value (Variable) /= "abc" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Variable test 1 is "
            &  Quote (Value (Variable))
            &  " /= ""abc"" (expected)"
         )  );
      elsif Link (Variable) /= (1, 3) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Variable test 1 is "
            &  Image (Link (Variable))
            &  " /= 1..3 (expected)"
         )  );
      end if;
      Append (Variable, "d", Direct_Link (Code, 4, 4));
      if Value (Variable) /= "abcd" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Variable test 1 is "
            &  Quote (Value (Variable))
            &  " /= ""abcd"" (expected)"
         )  );
      elsif Link (Variable) /= (1, 4) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Variable test 1 is "
            &  Image (Link (Variable))
            &  " /= 1..4 (expected)"
         )  );
      end if;
      Append (Variable, "ef", Direct_Link (Code, 5, 6));
      if Value (Variable) /= "abcdef" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Variable test 1 is "
            &  Quote (Value (Variable))
            &  " /= ""abcdef"" (expected)"
         )  );
      elsif Link (Variable) /= (1, 6) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Variable test 1 is "
            &  Image (Link (Variable))
            &  " /= 1..6 (expected)"
         )  );
      end if;
      Truncate (Variable);
      if Value (Variable) /= "abcd" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Variable test 1 is "
            &  Quote (Value (Variable))
            &  " /= ""abcd"" (expected)"
         )  );
      elsif Link (Variable) /= (1, 4) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Variable test 1 is "
            &  Image (Link (Variable))
            &  " /= 1..4 (expected)"
         )  );
      end if;
   end;
   Put_Line ("Variable test 2");
   declare
      use Parsers.String_Source;
      use Parsers.String_Patterns;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Variable : Variable_Type;
         Pattern  : constant Pattern_Type :=
                       Variable <= Text ("a");
      begin
         declare
            Line   : aliased String := "a";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Variable test 2 ""a"" not matched"
               );
            elsif Get_Pointer (Code) /= 2 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 2 ""a"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 2 (expected)"
               )  );
            elsif Value (Variable) /= "a" then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 2 value "
                  &  Quote (Value (Variable))
                  &  " /= ""a"" (expected)"
               )  );
            elsif Image (Link (Variable)) /= "1..1" then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 2 location "
                  &  Image (Link (Variable))
                  &  " /= 1..1 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Variable test 3");
   declare
      use Parsers.String_Source;
      use Parsers.String_Patterns;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         N       : Variable_Type;
         Pattern : constant Pattern_Type :=
                            N <= (Digit & (+(Digit or "_"))) & Fence;
      begin
         declare
            Line   : aliased String := "12_4a";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Variable test 3 ""12_4"" not matched"
               );
            elsif Get_Pointer (Code) /= 5 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 3 ""12_4"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 5 (expected)"
               )  );
            elsif Value (N) /= "12_4" then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 3 value "
                  &  Quote (Value (N))
                  &  " /= ""12_4"" (expected)"
               )  );
            elsif Image (Link (N)) /= "1..4" then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 3 location "
                  &  Image (Link (N))
                  &  " /= 1..4 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Variable test 4");
   declare
      use Parsers.String_Source;
      use Parsers.String_Patterns;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         N       : Variable_Type;
         Pattern : constant Pattern_Type :=
            N <= (  Digit
                 &  (  + (  Digit
                         or (  "_"
                            &  (  "_"       & Failure ("double '_'")
                               or not Digit & Failure ("closing '_'")
                               or ""
                         )  )  )
                 )  )
                 &  Fence;
      begin
         declare
            Line   : aliased String := "12_a";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Aborted then
               Raise_Exception
               (  Data_Error'Identity,
                  "Variable test 4 ""12_"" matched"
               );
            elsif Get_Pointer (Code) /= 4 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 4 ""12_"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 4 (expected)"
               )  );
            elsif Defined (N) then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 4 value is defined"
               )  );
            end if;
            Put_Line (Result.Message & " at " & Image (Result.Where));
         end;
      end;
   end;
   Put_Line ("Variable test 5");
   declare
      use Parsers.String_Source;
      use Parsers.String_Patterns;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         N       : Variable_Type;
         Pattern : constant Pattern_Type :=
            N <= (  Digit
                 &  (  + (  Digit
                         or (  "_"
                            &  (  "_"       & Failure ("double '_'")
                               or not Digit & Failure ("closing '_'")
                               or ""
                         )  )  )
                 )  )
                 &  Fence;
      begin
         declare
            Line   : aliased String := "12_4a";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Variable test 5 ""12_4"" not matched"
               );
            elsif Get_Pointer (Code) /= 5 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 5 ""12_4"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 5 (expected)"
               )  );
            elsif not Defined (N) then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 5 value is not defined"
               )  );
            elsif Value (N) /= "12_4" then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 5 value is not 12_4"
               )  );
            elsif Link (N) /= (1, 4) then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 5 location is "
                  &  Image (Link (N))
                  &  " is not 1..4 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Variable test 6");
   declare
      use Parsers.String_Source;
      use Parsers.String_Patterns;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         N       : Variable_Type;
         Pattern : constant Pattern_Type :=
                    (N <= Digit)
                 &  (  + (  N < Digit
                         or (  "_"
                            &  (  "_"       & Failure ("double '_'")
                               or not Digit & Failure ("closing '_'")
                               or ""
                         )  )  )
                    )
                 &  Fence;
      begin
         declare
            Line   : aliased String := "12_4a";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Variable test 6 ""12_4"" not matched"
               );
            elsif Get_Pointer (Code) /= 5 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 6 ""12_4"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 5 (expected)"
               )  );
            elsif not Defined (N) then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 6 value is not defined"
               )  );
            elsif Value (N) /= "124" then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 6 value is "
                  &  Quote (Value (N))
                  &  " /= ""124"" (expected)"
               )  );
            elsif Link (N) /= (1, 4) then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Variable test 6 location is "
                  &  Image (Link (N))
                  &  " is not 1..4 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("User pattern test 1");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type :=
                      Identifier_Patterns.Pattern & Text ("*");
      begin
         declare
            Line   : aliased String := "a_c*";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "User pattern test 2 ""a_c"" not matched"
               );
            elsif Get_Pointer (Code) /= 5 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "User pattern test 2 ""a_c"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 5 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("User pattern test 2");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type :=
                      Identifier_Patterns.Pattern;
      begin
         declare
            Line   : aliased String := "a1_*";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Aborted then
               Raise_Exception
               (  Data_Error'Identity,
                  "User pattern test 2 ""a1_"" not aborted"
               );
            elsif Get_Pointer (Code) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "User pattern test 2 ""a1_"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 1 (expected)"
               )  );
            end if;
            Put_Line (Result.Message & " at " & Image (Result.Where));
         end;
      end;
   end;
   Put_Line ("User pattern test 3");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type :=
                      Number_Patterns.Pattern (12) & Text ("*");
      begin
         declare
            Line   : aliased String := "a12b**";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "User pattern test 3 ""a12b*"" not matched"
               );
            elsif Get_Pointer (Code) /= 6 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "User pattern test 3 ""a12b*"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 6 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("User pattern test 4");
   declare
      use Strings_Edit.UTF8;
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type :=
            Normalized_String_Patterns.Pattern (Image (16#1E69#));
      begin
         declare
            Line   : aliased String := Image (16#0073#) &
                                       Image (16#0323#) &
                                       Image (16#0307#) &
                                       "abc";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "User pattern test 4 """ & Line & """ not matched"
               );
            elsif Get_Pointer (Code) /= 6 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "User pattern test 4 """ & Line & """ Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 6 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Non-empty pattern test 1");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := Nonempty (+Any) & "c";
      begin
         declare
            Line   : aliased String := "ac*";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Non-empty pattern test 1 ""ac"" not matched"
               );
            elsif Get_Pointer (Code) /= 3 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Non-empty pattern test 1 ""ac"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 3 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Non-empty pattern test 2");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := Nonempty (+Any) & "c";
      begin
         declare
            Line   : aliased String := "c*";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Failed then
               Raise_Exception
               (  Data_Error'Identity,
                  "Non-empty pattern test 2 ""ac"" matched"
               );
            elsif Get_Pointer (Code) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Non-empty pattern test 2 ""ac"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 1 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Put_Line pattern test");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type :=
                            "a" & Put_Line (+Any, ">", "<") & "b";
      begin
         declare
            Line   : aliased String := "aaaca*";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Failed then
               Raise_Exception
               (  Data_Error'Identity,
                  "Put_Line pattern test ""aaaca"" matched"
               );
            elsif Get_Pointer (Code) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Put_Line pattern test ""aaaca"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 1 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Anything test 1");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := "a" & Anything & "b";
      begin
         declare
            Line   : aliased String := "ab*";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Anything test 1 ""ab"" not matched"
               );
            elsif Get_Pointer (Code) /= 3 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Anything test 1 ""ab"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 3 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Anything test 2");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := "a" & Anything & "b";
      begin
         declare
            Line   : aliased String := "aab*";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Anything test 2 ""aab"" not matched"
               );
            elsif Get_Pointer (Code) /= 4 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Anything test 2 ""aab"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 4 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Anything test 3");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := "a" & Anything & "b";
      begin
         declare
            Line   : aliased String := "aac*";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome = Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Anything test 3 ""aac"" matched"
               );
            elsif Get_Pointer (Code) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Anything test 3 ""aac"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 1 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Trace pattern test");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := "a" & Trace (+Any) & "b";
      begin
         declare
            Line   : aliased String := "aaaca*";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Failed then
               Raise_Exception
               (  Data_Error'Identity,
                  "Trace pattern test ""aaaca"" matched"
               );
            elsif Get_Pointer (Code) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Trace pattern test ""aaaca"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 1 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Tracing test 1");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, True);
      declare
         Pattern : constant Pattern_Type := (-Any) & "a" & (-Any) & "b";
      begin
         declare
            Line   : aliased String := "aaaca";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Failed then
               Raise_Exception
               (  Data_Error'Identity,
                  "Tracing test 1 ""aaaca"" matched"
               );
            elsif Get_Pointer (Code) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Tracing test 1 ""aaaca"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 1 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Tracing test 2");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, True);
      declare
         Pattern : constant Pattern_Type :=
                            (-Any) & Fence & "a" & (-Any) & "b";
      begin
         declare
            Line   : aliased String := "aaaca";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Failed then
               Raise_Exception
               (  Data_Error'Identity,
                  "Tracing test 1 ""aaaca"" matched"
               );
            elsif Get_Pointer (Code) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Tracing test 1 ""aaaca"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 1 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Recursive test 1");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type :=
                            Def ("(" & Self & ")" or "");
      begin
         declare
            Line   : aliased String := "(())";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Recursive test 1 ""(())"" not matched"
               );
            elsif Get_Pointer (Code) /= 5 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Recursive test 1 ""(())"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 5 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Nil test");
   declare
      use Parsers.String_Patterns;
      use Parsers.String_Source;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := Any & Nil or "a";
      begin
         declare
            Line   : aliased String := "a";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Nil test ""a"" not matched"
               );
            elsif Get_Pointer (Code) /= 2 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Nil test ""a"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 1 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("As Text test");
   declare
      use Parsers.String_Source;
      use Parsers.String_Patterns;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         V       : Variable_Type;
         Pattern : constant Pattern_Type := (V <= Any) & "a" & Text (V);
      begin
         declare
            Line   : aliased String := "xax";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "As Text test ""xax"" not matched"
               );
            elsif Get_Pointer (Code) /= 4 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "As Text test ""xax"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 4 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("As Case_Insensitive_Text test");
   declare
      use Parsers.String_Source;
      use Parsers.String_Patterns;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         V       : Variable_Type;
         Pattern : constant Pattern_Type :=
                      (V <= Any) & "a" & Case_Insensitive_Text (V);
      begin
         declare
            Line   : aliased String := "xaX";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "As Case_Insensitive_Text test ""xaX"" not matched"
               );
            elsif Get_Pointer (Code) /= 4 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "As Case_Insensitive_Text test ""xaX"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 4 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("Natural_Number test");
   declare
      use Parsers.String_Source;
      use Parsers.String_Patterns;
      State : aliased Match_State (Stack_Size);
   begin
      Set_Tracing (State, Tracing);
      declare
         Pattern : constant Pattern_Type := Natural_Number (13);
      begin
         declare
            Line   : aliased String := "1AbCD";
            Code   : aliased Source (Line'Access);
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Natural_Number test ""1AbCD"" not matched"
               );
            elsif Get_Pointer (Code) /= 5 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Natural_Number test ""1AbCD"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 5 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("NL test 1");
   declare
      use Parsers.Multiline_Source;
      use Parsers.Multiline_Patterns;
      use Strings_Edit.Streams;
      LF    : constant Character := Character'Val (10);
      State : aliased Match_State (Stack_Size);
      Data  : aliased String_Stream (80);
      Text  : Variable_Type;
   begin
      Set_Tracing (State, Tracing);
      Set (Data, "abcd" & LF & "ef*");
      declare
         Code    : aliased Stream_IO.Source (Data'Access);
         Pattern : constant Pattern_Type :=
                      (Text <= (+Letter)) & End_of_Line & NL &
                      (Text < (+Letter))  & Fence;
      begin
         declare
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "NL test 1 ""abcdef"" not matched"
               );
            elsif Get_Pointer (Code) /= 3 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "NL test 1 ""abcdef"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 3 (expected)"
               )  );
            elsif Value (Text) /= "abcdef" then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "NL test 1 varaible"
                  &  Quote (Value (Text))
                  &  " /= ""abcdef"" (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("NL test 2");
   declare
      use Parsers.Multiline_Source;
      use Parsers.Multiline_Patterns;
      use Strings_Edit.Streams;
      LF    : constant Character := Character'Val (10);
      State : aliased Match_State (Stack_Size);
      Data  : aliased String_Stream (80);
   begin
      Set_Tracing (State, Tracing);
      Set (Data, "a" & LF & "c");
      declare
         Code    : aliased Stream_IO.Source (Data'Access);
         Pattern : constant Pattern_Type :=
                      "a" & End_of_Line & NL & "c" or "ab";
      begin
         declare
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "NL test 2 ""ac"" not matched"
               );
            elsif Get_Pointer (Code) /= 2 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "NL test 2 ""ac"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 2 (expected)"
               )  );
            end if;
         end;
      end;
   end;
   Put_Line ("NL test 3");
   declare
      use Parsers.Multiline_Source;
      use Parsers.Multiline_Patterns;
      use Strings_Edit.Streams;
      LF    : constant Character := Character'Val (10);
      State : aliased Match_State (Stack_Size);
      Data  : aliased String_Stream (80);
   begin
      Set_Tracing (State, Tracing);
      Set (Data, "a" & LF & "b");
      declare
         Code    : aliased Stream_IO.Source (Data'Access);
         Pattern : constant Pattern_Type :=
                      "a" & End_of_Line & NL & "c" or "ab";
      begin
         declare
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Aborted then
               Raise_Exception
               (  Data_Error'Identity,
                  "NL test 2 ""ab"" not aborted"
               );
            elsif Get_Pointer (Code) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "NL test 2 ""ac"" Pointer"
                  &  Integer'Image (Get_Pointer (Code))
                  &  " /= 1 (expected)"
               )  );
            end if;
            Put_Line (Result.Message & " at " & Image (Result.Where));
         end;
      end;
   end;
   Put_Line ("Spreadsheet test 1");
   declare
      use Parsers.Multiline_Source;
      use Parsers.Multiline_Patterns;
      use Strings_Edit.Streams;
      use Parsers.Multiline_Patterns.Fields;
      use Parsers.Multiline_Patterns.Floats;
      use Parsers.Multiline_Patterns.Integers;
      use Float_Arrays;
      use Integer_Arrays;
      use String_Arrays;
      LF    : constant Character := Character'Val (10);
      State : aliased Match_State (Stack_Size);
      Data  : aliased String_Stream (80);
   begin
      Set_Tracing (State, Tracing);
      Set
      (   Data,
          "123, A, 0.0" & LF &
          "456 ,B, 1"
      );
      declare
         Code           : aliased Stream_IO.Source (Data'Access);
         Column_1_Data  : aliased Integer_Arrays.Unbounded_Array;
         Column_1_Index : aliased Positive := 1;
         Column_2_Data  : aliased String_Arrays.Unbounded_Ptr_Array;
         Column_2_Index : aliased Positive := 1;
         Column_3_Data  : aliased Float_Arrays.Unbounded_Array;
         Column_3_Index : aliased Positive := 1;
         Pattern : constant Pattern_Type :=
            + (  Blank_Or_Empty                                      &
                 Integer_Pattern
                 (  Column_1_Data'Access,
                    Column_1_Index'Access
                 )                                                   &
                 Blank_Or_Empty                                      &
                 ","                                                 &
                 Blank_Or_Empty                                      &
                 Parsers.Multiline_Patterns.Fields.Field_Pattern
                 (  Column_2_Data'Access,
                    Column_2_Index'Access,
                    ","
                 )                                                   &
                 ","                                                 &
                 Blank_Or_Empty                                      &
                 Float_Pattern
                 (  Column_3_Data'Access,
                    Column_3_Index'Access
                 )                                                   &
                 Blank_Or_Empty                                      &
                 End_of_Line                                         &
                 NL_or_EOF
              or Failure
              );
      begin
         declare
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "Spreadsheet test 1 not matched"
               );
            elsif Column_1_Index /= 3 then
               Raise_Exception
               (  Data_Error'Identity,
                  "Spreadsheet test 1 column 1 size error"
               );
            elsif Column_2_Index /= 3 then
               Raise_Exception
               (  Data_Error'Identity,
                  "Spreadsheet test 1 column 2 size error"
               );
            elsif Column_3_Index /= 3 then
               Raise_Exception
               (  Data_Error'Identity,
                  "Spreadsheet test 1 column 3 size error"
               );
            elsif Get (Column_1_Data, 1) /= 123 then
               Raise_Exception
               (  Data_Error'Identity,
                  "Spreadsheet test 1 1.1 error"
               );
            elsif Get (Column_1_Data, 2) /= 456 then
               Raise_Exception
               (  Data_Error'Identity,
                  "Spreadsheet test 1 2.1 error"
               );
            end if;
         end;
         for Row in 1..Column_1_Index - 1 loop
            declare
               use Strings_Edit;
               use Strings_Edit.Floats;
               use Strings_Edit.Integers;
               Line    : String (1..80);
               Pointer : Integer := 1;
            begin
               Put (Line, Pointer, Get (Column_1_Data, Row));
               Put (Line, Pointer, ", ");
               Put (Line, Pointer, Get (Column_2_Data, Row).all);
               Put (Line, Pointer, ", ");
               Put (Line, Pointer, Get (Column_3_Data, Row));
               Put_Line (Line (1..Pointer - 1));
            end;
         end loop;
      end;
   end;
   Put_Line ("User variable test 1");
   declare
      use Parsers.Multiline_Source;
      use Parsers.Multiline_Patterns;
      use Strings_Edit.Streams;
      use User_Variable_Handles;
      LF    : constant Character := Character'Val (10);
      State : aliased Match_State (Stack_Size);
      Data  : aliased String_Stream (80);
   begin
      Set_Tracing (State, Tracing);
      Set
      (   Data,
          "1, 2, 3" & LF &
          "4, 5, 6"
      );
      declare
         Code   : aliased Stream_IO.Source (Data'Access);
         Matrix : constant User_Variable_Handles.Handle :=
                     Ref (new Matrix_Variable (2, 3));
         Value  : Matrix_Variable renames
                     Matrix_Variable (Ptr (Matrix).all);
         Pattern : constant Pattern_Type :=
            + (  Blank_Or_Empty                                      &
                 (Matrix < Natural_Number)                           &
                 Blank_Or_Empty & ","  & Blank_Or_Empty              &
                 (Matrix < Natural_Number)                           &
                 Blank_Or_Empty & ","  & Blank_Or_Empty              &
                 (Matrix < Natural_Number)                           &
                 Blank_Or_Empty                                      &
                 End_of_Line                                         &
                 NL_or_EOF
              or Failure
              );
      begin
         declare
            Result : constant Result_Type :=
                     Match (Pattern, Code'Access, State'Access);
         begin
            if Result.Outcome /= Successful then
               Raise_Exception
               (  Data_Error'Identity,
                  "User variable test 1 not matched"
               );
            elsif Value.Row /= 3 then
               Raise_Exception
               (  Data_Error'Identity,
                  "User variable test 1 rows error"
               );
            elsif Value.Column /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  "User variable test 1 columns error"
               );
            elsif Value.Value (1, 1) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  "User variable test 1 element 1.1 error"
               );
            elsif Value.Value (2, 3) /= 6 then
               Raise_Exception
               (  Data_Error'Identity,
                  "User variable test 1 element 2.3 error"
               );
            end if;
         end;
      end;
   end;

exception
   when Error : others =>
      Put_Line ("Error: "& Exception_Information (Error));
end Test_Patterns;
