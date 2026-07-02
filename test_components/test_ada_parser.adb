--                                                                    --
--  procedure Test_Ada_Parser       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2004       --
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

with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Text_IO;               use Ada.Text_IO;
with Parsers.Multiline_Source;  use Parsers.Multiline_Source;
with Strings_Edit.Integers;     use Strings_Edit.Integers;
with Strings_Edit.Quoted;       use Strings_Edit.Quoted;

with Parsers.Multiline_Source.Text_IO;
with Stack_Storage;
with Test_Parsers;

procedure Test_Ada_Parser is
   use Test_Parsers.Ada_Parsers;
   use Lexers;
   use Tokens;
   type Line_Numers is array (Positive range <>) of Line_Number;

   File   : aliased File_Type;
   Arena  : aliased Stack_Storage.Pool (2048, 128);
   Parser : Ada_Expression (Arena'Access);
   Result : Tokens.Argument_Token;
   Stub   : Node_Ptr;            -- The list of lines to print the tree:
   Lines  : constant Line_Numers := (1 => 258);
begin
   Open (File, In_File, "test_ada_parser.txt");
   declare
      Code    : Parsers.Multiline_Source.Text_IO.Source (File'Access);
      Checked : Boolean;
      Line    : Line_Ptr;
      First   : Integer;
      Next    : Integer;
      Pointer : Integer;
      Last    : Integer;
   begin
      loop
         Get_Line (Code, Line, Pointer, Last);
         if Strings_Edit.Is_Prefix ("folding>", Line (Pointer..Last))
         then
            Set_Pointer (Code, Pointer + 8);
            Set_Constant_Folding (Parser, True);
         else
            Set_Constant_Folding (Parser, False);
         end if;
         Push_Stub (Parser, Stub); -- Mark the stack
         begin
            Checked := False;
            Parse (Parser, Code, Result);
            for Index in Lines'Range loop
               if Lines (Index) in Result.Location.First.Line
                                .. Result.Location.Next.Line
               then
                  Test_Parsers.IO.Put (Result);
                  exit;
               end if;
            end loop;
            Get_Line (Code, Line, Pointer, Last);
            for Index in reverse Pointer..Last loop
               if Line (Index) = '?' then
                  Pointer := Index + 1;
                  Get (Line (Index..Last), Pointer, First);
                  Pointer := Pointer + 1;
                  Get (Line (Index..Last), Pointer, Next);
                  Pointer := Pointer + 1;
                  if Result.Location.First.Column /= First or else
                     Result.Location.Next.Column  /= Next  then
                     Put_Line
                     (  Image (Link (Code)) &  ">location: "
                     &  Image (Result.Location.First.Column)
                     &  ".."
                     &  Image (Result.Location.Next.Column)
                     &  " /= "
                     &  Image (First)
                     &  ".."
                     &  Image (Next)
                     &  " (expected)"
                     );
                     raise Data_Error;
                  elsif Image (Result.Value.all) /= Line (Pointer..Last)
                  then
                     Put_Line (Image (Link (Code)));
                     Put ("     got>");
                     Put_Line (Image (Result.Value.all));
                     Put ("expected>");
                     Put (Line (Pointer..Last));
                     raise Data_Error;
                  end if;
                  Checked := True;
                  exit;
               elsif Line (Index) = '@' then
                  Test_Parsers.Dot.Put
                  (  Result,
                     Line (Index + 2..Last),
                     Line (Index + 1) = '+'
                  );
                  Checked := True;
                  exit;
               end if;
            end loop;
            if not Checked then
               Put_Line
               (  Image (Result.Location)
               &  ": "
               &  Image (Result.Value.all)
               &  " ends at "
               &  Image (Link (Code))
               );
            end if;
         exception
            when Error : Parsers.Syntax_Error =>
               declare
                  Message : constant String :=
                            Exception_Message (Error);
               begin
                  Checked := False;
                  Get_Line (Code, Line, Pointer, Last);
                  for Index in reverse Pointer..Last loop
                     if Line (Index) = '?' then
                        if Message /= Line (Index + 1..Last) then
                           Put_Line (Image (Link (Code)));
                           Put (" message>");
                           Put_Line (Quote (Message));
                           Put ("expected>");
                           Put_Line (Quote (Line (Index + 1..Last)));
                           raise Data_Error;
                        end if;
                        Checked := True;
                        exit;
                     end if;
                  end loop;
                  if not Checked then
                     Put_Line
                     (  Image (Link (Code))
                     &  ">Error : "
                     &  Message
                     );
                  end if;
               exception
                  when End_Error =>
                     Put_Line
                     (  Image (Link (Code))
                     &  ">Error : "
                     &  Message
                     );
               end;
         end;
         Free (Parser, Stub);      -- Release the tree on the stack
         Next_Line (Code);
      end loop;
   exception
      when End_Error =>
         null;
   end;
   Set_Exit_Status (Success);
exception
   when Data_Error =>
      Set_Exit_Status (Failure);
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
      Set_Exit_Status (Failure);
end Test_Ada_Parser;
