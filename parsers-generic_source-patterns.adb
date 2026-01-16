--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.Patterns             Luebeck            --
--  Implementation                                 Summer, 2025       --
--                                                                    --
--                                Last revision :  16:57 04 Jan 2026  --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

with Strings_Edit.UTF8.Maps.Constants;
use  Strings_Edit.UTF8.Maps.Constants;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Strings_Edit.UTF8.Categorization;

package body Parsers.Generic_Source.Patterns is

   procedure Dump (Stack : Statement_Stack; Prefix : String := "") is
      use Ada.Text_IO;
      Prompt : String := Prefix;
   begin
      if Stack.Count = 0 then
         Put_Line (Prefix & "empty");
         return;
      end if;
      for Index in reverse 1..Stack.Count loop
         declare
            This : Statement_Data renames Stack.Stack (Index);
         begin
            case This.Kind_Of is
               when No_Statement =>
                  null;
               when Alternate_Statement =>
                  Put_Line
                  (  Prompt
                  &  "alternative"
                  &  Integer'Image (This.Alternate_Current)
                  &  " at"
                  &  Integer'Image (This.Alternate_Pointer)
                  &  ": "
                  &  Image (This.Alternate_Statement.all)
                  );
               when Anything_Statement =>
                  Put_Line
                  (  Prompt
                  &  "anything at"
                  &  Integer'Image (This.Anything_Pointer)
                  );
               when Goto_Statement =>
                  Put_Line
                  (  Prompt
                  &  "Goto"
                  );
               when Eager_Statement =>
                  Put_Line
                  (  Prompt
                  &  "eager"
                  &  Integer'Image (This.Eager_Count)
                  &  ": "
                  &  Image (This.Eager_Statement.all)
                  );
               when Hook_Statement =>
                  Put_Line
                  (  Prompt
                  &  Image (This.Hook_Statement.Hook.all)
                  &  " at"
                  &  Integer'Image (This.Hook_Pointer)
                  &  " ->"
                  &  Integer'Image (This.Hook_Stub)
                  &  ": "
                  &  Image (This.Hook_Statement.all)
                  );
               when Fence_Statement =>
                  Put_Line
                  (  Prompt
                  &  "fence : "
                  &  Image (This.Fence_Statement.all)
                  );
               when Lazy_Statement =>
                  Put_Line
                  (  Prompt
                  &  "lazy at"
                  &  Integer'Image (This.Lazy_Pointer)
                  &  " ->"
                  &  Integer'Image (This.Lazy_Parent)
                  &  ": "
                  &  Image (This.Lazy_Statement.all)
                  );
               when Next_Line_Statement =>
                  Put_Line
                  (  Prompt
                  &  "NL "
                  &  Image (This.Next_Line_At)
                  );
               when Next_Line_Or_EOF_Statement =>
                  Put_Line
                  (  Prompt
                  &  "NL_or_EOF "
                  &  Image (This.Next_Line_At)
                  );
               when Nil_Statement =>
                  Put_Line
                  (  Prompt
                  &  "Nil "
                  );
               when Nonempty_Statement =>
                  Put_Line
                  (  Prompt
                  &  "Nonempty at"
                  &  Integer'Image (This.Nonempty_Pointer)
                  &  " ->"
                  &  Integer'Image (This.Nonempty_Stub)
                  &  ": "
                  &  Image (This.Nonempty_Statement.all)
                  );
               when Not_Statement =>
                  Put_Line
                  (  Prompt
                  &  "Not at"
                  &  Integer'Image (This.Not_Pointer)
                  &  " ->"
                  &  Integer'Image (This.Not_Stub)
                  &  ": "
                  &  Image (This.Not_Statement.all)
                  );
               when Return_Statement =>
                  null;
               when Repeater_Statement =>
                  Put_Line
                  (  Prompt
                  &  "repeater"
                  &  Integer'Image (This.Repeater_Count)
                  &  ": "
                  &  Image (This.Repeater_Statement.all)
                  );
               when Sequence_Statement =>
                  Put_Line
                  (  Prompt
                  &  "sequence"
                  &  Integer'Image (This.Sequence_Current)
                  &  ": "
                  &  Image (This.Sequence_Statement.all)
                  );
               when Stub_Statement =>
                  Put_Line
                  (  Prompt
                  &  "stub"
                  &  Integer'Image (This.Stub_Data)
                  &  " at"
                  &  Integer'Image (This.Stub_Pointer)
                  );
            end case;
         end;
         if Index = Stack.Count then
            Prompt := (others => ' ');
         end if;
      end loop;
   end Dump;

   procedure Pop (Stack : in out Statement_Stack) is
   begin
      if Stack.Count = 0 then
         Raise_Exception
         (  Program_Error'Identity,
            "Empty stack"
         );
      end if;
      Stack.Count := Stack.Count - 1;
   end Pop;

   procedure Push
             (  Stack : in out Statement_Stack;
                Data  : Statement_Data
             )  is
   begin
      if Stack.Count = Stack.Size then
         Raise_Exception
         (  Storage_Error'Identity,
            "Stack is full," & Integer'Image (Stack.Size) & " items"
         );
      end if;
      Stack.Count := Stack.Count + 1;
      Stack.Stack (Stack.Count) := Data;
   end Push;

   function Top (Stack : Statement_Stack) return Statement_Data is
   begin
      if Stack.Count = 0 then
         return (Kind_Of => No_Statement);
      else
         return Stack.Stack (Stack.Count);
      end if;
   end Top;

   function Top (Stack : Statement_Stack) return Statement_Type is
   begin
      if Stack.Count = 0 then
         return No_Statement;
      else
         return Stack.Stack (Stack.Count).Kind_Of;
      end if;
   end Top;
------------------------------------------------------------------------
   function Do_Match
            (  Pattern : Pattern_Ref;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      Current : Pattern_Ref;
      Self    : Pattern_Ref;
   --
   -- Clean -- The MSS stack
   --
   --    Stup - Clean up to this statement
   --
   -- Returns :
   --
   --    True if cleanup stopped at the next line statement
   --
      function Clean (Stub : Integer) return Boolean is
      begin
         while State.MSS.Count > Stub loop
            declare
               This : constant Statement_Data := Top (State.MSS);
            begin
               case This.Kind_Of is
                  when No_Statement        |
                       Alternate_Statement |
                       Anything_Statement  |
                       Eager_Statement     |
                       Fence_Statement     |
                       Goto_Statement      |
                       Lazy_Statement      |
                       Nil_Statement       |
                       Nonempty_Statement  |
                       Not_Statement       |
                       Repeater_Statement  |
                       Return_Statement    |
                       Sequence_Statement  |
                       Stub_Statement      =>
                     Pop (State.MSS);
                  when Hook_Statement =>
                     Pop (State.MSS);
                     On_Failure
                     (  This.Hook_Statement.Hook.all,
                        Source.all
                     );
                  when Next_Line_Statement        |
                       Next_Line_Or_EOF_Statement =>
                     return True;
               end case;
            end;
         end loop;
         return False;
      end Clean;

      function Line_Changed (Stub : Integer) return Boolean is
      begin
         for Item in Stub + 1..State.MSS.Count loop
            case State.MSS.Stack (Item).Kind_Of is
               when No_Statement        |
                    Alternate_Statement |
                    Anything_Statement  |
                    Eager_Statement     |
                    Goto_Statement      |
                    Hook_Statement      |
                    Fence_Statement     |
                    Lazy_Statement      |
                    Nil_Statement       |
                    Nonempty_Statement  |
                    Not_Statement       |
                    Return_Statement    |
                    Repeater_Statement  |
                    Sequence_Statement  |
                    Stub_Statement      =>
                  null;
               when Next_Line_Statement        |
                    Next_Line_Or_EOF_Statement =>
                  return True;
            end case;
         end loop;
         return False;
      end Line_Changed;

      procedure Put_Source (EOL : Boolean := True) is
         use Ada.Text_IO;
         Line    : Line_Ptr_Type;
         Pointer : Integer;
         Last    : Integer;
      begin
         Get_Line (Source.all, Line, Pointer, Last);
         for Index in Col..32 loop
            Put (' ');
         end loop;
         Put ('|');
         Put (Line (Pointer..Get_Backup_Pointer (Source.all) - 1));
         Put
         (  ">"
         &  Line (Get_Backup_Pointer (Source.all)..Pointer - 1)
         &  "<"
         );
         Put (Line (Get_Pointer (Source.all)..Last) & "| ");
         Put (Image (Get_Backup_Pointer (Source.all)));
         Put ("..");
         Put (Image (Get_Pointer (Source.all) - 1));
         if EOL then
            New_Line;
         end if;
      end Put_Source;
   begin
      if Pattern = null then
         Self := Pattern_Ref (Ptr (State.Pattern));
         if Self = null then
            Raise_Exception
            (  Ada.Text_IO.Use_Error'Identity,
               "Pattern has no been matched"
            );
         end if;
         goto Failure; -- Do rematch
      else
         if Pattern.all in Goto_Pattern then
            Raise_Exception
            (  Ada.Text_IO.Use_Error'Identity,
               "Pattern is Self"
            );
         end if;
         Current := Pattern;
         Self    := Pattern;
      end if;

<<Process>>
      if State.Trace then
         Ada.Text_IO.Put
         (  "Process "
         &  Statement_Type'Image (Get_Type (Current.all))
         &  " "
         );
         Put_Source (False);
         Ada.Text_IO.Put_Line (": " & Image (Current.all));
      end if;
      case Get_Type (Current.all) is
         when No_Statement =>
            declare
               Result : constant Result_Type :=
                        Match (Current.all, Source, State);
            begin
               case Result.Outcome is
                  when Successful =>
                     goto Success;
                  when Failed =>
                     goto Failure;
                  when Aborted =>
                     return Result;
               end case;
            end;
         when Alternate_Statement =>
            declare
               This : Alternate_Pattern renames
                      Alternate_Pattern (Current.all);
            begin
               Push
               (  State.MSS,
                  (  Kind_Of             => Alternate_Statement,
                     Alternate_Statement => This'Unchecked_Access,
                     Alternate_Current   => 1,
                     Alternate_Pointer   => Get_Pointer (Source.all)
               )  );
               Current := Pattern_Ref (Ptr (This.Items (1)));
               goto Process;
            end;
         when Anything_Statement =>
            declare
               This : Anything_Pattern renames
                      Anything_Pattern (Current.all);
            begin
               Push
               (  State.MSS,
                  (  Kind_Of            => Anything_Statement,
                     Anything_Statement => This'Unchecked_Access,
                     Anything_Pointer   => Get_Pointer (Source.all)
               )  );
               goto Success;
            end;
         when Eager_Statement =>
            declare
               This : Eager_Pattern renames Eager_Pattern (Current.all);
            begin
               Push
               (  State.ASS,
                  (  Kind_Of         => Eager_Statement,
                     Eager_Statement => This'Unchecked_Access,
                     Eager_Count     => 0
               )  );
               Push
               (  State.MSS,
                  (  Kind_Of      => Stub_Statement,
                     Stub_Pointer => Get_Pointer (Source.all),
                     Stub_Data    => 1
               )  );
               Current := Pattern_Ref (Ptr (This.Pattern));
               goto Process;
            end;
         when Fence_Statement =>
            declare
               This : Fence_Pattern renames Fence_Pattern (Current.all);
            begin
               Push
               (  State.MSS,
                  (  Kind_Of         => Fence_Statement,
                     Fence_Statement => This'Unchecked_Access,
                     Fence_Pointer   => Get_Pointer (Source.all)
               )  );
               goto Success;
            end;
         when Goto_Statement =>
            declare
               This : Goto_Pattern renames
                      Goto_Pattern (Current.all);
            begin
               if This.Pattern = null then
                  Current := Self;
               else
                  Current := This.Pattern;
               end if;
               goto Process;
            end;
         when Hook_Statement =>
            declare
               This : Hook_Pattern renames Hook_Pattern (Current.all);
            begin
               Push
               (  State.ASS,
                  (  Kind_Of        => Active_Hook_Statement,
                     Hook_Statement => This'Unchecked_Access,
                     Hook_Pointer   => Get_Pointer (Source.all),
                     Hook_Stub      => State.MSS.Count
               )  );
               Current := Pattern_Ref (Ptr (This.Pattern));
               goto Process;
            end;
         when Lazy_Statement =>
            declare
               This : Lazy_Pattern renames Lazy_Pattern (Current.all);
            begin
               Push
               (  State.MSS,
                  (  Kind_Of        => Passive_Lazy_Statement,
                     Lazy_Statement => This'Unchecked_Access,
                     Lazy_Parent    => 0,
                     Lazy_Pointer   => Get_Pointer (Source.all)
               )  );
               Push
               (  State.MSS,
                  (  Kind_Of        => Passive_Lazy_Statement,
                     Lazy_Statement => This'Unchecked_Access,
                     Lazy_Parent    => State.MSS.Count,
                     Lazy_Pointer   => Get_Pointer (Source.all)
               )  );
               goto Success;
            end;
         when Nil_Statement =>
            goto Failure;
         when Next_Line_Statement =>
            declare
               This  : Next_Line_Pattern renames
                          Next_Line_Pattern (Current.all);
               Where : constant Location_Type := Link (Source.all);
            begin
               Next_Line (Source.all);
               Push
               (  State.MSS,
                  (  Kind_Of             => Next_Line_Statement,
                     Next_Line_Statement => This'Unchecked_Access,
                     Next_Line_At        => Where & Link (Source.all)
               )  );
               goto Success;
            exception
               when Ada.Text_IO.End_Error =>
                  return
                  (  Aborted,
                     End_Of_File_Error'Length,
                     Where,
                     End_Of_File_Error
                  );
               when Error : Ada.Text_IO.Data_Error =>
                  declare
                     Message : constant String :=
                                        Exception_Message (Error);
                  begin
                     return
                     (  Aborted,
                        Message'Length,
                        Where,
                        Message
                     );
                  end;
            end;
         when Next_Line_Or_EOF_Statement =>
            declare
               This  : Next_Line_Pattern renames
                          Next_Line_Pattern (Current.all);
               Where : constant Location_Type := Link (Source.all);
            begin
               Next_Line (Source.all);
               Push
               (  State.MSS,
                  (  Kind_Of             => Next_Line_Statement,
                     Next_Line_Statement => This'Unchecked_Access,
                     Next_Line_At        => Where & Link (Source.all)
               )  );
               goto Success;
            exception
               when Ada.Text_IO.End_Error =>
                  return Matched;
               when Error : Ada.Text_IO.Data_Error =>
                  declare
                     Message : constant String :=
                                        Exception_Message (Error);
                  begin
                     return
                     (  Aborted,
                        Message'Length,
                        Where,
                        Message
                     );
                  end;
            end;
         when Nonempty_Statement =>
            declare
               This : Nonempty_Pattern renames
                                       Nonempty_Pattern (Current.all);
            begin
               Push
               (  State.ASS,
                  (  Kind_Of            => Nonempty_Statement,
                     Nonempty_Statement => This'Unchecked_Access,
                     Nonempty_Pointer   => Get_Pointer (Source.all),
                     Nonempty_Stub      => State.MSS.Count
               )  );
               Current := Pattern_Ref (Ptr (This.Pattern));
               goto Process;
            end;
         when Not_Statement =>
            declare
               This : Not_Pattern renames Not_Pattern (Current.all);
            begin
               Push
               (  State.ASS,
                  (  Kind_Of       => Not_Statement,
                     Not_Statement => This'Unchecked_Access,
                     Not_Pointer   => Get_Pointer (Source.all),
                     Not_Stub      => State.MSS.Count
               )  );
               Current := Pattern_Ref (Ptr (This.Pattern));
               goto Process;
            end;
         when Return_Statement =>
            declare
               This : Return_Pattern renames
                      Return_Pattern (Current.all);
            begin
               case This.Result is
                  when Aborted =>
                     return
                     (  Aborted,
                        This.Message'Length,
                        Link (Source.all),
                        This.Message
                     );
                  when Successful =>
                     return Matched;
                  when Failed =>
                     return Unmatched;
               end case;
            end;
         when Repeater_Statement =>
            declare
               This : Repeater_Pattern renames
                      Repeater_Pattern (Current.all);
            begin
               Push
               (  State.ASS,
                  (  Kind_Of            => Repeater_Statement,
                     Repeater_Statement => This'Unchecked_Access,
                     Repeater_Count     => 1
               )  );
               Push
               (  State.MSS,
                  (  Kind_Of      => Stub_Statement,
                     Stub_Pointer => Get_Pointer (Source.all),
                     Stub_Data    => 0
               )  );
               Current := Pattern_Ref (Ptr (This.Pattern));
               goto Process;
            end;
         when Sequence_Statement =>
            declare
               This : Sequence_Pattern renames
                      Sequence_Pattern (Current.all);
            begin
               Push
               (  State.ASS,
                  (  Kind_Of            => Sequence_Statement,
                     Sequence_Statement => This'Unchecked_Access,
                     Sequence_Current   => 1
               )  );
               Push
               (  State.MSS,
                  (  Kind_Of      => Stub_Statement,
                     Stub_Pointer => Get_Pointer (Source.all),
                     Stub_Data    => 0
               )  );
               Current := Pattern_Ref (Ptr (This.Items (1)));
               goto Process;
            end;
         when Stub_Statement =>
            null;
      end case;

<<Success>>
      if State.Trace then
         Ada.Text_IO.Put
         (  "Success "
         &  Statement_Type'Image (Top (State.ASS))
         );
         Put_Source;
      end if;
      case Top (State.ASS) is
         when No_Statement =>
            return Matched;
         when Alternate_Statement =>
            Pop (State.ASS);
            goto Success;
         when Anything_Statement =>
            Raise_Exception
            (  Program_Error'Identity,
               "Unexpected anything statement success"
            );
         when Active_Lazy_Statement =>
            declare
               Data : constant Active_Lazy_Data := Top (State.ASS);
            begin
               Push
               (  State.MSS,
                  (  Kind_Of        => Passive_Lazy_Statement,
                     Lazy_Statement => Data.Lazy_Statement,
                     Lazy_Parent    => Data.Lazy_Parent,
                     Lazy_Pointer   => Data.Lazy_Pointer
               )  );
               Pop (State.ASS);
               goto Success;
            end;
         when Active_Hook_Statement =>
            declare
               Data : constant Active_Hook_Data := Top (State.ASS);
            begin
               if Line_Changed (Data.Hook_Stub) then
                  declare
                     Where  : constant Location_Type :=
                              State.MSS.Stack
                              (  State.MSS.Count
                              ) .Next_Line_At;
                     Result : constant Result_Type :=
                              On_Line_Change
                              (  Data.Hook_Statement.Hook,
                                 Source,
                                 Where
                              );
                  begin
                     case Result.Outcome is
                        when Aborted =>
                           return Result;
                        when Successful =>
                           null;
                        when Failed =>
                           return
                           (  Aborted,
                              Multiline_Error'Length,
                              Where,
                              Multiline_Error
                           );
                     end case;
                  end;
               end if;
               On_Success
               (  Data.Hook_Statement.Hook.all,
                  Source.all,
                  Data.Hook_Pointer,
                  Get_Pointer (Source.all) - 1
               );
               Push
               (  State.MSS,
                  (  Kind_Of        => Passive_Hook_Statement,
                     Hook_Statement => Data.Hook_Statement,
                     Hook_Pointer   => Data.Hook_Pointer,
                     Hook_Stub      => Data.Hook_Stub
               )  );
               Pop (State.ASS);
               goto Success;
            end;
         when Eager_Statement =>
            declare
               Data : Eager_Data renames
                         State.ASS.Stack (State.ASS.Count);
               This : Eager_Pattern renames Data.Eager_Statement.all;
            begin
               Data.Eager_Count := Data.Eager_Count + 1;
               Push
               (  State.MSS,
                  (  Kind_Of      => Stub_Statement,
                     Stub_Pointer => Get_Pointer (Source.all),
                     Stub_Data    => 1
               )  );
               Current := Pattern_Ref (Ptr (This.Pattern));
               goto Process;
            end;
         when Fence_Statement =>
            Raise_Exception
            (  Program_Error'Identity,
               "Unexpected fence statement success"
            );
         when Goto_Statement =>
            Raise_Exception
            (  Program_Error'Identity,
               "Unexpected call statement success"
            );
         when Nonempty_Statement =>
            declare
               Data : constant Nonempty_Data := Top (State.ASS);
            begin
               if not Line_Changed (Data.Nonempty_Stub) and then
                  Get_Pointer (Source.all) = Data.Nonempty_Pointer then
                  if Clean (Data.Nonempty_Stub) then
                     return
                     (  Aborted,
                        Return_Error'Length,
                        Top (State.MSS).Next_Line_At,
                        Return_Error
                     );
                  else
                     goto Failure;
                  end if;
               else
                  Push (State.MSS, Data);
                  Pop  (State.ASS);
                  goto Success;
               end if;
            end;
         when Next_Line_Statement | Next_Line_Or_EOF_Statement =>
            Raise_Exception
            (  Program_Error'Identity,
               "Next line statement error"
            );
         when Nil_Statement =>
            Raise_Exception
            (  Program_Error'Identity,
               "Unexpected Nil statement success"
            );
         when Not_Statement =>
            declare
               Data : constant Not_Data := Top (State.ASS);
            begin
               Pop (State.ASS);
               if Clean (Data.Not_Stub) then
                  return
                  (  Aborted,
                     Return_Error'Length,
                     Top (State.MSS).Next_Line_At,
                     Return_Error
                  );
               else
                  Set_Pointer (Source.all, Data.Not_Pointer, False);
                  goto Failure;
               end if;
            end;
         when Passive_Hook_Statement =>
            Raise_Exception
            (  Program_Error'Identity,
               "Hook statement error"
            );
         when Passive_Lazy_Statement =>
            Raise_Exception
            (  Program_Error'Identity,
               "Lazy statement error"
            );
         when Repeater_Statement =>
            declare
               Data : Repeater_Data renames
                         State.ASS.Stack (State.ASS.Count);
               This : Repeater_Pattern renames
                         Data.Repeater_Statement.all;
            begin
               if Data.Repeater_Count = This.Count then
                  Push (State.MSS, Data);
                  Pop  (State.ASS);
                  goto Success;
               else
                  Data.Repeater_Count := Data.Repeater_Count + 1;
                  Push
                  (  State.MSS,
                     (  Kind_Of      => Stub_Statement,
                        Stub_Pointer => Get_Pointer (Source.all),
                        Stub_Data    => 0
                  )  );
                  Current := Pattern_Ref (Ptr (This.Pattern));
                  goto Process;
               end if;
            end;
         when Return_Statement =>
            Raise_Exception
            (  Program_Error'Identity,
               "Unexpected return statement success"
            );
         when Sequence_Statement =>
            declare
               Data : Sequence_Data renames
                         State.ASS.Stack (State.ASS.Count);
               This : Sequence_Pattern renames
                         Data.Sequence_Statement.all;
            begin
               if Data.Sequence_Current = This.Length then
                  Push (State.MSS, Data);
                  Pop  (State.ASS);
                  goto Success;
               else
                  Data.Sequence_Current := Data.Sequence_Current + 1;
                  Push
                  (  State.MSS,
                     (  Kind_Of      => Stub_Statement,
                        Stub_Pointer => Get_Pointer (Source.all),
                        Stub_Data    => 0
                  )  );
                  Current :=
                     Pattern_Ref
                     (  Ptr (This.Items (Data.Sequence_Current))
                     );
                  goto Process;
               end if;
            end;
         when Stub_Statement =>
            Push (State.MSS, Top (State.ASS));
            Pop  (State.ASS);
            goto Success;
      end case;

<<Failure>>
      if State.Trace then
         Ada.Text_IO.Put
         (  "Failure "
         &  Statement_Type'Image (Top (State.ASS))
         );
         Put_Source;
      end if;
      case Top (State.ASS) is
         when No_Statement =>
            if Top (State.MSS) = No_Statement then
               return Unmatched;
            else
               Push (State.ASS, Top (State.MSS));
               Pop  (State.MSS);
               goto Failure;
            end if;
         when Alternate_Statement =>
            declare
               Data : Alternate_Data renames
                         State.ASS.Stack (State.ASS.Count);
               This : Alternate_Pattern renames
                         Data.Alternate_Statement.all;
            begin
               Set_Pointer (Source.all, Data.Alternate_Pointer, False);
               if Data.Alternate_Current = This.Length then
                  Pop (State.ASS);
                  goto Failure;
               else
                  Data.Alternate_Current := Data.Alternate_Current + 1;
                  Push (State.MSS, Data);
                  Pop  (State.ASS);
                  Current :=
                     Pattern_Ref
                     (  Ptr (This.Items (Data.Alternate_Current))
                     );
                  goto Process;
               end if;
            end;
         when Anything_Statement =>
            declare
               use Strings_Edit.UTF8;
               Data      : constant Anything_Data := Top (State.ASS);
               Line      : Line_Ptr_Type;
               Pointer   : aliased Integer;
               Last      : Integer;
               Character : UTF8_Code_Point;
            begin
               Pop (State.ASS);
               Get_Line (Source.all, Line, Pointer, Last);
               if Pointer > Last then
                  Set_Pointer
                  (  Source.all,
                     Data.Anything_Pointer,
                     False
                  );
                  goto Failure;
               else
                  Get (Line (Pointer..Last), Pointer, Character);
                  Set_Pointer (Source.all, Pointer, False);
                  Push (State.MSS, Data);
                  goto Success;
               end if;
            exception
               when Ada.Text_IO.Data_Error =>
                  return
                  (  Aborted,
                     Encoding_Error'Length,
                     Link (Source.all),
                     Encoding_Error
                  );
            end;
         when Eager_Statement =>
            if Top (State.MSS) = Stub_Statement then -- Our stub
               declare
                  Data : Eager_Data renames
                            State.ASS.Stack (State.ASS.Count);
                  Stub : Stub_Data renames
                            State.MSS.Stack (State.MSS.Count);
               begin
                  Set_Pointer (Source.all, Stub.Stub_Pointer, False);
                  if Stub.Stub_Data = 1 then
                     Stub.Stub_Data := 0;
                     Push (State.MSS, Data);
                     Pop (State.ASS);
                     goto Success;
                  elsif Data.Eager_Count > 0 then
                     Data.Eager_Count := Data.Eager_Count - 1;
                     State.MSS.Stack (State.MSS.Count) := Data;
                     Pop (State.ASS);
                     goto Success;
                  else
                     Pop (State.MSS);
                     Pop (State.ASS);
                     goto Failure;
                  end if;
               end;
            end if;
            Push (State.ASS, Top (State.MSS));
            Pop  (State.MSS);
            goto Failure;
         when Active_Hook_Statement =>
            declare
               Data : Active_Hook_Data renames
                         State.ASS.Stack (State.ASS.Count);
            begin
               if State.MSS.Count = Data.Hook_Stub then
                  Set_Pointer (Source.all, Data.Hook_Pointer, False);
                  Pop (State.ASS);
                  goto Failure;
               else
                  Push (State.ASS, Top (State.MSS));
                  Pop  (State.MSS);
                  goto Failure;
               end if;
            end;
         when Active_Lazy_Statement =>
            declare
               Data : Active_Lazy_Data renames
                         State.ASS.Stack (State.ASS.Count);
            begin
               if State.MSS.Count = Data.Lazy_Parent then
                  declare
                     This : Passive_Lazy_Data renames
                               State.MSS.Stack (State.MSS.Count);
                  begin
                     Set_Pointer (Source.all, This.Lazy_Pointer, False);
                     if This.Lazy_Parent = 0 then
                        Pop (State.MSS);
                        Pop (State.ASS);
                        goto Failure;
                     else
                        Data.Lazy_Pointer := This.Lazy_Pointer;
                        Data.Lazy_Parent  := This.Lazy_Parent;
                        Pop (State.MSS);
                        goto Failure;
                     end if;
                  end;
               else
                  Push (State.ASS, Top (State.MSS));
                  Pop  (State.MSS);
                  goto Failure;
               end if;
            end;
         when Fence_Statement =>
            declare
               Data : constant Fence_Data := Top (State.ASS);
            begin
               Set_Pointer (Source.all, Data.Fence_Pointer, False);
               Pop (State.ASS);
               goto Failure;
            end;
         when Goto_Statement =>
            Raise_Exception
            (  Program_Error'Identity,
               "Unexpected goto statement failure"
            );
         when Next_Line_Statement | Next_Line_Or_EOF_Statement =>
            return
            (  Aborted,
               Return_Error'Length,
               Top (State.ASS).Next_Line_At,
               Return_Error
            );
         when Nil_Statement =>
            Raise_Exception
            (  Program_Error'Identity,
               "Unexpected Nil statement success"
            );
         when Nonempty_Statement =>
            declare
               Data : constant Nonempty_Data := Top (State.ASS);
            begin
               if State.MSS.Count = Data.Nonempty_Stub then
                  Set_Pointer
                  (  Source.all,
                     Data.Nonempty_Pointer,
                     False
                  );
                  Pop  (State.ASS);
                  goto Failure;
               else
                  Push (State.ASS, Top (State.MSS));
                  Pop  (State.MSS);
                  goto Failure;
               end if;
            end;
         when Not_Statement =>
            declare
               Data : constant Not_Data := Top (State.ASS);
            begin
               Set_Pointer (Source.all, Data.Not_Pointer, False);
               Push (State.MSS, Data);
               Pop  (State.ASS);
               goto Success;
            end;
         when Passive_Hook_Statement =>
            declare
               Data : Passive_Hook_Data renames
                         State.ASS.Stack (State.ASS.Count);
            begin
               On_Failure
               (  Data.Hook_Statement.Hook.all,
                  Source.all
               );
               Set_Pointer (Source.all, Data.Hook_Pointer, False);
               State.ASS.Stack (State.ASS.Count) :=
                  (  Kind_Of        => Active_Hook_Statement,
                     Hook_Statement => Data.Hook_Statement,
                     Hook_Pointer   => Data.Hook_Pointer,
                     Hook_Stub      => Data.Hook_Stub
                  );
               goto Failure;
            end;
         when Passive_Lazy_Statement =>
            declare
               Data : Passive_Lazy_Data renames
                         State.ASS.Stack (State.ASS.Count);
            begin
               Push (State.MSS, Data);
               State.ASS.Stack (State.ASS.Count) :=
                  (  Kind_Of        => Active_Lazy_Statement,
                     Lazy_Statement => Data.Lazy_Statement,
                     Lazy_Pointer   => Get_Pointer (Source.all),
                     Lazy_Parent    => State.MSS.Count
                  );
               Current :=
                  Pattern_Ref (Ptr (Data.Lazy_Statement.Pattern));
               goto Process;
            end;
         when Repeater_Statement =>
            while Top (State.MSS) = Stub_Statement loop
               Set_Pointer
               (  Source.all,
                  Top (State.MSS).Stub_Pointer,
                  False
               );
               Pop (State.MSS);
               declare
                  Data : Repeater_Data renames
                            State.ASS.Stack (State.ASS.Count);
               begin
                  if Data.Repeater_Count = 1 then
                     Pop (State.ASS);
                     goto Failure;
                  end if;
                  Data.Repeater_Count := Data.Repeater_Count - 1;
               end;
            end loop;
            Push (State.ASS, Top (State.MSS));
            Pop  (State.MSS);
            goto Failure;
         when Return_Statement =>
            Raise_Exception
            (  Program_Error'Identity,
               "Unexpected return statement failure"
            );
         when Sequence_Statement =>
            while Top (State.MSS) = Stub_Statement loop
               Set_Pointer
               (  Source.all,
                  Top (State.MSS).Stub_Pointer,
                  False
               );
               Pop (State.MSS);
               declare
                  Data : Sequence_Data renames
                            State.ASS.Stack (State.ASS.Count);
               begin
                  if Data.Sequence_Current = 1 then
                     Pop (State.ASS);
                     goto Failure;
                  end if;
                  Data.Sequence_Current := Data.Sequence_Current - 1;
               end;
            end loop;
            Push (State.ASS, Top (State.MSS));
            Pop (State.MSS);
            goto Failure;
         when Stub_Statement =>
            Set_Pointer
            (  Source.all,
               Top (State.ASS).Stub_Pointer,
               False
            );
            Pop (State.ASS);
            goto Failure;
      end case;
   end Do_Match;

   function Match
            (  Pattern : Pattern_Type;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
   begin
      State.ASS.Count := 0;
      State.MSS.Count := 0;
      if Is_Valid (Pattern) then
         State.Pattern := Pattern;
         return Do_Match (Pattern_Ref (Ptr (Pattern)), Source, State);
      else
         Invalidate (State.Pattern);
         return Matched;
      end if;
   end Match;

   function Match
            (  Pattern : Pattern_Object;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
   begin
      return Unmatched;
   end Match;

   function Rematch
            (  Source : access Source_Type;
               State  : access Match_State
            )  return Result_Type is
   begin
      return Do_Match (null, Source, State);
   end Rematch;
------------------------------------------------------------------------
   function "abs" (Text : String)
      return Pattern_Type is
      Result_Ptr : constant Pattern_Ptr :=
                        new Case_Insensitive_Text_Pattern (Text'Length);
   begin
      Case_Insensitive_Text_Pattern (Result_Ptr.all).Text := Text;
      return Ref (Result_Ptr);
   end "abs";

   function "&" (Left, Right : Pattern_Type) return Pattern_Type is
      Left_Ptr   : constant Pattern_Ptr := Ptr (Left);
      Right_Ptr  : constant Pattern_Ptr := Ptr (Right);
      Result_Ptr : Pattern_Ptr;
   begin
      if Left_Ptr = null then
         return Right;
      elsif Right_Ptr = null then
         return Left;
      elsif Left_Ptr.all in Sequence_Pattern'Class then
         declare
            Left : Sequence_Pattern renames
                   Sequence_Pattern (Left_Ptr.all);
         begin
            if Right_Ptr.all in Sequence_Pattern'Class then
               declare
                  Right : Sequence_Pattern renames
                          Sequence_Pattern (Right_Ptr.all);
               begin
                  Result_Ptr :=
                     new Sequence_Pattern (Left.Length + Right.Length);
                  Sequence_Pattern (Result_Ptr.all).Items :=
                     Left.Items & Right.Items;
               end;
            else
               Result_Ptr :=
                  new Sequence_Pattern (Left.Length + 1);
               Sequence_Pattern (Result_Ptr.all).Items :=
                  Left.Items & Right;
            end if;
         end;
      else
         if Right_Ptr.all in Sequence_Pattern'Class then
            declare
               Right : Sequence_Pattern renames
                       Sequence_Pattern (Right_Ptr.all);
            begin
               Result_Ptr :=
                  new Sequence_Pattern (1 + Right.Length);
               Sequence_Pattern (Result_Ptr.all).Items :=
                  Left & Right.Items;
            end;
         else
            Result_Ptr := new Sequence_Pattern (2);
            Sequence_Pattern (Result_Ptr.all).Items := Left & Right;
         end if;
      end if;
      return Ref (Result_Ptr);
   end "&";

   function "&" (Left : String; Right : Pattern_Type)
      return Pattern_Type is
      Right_Ptr : constant Pattern_Ptr := Ptr (Right);
   begin
      if Right_Ptr = null then
         return Text (Left);
      elsif Right_Ptr.all in Text_Pattern'Class then
         return Text (Left & Text_Pattern'Class (Right_Ptr.all).Text);
      else
         return Text (Left) & Right;
      end if;
   end "&";

   function "&" (Left : Pattern_Type; Right : String)
      return Pattern_Type is
      Left_Ptr : constant Pattern_Ptr := Ptr (Left);
   begin
      if Left_Ptr = null then
         return Text (Right);
      elsif Left_Ptr.all in Text_Pattern'Class then
         return Text (Text_Pattern'Class (Left_Ptr.all).Text & Right);
      else
         return Left & Text (Right);
      end if;
   end "&";

   function "or" (Left, Right : Pattern_Type) return Pattern_Type is
      Left_Ptr   : constant Pattern_Ptr := Ptr (Left);
      Right_Ptr  : constant Pattern_Ptr := Ptr (Right);
      Result_Ptr : Pattern_Ptr;
   begin
      if Left_Ptr = null then
         if Right_Ptr = null then
            return Empty;
         else
            return Empty or Right;
         end if;
      elsif Right_Ptr = null then
         return Left or Right;
      elsif Left_Ptr.all in Alternate_Pattern'Class then
         declare
            Left : Alternate_Pattern renames
                   Alternate_Pattern (Left_Ptr.all);
         begin
            if Right_Ptr.all in Alternate_Pattern'Class then
               declare
                  Right : Alternate_Pattern renames
                          Alternate_Pattern (Right_Ptr.all);
               begin
                  Result_Ptr :=
                     new Alternate_Pattern (Left.Length + Right.Length);
                  Alternate_Pattern (Result_Ptr.all).Items :=
                     Left.Items & Right.Items;
               end;
            else
               Result_Ptr :=
                  new Alternate_Pattern (Left.Length + 1);
               Alternate_Pattern (Result_Ptr.all).Items :=
                  Left.Items & Right;
            end if;
         end;
      else
         if Right_Ptr.all in Alternate_Pattern'Class then
            declare
               Right : Alternate_Pattern renames
                       Alternate_Pattern (Right_Ptr.all);
            begin
               Result_Ptr :=
                  new Alternate_Pattern (1 + Right.Length);
               Alternate_Pattern (Result_Ptr.all).Items :=
                  Left & Right.Items;
            end;
         else
            Result_Ptr := new Alternate_Pattern (2);
            Alternate_Pattern (Result_Ptr.all).Items := Left & Right;
         end if;
      end if;
      return Ref (Result_Ptr);
   end "or";

   function "or" (Left, Right : String) return Pattern_Type is
   begin
      return Text (Left) or Text (Right);
   end "or";

   function "or" (Left : String; Right : Pattern_Type)
      return Pattern_Type is
   begin
      return Text (Left) or Right;
   end "or";

   function "or" (Left : Pattern_Type; Right : String)
      return Pattern_Type is
   begin
      return Left or Text (Right);
   end "or";

   function "+" (Left : Pattern_Type) return Pattern_Type is
   begin
      if Voidable (Left) then
         Raise_Exception
         (  Constraint_Error'Identity,
            Voidable_Error
         );
      end if;
      declare
         Result_Ptr : constant Pattern_Ptr := new Eager_Pattern;
      begin
         Eager_Pattern (Result_Ptr.all).Pattern := Left;
         return Ref (Result_Ptr);
      end;
   end "+";

   function "+" (Left : String) return Pattern_Type is
   begin
      if Left'Length = 0 then
         Raise_Exception
         (  Constraint_Error'Identity,
            Voidable_Error
         );
      end if;
      declare
         Result_Ptr : constant Pattern_Ptr := new Eager_Pattern;
      begin
         Eager_Pattern (Result_Ptr.all).Pattern := Text (Left);
         return Ref (Result_Ptr);
      end;
   end "+";

   function "-" (Left : Pattern_Type) return Pattern_Type is
   begin
      if Voidable (Left) then
         Raise_Exception
         (  Constraint_Error'Identity,
            Voidable_Error
         );
      end if;
      declare
         Result_Ptr : constant Pattern_Ptr := new Lazy_Pattern;
      begin
         Lazy_Pattern (Result_Ptr.all).Pattern := Left;
         return Ref (Result_Ptr);
      end;
   end "-";

   function "-" (Left : String) return Pattern_Type is
   begin
      if Left'Length = 0 then
         Raise_Exception
         (  Constraint_Error'Identity,
            Voidable_Error
         );
      end if;
      declare
         Result_Ptr : constant Pattern_Ptr := new Lazy_Pattern;
      begin
         Lazy_Pattern (Result_Ptr.all).Pattern := Text (Left);
         return Ref (Result_Ptr);
      end;
   end "-";

   function "*" (Left : Positive; Right : Pattern_Type)
      return Pattern_Type is
      Right_Ptr  : constant Pattern_Ptr := Ptr (Right);
   begin
      if Right_Ptr = null then
         return Empty;
      end if;
      declare
         Result_Ptr : constant Pattern_Ptr := new Repeater_Pattern;
         This       : Repeater_Pattern renames
                      Repeater_Pattern (Result_Ptr.all);
      begin
         if Right_Ptr.all in Repeater_Pattern'Class then
            declare
               Right : Repeater_Pattern renames
                       Repeater_Pattern (Right_Ptr.all);
            begin
               This.Pattern := Right.Pattern;
               This.Count   := Left * Right.Count;
            end;
         else
            This.Pattern := Right;
            This.Count   := Left;
         end if;
         return Ref (Result_Ptr);
      end;
   end "*";

   function "*" (Left : Pattern_Type; Right : Positive)
      return Pattern_Type is
   begin
      return Right * Left;
   end "*";

   function "*" (Left : String; Right : Positive)
      return Pattern_Type is
      Data    : String (1..Left'Length * Right);
      Pointer : Integer := 1;
   begin
      for Index in 1..Right loop
         Data (Pointer..Pointer + Left'Length - 1) := Left;
         Pointer := Pointer + Left'Length;
      end loop;
      return Text (Data);
   end "*";

   function "*" (Left : Positive; Right : String) return Pattern_Type is
   begin
       return Right * Left;
   end "*";

   function "not" (Left : Pattern_Type) return Pattern_Type is
      Left_Ptr   : constant Pattern_Ptr := Ptr (Left);
      Result_Ptr : constant Pattern_Ptr := new Not_Pattern;
      This       : Not_Pattern renames Not_Pattern (Result_Ptr.all);
   begin
      if Left_Ptr = null then
         This.Pattern := Empty;
      else
         This.Pattern := Left;
      end if;
      return Ref (Result_Ptr);
   end "not";

   function "not" (Left : String) return Pattern_Type is
      Result_Ptr : constant Pattern_Ptr := new Not_Pattern;
      This       : Not_Pattern renames Not_Pattern (Result_Ptr.all);
   begin
      This.Pattern := Text (Left);
      return Ref (Result_Ptr);
   end "not";

   function Alphanumeric return Pattern_Type is
   begin
      return Ref (new Alphanumeric_Pattern);
   end Alphanumeric;

   function Any return Pattern_Type is
   begin
      return Ref (new Any_Pattern);
   end Any;

   function Any_Of (List : String) return Pattern_Type is
   begin
      return Any_Of (To_Set (List));
   end Any_Of;

   function Any_Of (List : Unicode_Set) return Pattern_Type is
      Result_Ptr : constant Pattern_Ptr := new Any_Of_Pattern;
   begin
      Any_Of_Pattern (Result_Ptr.all).Set := List;
      return Ref (Result_Ptr);
   end Any_Of;

   function Anything return Pattern_Type is
   begin
      return Ref (new Anything_Pattern);
   end Anything;

   function Blank return Pattern_Type is
      Result_Ptr : constant Pattern_Ptr := new Blank_Pattern;
   begin
      Blank_Pattern (Result_Ptr.all).Mode := Non_Empty_Chain;
      return Ref (Result_Ptr);
   end Blank;

   function Blank_Or_Empty return Pattern_Type is
      Result_Ptr : constant Pattern_Ptr := new Blank_Pattern;
   begin
      Blank_Pattern (Result_Ptr.all).Mode := Chain;
      return Ref (Result_Ptr);
   end Blank_Or_Empty;

   function Case_Insensitive_Text (Text : String)
      return Pattern_Type is
      Result_Ptr : constant Pattern_Ptr :=
                        new Case_Insensitive_Text_Pattern (Text'Length);
   begin
      Case_Insensitive_Text_Pattern (Result_Ptr.all).Text := Text;
      return Ref (Result_Ptr);
   end Case_Insensitive_Text;

   function Case_Insensitive_Text (Variable : Variable_Type)
      return Pattern_Type is
      Result_Ptr : constant Pattern_Ptr :=
                        new As_Case_Insensitive_Text_Pattern;
   begin
      As_Case_Insensitive_Text_Pattern (Result_Ptr.all).Variable :=
         Variable;
      return Ref (Result_Ptr);
   end Case_Insensitive_Text;

   function Def (Pattern : Pattern_Type) return Pattern_Type is
      Object_Ptr : constant Pattern_Ptr := Ptr (Pattern);
   begin
      if Object_Ptr = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Null pattern"
         );
      elsif not Resolve (Object_Ptr, Pattern_Ref (Object_Ptr)) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Recursive pattern self reference is not resolved"
         );
      end if;
      return Pattern;
   end Def;

   function Digit return Pattern_Type is
   begin
      return Ref (new Digit_Pattern);
   end Digit;

   function Empty return Pattern_Type is
   begin
      return Ref (new Empty_Pattern);
   end Empty;

   function End_Of_Line return Pattern_Type is
   begin
      return Ref (new End_Of_Line_Pattern);
   end End_Of_Line;

   function Failure return Pattern_Type is
   begin
      return Ref (new Return_Pattern (Failed, 0));
   end Failure;

   function Failure (Message : String) return Pattern_Type is
      Result_Ptr : constant Pattern_Ptr :=
                        new Return_Pattern (Aborted, Message'Length);
      This       : Return_Pattern renames Return_Pattern (Result_Ptr.all);
   begin
      This.Message := Message;
      return Ref (Result_Ptr);
   end Failure;

   function Fence return Pattern_Type is
   begin
      return Ref (new Fence_Pattern);
   end Fence;

   function Field (List : String) return Pattern_Type is
   begin
      return Field (To_Set (List));
   end Field;

   function Field (List : Unicode_Set) return Pattern_Type is
      Result_Ptr : constant Pattern_Ptr := new Field_Pattern;
   begin
      Field_Pattern (Result_Ptr.all).Set := List;
      return Ref (Result_Ptr);
   end Field;

   function Fixed_Point_Number (Base : Strings_Edit.NumberBase := 10)
      return Pattern_Type is
   begin
      return Ref (new Fixed_Point_Number_Pattern (Base));
   end Fixed_Point_Number;

   function Floating_Point_Number return Pattern_Type is
   begin
      return Ref (new Floating_Point_Number_Pattern);
   end Floating_Point_Number;

   procedure Get
             (  Line    : String;
                Pointer : in out Integer;
                Base    : Strings_Edit.NumberBase;
                Got_It  : out Boolean
             )  is
      Index : Integer := Pointer;
      Digit : Integer;
   begin
      while Index <= Line'Last loop
         case Line (Index) is
            when '0'..'9' =>
               Digit := Character'Pos (Line (Index))
                      - Character'Pos ('0');
            when 'A'..'F' =>
               Digit := Character'Pos (Line (Index))
                      - Character'Pos ('A')
                      + 10;
            when 'a'..'f' =>
               Digit := Character'Pos (Line (Index))
                      - Character'Pos ('a')
                      + 10;
            when others =>
               exit;
         end case;
         exit when Digit not in 0..Base - 1;
         Index := Index + 1;
      end loop;
      if Pointer = Index then
         Got_It := False;
      else
         Got_It  := True;
         Pointer := Index;
      end if;
   end Get;

   function Get_Tracing (State : Match_State) return Boolean is
   begin
      return State.Trace;
   end Get_Tracing;

   function Get_Type (Alternation : Alternate_Pattern)
      return Statement_Type is
   begin
      return Alternate_Statement;
   end Get_Type;

   function Get_Type (Pattern : Anything_Pattern)
      return Statement_Type is
   begin
      return Anything_Statement;
   end Get_Type;

   function Get_Type (Pattern : Goto_Pattern) return Statement_Type is
   begin
      return Goto_Statement;
   end Get_Type;

   function Get_Type (Repeater : Eager_Pattern) return Statement_Type is
   begin
      return Eager_Statement;
   end Get_Type;

   function Get_Type (Pattern : Fence_Pattern) return Statement_Type is
   begin
      return Fence_Statement;
   end Get_Type;

   function Get_Type (Pattern : Hook_Pattern) return Statement_Type is
   begin
      return Active_Hook_Statement;
   end Get_Type;

   function Get_Type (Repeater : Lazy_Pattern) return Statement_Type is
   begin
      return Active_Lazy_Statement;
   end Get_Type;

   function Get_Type (Pattern : Next_Line_Pattern)
      return Statement_Type is
   begin
      return Next_Line_Statement;
   end Get_Type;

   function Get_Type (Pattern : Next_Line_Or_EOF_Pattern)
      return Statement_Type is
   begin
      return Next_Line_Or_EOF_Statement;
   end Get_Type;

   function Get_Type (Pattern : Nil_Pattern)
      return Statement_Type is
   begin
      return Nil_Statement;
   end Get_Type;

   function Get_Type (Pattern : Nonempty_Pattern)
      return Statement_Type is
   begin
      return Nonempty_Statement;
   end Get_Type;

   function Get_Type (Pattern : Not_Pattern) return Statement_Type is
   begin
      return Not_Statement;
   end Get_Type;

   function Get_Type (Repeater : Repeater_Pattern)
      return Statement_Type is
   begin
      return Repeater_Statement;
   end Get_Type;

   function Get_Type (Pattern : Return_Pattern)
      return Statement_Type is
   begin
      return Return_Statement;
   end Get_Type;

   function Get_Type (Sequence : Sequence_Pattern)
      return Statement_Type is
   begin
      return Sequence_Statement;
   end Get_Type;

   function Get_Type (Pattern : Pattern_Object) return Statement_Type is
   begin
      return No_Statement;
   end Get_Type;

   function Image (Pattern : Alphanumeric_Pattern) return String is
   begin
      return "Alphanumeric";
   end Image;

   function Image (Pattern : Any_Pattern) return String is
   begin
      return "Any";
   end Image;

   function Image (Pattern : Any_Of_Pattern) return String is
   begin
      return "Any_Of (" & Quote (To_Sequence (Pattern.Set)) & ")";
   end Image;

   function Image (Pattern : Blank_Pattern) return String is
   begin
      case Pattern.Mode is
         when Single =>
            return "Space";
         when Chain =>
            return "Blank_Or_Empty";
         when Non_Empty_Chain =>
            return "Blank";
      end case;
   end Image;

   function Image (Alternation : Alternate_Pattern) return String is
   begin
      return "(" & Image (Alternation.Items, " or ") & ")";
   end Image;

   function Image (Pattern : Anything_Pattern) return String is
   begin
      return "Anything";
   end Image;

   function Image (Pattern : As_Case_Insensitive_Text_Pattern)
      return String is
   begin
      return "As_Case_Insensitive_Text";
   end Image;

   function Image (Pattern : As_Text_Pattern) return String is
   begin
      return "As_Text";
   end Image;

   function Image (Text : Case_Insensitive_Text_Pattern)
      return String is
   begin
      return "Case_Insensitive_Text (" & Quote (Text.Text) & ")";
   end Image;

   function Image (Pattern : Goto_Pattern)
      return String is
   begin
      return "Self";
   end Image;

   function Image (Pattern : Digit_Pattern)
      return String is
   begin
      return "Digit";
   end Image;

   function Image (Repeater : Eager_Pattern) return String is
      Object_Ptr : constant Pattern_Ptr := Ptr (Repeater.Pattern);
   begin
      return "+" & Image (Object_Ptr.all);
   end Image;

   function Image (Pattern : Empty_Pattern) return String is
   begin
      return "Empty";
   end Image;

   function Image (Pattern : End_Of_Line_Pattern) return String is
   begin
      return "End_Of_Line";
   end Image;

   function Image (Pattern : Fence_Pattern) return String is
   begin
      return "Fence";
   end Image;

   function Image (Pattern : Field_Pattern) return String is
   begin
      return "Field (" & Quote (To_Sequence (Pattern.Set)) & ")";
   end Image;

   function Image (Pattern : Fixed_Point_Number_Pattern)
      return String is
   begin
      return "Fixed_Point_Number (" & Image (Pattern.Base) & ")";
   end Image;

   function Image (Pattern : Floating_Point_Number_Pattern)
      return String is
   begin
      return "Floating_Point_Number";
   end Image;

   function Image (Pattern : Hook_Pattern) return String is
   begin
      return Image (Pattern.Hook.all) &
             " ("                     &
             Image (Pattern.Pattern)  &
             ")";
   end Image;

   function Image (Repeater : Lazy_Pattern) return String is
      Object_Ptr : constant Pattern_Ptr := Ptr (Repeater.Pattern);
   begin
      return "-" & Image (Object_Ptr.all);
   end Image;

   function Image (Pattern : Letter_Pattern) return String is
   begin
      return "Letter";
   end Image;

   function Image (Pattern : Lower_Case_Letter_Pattern) return String is
   begin
      return "Lower_Case_Letter";
   end Image;

   function Image (Pattern : Natural_Number_Pattern)
      return String is
   begin
      return "Natural_Number (" & Image (Pattern.Base) & ")";
   end Image;

   function Image (Pattern : Nil_Pattern) return String is
   begin
      return "Nil";
   end Image;

   function Image (Pattern : Next_Line_Pattern) return String is
   begin
      return "NL";
   end Image;

   function Image (Pattern : Next_Line_Or_EOF_Pattern) return String is
   begin
      return "NL_or_EOF";
   end Image;

   function Image (Pattern : Nonempty_Pattern) return String is
   begin
      return "Nonempty (" & Image (Pattern.Pattern) & ")";
   end Image;

   function Image (Pattern : Not_Pattern) return String is
   begin
      return "Not (" & Image (Pattern.Pattern) & ")";
   end Image;

   function Image (Pattern : Pattern_Type) return String is
      Object_Ptr : constant Pattern_Ptr := Ptr (Pattern);
   begin
      if Object_Ptr = null then
         return "null";
      else
         return Image (Object_Ptr.all);
      end if;
   end Image;

   function Image
            (  Items     : Pattern_Array;
               Delimiter : String := ", "
            )  return String is
   begin
      if Items'Length = 1 then
         return Image (Items (Items'First));
      else
         return Image (Items (Items'First)) &
                Delimiter                   &
                Image (Items (Items'First + 1..Items'Last), Delimiter);
      end if;
   end Image;

   function Image (Hook : Print_Handler) return String is
   begin
      case Hook.Mode is
         when Put_Mode =>
            return "Put";
         when Put_Line_Mode =>
            return "Put_Line";
         when Trace_Mode =>
            return "Trace";
      end case;
   end Image;

   function Image (Repeater : Repeater_Pattern) return String is
   begin
      return Image (Repeater.Pattern) &
             " *"                     &
             Integer'Image (Repeater.Count);
   end Image;

   function Image (Pattern : Return_Pattern) return String is
   begin
      case Pattern.Result is
         when Successful =>
            return "Success";
         when Failed =>
            return "Failure";
         when Aborted =>
            return "Failure (" & Quote (Pattern.Message) & ")";
      end case;
   end Image;

   function Image (Sequence : Sequence_Pattern) return String is
   begin
      return "(" & Image (Sequence.Items, " & ") & ")";
   end Image;

   function Image (Pattern : Subscript_Digit_Pattern)
      return String is
   begin
      return "Subscript_Digit";
   end Image;

   function Image (Pattern : Superscript_Digit_Pattern)
      return String is
   begin
      return "Superscript_Digit";
   end Image;

   function Image (Text : Text_Pattern) return String is
   begin
      return "Text (" & Quote (Text.Text) & ")";
   end Image;

   function Image (Pattern : Upper_Case_Letter_Pattern) return String is
   begin
      return "Upper_Case_Letter";
   end Image;

   function Image (Pattern : XML_Literal_Pattern) return String is
   begin
      return "XML_Literal";
   end Image;

   function Letter return Pattern_Type is
   begin
      return Ref (new Letter_Pattern);
   end Letter;

   function Lower_Case_Letter return Pattern_Type is
   begin
      return Ref (new Lower_Case_Letter_Pattern);
   end Lower_Case_Letter;

   function Match
            (  Pattern : Alphanumeric_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      use Strings_Edit.UTF8;
      use Strings_Edit.UTF8.Categorization;
      Line      : Line_Ptr_Type;
      Pointer   : aliased Integer;
      Last      : Integer;
      Character : UTF8_Code_Point;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      if Pointer <= Last then
         Get (Line (Pointer..Last), Pointer, Character);
         if Is_Alphanumeric (Character) then
            Set_Pointer (Source.all, Pointer, False);
            return Matched;
         end if;
      end if;
      return Unmatched;
   exception
      when Ada.Text_IO.Data_Error =>
         return
         (  Aborted,
            Encoding_Error'Length,
            Link (Source.all),
            Encoding_Error
         );
   end Match;

   function Match
            (  Pattern : Any_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      use Strings_Edit.UTF8;
      Line      : Line_Ptr_Type;
      Pointer   : aliased Integer;
      Last      : Integer;
      Character : UTF8_Code_Point;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      if Pointer <= Last then
         Get (Line (Pointer..Last), Pointer, Character);
         Set_Pointer (Source.all, Pointer, False);
         return Matched;
      else
         return Unmatched;
      end if;
   exception
      when Ada.Text_IO.Data_Error =>
         return
         (  Aborted,
            Encoding_Error'Length,
            Link (Source.all),
            Encoding_Error
         );
   end Match;

   function Match
            (  Pattern : Any_Of_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      use Strings_Edit.UTF8;
      Line      : Line_Ptr_Type;
      Pointer   : aliased Integer;
      Last      : Integer;
      Character : UTF8_Code_Point;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      if Pointer <= Last then
         Get (Line (Pointer..Last), Pointer, Character);
         if Is_In (Character, Pattern.Set) then
            Set_Pointer (Source.all, Pointer, False);
            return Matched;
         else
            return Unmatched;
         end if;
      else
         return Unmatched;
      end if;
   exception
      when Ada.Text_IO.Data_Error =>
         return
         (  Aborted,
            Encoding_Error'Length,
            Link (Source.all),
            Encoding_Error
         );
   end Match;

   function Match
            (  Pattern : As_Text_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
   begin
      if Defined (Pattern.Variable) then
         declare
            Text : constant String := Value (Pattern.Variable);
         begin
            Get_Line (Source.all, Line, Pointer, Last);
            if Last - Pointer + 1 >= Text'Length then
               if Line (Pointer..Pointer + Text'Length - 1) = Text then
                  Set_Pointer
                  (  Source.all,
                     Pointer + Text'Length,
                     False
                  );
                  return Matched;
               end if;
            end if;
         end;
      end if;
      return Unmatched;
   end Match;

   function Match
            (  Pattern : As_Case_Insensitive_Text_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
   begin
      if not Defined (Pattern.Variable) then
         return Unmatched;
      end if;
      declare
         Text    : constant String := Value (Pattern.Variable);
         Line    : Line_Ptr_Type;
         Pointer : aliased Integer;
         Last    : Integer;
      begin
         Get_Line (Source.all, Line, Pointer, Last);
         if Get
            (  Line (Pointer..Last),
               Text,
               Pointer'Access,
               Lower_Case_Map
            )  then
            Set_Pointer (Source.all, Pointer, False);
            return Matched;
         else
            return Unmatched;
         end if;
      end;
   exception
      when Ada.Text_IO.Data_Error =>
         return
         (  Aborted,
            Encoding_Error'Length,
            Link (Source.all),
            Encoding_Error
         );
   end Match;

   function Match
            (  Pattern : Blank_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      use Strings_Edit.UTF8;
      use Strings_Edit.UTF8.Categorization;
      Line      : Line_Ptr_Type;
      Pointer   : aliased Integer;
      Last      : Integer;
      Index     : Integer;
      Result    : Integer;
      Character : UTF8_Code_Point;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      Index := Pointer;
      case Pattern.Mode is
         when Single =>
            if Pointer <= Last then
               Get (Line (Pointer..Last), Index, Character);
               if Character = 16#9# or else Is_Space (Character) then
                  Set_Pointer (Source.all, Index, False);
                  return Matched;
               end if;
            end if;
            return Unmatched;
         when Chain | Non_Empty_Chain =>
            while Index <= Last loop
               Result := Index;
               Get (Line (Pointer..Last), Index, Character);
               exit when Character /= 16#9# and then
                     not Is_Space (Character);
            end loop;
            if Index = Pointer or else Result = Pointer then
               if Pattern.Mode = Chain then
                  return Matched;
               else
                  return Unmatched;
               end if;
            else
               Set_Pointer (Source.all, Result, False);
               return Matched;
            end if;
      end case;
   exception
      when Ada.Text_IO.Data_Error =>
         return
         (  Aborted,
            Encoding_Error'Length,
            Link (Source.all),
            Encoding_Error
         );
   end Match;

   function Match
            (  Text   : Case_Insensitive_Text_Pattern;
               Source : access Source_Type;
               State  : access Match_State
            )  return Result_Type is
      Line    : Line_Ptr_Type;
      Pointer : aliased Integer;
      Last    : Integer;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      if Get
         (  Line (Pointer..Last),
            Text.Text,
            Pointer'Access,
            Lower_Case_Map
         )  then
         Set_Pointer (Source.all, Pointer, False);
         return Matched;
      else
        return Unmatched;
      end if;
   exception
      when Ada.Text_IO.Data_Error =>
         return
         (  Aborted,
            Encoding_Error'Length,
            Link (Source.all),
            Encoding_Error
         );
   end Match;

   function Match
            (  Pattern : Digit_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      if Pointer <= Last and then Line (Pointer) in '0'..'9' then
         Set_Pointer (Source.all, Pointer + 1, False);
         return Matched;
      else
         return Unmatched;
      end if;
   end Match;

   function Match
            (  Pattern : Empty_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
   begin
      return Matched;
   end Match;

   function Match
            (  Pattern : End_Of_Line_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      if Pointer > Last or else
         (Pointer = Last and then Line (Pointer) = Character'Val (13))
      then
         return Matched;
      else
         return Unmatched;
      end if;
   end Match;

   function Match
            (  Pattern : Field_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      use Strings_Edit.UTF8;
      Line      : Line_Ptr_Type;
      Pointer   : aliased Integer;
      Last      : Integer;
      Current   : Integer;
      Start     : Integer;
      Character : UTF8_Code_Point;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      Start := Pointer;
      while Pointer <= Last loop
         Current := Pointer;
         Get (Line (Pointer..Last), Pointer, Character);
         if Is_In (Character, Pattern.Set) then
            Pointer := Current;
            exit;
         end if;
      end loop;
      if Pointer = Start then
         return Unmatched;
      else
         Set_Pointer (Source.all, Pointer, False);
         return Matched;
      end if;
   exception
      when Ada.Text_IO.Data_Error =>
         return
         (  Aborted,
            Encoding_Error'Length,
            Link (Source.all),
            Encoding_Error
         );
   end Match;

   function Match
            (  Pattern : Fixed_Point_Number_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
      After   : Boolean := False;
      Fore    : Boolean;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      Get (Line (Pointer..Last), Pointer, Pattern.Base, Fore);
      if Pointer <= Last and then Line (Pointer) = '.' then
         Pointer := Pointer + 1;
         Get (Line (Pointer..Last), Pointer, Pattern.Base, After);
      end if;
      if Fore or After then
         Set_Pointer (Source.all, Pointer, False);
         return Matched;
      else
         return Unmatched;
      end if;
   end Match;

   function Match
            (  Pattern : Floating_Point_Number_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
      After   : Boolean := False;
      Fore    : Boolean;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      if Pointer <= Last then
         if Line (Pointer) = '+' or else Line (Pointer) = '-' then
            Pointer := Pointer + 1;
         end if;
      end if;
      Get (Line (Pointer..Last), Pointer, 10, Fore);
      if Pointer <= Last and then Line (Pointer) = '.' then
         Pointer := Pointer + 1;
         Get (Line (Pointer..Last), Pointer, 10, After);
      end if;
      if not (Fore or After) then
         return Unmatched;
      end if;
      if Pointer <= Last then
         if Line (Pointer) = 'e' or else Line (Pointer) = 'E' then
            Pointer := Pointer + 1;
            if Pointer <= Last then
               if Line (Pointer) = '+' or else Line (Pointer) = '-' then
                  Pointer := Pointer + 1;
               end if;
            end if;
            Get (Line (Pointer..Last), Pointer, 10, After);
            if not After then
               return Unmatched;
            end if;
         end if;
      end if;
      Set_Pointer (Source.all, Pointer, False);
      return Matched;
   end Match;

   function Match
            (  Pattern : Letter_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      use Strings_Edit.UTF8;
      use Strings_Edit.UTF8.Categorization;
      Line      : Line_Ptr_Type;
      Pointer   : aliased Integer;
      Last      : Integer;
      Character : UTF8_Code_Point;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      if Pointer <= Last then
         Get (Line (Pointer..Last), Pointer, Character);
         if Is_Letter (Character) then
            Set_Pointer (Source.all, Pointer, False);
            return Matched;
         end if;
      end if;
      return Unmatched;
   exception
      when Ada.Text_IO.Data_Error =>
         return
         (  Aborted,
            Encoding_Error'Length,
            Link (Source.all),
            Encoding_Error
         );
   end Match;

   function Match
            (  Pattern : Lower_Case_Letter_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      use Strings_Edit.UTF8;
      use Strings_Edit.UTF8.Categorization;
      Line      : Line_Ptr_Type;
      Pointer   : aliased Integer;
      Last      : Integer;
      Character : UTF8_Code_Point;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      if Pointer <= Last then
         Get (Line (Pointer..Last), Pointer, Character);
         if Is_Lower (Character) then
            Set_Pointer (Source.all, Pointer, False);
            return Matched;
         end if;
      end if;
      return Unmatched;
   exception
      when Ada.Text_IO.Data_Error =>
         return
         (  Aborted,
            Encoding_Error'Length,
            Link (Source.all),
            Encoding_Error
         );
   end Match;

   function Match
            (  Pattern : Natural_Number_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
      Got_It  : Boolean;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      Get (Line (Pointer..Last), Pointer, Pattern.Base, Got_It);
      if Got_It then
         Set_Pointer (Source.all, Pointer, False);
         return Matched;
      else
         return Unmatched;
      end if;
   end Match;

   function Match
            (  Pattern : Subscript_Digit_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      use Strings_Edit.UTF8;
      use Strings_Edit.UTF8.Categorization;
      Line      : Line_Ptr_Type;
      Pointer   : aliased Integer;
      Last      : Integer;
      Character : UTF8_Code_Point;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      if Pointer <= Last then
         Get (Line (Pointer..Last), Pointer, Character);
         if Is_Subscript_Digit (Character) then
            Set_Pointer (Source.all, Pointer, False);
            return Matched;
         end if;
      end if;
      return Unmatched;
   exception
      when Ada.Text_IO.Data_Error =>
         return
         (  Aborted,
            Encoding_Error'Length,
            Link (Source.all),
            Encoding_Error
         );
   end Match;

   function Match
            (  Pattern : Superscript_Digit_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      use Strings_Edit.UTF8;
      use Strings_Edit.UTF8.Categorization;
      Line      : Line_Ptr_Type;
      Pointer   : aliased Integer;
      Last      : Integer;
      Character : UTF8_Code_Point;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      if Pointer <= Last then
         Get (Line (Pointer..Last), Pointer, Character);
         if Is_Superscript_Digit (Character) then
            Set_Pointer (Source.all, Pointer, False);
            return Matched;
         end if;
      end if;
      return Unmatched;
   exception
      when Ada.Text_IO.Data_Error =>
         return
         (  Aborted,
            Encoding_Error'Length,
            Link (Source.all),
            Encoding_Error
         );
   end Match;

   function Match
            (  Text   : Text_Pattern;
               Source : access Source_Type;
               State  : access Match_State
            )  return Result_Type is
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      if Last - Pointer + 1 >= Text.Length then
         if Line (Pointer..Pointer + Text.Length - 1) = Text.Text then
            Set_Pointer (Source.all, Pointer + Text.Length, False);
            return Matched;
         end if;
      end if;
      return Unmatched;
   end Match;

   function Match
            (  Pattern : Upper_Case_Letter_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      use Strings_Edit.UTF8;
      use Strings_Edit.UTF8.Categorization;
      Line      : Line_Ptr_Type;
      Pointer   : aliased Integer;
      Last      : Integer;
      Character : UTF8_Code_Point;
   begin
      Get_Line (Source.all, Line, Pointer, Last);
      if Pointer <= Last then
         Get (Line (Pointer..Last), Pointer, Character);
         if Is_Upper (Character) then
            Set_Pointer (Source.all, Pointer, False);
            return Matched;
         end if;
      end if;
      return Unmatched;
   exception
      when Ada.Text_IO.Data_Error =>
         return
         (  Aborted,
            Encoding_Error'Length,
            Link (Source.all),
            Encoding_Error
         );
   end Match;

   function Match
            (  Pattern : XML_Literal_Pattern;
               Source  : access Source_Type;
               State   : access Match_State
            )  return Result_Type is
      use Strings_Edit;
      use Strings_Edit.UTF8;
      Line_Ptr  : Line_Ptr_Type;
      Pointer   : aliased Integer;
      Last      : Integer;
      Quote     : Character;
      Character : UTF8_Code_Point;
   begin
      Get_Line (Source.all, Line_Ptr, Pointer, Last);
      if Pointer > Last then
         return Unmatched;
      end if;
      declare
         Line : String renames Line_Ptr (Pointer..Last);
      begin
         Quote := Line (Pointer);
         if Quote /= ''' and then Quote /= '"' then
            return Unmatched;
         end if;
         Pointer := Pointer + 1;
         while Pointer <= Last loop
            case Line (Pointer) is
               when ''' | '"' =>
                  if Quote = Line (Pointer) then
                     Set_Pointer (Source.all, Pointer + 1, False);
                     return Matched;
                  end if;
                  Pointer := Pointer + 1;
               when '&' =>
                  Pointer := Pointer + 1;
                  if Is_Prefix ("amp;", Line, Pointer) then
                     Pointer := Pointer + 4;
                  elsif Is_Prefix ("lt;", Line, Pointer) then
                     Pointer := Pointer + 3;
                  elsif Is_Prefix ("gt;", Line, Pointer) then
                     Pointer := Pointer + 3;
                  elsif Is_Prefix ("apos;", Line, Pointer) then
                     Pointer := Pointer + 5;
                  elsif Is_Prefix ("quot;", Line, Pointer) then
                     Pointer := Pointer + 5;
                  else
                     return Unmatched;
                  end if;
               when others =>
                  Get (Line (Pointer..Last), Pointer, Character);
            end case;
         end loop;
      end;
      return Unmatched;
   exception
      when Ada.Text_IO.Data_Error =>
         return
         (  Aborted,
            Encoding_Error'Length,
            Link (Source.all),
            Encoding_Error
         );
   end Match;

   function Natural_Number (Base : Strings_Edit.NumberBase := 10)
      return Pattern_Type is
   begin
      return Ref (new Natural_Number_Pattern (Base));
   end Natural_Number;

   function Nil return Pattern_Type is
   begin
      return Ref (new Nil_Pattern);
   end Nil;

   function NL return Pattern_Type is
   begin
      return Ref (new Next_Line_Pattern);
   end NL;

   function NL_Or_EOF return Pattern_Type is
   begin
      return Ref (new Next_Line_Or_EOF_Pattern);
   end NL_Or_EOF;

   function Nonempty (Pattern : Pattern_Type) return Pattern_Type is
      Result_Ptr : constant Pattern_Ptr := new Nonempty_Pattern;
      This       : Nonempty_Pattern renames
                      Nonempty_Pattern (Result_Ptr.all);
   begin
      This.Pattern := Pattern;
      return Ref (Result_Ptr);
   end Nonempty;

   procedure On_Failure
             (  Hook   : in out Print_Handler;
                Source : in out Source_Type
             )  is
   begin
      null;
   end On_Failure;

   function On_Line_Change
            (  Hook   : access Print_Handler;
               Source : access Source_Type;
               Where  : Location_Type
            )  return Result_Type is
   begin
      return Unmatched;
   end On_Line_Change;

   procedure On_Success
             (  Hook   : in out Print_Handler;
                Source : in out Source_Type;
                From   : Integer;
                To     : Integer
             )  is
      use Ada.Text_IO;
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
   begin
      Get_Line (Source, Line, Pointer, Last);
      case Hook.Mode is
         when Put_Line_Mode =>
            Put_Line
            (  Hook.File.all,
               Hook.Prefix & Line (From..To) & Hook.Suffix
            );
         when Put_Mode =>
            Put
            (  Hook.File.all,
               Hook.Prefix & Line (From..To) & Hook.Suffix
            );
         when Trace_Mode =>
            Put (Hook.File.all, Line (Line'First..From - 1));
            Put (Hook.File.all, Hook.Prefix);
            Put (Hook.File.all, Line (From..To));
            Put (Hook.File.all, Hook.Suffix);
            Put (Hook.File.all, Line (To + 1..Last));
            New_Line;
      end case;
   end On_Success;

   function Print
            (  File    : Ada.Text_IO.File_Access;
               Pattern : Pattern_Type;
               Mode    : Print_Mode;
               Prefix  : String;
               Suffix  : String
            )  return Pattern_Type is
      use Hook_Handlers;
      Hook   : constant Hook_Handlers.Handle :=
               Ref (new Print_Handler
                        (  Mode,
                           Prefix'Length,
                           Suffix'Length
                   )    );
      This   : Print_Handler renames Print_Handler (Ptr (Hook).all);
      Result : Pattern_Ptr;
   begin
      This.File   := File;
      This.Prefix := Prefix;
      This.Suffix := Suffix;
      Result := new Hook_Pattern (Ptr (Hook));
      Hook_Pattern (Result.all).Handler := Hook;
      Hook_Pattern (Result.all).Pattern := Pattern;
      return Ref (Result);
   end Print;

   function Put
            (  File    : Ada.Text_IO.File_Access;
               Pattern : Pattern_Type;
               Prefix  : String := "";
               Suffix  : String := ""
            )  return Pattern_Type is
   begin
      return Print (File, Pattern, Put_Mode, Prefix, Suffix);
   end Put;

   function Put
            (  File   : Ada.Text_IO.File_Access;
               Text   : String;
               Prefix : String := "";
               Suffix : String := ""
            )  return Pattern_Type is
   begin
      return Print
             (  File,
                Patterns.Text (Text),
                Put_Mode,
                Prefix,
                Suffix
             );
   end Put;

   function Put
            (  Pattern : Pattern_Type;
               Prefix  : String := "";
               Suffix  : String := ""
            )  return Pattern_Type is
   begin
      return Print
             (  Ada.Text_IO.Standard_Output,
                Pattern,
                Put_Mode,
                Prefix,
                Suffix
             );
   end Put;

   function Put
            (  Text   : String;
               Prefix : String := "";
               Suffix : String := ""
            )  return Pattern_Type is
   begin
      return Print
             (  Ada.Text_IO.Standard_Output,
                Patterns.Text (Text),
                Put_Mode,
                Prefix,
                Suffix
             );
   end Put;

   function Put_Line
            (  File    : Ada.Text_IO.File_Access;
               Pattern : Pattern_Type;
               Prefix  : String := "";
               Suffix  : String := ""
            )  return Pattern_Type is
   begin
      return Print (File, Pattern, Put_Line_Mode, Prefix, Suffix);
   end Put_Line;

   function Put_Line
            (  File   : Ada.Text_IO.File_Access;
               Text   : String;
               Prefix : String := "";
               Suffix : String := ""
            )  return Pattern_Type is
   begin
      return Print
             (  File,
                Patterns.Text (Text),
                Put_Line_Mode,
                Prefix,
                Suffix
             );
   end Put_Line;

   function Put_Line
            (  Pattern : Pattern_Type;
               Prefix  : String := "";
               Suffix  : String := ""
            )  return Pattern_Type is
   begin
      return Print
             (  Ada.Text_IO.Standard_Output,
                Pattern,
                Put_Line_Mode,
                Prefix,
                Suffix
             );
   end Put_Line;

   function Put_Line
            (  Text   : String;
               Prefix : String := "";
               Suffix : String := ""
            )  return Pattern_Type is
   begin
      return Print
             (  Ada.Text_IO.Standard_Output,
                Patterns.Text (Text),
                Put_Line_Mode,
                Prefix,
                Suffix
             );
   end Put_Line;

   function Ref (Thing : Pattern_Ptr) return Pattern_Type is
   begin
      return (Pattern_Handles.Ref (Thing) with null record);
   end Ref;

   function Resolve
            (  Pattern   : access Alternate_Pattern;
               Reference : Pattern_Ref
            )  return Boolean is
      Result : Boolean := False;
   begin
      for Index in Pattern.Items'Range loop
         Result := Result
                or Resolve (Ptr (Pattern.Items (Index)), Reference);
      end loop;
      return Result;
   end Resolve;

   function Resolve
            (  Pattern   : access Eager_Pattern;
               Reference : Pattern_Ref
            )  return Boolean is
   begin
      return Resolve (Ptr (Pattern.Pattern), Reference);
   end Resolve;

   function Resolve
            (  Pattern   : access Hook_Pattern;
               Reference : Pattern_Ref
            )  return Boolean is
   begin
      return Resolve (Ptr (Pattern.Pattern), Reference);
   end Resolve;

   function Resolve
            (  Pattern   : access Lazy_Pattern;
               Reference : Pattern_Ref
            )  return Boolean is
   begin
      return Resolve (Ptr (Pattern.Pattern), Reference);
   end Resolve;

   function Resolve
            (  Pattern   : access Not_Pattern;
               Reference : Pattern_Ref
            )  return Boolean is
   begin
      return Resolve (Ptr (Pattern.Pattern), Reference);
   end Resolve;

   function Resolve
            (  Pattern   : access Nonempty_Pattern;
               Reference : Pattern_Ref
            )  return Boolean is
   begin
      return Resolve (Ptr (Pattern.Pattern), Reference);
   end Resolve;

   function Resolve
            (  Pattern   : access Goto_Pattern;
               Reference : Pattern_Ref
            )  return Boolean is
   begin
      if Pattern.Pattern = null then
         Pattern.Pattern := Reference;
         return True;
      else
         return False;
      end if;
   end Resolve;

   function Resolve
            (  Pattern   : access Pattern_Object;
               Reference : Pattern_Ref
            )  return Boolean is
   begin
      return False;
   end Resolve;

   function Resolve
            (  Repeater  : access Repeater_Pattern;
               Reference : Pattern_Ref
            )  return Boolean is
   begin
      return Resolve (Ptr (Repeater.Pattern), Reference);
   end Resolve;

   function Resolve
            (  Pattern   : access Sequence_Pattern;
               Reference : Pattern_Ref
            )  return Boolean is
      Result : Boolean := False;
   begin
      for Index in Pattern.Items'Range loop
         Result := Result
                or Resolve (Ptr (Pattern.Items (Index)), Reference);
      end loop;
      return Result;
   end Resolve;

   function Self return Pattern_Type is
   begin
      return Ref (new Goto_Pattern);
   end Self;

   procedure Set_Tracing (State : in out Match_State; On : Boolean) is
   begin
      State.Trace := On;
   end Set_Tracing;

   function Space return Pattern_Type is
      Result_Ptr : constant Pattern_Ptr := new Blank_Pattern;
   begin
      Blank_Pattern (Result_Ptr.all).Mode := Single;
      return Ref (Result_Ptr);
   end Space;

   function Subscript_Digit return Pattern_Type is
   begin
      return Ref (new Subscript_Digit_Pattern);
   end Subscript_Digit;

   function Superscript_Digit return Pattern_Type is
   begin
      return Ref (new Superscript_Digit_Pattern);
   end Superscript_Digit;

   function Success return Pattern_Type is
   begin
      return Ref (new Return_Pattern (Successful, 0));
   end Success;

   function Text (Text : String) return Pattern_Type is
      Result_Ptr : constant Pattern_Ptr :=
                            new Text_Pattern (Text'Length);
   begin
      Text_Pattern (Result_Ptr.all).Text := Text;
      return Ref (Result_Ptr);
   end Text;

   function Text (Variable : Variable_Type) return Pattern_Type is
      Result_Ptr : constant Pattern_Ptr := new As_Text_Pattern;
   begin
      As_Text_Pattern (Result_Ptr.all).Variable := Variable;
      return Ref (Result_Ptr);
   end Text;

   function Trace
            (  File    : Ada.Text_IO.File_Access;
               Pattern : Pattern_Type;
               Prefix  : String := "|";
               Suffix  : String := "|"
            )  return Pattern_Type is
   begin
      return Print (File, Pattern, Trace_Mode, Prefix, Suffix);
   end Trace;

   function Trace
            (  File   : Ada.Text_IO.File_Access;
               Text   : String;
               Prefix : String := "|";
               Suffix : String := "|"
            )  return Pattern_Type is
   begin
      return Print
             (  Ada.Text_IO.Standard_Output,
                Patterns.Text (Text),
                Trace_Mode,
                Prefix,
                Suffix
             );
   end Trace;

   function Trace
            (  Pattern : Pattern_Type;
               Prefix  : String := "|";
               Suffix  : String := "|"
            )  return Pattern_Type is
   begin
      return Print
             (  Ada.Text_IO.Standard_Output,
                Pattern,
                Trace_Mode,
                Prefix,
                Suffix
             );
   end Trace;

   function Trace
            (  Text   : String;
               Prefix : String := "|";
               Suffix : String := "|"
            )  return Pattern_Type is
   begin
      return Print
             (  Ada.Text_IO.Standard_Output,
                Patterns.Text (Text),
                Trace_Mode,
                Prefix,
                Suffix
             );
   end Trace;

   function Upper_Case_Letter return Pattern_Type is
   begin
      return Ref (new Upper_Case_Letter_Pattern);
   end Upper_Case_Letter;

   function Voidable
            (  Pattern   : Alphanumeric_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Alternation : Alternate_Pattern;
               Recursive   : Boolean
            )  return Boolean is
   begin
      for Index in Alternation.Items'Range loop
         if Voidable
            (  Ptr (Alternation.Items (Index)).all,
               Recursive
            )  then
            return True;
         end if;
      end loop;
      return False;
   end Voidable;

   function Voidable
            (  Pattern   : Any_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Pattern   : Any_Of_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Pattern   : Anything_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return True;
   end Voidable;

   function Voidable
            (  Pattern   : As_Case_Insensitive_Text_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return True;
   end Voidable;

   function Voidable
            (  Pattern   : As_Text_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return True;
   end Voidable;

   function Voidable
            (  Pattern   : Blank_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return Pattern.Mode = Chain;
   end Voidable;

   function Voidable
            (  Pattern   : Goto_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      if Recursive then
         return True;
      else
         return Voidable (Pattern.Pattern.all, True);
      end if;
   end Voidable;

   function Voidable
            (  Text      : Case_Insensitive_Text_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return Text.Length = 0;
   end Voidable;

   function Voidable
            (  Pattern   : Digit_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Repeater  : Eager_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return True;
   end Voidable;

   function Voidable
            (  Pattern   : Empty_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return True;
   end Voidable;

   function Voidable
            (  Pattern   : End_Of_Line_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return True;
   end Voidable;

   function Voidable
            (  Pattern   : Fence_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return True;
   end Voidable;

   function Voidable
            (  Pattern   : Field_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Pattern   : Floating_Point_Number_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Pattern   : Fixed_Point_Number_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Pattern   : Hook_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return Voidable (Pattern.Pattern);
   end Voidable;

   function Voidable
            (  Repeater  : Lazy_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return True;
   end Voidable;

   function Voidable
            (  Pattern   : Letter_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Pattern   : Lower_Case_Letter_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Pattern   : Natural_Number_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Pattern   : Next_Line_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Pattern   : Nil_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Pattern   : Nonempty_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Pattern   : Not_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return True;
   end Voidable;

   function Voidable (Pattern : Pattern_Type) return Boolean is
      Object_Ptr : constant Pattern_Ptr := Ptr (Pattern);
   begin
      return Object_Ptr = null or else Voidable (Object_Ptr.all, False);
   end Voidable;

   function Voidable
            (  Repeater  : Repeater_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
     return False;
   end Voidable;

   function Voidable
            (  Pattern   : Return_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Sequence  : Sequence_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      for Index in Sequence.Items'Range loop
         if not Voidable
                (  Ptr (Sequence.Items (Index)).all,
                   Recursive
                )  then
            return False;
         end if;
      end loop;
      return True;
   end Voidable;

   function Voidable
            (  Pattern   : Subscript_Digit_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Pattern   : Superscript_Digit_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Text      : Text_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return Text.Length = 0;
   end Voidable;

   function Voidable
            (  Pattern   : Upper_Case_Letter_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function Voidable
            (  Pattern   : XML_Literal_Pattern;
               Recursive : Boolean
            )  return Boolean is
   begin
      return False;
   end Voidable;

   function XML_Literal return Pattern_Type is
   begin
      return Ref (new XML_Literal_Pattern);
   end XML_Literal;

------------------------------------------------------------------------
   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);
   procedure Free is
      new Ada.Unchecked_Deallocation (Segment_Array, Segment_Ptr);

   procedure Append
             (  Variable : in out Variable_Type;
                Value    : String;
                Where    : Location_Type
             )  is
      use Variable_Handles;
      This   : Variable_Object'Class renames Ptr (Variable.Handle).all;
      Length : Natural;
   begin
      if This.Length < 0 then
         Set (Variable, Value, Where);
         return;
      end if;
      if This.History = null then
         This.History := new Segment_Array (1..Segment_Increment);
      elsif This.Depth = This.History'Last then
         declare
            Ptr : constant Segment_Ptr :=
                       new Segment_Array
                           (  1
                           .. This.Depth + Segment_Increment
                           );
         begin
            Ptr (1..This.Depth) := This.History (1..This.Depth);
            Free (This.History);
            This.History := Ptr;
         end;
      end if;
      if This.Depth = 0 then
         Length := This.Length;
         This.History (This.Depth + 1) :=
         (  Where & This.Where,
            Length + Value'Length
         );
      else
         Length := This.History (This.Depth).Length;
         This.History (This.Depth + 1) :=
         (  Where & This.History (This.Depth).Where,
            Length + Value'Length
         );
      end if;
      This.Depth := This.Depth + 1;
      if This.Value'Length < Length + Value'Length then
         declare
            Ptr : constant String_Ptr :=
                       new String
                           (  1
                           .. Length + Value'Length + String_Increment
                           );
         begin
            Ptr (1..Length) := This.Value (1..Length);
            Free (This.Value);
            This.Value := Ptr;
         end;
      end if;
      This.Value (Length + 1..Length + Value'Length) := Value;
   end Append;

   function Defined (Variable : Variable_Type) return Boolean is
      use Variable_Handles;
   begin
      return Ptr (Variable.Handle).Length >= 0;
   end Defined;

   procedure Finalize (Variable : in out Variable_Object) is
   begin
      if Variable.Value /= null then
         Free (Variable.Value);
      end if;
      if Variable.History /= null then
         Free (Variable.History);
      end if;
   end Finalize;

   function Image (Hook : Assignment_Handler) return String is
   begin
      if Hook.Append then
         return "Append";
      else
         return "Assign";
      end if;
   end Image;

   function Image (Hook : User_Assignment_Handler) return String is
   begin
      if Hook.Append then
         return "Append";
      else
         return "Assign";
      end if;
   end Image;

   function Image (Hook : Variable_Assignment_Handler) return String is
   begin
      if Hook.Append then
         return "Append";
      else
         return "Assign";
      end if;
   end Image;

   function Link (Variable : Variable_Type) return Location_Type is
      use Variable_Handles;
      This : Variable_Object'Class renames Ptr (Variable.Handle).all;
   begin
      if This.Length < 0 then
         raise Constraint_Error;
      else
         if This.Depth > 0 then
            return This.History (This.Depth).Where;
         else
            return This.Where;
         end if;
      end if;
   end Link;

   procedure On_Failure
             (  Hook   : in out Assignment_Handler;
                Source : in out Source_Type
             )  is
   begin
      if Hook.Append then
         Truncate (Hook.Variable);
      else
         Reset (Hook.Variable);
      end if;
   end On_Failure;

   procedure On_Failure
             (  Hook   : in out User_Assignment_Handler;
                Source : in out Source_Type
             )  is
      use User_Variable_Handles;
   begin
      Delete (Ptr (Hook.Variable).all, Source, Hook.Append);
   end On_Failure;

   procedure On_Failure
             (  Hook   : in out Variable_Assignment_Handler;
                Source : in out Source_Type
             )  is
   begin
      if Hook.On_Failure /= null then
         Hook.On_Failure (Hook, Source);
      end if;
   end On_Failure;

   function On_Line_Change
            (  Hook   : access Assignment_Handler;
               Source : access Source_Type;
               Where  : Location_Type
            )  return Result_Type is
   begin
      return Unmatched;
   end On_Line_Change;

   function On_Line_Change
            (  Hook   : access User_Assignment_Handler;
               Source : access Source_Type;
               Where  : Location_Type
            )  return Result_Type is
      use User_Variable_Handles;
   begin
      return On_Line_Change (Ptr (Hook.Variable), Source, Where);
   end On_Line_Change;

   function On_Line_Change
            (  Hook   : access Variable_Assignment_Handler;
               Source : access Source_Type;
               Where  : Location_Type
            )  return Result_Type is
   begin
      if Hook.On_Failure /= null then
         return Hook.On_Line_Change (Hook, Source, Where);
      else
         return Matched;
      end if;
   end On_Line_Change;

   procedure On_Success
             (  Hook   : in out Assignment_Handler;
                Source : in out Source_Type;
                From   : Integer;
                To     : Integer
             )  is
      Line    : Line_Ptr_Type;
      Pointer : aliased Integer;
      Last    : Integer;
   begin
      Get_Line (Source, Line, Pointer, Last);
      if Hook.Append then
         Append
         (  Hook.Variable,
            Line (From..To),
            Direct_Link (Source, From, To)
         );
      else
         Set
         (  Hook.Variable,
            Line (From..To),
            Direct_Link (Source, From, To)
         );
      end if;
   end On_Success;

   procedure On_Success
             (  Hook   : in out User_Assignment_Handler;
                Source : in out Source_Type;
                From   : Integer;
                To     : Integer
             )  is
      use User_Variable_Handles;
      Line    : Line_Ptr_Type;
      Pointer : aliased Integer;
      Last    : Integer;
   begin
      Get_Line (Source, Line, Pointer, Last);
      Add
      (  Ptr (Hook.Variable).all,
         Source,
         Line (From..To),
         Direct_Link (Source, From, To),
         Hook.Append
      );
   end On_Success;

   procedure On_Success
             (  Hook   : in out Variable_Assignment_Handler;
                Source : in out Source_Type;
                From   : Integer;
                To     : Integer
             )  is
   begin
      if Hook.On_Success /= null then
         Hook.On_Success (Hook, Source, From, To);
      end if;
   end On_Success;

   procedure Reset (Variable : in out Variable_Type) is
      use Variable_Handles;
      This : Variable_Object'Class renames Ptr (Variable.Handle).all;
   begin
      This.Length := -1;
      This.Depth  :=  0;
   end Reset;

   procedure Set
             (  Variable : in out Variable_Type;
                Value    : String;
                Where    : Location_Type
             )  is
      use Variable_Handles;
      This : Variable_Object'Class renames Ptr (Variable.Handle).all;
   begin
      if This.Value = null then
         This.Value := new String (1..Value'Length);
         This.Value.all := Value;
      elsif This.Value'Length >= Value'Length then
         This.Value (1..Value'Length) := Value;
      else
         Free (This.Value);
         This.Value := new String (1..Value'Length);
         This.Value.all := Value;
      end if;
      This.Length := Value'Length;
      This.Where  := Where;
   end Set;

   procedure Truncate (Variable : in out Variable_Type) is
      use Variable_Handles;
      This : Variable_Object'Class renames Ptr (Variable.Handle).all;
   begin
      if This.Depth > 0 then
         This.Depth := This.Depth - 1;
      else
         This.Length := -1;
      end if;
   end Truncate;

   function Value (Variable : Variable_Type) return String is
      use Variable_Handles;
      This  : Variable_Object'Class renames Ptr (Variable.Handle).all;
      Depth : Natural renames This.Depth;
   begin
      if This.Length < 0 then
         raise Constraint_Error;
      else
         if Depth > 0 then
            return This.Value (1..This.History (Depth).Length);
         else
            return This.Value (1..This.Length);
         end if;
      end if;
   end Value;

   function "<="
            (  Left  : Variable_Type;
               Right : Pattern_Type
            )  return Pattern_Type is
      use Hook_Handlers;
      Hook   : constant Hook_Handlers.Handle :=
                        Ref (new Assignment_Handler (False));
      This   : Assignment_Handler renames
                  Assignment_Handler (Ptr (Hook).all);
      Result : Pattern_Ptr;
   begin
      This.Variable := Left;
      Result        := new Hook_Pattern (Ptr (Hook));
      Hook_Pattern (Result.all).Handler := Hook;
      Hook_Pattern (Result.all).Pattern := Right;
      return Ref (Result);
   end "<=";

   function "<="
            (  Left  : User_Variable_Handles.Handle;
               Right : Pattern_Type
            )  return Pattern_Type is
      use Hook_Handlers;
      Hook   : constant Hook_Handlers.Handle :=
                        Ref (new User_Assignment_Handler (False));
      This   : User_Assignment_Handler renames
                  User_Assignment_Handler (Ptr (Hook).all);
      Result : Pattern_Ptr;
   begin
      This.Variable := Left;
      Result        := new Hook_Pattern (Ptr (Hook));
      Hook_Pattern (Result.all).Handler := Hook;
      Hook_Pattern (Result.all).Pattern := Right;
      return Ref (Result);
   end "<=";

   function "<="
            (  Left  : Variable_Type;
               Right : String
            )  return Pattern_Type is
   begin
      return Left <= Text (Right);
   end "<=";

   function "<="
            (  Left  : User_Variable_Handles.Handle;
               Right : String
            )  return Pattern_Type is
   begin
      return Left <= Text (Right);
   end "<=";

   function "<"
            (  Left  : Variable_Type;
               Right : Pattern_Type
            )  return Pattern_Type is
      use Hook_Handlers;
      Hook   : constant Hook_Handlers.Handle :=
                        Ref (new Assignment_Handler (True));
      This   : Assignment_Handler renames
                  Assignment_Handler (Ptr (Hook).all);
      Result : Pattern_Ptr;
   begin
      This.Variable := Left;
      Result        := new Hook_Pattern (Ptr (Hook));
      Hook_Pattern (Result.all).Handler := Hook;
      Hook_Pattern (Result.all).Pattern := Right;
      return Ref (Result);
   end "<";

   function "<"
            (  Left  : User_Variable_Handles.Handle;
               Right : Pattern_Type
            )  return Pattern_Type is
      use Hook_Handlers;
      Hook   : constant Hook_Handlers.Handle :=
                        Ref (new User_Assignment_Handler (True));
      This   : User_Assignment_Handler renames
                  User_Assignment_Handler (Ptr (Hook).all);
      Result : Pattern_Ptr;
   begin
      This.Variable := Left;
      Result        := new Hook_Pattern (Ptr (Hook));
      Hook_Pattern (Result.all).Handler := Hook;
      Hook_Pattern (Result.all).Pattern := Right;
      return Ref (Result);
   end "<";

   function "<"
            (  Left  : Variable_Type;
               Right : String
            )  return Pattern_Type is
   begin
      return Left < Text (Right);
   end "<";

   function "<"
            (  Left  : User_Variable_Handles.Handle;
               Right : String
            )  return Pattern_Type is
   begin
      return Left < Text (Right);
   end "<";

end Parsers.Generic_Source.Patterns;
