--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Generic_Text_IO                          Summer, 2025       --
--  Implementation                                                    --
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

package body Parsers.Generic_Ada_Parser.Generic_Text_IO is
   use Sources;

   type Nesting is (Top, Child, Inline);

   procedure Put
             (  Tree   : Tokens.Argument_Token;
                Output : File_Type := Standard_Output;
                Prefix : String    := ""
             )  is
      procedure Do_Put
                (  Argument : Tokens.Argument_Token;
                   Prefix   : String;
                   Nested   : Nesting := Child
                )  is
      begin
         case Nested is
            when Top =>
               Put (Output, Prefix);
            when Child =>
               if Prefix'Length >= 3 then
                  Put
                  (  Output,
                     Prefix (Prefix'First..Prefix'Last - 3) & "|__"
                  );
               end if;
            when Inline =>
               null;
         end case;
         case Get_Class (Argument.Value.all) is
            when Expression_Node =>
               declare
                  This : Expression'Class renames
                         Expression'Class (Argument.Value.all);
               begin
                  Put (Output, Image (This.Operation));
                  Put_Line (Output, " at " & Image (This.Location));
                  for Index in This.Operands'First
                            .. This.Operands'Last - 1 loop
                     Do_Put (This.Operands (Index), Prefix & "|  ");
                  end loop;
                  Do_Put
                  (  This.Operands (This.Operands'Last),
                     Prefix & "   "
                  );
               end;
            when Case_Node =>
               declare
                  This : Case_Expression'Class renames
                         Case_Expression'Class (Argument.Value.all);
               begin
                  Put (Output, "case ");
                  Do_Put
                  (  This.Selector,
                     Prefix & "     ",
                     Inline
                  );
                  for Index in This.Alternatives'Range loop
                     Put (Output, Prefix & "   when ");
                     Do_Put
                     (  This.Alternatives (Index).Guard,
                        Prefix & "        ",
                        Inline
                     );
                     Put (Output, Prefix & "      ");
                     Do_Put
                     (  This.Alternatives (Index).Value,
                        Prefix & "      ",
                        Inline
                     );
                  end loop;
                  if This.Has_Others then
                     Put (Prefix & "   when others");
                     Do_Put
                     (  This.Others_Alternative,
                        Prefix & "      ",
                        Inline
                     );
                  end if;
               end;
            when If_Node =>
               declare
                  This : If_Expression'Class renames
                         If_Expression'Class (Argument.Value.all);
               begin
                  Put (Output, "if ");
                  Do_Put
                  (  This.Alternatives (This.Alternatives'First).Guard,
                     Prefix & "   ",
                     Inline
                  );
                  Put_Line (Output, Prefix & "then ");
                  Put (Output, Prefix & "   ");
                  Do_Put
                  (  This.Alternatives (This.Alternatives'First).Value,
                     Prefix & "   ",
                     Inline
                  );
                  for Index in This.Alternatives'First + 1
                            .. This.Alternatives'Last loop
                     Put (Output, Prefix & "elsif ");
                     Do_Put
                     (  This.Alternatives (Index).Guard,
                        Prefix & "      ",
                        Inline
                     );
                     Put_Line (Output, Prefix & "then ");
                     Put (Output, Prefix & "   ");
                     Do_Put
                     (  This.Alternatives (Index).Value,
                        Prefix & "   ",
                        Inline
                     );
                  end loop;
                  if This.Has_Else then
                     Put_Line (Prefix & "else");
                     Put (Output, Prefix & "   ");
                     Do_Put
                     (  This.Else_Alternative,
                        Prefix & "   ",
                        Inline
                     );
                  end if;
               end;
            when For_Node =>
               declare
                  This : For_Expression'Class renames
                         For_Expression'Class (Argument.Value.all);
                  function Qualifier return String is
                  begin
                     case This.Qualifier is
                        when For_All =>
                           return "all ";
                        when For_Some =>
                           return "some ";
                        when For_Any =>
                           return "";
                     end case;
                  end Qualifier;

                  function Do_In return String is
                     function Do_Reverse return String is
                     begin
                        if 0 = (This.Options and For_Reverse) then
                           return "";
                        else
                           return "reverse ";
                        end if;
                     end Do_Reverse;
                  begin
                     if 0 = (This.Options and For_Of) then
                        if 0 = (This.Options and For_Range) then
                           return "in " & Do_Reverse;
                        else
                           return "in "                             &
                                  Do_Reverse                        &
                                  Image (This.Range_Type.Value.all) &
                                  " range ";
                        end if;
                     else
                        return "of " & Do_Reverse;
                     end if;
                  end Do_In;

                  For_Prefix : constant String :=
                               "for "                            &
                               Qualifier                         &
                               Image (This.Identifier.Value.all) &
                               " "                               &
                               Do_In;
               begin
                  if 0 /= (This.Options and For_Parallel) then
                     Put_Line (Output, "parallel");
                     if 0 /= (This.Options and For_Chunk) then
                        Put (Output, Prefix & "   (");
                        Do_Put
                        (  This.Chunk,
                           Prefix & "    ",
                           Inline
                        );
                        Put_Line (Output, Prefix & "   )");
                     end if;
                     if This.Count > 0 then
                        Put_Line (Output, Prefix & "with ");
                        for Index in This.Aspects'Range loop
                           Put (Output, Prefix & "      ");
                           Do_Put
                           (  This.Aspects (Index).Guard,
                              Prefix & "      ",
                              Inline
                           );
                           Put (Output, Prefix & "   => ");
                           Do_Put
                           (  This.Aspects (Index).Value,
                              Prefix & "   ",
                              Inline
                           );
                        end loop;
                     end if;
                     Put (Output, Prefix & For_Prefix);
                     Do_Put
                     (  This.Iterator,
                        Prefix & (For_Prefix'Range => ' '),
                        Inline
                     );
                  else
                     Put (Output, For_Prefix);
                     Do_Put
                     (  This.Iterator,
                        Prefix & (For_Prefix'Range => ' '),
                        Inline
                     );
                  end if;
                  if 0 /= (This.Options and For_Key) then
                     Put (Output, Prefix & "use ");
                     Do_Put
                     (  This.Key,
                        Prefix & "    ",
                        Inline
                     );
                  end if;
                  if 0 /= (This.Options and For_Condition) then
                     Put (Output, Prefix & "when ");
                     Do_Put
                     (  This.Condition,
                        Prefix & "     ",
                        Inline
                     );
                  end if;
                  Put (Output, Prefix & "=> ");
                  Do_Put
                  (  This.Expression,
                     Prefix & "   ",
                     Inline
                  );
               end;
            when Raise_Node =>
               declare
                  This : Raise_Expression'Class renames
                         Raise_Expression'Class (Argument.Value.all);
               begin
                  Put (Output, "raise ");
                  Do_Put
                  (  This.Name,
                     Prefix & "     ",
                     Inline
                  );
                  if This.Has_Message then
                     Put (Output, Prefix & "   with ");
                     Do_Put
                     (  This.Message,
                        Prefix & "      ",
                        Inline
                     );
                  end if;
               end;
            when Declare_Node =>
               declare
                  This : Declare_Expression'Class renames
                         Declare_Expression'Class (Argument.Value.all);
               begin
                  Put_Line (Output, Prefix & "declare");
                  for Index in This.Items'Range loop
                     declare
                        Item : Declare_Token renames This.Items (Index);
                     begin
                        Put (Output, Prefix & "   ");
                        Do_Put
                        (  Item.Value.Name,
                           Prefix & "   ",
                           Inline
                        );
                        if Item.Value.all in Declare_Renaming_Item then
                           declare
                              Object : Declare_Renaming_Item renames
                                       Declare_Renaming_Item
                                       (  Item.Value.all
                                       );
                           begin
                              Put
                              (  Output,
                                 Prefix & "      renames "
                              );
                              Do_Put
                              (  Object.Object,
                                 Prefix & "              ",
                                 Inline
                              );
                           end;
                        else
                           declare
                              Object : Declare_Object_Item renames
                                       Declare_Object_Item
                                       (  Item.Value.all
                                       );
                           begin
                              case Object.Kind_Of is
                                 when Immutable =>
                                    Put
                                    (  Output,
                                       Prefix & "      : constant "
                                    );
                                    Do_Put
                                    (  Object.Object,
                                       Prefix & "                 ",
                                       Inline
                                    );
                                    Put
                                    (  Output,
                                       Prefix & "         := "
                                    );
                                    Do_Put
                                    (  Object.Value,
                                       Prefix & "            ",
                                       Inline
                                    );
                                 when Initialized =>
                                    Put (Output, Prefix & "      : ");
                                    Do_Put
                                    (  Object.Object,
                                       Prefix & "        ",
                                       Inline
                                    );
                                    Put
                                    (  Output,
                                       Prefix & "          := "
                                    );
                                    Do_Put
                                    (  Object.Value,
                                       Prefix & "             ",
                                       Inline
                                    );
                                 when Uninitialized =>
                                    Put (Output, Prefix & "      : ");
                                    Do_Put
                                    (  Object.Object,
                                       Prefix & "        ",
                                       Inline
                                    );
                              end case;
                           end;
                        end if;
                     end;
                  end loop;
                  Put_Line (Output, Prefix & "begin");
                  Put (Output, Prefix & "   ");
                  Do_Put
                  (  This.Expression,
                     Prefix & "   ",
                     Inline
                  );
               end;
            when Declare_Item_Node =>
               null;
            when Term_Node =>
               Put (Output, Image (Argument.Value.all));
               Put_Line (Output, " at " & Image (Argument.Location));
         end case;
      end Do_Put;
   begin
      Do_Put (Tree, Prefix, Top);
   end Put;

   procedure Put
             (  Tree   : Tokens.Argument_Token;
                Name   : String;
                Prefix : String := ""
             )  is
      function Get_Name return String is
      begin
         for Index in reverse Name'Range loop
            if Name (Index) = '.' then
               return Name;
            end if;
         end loop;
         return Name & ".txt";
      end Get_Name;
      File   : constant String := Get_Name;
      Output : File_Type;
   begin
      Create (Output, Out_File, File);
      begin
         Put (Tree, Output, Prefix);
         Close (Output);
      exception
         when others =>
            Close (Output);
            raise;
      end;
   end Put;

end Parsers.Generic_Ada_Parser.Generic_Text_IO;
