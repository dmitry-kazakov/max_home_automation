--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Generic_Dot                              Summer, 2025       --
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

with Ada.Text_IO.Text_Streams;

package body Parsers.Generic_Ada_Parser.Generic_Dot is
   use Sources;

   pragma Assert (Stream_Element'Size = Character'Size);

   LF   : constant Character := Character'Val (10);
   Edge : constant String    := " -- ";

   procedure Put
             (  Tree           : Tokens.Argument_Token;
                Output         : access Root_Stream_Type'Class;
                Show_Locations : Boolean := True
             )  is
      procedure Write (Text : String) is
         Data : Stream_Element_Array (1..Text'Length);
         pragma Import (Ada, Data);
         for Data'Address use Text'Address;
      begin
         Write (Output.all, Data);
      end Write;

      procedure Name (Text : String; Quote : Boolean := True) is
         Start : Integer;
         procedure Flush (Stop : Integer) is
         begin
            if Stop > Start then
               Write (Text (Start..Stop - 1));
            end if;
         end Flush;
      begin
         if Quote then
            Character'Write (Output, '"');
         end if;
         Start := Text'First;
         for Index in Text'Range loop
            case Text (Index) is
               when '"' =>
                  Flush (Index);
                  Write ("\""");
                  Start := Index + 1;
               when LF =>
                  Flush (Index);
                  Write ("\\n");
                  Start := Index + 1;
               when others =>
                  null;
            end case;
         end loop;
         Flush (Text'Last + 1);
         if Quote then
            Character'Write (Output, '"');
         end if;
      end Name;

      procedure Name (Where : Location_Type) is
      begin
         Name (Image (Where));
      end Name;

      procedure Label (Text : String) is
      begin
         Write (" [label=""");
         Name (Text, False);
         Write ("""];" & LF);
      end Label;

      procedure Label (Text : String; Where : Location_Type) is
      begin
         Write (" [label=""");
         Name (Text, False);
         if Show_Locations then
            Write ("\n" & Image (Where) & """];" & LF);
         else
            Write ("""];" & LF);
         end if;
      end Label;

      procedure Put (Node : Tokens.Argument_Token) is
      begin
         case Get_Class (Node.Value.all) is
            when Expression_Node =>
               declare
                  This : Expression'Class renames
                         Expression'Class (Node.Value.all);
               begin
                  for Index in This.Operands'Range loop
                     Name (This.Location);
                     Write (Edge);
                     Put (This.Operands (Index));
                  end loop;
                  Name (This.Location);
                  Label (Image (This.Operation), This.Location);
               end;
            when Case_Node =>
               declare
                  This : Case_Expression'Class renames
                         Case_Expression'Class (Node.Value.all);
               begin
                  Name (Node.Location);
                  Write (Edge);
                  Put (This.Selector);
                  for Index in This.Alternatives'Range loop
                     declare
                        Item  : Alternative_Pair renames
                                This.Alternatives (Index);
                        Where : constant Location_Type :=
                                         Item.Guard.Location &
                                         Item.Value.Location;
                     begin
                        Name (Node.Location);
                        Write (Edge);
                        Name (Where);
                        Write (Edge);
                        Put (Item.Guard);
                        Name (Where);
                        Write (Edge);
                        Put (Item.Value);
                        Name (Where);
                        Label ("=>");
                     end;
                  end loop;
                  if This.Has_Others then
                     declare
                        Where : constant String :=
                           Image (This.Others_Alternative.Location) &
                           ".others";
                     begin
                        Name (Where);
                        Write (" [label=""others =>""];" & LF);
                        Name (Node.Location);
                        Write (Edge);
                        Name (Where);
                        Write (Edge);
                        Put (This.Others_Alternative);
                        Name (Where);
                        Label ("others");
                     end;
                  end if;
                  Name (Node.Location);
                  Label ("case");
               end;
            when If_Node =>
               declare
                  This : If_Expression'Class renames
                         If_Expression'Class (Node.Value.all);
               begin
                  for Index in This.Alternatives'Range loop
                     declare
                        Item  : Alternative_Pair renames
                                This.Alternatives (Index);
                        Where : constant Location_Type :=
                                         Item.Guard.Location &
                                         Item.Value.Location;
                     begin
                        Name (Node.Location);
                        Write (Edge);
                        Name (Where);
                        Write (Edge);
                        Put (Item.Guard);
                        Name (Where);
                        Write (Edge);
                        Put (Item.Value);
                        Name (Where);
                        Label ("then");
                     end;
                  end loop;
                  if This.Has_Else then
                     declare
                        Where : constant String :=
                                Image (This.Else_Alternative.Location) &
                                ".else";
                     begin
                        Name (Node.Location);
                        Write (Edge);
                        Name (Where);
                        Write (Edge);
                        Put (This.Else_Alternative);
                        Name (Where);
                        Label ("else");
                     end;
                  end if;
                  Name (Node.Location);
                  Label ("if");
               end;
            when For_Node =>
               declare
                  This     : For_Expression'Class renames
                             For_Expression'Class (Node.Value.all);
                  For_Name : constant String :=
                                Image (This.Iterator.Location) & ".for";

                  function Operator return String is
                     function Suffix return String is
                     begin
                        case This.Qualifier is
                           when For_Any =>
                              return "";
                           when For_All =>
                              return " all";
                           when For_Some =>
                              return " some";
                        end case;
                     end Suffix;
                  begin
                     if 0 = (This.Options and For_Parallel) then
                        return "for" & Suffix;
                     else
                        return "parallel for" & Suffix;
                     end if;
                  end Operator;
               begin
                  if 0 /= (This.Options and For_Chunk) then
                     declare
                        Where : constant String :=
                           Image (This.Chunk.Location) & ".chunk";
                     begin
                        Name (For_Name);
                        Write (Edge);
                        Name (Where);
                        Write (Edge);
                        Put (This.Chunk);
                        Name (Where);
                        Label ("()");
                     end;
                  end if;
                  if This.Count > 0 then
                     declare
                        Where : constant String :=
                           Image (This.Iterator.Location) & ".with";
                     begin
                        for Index in This.Aspects'Range loop
                           declare
                              Item  : Alternative_Pair renames
                                      This.Aspects (Index);
                              Where : constant Location_Type :=
                                         Item.Guard.Location &
                                         Item.Value.Location;
                           begin
                              Name (For_Name);
                              Write (Edge);
                              Name (Where);
                              Write (Edge);
                              Put (Item.Guard);
                              Name (Where);
                              Write (Edge);
                              Put (Item.Value);
                           end;
                        end loop;
                        Name (Where);
                        Label ("=>");
                     end;
                  end if;
                  declare
                     Where : constant Location_Type :=
                                      This.Iterator.Location &
                                      This.Expression.Location;
                  begin
                     Name (For_Name);
                     Write (Edge);
                     Name (Where);
                     Write (Edge);
                     declare
                        In_At : constant String :=
                           Image (This.Identifier.Location) & ".in";
                     begin
                        Name (In_At);
                        Character'Write (Output, LF);
                        if 0 = (This.Options and For_Of) then
                           Name (In_At);
                           if 0 = (This.Options and For_Reverse) then
                              Label ("in");
                           else
                              Label ("in reverse");
                           end if;
                        else
                           Name (In_At);
                           if 0 = (This.Options and For_Reverse) then
                              Label ("of");
                           else
                              Label ("of reverse");
                           end if;
                        end if;
                        Name (In_At);
                        Write (Edge);
                        Put (This.Identifier);
                        if 0 = (This.Options and For_Range) then
                           Name (In_At);
                           Write (Edge);
                           Put (This.Iterator);
                        else
                           declare
                              Range_At : constant String :=
                                 Image (This.Range_Type.Location) &
                                 ".range";
                           begin
                              Name (In_At);
                              Write (Edge);
                              Name (Range_At);
                              Write (Edge);
                              Put (This.Range_Type);
                              Name (Range_At);
                              Write (Edge);
                              Put (This.Iterator);
                              Name (Range_At);
                              Label ("range");
                           end;
                        end if;
                     end;
                     if 0 /= (This.Options and For_Key) then
                        declare
                           Key_Name : constant String :=
                              Image (This.Iterator.Location) & ".key";
                        begin
                           Name (Where);
                           Write (Edge);
                           Name (Key_Name);
                           Write (Edge);
                           Put (This.Key);
                           Name (Key_Name);
                           Label ("use");
                        end;
                     end if;
                     if 0 /= (This.Options and For_Condition) then
                        declare
                           When_Name : constant String :=
                              Image (This.Iterator.Location) & ".when";
                        begin
                           Name (Where);
                           Write (Edge);
                           Name (When_Name);
                           Write (Edge);
                           Put (This.Condition);
                           Name (When_Name);
                           Label ("when");
                        end;
                     end if;
                     Name (Where);
                     Write (Edge);
                     Put (This.Expression);
                     Name (Where);
                     Label ("=>");
                  end;
                  Name (For_Name);
                  Label (Operator);
               end;
            when Raise_Node =>
               declare
                  This  : Raise_Expression'Class renames
                          Raise_Expression'Class (Node.Value.all);
                  Where : constant String :=
                             Image (This.Name.Location) & ".raise";
               begin
                  Name (Where);
                  Write (Edge);
                  Put (This.Name);
                  if This.Has_Message then
                     Name (Where);
                     Write (Edge);
                     Put (This.Message);
                  end if;
                  Name (Where);
                  Label ("raise");
               end;
            when Declare_Item_Node =>
               null;
            when Declare_Node =>
               declare
                  This  : Declare_Expression'Class renames
                          Declare_Expression'Class (Node.Value.all);
                  Block : constant String :=
                          Image (This.Expression.Location) & ".block";
                  Decl  : constant String :=
                          Image (This.Expression.Location) & ".declare";
                  procedure Put
                            (  Item    : Declare_Object_Item'Class;
                               Location : Sources.Location_Type
                            )  is
                     Colon : constant String :=
                                      Image (Location) & ".Colon";
                  begin
                     Name (Decl);
                     Write (Edge);
                     Name (Colon);
                     Write (Edge);
                     Put (Item.Name);
                     Name (Colon);
                     Write (Edge);
                     Put (Item.Object);
                     if Item.Kind_Of in Immutable..Initialized then
                        Name (Colon);
                        Write (Edge);
                        Put (Item.Value);
                     end if;
                     Name (Colon);
                     if Item.Kind_Of = Immutable then
                        Label (": constant");
                     else
                        Label (":");
                     end if;
                  end Put;

                  procedure Put
                            (  Item     : Declare_Renaming_Item'Class;
                               Location : Sources.Location_Type
                            )  is
                     Where : constant String :=
                             Image (Location) & ".renames";
                  begin
                     Name (Decl);
                     Write (Edge);
                     Name (Where);
                     Write (Edge);
                     Put (Item.Name);
                     Name (Where);
                     Write (Edge);
                     Put (Item.Object);
                     Name (Where);
                     Label ("renames");
                  end Put;
               begin
                  Name (Block);
                  Write (Edge);
                  Name (Decl);
                  for Index in This.Items'Range loop
                     declare
                        Item : Declare_Token renames This.Items (Index);
                     begin
                        if Item.Value.all in Declare_Object_Item'Class
                        then
                           Put
                           (  Declare_Object_Item'Class
                              (  Item.Value.all
                              ),
                              Item.Location
                           );
                        else
                           Put
                           (  Declare_Renaming_Item'Class
                              (  Item.Value.all
                              ),
                              Item.Location
                           );
                        end if;
                     end;
                  end loop;
                  Name (Block);
                  Write (Edge);
                  Put (This.Expression);
                  Name (Decl);
                  Label ("declaration");
                  Name (Block);
                  Label ("block");
               end;
            when Term_Node =>
               if Node.Value.all in String_Literal'Class then
                  declare
                     This : String_Literal'Class renames
                            String_Literal'Class (Node.Value.all);
                  begin
                     Name (Node.Location);
                     Character'Write (Output, LF);
                     Name (Node.Location);
                     Label ('"' & This.Value & '"', Node.Location);
                  end;
               else
                  Name (Node.Location);
                  Character'Write (Output, LF);
                  Name (Node.Location);
                  Label (Image (Node.Value.all), Node.Location);
               end if;
         end case;
      end Put;
   begin
      Write ("graph Expression" & " {" & LF);
      Put (Tree);
      Write ("}");
   end Put;

   procedure Put
             (  Tree           : Tokens.Argument_Token;
                Output         : File_Type;
                Show_Locations : Boolean := True
             )  is
   begin
      Put (Tree, Text_Streams.Stream (Output), Show_Locations);
   end Put;

   procedure Put
             (  Tree           : Tokens.Argument_Token;
                Output         : Stream_IO.File_Type;
                Show_Locations : Boolean := True
             )  is
   begin
      Put (Tree, Stream_IO.Stream (Output), Show_Locations);
   end Put;

   procedure Put
             (  Tree           : Tokens.Argument_Token;
                Name           : String;
                Show_Locations : Boolean := True
             )  is
      function Get_Name return String is
      begin
         for Index in reverse Name'Range loop
            if Name (Index) = '.' then
               return Name;
            end if;
         end loop;
         return Name & ".dot";
      end Get_Name;
      File   : constant String := Get_Name;
      Output : File_Type;
   begin
      Create (Output, Out_File, File);
      begin
         Put (Tree, Output, Show_Locations);
         Close (Output);
      exception
         when others =>
            Close (Output);
            raise;
      end;
   end Put;

end Parsers.Generic_Ada_Parser.Generic_Dot;
