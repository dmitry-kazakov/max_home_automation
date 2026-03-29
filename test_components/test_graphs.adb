--                                                                    --
--  procedure Test_Graphs           Copyright (c)  Dmitry A. Kazakov  --
--  Test program                                   Luebeck            --
--                                                 Winter, 2009       --
--                                                                    --
--                                Last revision :  12:14 29 Mar 2026  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of  the  GNU  Library  General  Public  --
--  License  as  published  by  the Free Software Foundation; either  --
--  version 2 of the License, or (at your option) any later version.  --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--                                                                    --
--  This  library is distributed in the hope that it will be useful,  --
--  but WITHOUT ANY WARRANTY; without even the implied  warranty  of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  Library General Public License for more details.                  --
--                                                                    --
--  You  should  have  received  a  copy  of the GNU Library General  --
--  Public  License  along  with  this library; if not, write to the  --
--  Free Software Foundation, Inc.,  675  Mass  Ave,  Cambridge,  MA  --
--  02139, USA.                                                       --
--____________________________________________________________________--

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.Text_IO;        use Ada.Text_IO;

with Test_String_Graph;
with Test_Suffix_Tree;

procedure Test_Graphs is
begin
   declare
      use Test_Suffix_Tree;
      use Character_Weighted_Graphs;
      Tree : Suffix_Tree := Build ("mississippi");
   begin
      Print (Tree);
      Delete (Tree);
   end;
   declare
      use Test_String_Graph.Directed;
      procedure Print (Vertex : Node; Prefix : String := "") is
      begin
         Put_Line (Prefix & Vertex.all);
         for Child in 1..Get_Children_Number (Vertex) loop
            Print (Get_Child (Vertex, Child), Prefix & "  ");
         end loop;
      end Print;
   begin
      declare
         A : Node := new String'("A");
      begin
         Free (A); -- Free an unconnected node
      end;
      declare
         A : Node := new String'("A");
      begin
         Connect (A, new String'("B"));
         Connect (Get_Child (A, 1), new String'("C"));
         Connect (A, new String'("D"));
         if not Is_Ancestor (A, Get_Child (A, 1)) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: in Is_Ancestor"
            );
         end if;
         if not Is_Descendant (Get_Child (A, 1), A) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: in Is_Descendant"
            );
         end if;
         Print (A);
         Put_Line ("----------");
         Delete (A, Any); -- Clean up
      end;
      declare
         A : Node := new String'("A");
         B : constant Node := new String'("B");
         C : Node := new String'("C");
         D : constant Node := new String'("D");
         E : Node := new String'("E");
      begin
         Connect (A, C);  -- A --> C --> D
         Connect (B, C);  -- B -->   --> E
         Connect (C, D);
         Connect (C, E);
         if not Is_Ancestor (A, E) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: in Is_Ancestor"
            );
         end if;
         if not Is_Sibling (D, E) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: in Is_Sibling"
            );
         end if;
         Print (A);
         Put_Line ("----------");
         Print (B);
         Put_Line ("----------");
         Remove (E);
         if Is_Ancestor (A, E) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: in Is_Ancestor"
            );
         end if;              -- A --> D
         Free (E);            -- B -->
         Print (A);
         Put_Line ("----------");
         Remove (C);
         Free (C);
         Print (A);
         Put_Line ("----------");
         Delete (A, Any);
      end;
   end;
   declare
      use Test_String_Graph;
      use Test_String_Graph.Undirected;
      use Test_String_Graph.Undirected.Node_Sets;

      procedure Print (X : Set; Prefix : String) is
      begin
         Put (Prefix);
         for Index in 1..Get_Size (X) loop
            if Index > 1 then
               Put (", ");
            end if;
            Put (Get (X, Index).all);
         end loop;
         New_Line;
      end Print;

      function Bron_Kerbosch (Nodes : Set) return Node_Set_Array is
         use Node_Set_Arrays;
         Cliques : Node_Set_Arrays.Unbounded_Array;
         Count   : Natural := 0;
         Size    : Natural := 2;
         P, R, X : Set;

         procedure Internal (R, P, X : in out Set; Prefix : String) is
            Pivot : Node;
            Max   : Integer := -1;
         begin
            if Is_Empty (P) and then Is_Empty (X) then
               declare
                  New_Size : constant Natural := Get_Size (R);
               begin
                  if New_Size >= Size then
                     if New_Size > Size then
                        Count := 1;
                     else
                        Count := Count + 1;
                     end if;
                     Put (Cliques, Count, R);
                     Size := New_Size;
                     return;
                  end if;
               end;
            end if;
            for Index in 1..Get_Size (P) loop
               if Get_Adjacent_Number (Get (P, Index)) > Max then
                  Pivot := Get (P, Index);
                  Max   := Get_Adjacent_Number (Pivot);
               end if;
            end loop;
            for Index in 1..Get_Size (X) loop
               if Get_Adjacent_Number (Get (X, Index)) > Max then
                  Pivot := Get (X, Index);
                  Max   := Get_Adjacent_Number (Pivot);
               end if;
            end loop;
            for Index in reverse 1..Get_Size (P) loop
               declare
                  V : constant Node := Get (P, Index);
               begin
                  if not Related (V, Pivot) then
                     declare
                        R1 : Set := R;
                        P1 : Set := P and Get_Adjacent (V);
                        X1 : Set := X and Get_Adjacent (V);
                     begin
                        Add (R1, V);
                        Internal (R1, P1, X1, Prefix & "   ");
                        Remove (P, V);
                        Add (X, V);
                     end;
                  end if;
               end;
            end loop;
         end Internal;
      begin
         P := Nodes;
         Internal (R, P, X, "");
         if Count > 0 then
            return Cliques.Vector (1..Count);
         else
            return (1..0 => Undirected.Node_Sets.Create);
         end if;
      end Bron_Kerbosch;

   begin
      declare
         G : Set;
      begin
         for I in 1..6 loop
            Add (G, new String'(" "));
         end loop;
         for I in 1..6 loop
            Get (G, I) (1) := Character'Val (I + Character'Pos ('0'));
         end loop;
         Connect (Get (G, 1), Get (G, 2));
         Connect (Get (G, 1), Get (G, 5));
         Connect (Get (G, 2), Get (G, 3));
         Connect (Get (G, 2), Get (G, 5));
         Connect (Get (G, 5), Get (G, 4));
         Connect (Get (G, 3), Get (G, 4));
         Connect (Get (G, 6), Get (G, 4));
         declare
            Cliques : constant Node_Set_Array := Bron_Kerbosch (G);
         begin
            Put_Line ("Maximal cliques:");
            for I in Cliques'Range loop
               for J in 1..Get_Size (Cliques (I)) loop
                  if J > 1 then
                     Put (", ");
                  end if;
                  Put (Get (Cliques (I), J).all);
               end loop;
               New_Line;
            end loop;
         end;
      end;
      declare
         N : Set;
         A : Node := new String'("A");
         B : Node := new String'("B");
         C : Node := new String'("C");
         D : Node := new String'("D");
         E : Node := new String'("E");
         F : Node := new String'("F");
      begin
         if Is_Connected (A) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Undirected graph error: in Is_Connected"
            );
         end if;
         Connect (A, B);
         Connect (B, A);
         Connect (A, C);
         Connect (B, C);
         Connect (D, E);
         Connect (D, F);
         Connect (F, E);
         if not Is_Connected (A) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Undirected graph error: in Is_Connected"
            );
         end if;
         if not Is_Connected (D, F) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Undirected graph error: in Is_Connected (D, F)"
            );
         end if;
         if Is_Connected (A, F) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Undirected graph error: in Is_Connected (A, F)"
            );
         end if;
         Add (N, A); Add (N, B); Add (N, C); Add (N, D);
         Add (N, E); Add (N, F);
         declare
            Cliques : constant Node_Set_Array := Bron_Kerbosch (N);
         begin
            Put_Line ("Maximal cliques:");
            for I in Cliques'Range loop
               for J in 1..Get_Size (Cliques (I)) loop
                  if J > 1 then
                     Put (", ");
                  end if;
                  Put (Get (Cliques (I), J).all);
               end loop;
               New_Line;
            end loop;
         end;
         Disconnect (A, B);
         if Related (A, B) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Undirected graph error: in Related (A, B)"
            );
         end if;
         Delete (A);
      end;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Graphs;
