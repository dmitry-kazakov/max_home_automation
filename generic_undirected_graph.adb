--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Undirected_Graph                    Luebeck            --
--  Implementation                                 Spring, 2026       --
--                                                                    --
--                                Last revision :  12:14 29 Mar 2026  --
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

with Ada.Numerics;  use Ada.Numerics;

with Ada.Unchecked_Conversion;

package body Generic_Undirected_Graph is
   use Node_Arrays;
   use Node_Address_Sets;
   use Node_Sets;

   Offset : Storage_Offset := -1;   -- Offset to the object's dope
   Blocks : Address := Null_Address;
--
-- Deref -- Node to its address conversion
--
   function Deref (Item : Node) return Address;

   type Nodes_Set (Size : Natural) is limited record
      Last : Natural;
      List : Nodes_Array (1..Size);
   end record;
   type Nodes_Set_Ptr is access Nodes_Set;
   for Nodes_Set_Ptr'Storage_Pool use Pool;
   procedure Free is
     new Ada.Unchecked_Deallocation (Nodes_Set, Nodes_Set_Ptr);
--
-- Add -- Node into a set
--
   procedure Add
             (  Set          : in out Address;
                Item         : Node;
                Minimal_Size : Positive
             );
   procedure Add
             (  Set          : in out Nodes_Set_Ptr;
                Item         : Node;
                Minimal_Size : Positive
             );
   pragma Inline (Add);
--
-- Find -- Binary search for a node
--
--    Vector - The nodes array
--    Size   - Of the array
--    Item   - To search for
--
-- Returns :
--
--    Node postion if found, negated would be position if not found
--
   function Find
            (  Vector : Nodes_Array;
               Size   : Positive;
               Item   : Node
            )  return Integer;
--
-- Find -- Location
--
   function Find (Set : Nodes_Set_Ptr; Item : Node) return Natural;
--
-- Is_In -- Membership test
--
   function Is_In (Set : Nodes_Set; Item : Node) return Boolean;
   function Is_In (Set : Nodes_Set_Ptr; Item : Node) return Boolean;
   pragma Inline (Is_In);
--
-- Remove -- Node from a set
--
   procedure Remove (Set : Nodes_Set_Ptr; Item : Node);
   procedure Remove (Set : Address; Item : Node);
   pragma Inline (Remove);

   type Node_Header is limited record
      Adjacent : Address;
   end record;
   type Node_Header_Ptr is access all Node_Header;

   Header_Size : constant Storage_Offset :=
                    Node_Header'Max_Size_In_Storage_Elements;

   function Header is
      new Ada.Unchecked_Conversion (Address, Node_Header_Ptr);

   function Nodes is
      new Ada.Unchecked_Conversion (Address, Nodes_Set_Ptr);

   procedure Add
             (  Set          : in out Nodes_Set_Ptr;
                Item         : Node;
                Minimal_Size : Positive
             )  is
   begin
      if Set = null then
         Set := new Nodes_Set (Minimal_Size);
         Set.Last := 1;
         Set.List (1) := Item;
      elsif Set.Last = 0 then
         Set.Last := 1;
         Set.List (1) := Item;
      else
         declare
            Location : Integer := Find (Set.List, Set.Last, Item);
         begin
            if Location < 0 then
               Location := -Location;
               if Set.Size = Set.Last then
                  declare
                     Ptr : constant Nodes_Set_Ptr :=
                              new Nodes_Set
                                  (  Set.Size
                                  +  Natural'Max
                                     (  Minimal_Size,
                                        (  (  Set.Size
                                           *  (100 + Increment)
                                           )
                                        /  100
                                  )  )  );
                  begin
                     Ptr.List (1..Location - 1) :=
                        Set.List (1..Location - 1);
                     Ptr.List (Location + 1..Set.Size + 1) :=
                        Set.List (Location..Set.Size);
                     Free (Set);
                     Set := Ptr;
                  end;
               else
                  Set.List (Location + 1..Set.Last + 1) :=
                     Set.List (Location..Set.Last);
               end if;
               Set.List (Location) := Item;
               Set.Last := Set.Last + 1;
            elsif Set.List (Location) /= Item then
               raise Argument_Error;
            end if;
         end;
      end if;
   end Add;

   procedure Add
             (  Set          : in out Address;
                Item         : Node;
                Minimal_Size : Positive
             )  is
      This : Nodes_Set_Ptr := Nodes (Set);
   begin
      Add (This, Item, Minimal_Size);
      Set := This.all'Address;
   end Add;

   procedure Allocate
             (  Pool            : in out Node_Storage_Pool;
                Storage_Address : out Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             )  is
      Header_Alignment : constant Storage_Count :=
         Storage_Count'Max (Node_Header'Alignment, Alignment);
      Header_Offset    : constant Storage_Offset :=
         Header_Size + (-Header_Size) mod Header_Alignment;
   begin
      Allocate
      (  Pool.Host.all,
         Storage_Address,
         Size + Header_Offset,
         Header_Alignment
      );
      declare
         This : Node_Header renames Header (Storage_Address).all;
      begin
         This.Adjacent := Null_Address;
         if Offset < 0 then
            --
            -- The  offset  to  the  object  address  according  to  the
            -- attribute X'Address is unknown. For this reason the block
            -- allocated is added to the list  of  allocated  blocks  to
            -- determine the offset later.
            --
            if Blocks = Null_Address then
               This.Adjacent := Storage_Address;
               Blocks        := Storage_Address;
            else
               declare
                  Head : Node_Header renames Header (Blocks).all;
               begin
                  This.Adjacent := Head.Adjacent;
                  Head.Adjacent := Storage_Address;
               end;
            end if;
         end if;
      end;
      Storage_Address := Storage_Address + Header_Offset;
   end Allocate;

   procedure Connect (Left, Right : Node) is
   begin
      if Left = null or Right = null then
         raise Constraint_Error;
      else
         Add (Header (Deref (Left)).Adjacent, Right, Minimal_Size);
         begin
            Add (Header (Deref (Right)).Adjacent, Left, Minimal_Size);
         exception
            when others =>
               Remove (Header (Deref (Left)).Adjacent, Right);
               raise;
         end;
      end if;
   end Connect;

   procedure Deallocate
             (  Pool            : in out Node_Storage_Pool;
                Storage_Address : in Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             )  is
      Header_Alignment : constant Storage_Count :=
         Storage_Count'Max (Node_Header'Alignment, Alignment);
      Header_Offset    : constant Storage_Offset :=
         Header_Size + (-Header_Size) mod Header_Alignment;
   begin
      if Offset < 0 then
         --
         -- The node is deallocated before placement of any  other nodes
         -- in any graphs. It is removed  from  the  list  of  allocated
         -- blocks.
         --
         if Blocks = Null_Address then
            raise Program_Error;
         end if;
         declare
            Freed : constant Address := Storage_Address - Header_Offset;
            This  : Node_Header renames Header (Freed).all;
         begin
            if This.Adjacent = Freed then
               Blocks := Null_Address;
            else
               Header (This.Adjacent).Adjacent := This.Adjacent;
            end if;
         end;
      else
         --
         -- Checking  for  dangling pointers. No deallocated item can be
         -- in any of the lists.
         --
         declare
            Ptr  : Nodes_Set_Ptr;
            This : Node_Header renames
                   Header (Storage_Address - Header_Offset).all;
         begin
            if This.Adjacent /= Null_Address then
               Ptr := Nodes (This.Adjacent);
               if Ptr.Last > 0 then
                  raise Program_Error;
               end if;
               Free (Ptr);
            end if;
         end;
      end if;
      Deallocate
      (  Pool.Host.all,
         Storage_Address - Header_Offset,
         Size + Header_Offset,
         Header_Alignment
      );
   end Deallocate;

   procedure Delete (Vertex : in out Node) is
      Visited : Node_Address_Sets.Set;
      Queued  : Unbounded_Array;
      Current : Node     := Vertex;
      Count   : Positive := 1;
   begin
      if Vertex = null then
         return;
      end if;
      loop
         Add (Visited, Current);
         declare
            This : Node_Header renames Header (Deref (Current)).all;
         begin
            if This.Adjacent /= Null_Address then
               declare
                  Adjacent  : Nodes_Set renames
                                 Nodes (This.Adjacent).all;
                  Neighbour : Node;
               begin
                  for Index in 1..Adjacent.Last loop
                     Neighbour := Adjacent.List (Index);
                     if not Is_In (Visited, Neighbour) then
                        Put (Queued, Count, Neighbour);
                        Count := Count + 1;
                     end if;
                  end loop;
                  Adjacent.Last := 0;
               end;
            end if;
         end;
         Free (Current);
         exit when Count = 1;
         Count   := Count - 1;
         Current := Get (Queued, Count);
      end loop;
      Vertex := null;
   end Delete;

   function Deref (Item : Node) return Address is
      function Node_To_Address is
         new Ada.Unchecked_Conversion (Node, Address);
   begin
      if Item = null then
         raise Constraint_Error;
      end if;
      if Offset < 0 then
         --
         -- Searching  for the memory block closest to the given address
         -- from the left. The offset between Item and the block address
         -- is the size of the dope plus the size of the header.
         --
         if Blocks = Null_Address then
            raise Program_Error;
         end if;
         Offset := Storage_Offset'Last;
         declare
            Addr    : constant Address := Node_To_Address (Item);
            Current : Address := Blocks;
         begin
            loop
               if Current < Addr then
                  Offset := Storage_Offset'Min (Offset, Addr - Current);
               end if;
               declare
                  This : Node_Header renames Header (Current).all;
               begin
                  Current := This.Adjacent;
                  This.Adjacent := Null_Address;
               end;
               exit when Current = Blocks;
            end loop;
         end;
         if Offset = Storage_Offset'Last then
            raise Program_Error;
         end if;
      end if;
      return Node_To_Address (Item) - Offset;
   end Deref;

   procedure Disconnect (Left, Right : Node) is
      This : Node_Header renames Header (Deref (Left)).all;
      That : Node_Header renames Header (Deref (Right)).all;
   begin
      Remove (This.Adjacent, Right);
      Remove (That.Adjacent, Left);
   end Disconnect;

   function Find
            (  Vector : Nodes_Array;
               Size   : Positive;
               Item   : Node
            )  return Integer is
      From : Natural := 0;
      To   : Natural := Size + 1;
      This : Natural;
   begin
      loop
         This := (From + To) / 2;
         if Item = Vector (This) then
            return This;
         elsif Item.all'Address < Vector (This).all'Address then
            if This - From <= 1 then
               return -This;
            end if;
            To := This;
         else
            if To - This <= 1 then
               return - This - 1;
            end if;
            From := This;
         end if;
      end loop;
   end Find;

   function Find (Set : Nodes_Set_Ptr; Item : Node) return Natural is
   begin
      if Set /= null and then Set.Last > 0 then
         declare
            Location : constant Integer :=
                       Find (Set.List, Set.Last, Item);
         begin
            if Location > 0 then
               return Location;
            end if;
         end;
      end if;
      return 0;
   end Find;

   function Find (Left, Right : Node) return Natural is
      This : Node_Header renames Header (Deref (Left)).all;
   begin
      if Right = null then
         raise Constraint_Error;
      elsif This.Adjacent = Null_Address then
         return 0;
      else
         return Find (Nodes (This.Adjacent), Right);
      end if;
   end Find;

   function Get_Adjacent (Vertex : Node; No : Positive) return Node is
      This : Node_Header renames Header (Deref (Vertex)).all;
   begin
      if This.Adjacent = Null_Address then
         raise Constraint_Error;
      else
         declare
            Adjacent : Nodes_Set renames Nodes (This.Adjacent).all;
         begin
            if No > Adjacent.Last then
               raise Constraint_Error;
            else
               return Adjacent.List (No);
            end if;
         end;
      end if;
   end Get_Adjacent;

   function Get_Adjacent (Vertex : Node) return Nodes_Array is
      This : Node_Header renames Header (Deref (Vertex)).all;
   begin
      if This.Adjacent = Null_Address then
         return (1..0 => null);
      else
         declare
            Adjacent : Nodes_Set renames Nodes (This.Adjacent).all;
         begin
            return Adjacent.List (1..Adjacent.Last);
         end;
      end if;
   end Get_Adjacent;

   function Get_Adjacent (Vertex : Node) return Node_Sets.Set is
      This   : Node_Header renames Header (Deref (Vertex)).all;
      Result : Node_Sets.Set;
   begin
      if This.Adjacent /= Null_Address then
         declare
            Adjacent : Nodes_Set renames Nodes (This.Adjacent).all;
         begin
            for Index in 1..Adjacent.Last loop
               Add (Result, Adjacent.List (Index));
            end loop;
         end;
      end if;
      return Result;
   end Get_Adjacent;

   function Get_Adjacent_Number (Vertex : Node) return Natural is
      This : Node_Header renames Header (Deref (Vertex)).all;
   begin
      if This.Adjacent = Null_Address then
         return 0;
      else
         return Nodes (This.Adjacent).Last;
      end if;
   end Get_Adjacent_Number;

   function Is_Connected (Vertex : Node) return Boolean is
      This : Node_Header renames Header (Deref (Vertex)).all;
   begin
      return
      (  This.Adjacent /= Null_Address
      and then
         Nodes (This.Adjacent).Last > 0
      );
   end Is_Connected;

   function Is_Connected (Left, Right : Node) return Boolean is
      Visited : Node_Address_Sets.Set;
      Queued  : Unbounded_Array;
      Current : Node     := Right;
      Count   : Positive := 1;
   begin
      if Left = null or else Right = null then
         raise Constraint_Error;
      end if;
      loop
         declare
            This : Node_Header renames Header (Deref (Current)).all;
         begin
            Add (Visited, Current);
            if This.Adjacent /= Null_Address then
               declare
                  Other    : Node;
                  Adjacent : Nodes_Set renames
                                Nodes (This.Adjacent).all;
               begin
                  if Is_In (Adjacent, Left) then
                     return True;
                  end if;
                  for Index in 1..Adjacent.Last loop
                     Other := Adjacent.List (Index);
                     if not Is_In (Visited, Other) then
                        Put (Queued, Count, Other);
                        Count := Count + 1;
                     end if;
                  end loop;
               end;
            end if;
         end;
         exit when Count = 1;
         Count   := Count - 1;
         Current := Get (Queued, Count);
      end loop;
      return False;
   end Is_Connected;

   function Is_In (Set : Nodes_Set_Ptr; Item : Node) return Boolean is
   begin
      return
      (  Set /= null
      and then
         Set.Last > 0
      and then
         Find (Set.List, Set.Last, Item) > 0
      );
   end Is_In;

   function Is_In (Set : Nodes_Set; Item : Node) return Boolean is
   begin
      return Set.Last > 0 and then Find (Set.List, Set.Last, Item) > 0;
   end Is_In;

   function Precedes (Left, Right : Node) return Boolean is
   begin
      return
      (  Right /= null
      and then
         (Left = null or else Less (Left, Right))
      );
   end Precedes;

   function Related (Left, Right : Node) return Boolean is
      This : Node_Header renames Header (Deref (Left)).all;
   begin
      if Right = null then
         raise Constraint_Error;
      else
         return Is_In (Nodes (This.Adjacent), Right);
      end if;
   end Related;

   procedure Remove (Set : Nodes_Set_Ptr; Item : Node) is
   begin
      if Set.Last > 0 then
         declare
            Location : constant Integer :=
                       Find (Set.List, Set.Last, Item);
         begin
            if Location > 0 then
               Set.List (Location..Set.Last - 1) :=
                  Set.List (Location + 1..Set.Last);
               Set.Last := Set.Last - 1;
            end if;
         end;
      end if;
   end Remove;

   procedure Remove (Set : Address; Item : Node) is
   begin
      Remove (Nodes (Set), Item);
   end Remove;

   procedure Remove (Vertex : Node) is
      This : Node_Header renames Header (Deref (Vertex)).all;
   begin
      if This.Adjacent /= Null_Address then
         declare
            Adjacent : Nodes_Set renames Nodes (This.Adjacent).all;
         begin
            for I in 1..Adjacent.Last loop
               for J in 1..I - 1 loop
                  Connect (Adjacent.List (I), Adjacent.List (J));
               end loop;
               Remove
               (  Header (Deref (Adjacent.List (I))).Adjacent,
                  Vertex
               );
            end loop;
            Adjacent.Last := 0;
         end;
      end if;
   end Remove;

   function Same (Left, Right : Node) return Boolean is
   begin
      return
      (  Right = Left
      or else
         (  Left /= null
         and then
            Right /= null
         and then
            Equal (Left, Right)
      )  );
   end Same;

   function Storage_Size (Pool : Node_Storage_Pool)
      return Storage_Count is
   begin
      return Storage_Size (Pool.Host.all);
   end Storage_Size;

   function "<" (Left, Right : Node) return Boolean is
   begin
      return
      (  Right /= null
      and then
         (Left = null or else Left.all'Address < Right.all'Address)
      );
   end "<";

end Generic_Undirected_Graph;
