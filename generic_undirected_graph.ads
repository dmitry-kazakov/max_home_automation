--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Undirected_Graph                    Luebeck            --
--  Interface                                      Spring, 2026       --
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
--
--  This generic package provides undirected graphs of nodes.  Nodes can
--  be of  any  type.  The type  of the nodes is  the  package's  formal
--  parameter Node_Type.
--
--  The nodes of a graph are never copied when inserted or removed  from
--  the  graph. All operations are referential.
--
--     Node_Type    - The node type
--     Pool         - The storage pool to use for the nodes
--     Minimal_Size - Minimal additionally allocated size
--     Increment    - By which the map is enlarged if necessary
--     Equal        - Equivalence of the nodes in a set
--     Less         - Order of the nodes in a set
--
with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;
with System.Storage_Pools;     use System.Storage_Pools;

with Generic_Set;
with Generic_Unbounded_Array;
with Ada.Unchecked_Deallocation;

generic
   type Node_Type (<>) is limited private;
   Pool          : in out Root_Storage_Pool'Class;
   Minimal_Size  : Positive := 16;
   Increment     : Natural  := 50;
   with function Equal (Left, Right : access Node_Type)
           return Boolean is <>;
   with function Less (Left, Right : access Node_Type)
           return Boolean is <>;
package Generic_Undirected_Graph is
--
-- Node_Storage_Pool -- The type of a proxy pool that keeps the nodes.
--
--    Host - The pool to take the memory from
--
   type Node_Storage_Pool (Host : access Root_Storage_Pool'Class) is
      new Root_Storage_Pool with null record;
--
-- Allocate -- Overrides System.Storage_Pools...
--
   procedure Allocate
             (  Pool            : in out Node_Storage_Pool;
                Storage_Address : out Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             );
--
-- Deallocate -- Overrides System.Storage_Pools...
--
   procedure Deallocate
             (  Pool            : in out Node_Storage_Pool;
                Storage_Address : in Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             );
--
-- Storage_Size -- Overrides System.Storage_Pools...
--
   function Storage_Size (Pool : Node_Storage_Pool)
      return Storage_Count;
--
-- Node_Pool -- The pool of the graph nodes
--
   Node_Pool : Node_Storage_Pool (Pool'Access);
------------------------------------------------------------------------
-- Node -- A  reference  to  a  node  of  a graph. The node points to an
--         instance of the type node type. Nodes are  allocated  in  the
-- pool Node_Pool. When a node is deallocated it  is  checked  that  the
-- node is not in any graph, otherwise Program_Error is propagated.
--
   type Node is access Node_Type;
   for Node'Storage_Pool use Node_Pool;
   for Node'Size use Integer_Address'Size;
--
-- Nodes_Array -- List of nodes
--
   type Nodes_Array is array (Positive range <>) of Node;
--
-- Connect -- Add a new arc in the graph
--
--    Left  - The node
--    Right - The node
--
-- This  procedure  creates an edge Left-Right.  When  the edge  already
-- exists, this operation is void.
--
-- Exceptions :
--
--    Argument_Error   - Invalid nodes equivalent, but distinct
--    Constraint_Error - Left or Right
--
   procedure Connect (Left, Right : Node);
--
-- Delete -- A subgraph rooted in a node
--
--    Vertex - The root node
--
-- This procedure removes a subgraph containing Vertex.
--
   procedure Delete (Vertex : in out Node);
--
-- Disconnect -- Remove arc from the graph
--
--    Left  - A node
--    Right - A node
--
-- The edge Left-Right if any is removed.
--
-- Exceptions :
--
--    Constraint_Error - Left or Right is null
--
   procedure Disconnect (Left, Right : Node);
--
-- Find -- Get the position of an adjacent vertex
--
--    Left  - The node
--    Right - The node
--
-- Returns :
--
--    The number of Right or else 0 if it is not adjacent to Left
--
-- Exceptions :
--
--    Constraint_Error - Parent or Child is null
--
   function Find (Left, Right : Node) return Natural;
--
-- Free -- A node
--
-- Exceptions :
--
--    Program_Error - Node  is  in  a graph (including the case when the
--                    node is a parent or a child of itself
--
   procedure Free is new Ada.Unchecked_Deallocation (Node_Type, Node);
--
-- Get_Adjacent -- Get adjacent nodes
--
--    Vertex - The node
--
-- Returns :
--
--    The array of adjacent nodes
--
-- Exceptions :
--
--    Constraint_Error - Vertex is null
--
   function Get_Adjacent (Vertex : Node) return Nodes_Array;
--
-- Get_Adjacent -- Get an immediate neighbour by its number
--
--    Vertex - The node
--    No     - The position of the child 1..Get_Adjacent_Number
--
-- Returns :
--
--    The node
--
-- Exceptions :
--
--    Constraint_Error - No such child or Parent is null
--
   function Get_Adjacent (Vertex : Node; No : Positive) return Node;
--
-- Get_Children_Number -- Get the number of immediate descendants
--
--    Vertex - The node
--
-- Returns :
--
--    The number of adjacent node
--
-- Exceptions :
--
--    Constraint_Error - Vertex is null
--
   function Get_Adjacent_Number (Vertex : Node) return Natural;
--
-- Is_Connected -- Check for a path in the graph
--
--    Vertex - A node
--
-- Returns :
--
--    True if there edges connecting the node Vertex
--
-- Exceptions :
--
--    Constraint_Error - Vertex is null
--
   function Is_Connected (Vertex : Node) return Boolean;
--
-- Is_Connected -- Check for a path in the graph
--
--    Left  - A node
--    Right - A node
--
-- Returns :
--
--    True if the is a path connecting Left and Right
--
-- Exceptions :
--
--    Constraint_Error - Left or Right is null
--
   function Is_Connected (Left, Right : Node) return Boolean;
--
-- Precedes -- Node objects order
--
--    Left, Right - Nodes to compare
--
-- Returns :
--
--    True if Left precedes Right
--
   function Precedes (Left, Right : Node) return Boolean;
--
-- Related -- Graph relation
--
--    Left  - A node
--    Right - A node
--
-- Returns :
--
--    True if there is an edge Left-Right
--
-- Exceptions :
--
--    Constraint_Error - Left or Right is null
--
   function Related (Left, Right : Node) return Boolean;
--
-- Remove -- A node from the graph
--
--    Vertex - The node to remove
--
-- This  procedure  removes  Vertex  from  the  graph. Each pair of arcs
-- leading from a parent of Vertex to a child of, is replaced by an  arc
-- from the parent to the child.
--
   procedure Remove (Vertex : Node);
--
-- Same -- Node objects equivalence
--
--    Left, Right - Nodes to compare
--
-- Returns :
--
--    True if Left precedes Right
--
   function Same (Left, Right : Node) return Boolean;
--
-- Node_Arrays -- Unbounded arrays of nodes (instantiation)
--
   package Node_Arrays is
      new Generic_Unbounded_Array
          (  Index_Type        => Positive,
             Object_Type       => Node,
             Object_Array_Type => Nodes_Array,
             Null_Element      => null
          );
--
-- Node_Sets -- Sets of nodes
--
   package Node_Sets is
      new Generic_Set
          (  Object_Type  => Node,
             Null_Element => null,
             "="          => Same,
             "<"          => Precedes
          );

   function Get_Adjacent (Vertex : Node) return Node_Sets.Set;

private
   pragma Inline (Precedes);
   pragma Inline (Same);

   function "<" (Left, Right : Node) return Boolean;
   pragma Inline ("<");

   package Node_Address_Sets is new Generic_Set (Node, null);

end Generic_Undirected_Graph;
