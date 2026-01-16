--                                                                    --
--  package Generic_Lock_Free_FIFO  Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2025       --
--                                                                    --
--                                Last revision :  11:58 28 Mar 2025  --
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
--  FIFO is a simple first-in first-out queue. No locking is required by
--  any number of tasks.
--
with Ada.Finalization;
with Interfaces;

generic
   type Element_Type is private;
package Generic_Lock_Free_FIFO is
   type Sequence_No is new Interfaces.Unsigned_64;
--
-- FIFO -- The queue
--
--    Size - The maximal number of elements in the queue
--
   type FIFO (Size : Positive) is
      new Ada.Finalization.Limited_Controlled with private;
--
-- Delete -- A number of elements from the queue
--
--    Queue - The queue
--    Count - The number of elements to delete
--
-- The  procedure  removes  the specified number of elements from Queue.
-- When Count is greater than the current queue length, all elements are
-- removed.
--
   procedure Delete
             (  Queue : in out FIFO;
                Count : Natural := 1
             );
--
-- Free_Space -- Unused space in the queue
--
--    Queue - The queue
--
-- Returns :
--
--    The number of unused FIFO elements
--
   function Free_Space (Queue : FIFO) return Natural;
--
-- Get -- An element from the queue
--
--    Queue   - The queue
--    Element - From the queue
--  [ No ]    - The sequence number of the element
--    Empty   - Set to true if there is no element to get
--
   procedure Get
             (  Queue   : in out FIFO;
                Element : out Element_Type;
                Empty   : out Boolean
             );
   procedure Get
             (  Queue   : in out FIFO;
                Element : out Element_Type;
                No      : out Sequence_No;
                Empty   : out Boolean
             );
--
-- Get -- An element from the queue
--
--    Queue   - The queue
--    Checker - The element checker
--    Element - From the queue
--    No      - The sequence number of the element
--    Success - The element was extracted
--    Empty   - Set to true if there is no element to get
--
-- Since sequence Peek - Get is exposed to race condition this procedure
-- is provided to perform conditional element extraction.  The parameter
-- checker  is an object of the type derived from Abstract_Checker  with
-- a operation  Check used to valiadate element.
-- The parameters of Check are:
--
--    Checker - The element checker
--    Element - The element to check
--    No      - The element's sequence number
--
-- Returns:
--
--    True if the element can be extracted from Queue
--
-- The procedure sets  Success to true  if the element was extracted and
-- Element  and No  are set correspondingly.  When Success is False they
-- are  not changed.  Empty is set  to True if  Queue is  empty and then
-- Success is set to False.
--
   type Abstract_Checker is
      abstract new Ada.Finalization.Limited_Controlled with null record;
   function Check
            (  Checker : Abstract_Checker;
               Element : Element_Type;
               No      : Sequence_No
            )  return Boolean is abstract;
   procedure Get
             (  Queue   : in out FIFO;
                Checker : Abstract_Checker'Class;
                Element : out Element_Type;
                No      : out Sequence_No;
                Success : out Boolean;
                Empty   : out Boolean
             );
--
-- Get -- An element from the queue
--
--    Queue - The queue
--
-- Returns :
--
--    The element from the queue
--
-- Exceptions :
--
--    Constraint_Error - The queue is empty
--
   function Get (Queue : FIFO) return Element_Type;
--
-- Is_Empty -- Check if empty
--
--    Queue - The queue
--
-- Returns :
--
--    True if Queue is empty
--
   function Is_Empty (Queue : FIFO) return Boolean;
--
-- Is_Full -- Check if full
--
--    Queue - The queue
--
-- Returns :
--
--    True if Queue is full
--
   function Is_Full (Queue : FIFO) return Boolean;
--
-- Is_In -- Check if an element is in the FIFO
--
--    Queue - The queue
--    No    - The sequence number of the element
--
-- Returns :
--
--    True if element is in Queue
--
   function Is_In (Queue : FIFO; No : Sequence_No) return Boolean;
--
-- Peek -- An element from the queue leaving it there
--
--    Queue   - The queue
--    Element - From the queue
--  [ No ]    - The sequence number of the element
--    Empty   - Set to true if there is no element to get
--
   procedure Peek
             (  Queue   : FIFO;
                Element : out Element_Type;
                Empty   : out Boolean
             );
   procedure Peek
             (  Queue   : FIFO;
                Element : out Element_Type;
                No      : out Sequence_No;
                Empty   : out Boolean
             );
--
-- Peek -- An element from the queue leaving it there
--
--    Queue - The queue
--
-- Returns :
--
--    The element from the queue
--
-- Exceptions :
--
--    Constraint_Error - The queue is empty
--
   function Peek (Queue : FIFO) return Element_Type;
--
-- Put -- An element into the queue
--
--    Queue   - The queue
--    Element - To put into the queue
--  [ No ]    - Element identification (a unique sequence number)
--    Full    - Set to rue if element was not put
--
   procedure Put
             (  Queue   : in out FIFO;
                Element : Element_Type;
                Full    : out Boolean
             );
   procedure Put
             (  Queue   : in out FIFO;
                Element : Element_Type;
                No      : out Sequence_No;
                Full    : out Boolean
             );
--
-- Put -- An element into the queue
--
--    Queue   - The queue
--    Element - To put into the queue
--
-- Exceptions :
--
--    Constraint_Error - The queue is full
--
   procedure Put
             (  Queue   : in out FIFO;
                Element : Element_Type
             );
private
   pragma Inline (Delete);
   pragma Inline (Get);
   pragma Inline (Is_Empty);
   pragma Inline (Is_Full);
   pragma Inline (Peek);
   pragma Inline (Put);

   use Interfaces;

   type Element_Array is array (Positive range <>) of Element_Type;

   type FIFO_Ptr is access all FIFO'Class;
   type FIFO (Size : Positive) is
      new Ada.Finalization.Limited_Controlled with
   record
      Self     : FIFO_Ptr := FIFO'Unchecked_Access;
      Free     : aliased Unsigned_64 := 0;
      New_Free : aliased Unsigned_64 := 0;
      First    : aliased Unsigned_64 := 0;
      Buffer   : Element_Array (1..Size);
   end record;

end Generic_Lock_Free_FIFO;

