--                                                                    --
--  package Generic_Lock_Free_FIFO  Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

with Interfaces.C;  use Interfaces.C;

package body Generic_Lock_Free_FIFO is

   type bool is new Boolean;
   pragma Convention (C, bool);

   function Load
             (  Ptr      : access Unsigned_64;
                Memorder : int := 0
             )  return Unsigned_64;
   pragma Import (Intrinsic, Load, "__atomic_load_8");

   function Exchange_And_Swap
            (  Ptr              : access Unsigned_64;
               Expected         : access Unsigned_64;
               Desired          : Unsigned_64;
               Weak             : bool := False;
               Success_Memorder : int  := 0;
               Failure_Memorder : int  := 0
            )  return bool;
   pragma Import
          (  Intrinsic,
             Exchange_And_Swap,
             "__atomic_compare_exchange_8"
          );

   procedure Store
             (  Ptr      : access Unsigned_64;
                Val      : Unsigned_64;
                Memorder : int := 0
             );
   pragma Import (Intrinsic, Store, "__atomic_store_8");

   procedure Delete
             (  Queue : in out FIFO;
                Count : Natural := 1
             )  is
   begin
      if Count > 0 then
         loop
            declare
               Free      : aliased Unsigned_64;
               Old_First : aliased Unsigned_64;
               New_First : aliased Unsigned_64;
            begin
               Old_First := Load (Queue.First'Access);
               Free      := Load (Queue.Free'Access);
               New_First := Old_First
                         +  Unsigned_64'Min
                            (  Free - Old_First,
                               Unsigned_64 (Count)
                            );
               exit when True = Exchange_And_Swap
                                (  Queue.First'Access,
                                   Old_First'Access,
                                   New_First
                                );
            end;
         end loop;
      end if;
   end Delete;

   function Free_Space (Queue : FIFO) return Natural is
      Free      : aliased Unsigned_64;
      Old_First : aliased Unsigned_64;
      New_First : aliased Unsigned_64;
   begin
      loop
         Old_First := Load (Queue.Self.First'Access);
         Free      := Load (Queue.Self.Free'Access);
         New_First := Load (Queue.Self.First'Access);
         if Old_First = New_First then
            return Queue.Size - Natural (New_First - Free);
         end if;
      end loop;
   end Free_Space;

   procedure Get
             (  Queue   : in out FIFO;
                Checker : Abstract_Checker'Class;
                Element : out Element_Type;
                No      : out Sequence_No;
                Success : out Boolean;
                Empty   : out Boolean
             )  is
      Free      : aliased Unsigned_64;
      Old_First : aliased Unsigned_64;
      New_First : aliased Unsigned_64;
      Result    : Element_Type;
   begin
      loop
         Old_First := Load (Queue.First'Access);
         Free      := Load (Queue.Free'Access);
         New_First := Load (Queue.First'Access);
         if Old_First = New_First then
            Empty := Old_First = Free;
            if Empty then
               Success := False;
               return;
            end if;
            Result := Queue.Buffer
                      (  Positive
                         (  Old_First mod Unsigned_64 (Queue.Size) + 1
                      )  );
            if not Check
                   (  Checker,
                      Result,
                      Sequence_No (Old_First)
                   )  then
               Success := False;
               return;
            end if;
            New_First := Old_First + 1;
            exit when True = Exchange_And_Swap
                             (  Queue.First'Access,
                                Old_First'Access,
                                New_First
                             );
         end if;
      end loop;
      No      := Sequence_No (Old_First);
      Element := Result;
      Success := True;
   end Get;

   procedure Get
             (  Queue   : in out FIFO;
                Element : out Element_Type;
                No      : out Sequence_No;
                Empty   : out Boolean
             )  is
      Free      : aliased Unsigned_64;
      Old_First : aliased Unsigned_64;
      New_First : aliased Unsigned_64;
      Result    : Element_Type;
   begin
      loop
         Old_First := Load (Queue.First'Access);
         Free      := Load (Queue.Free'Access);
         New_First := Load (Queue.First'Access);
         if Old_First = New_First then
            Empty := Old_First = Free;
            if Empty then
               return;
            end if;
            Result := Queue.Buffer
                      (  Positive
                         (  Old_First mod Unsigned_64 (Queue.Size) + 1
                      )  );
            New_First := Old_First + 1;
            exit when True = Exchange_And_Swap
                             (  Queue.First'Access,
                                Old_First'Access,
                                New_First
                             );
         end if;
      end loop;
      No      := Sequence_No (Old_First);
      Element := Result;
   end Get;

   procedure Get
             (  Queue   : in out FIFO;
                Element : out Element_Type;
                Empty   : out Boolean
             )  is
      No : Sequence_No;
   begin
      Get (Queue, Element, No, Empty);
   end Get;

   function Get (Queue : FIFO) return Element_Type is
      Element : Element_Type;
      Empty   : Boolean;
   begin
      Get (Queue.Self.all, Element, Empty);
      if Empty then
         raise Constraint_Error;
      else
         return Element;
      end if;
   end Get;

   function Is_Empty (Queue : FIFO) return Boolean is
      Free      : aliased Unsigned_64;
      Old_First : aliased Unsigned_64;
      New_First : aliased Unsigned_64;
   begin
      loop
         Old_First := Load (Queue.Self.First'Access);
         Free      := Load (Queue.Self.Free'Access);
         New_First := Load (Queue.Self.First'Access);
         if Old_First = New_First then
            return Old_First = Free;
         end if;
      end loop;
   end Is_Empty;

   function Is_Full (Queue : FIFO) return Boolean is
      Free      : aliased Unsigned_64;
      Old_First : aliased Unsigned_64;
      New_First : aliased Unsigned_64;
   begin
      loop
         Old_First := Load (Queue.Self.First'Access);
         Free      := Load (Queue.Self.Free'Access);
         New_First := Load (Queue.Self.First'Access);
         if Old_First = New_First then
            return Old_First + 1 - Free >= Unsigned_64 (Queue.Size);
         end if;
      end loop;
   end Is_Full;

   function Is_In (Queue : FIFO; No : Sequence_No) return Boolean is
      Free  : aliased Unsigned_64;
      First : aliased Unsigned_64;
   begin
      First := Load (Queue.Self.First'Access);
      Free  := Load (Queue.Self.Free'Access);
      return Unsigned_64 (No) in First..Free - 1;
   end Is_In;

   procedure Peek
             (  Queue   : FIFO;
                Element : out Element_Type;
                No      : out Sequence_No;
                Empty   : out Boolean
             )  is
      Free      : aliased Unsigned_64;
      Old_First : aliased Unsigned_64;
      New_First : aliased Unsigned_64;
      Result    : Element_Type;
   begin
      loop
         Old_First := Load (Queue.Self.First'Access);
         Free      := Load (Queue.Self.Free'Access);
         New_First := Load (Queue.Self.First'Access);
         if Old_First = New_First then
            Empty := Old_First = Free;
            if Empty then
               return;
            end if;
            Result := Queue.Buffer
                      (  Positive
                         (  Old_First mod Unsigned_64 (Queue.Size) + 1
                      )  );
            New_First := Load (Queue.Self.First'Access);
            exit when Old_First = New_First;
         end if;
      end loop;
      No := Sequence_No (Old_First);
      Element := Result;
   end Peek;

   procedure Peek
             (  Queue   : FIFO;
                Element : out Element_Type;
                Empty   : out Boolean
             )  is
      No : Sequence_No;
   begin
      Peek (Queue, Element, No, Empty);
   end Peek;

   function Peek (Queue : FIFO) return Element_Type is
      Empty   : Boolean;
      Element : Element_Type;
   begin
      Peek (Queue, Element, Empty);
      if Empty then
         raise Constraint_Error;
      else
         return Element;
      end if;
   end Peek;

   procedure Put
             (  Queue   : in out FIFO;
                Element : Element_Type;
                No      : out Sequence_No;
                Full    : out Boolean
             )  is
      First    : aliased Unsigned_64;
      Old_Free : aliased Unsigned_64;
      New_Free : aliased Unsigned_64;
   begin
      loop
         Old_Free := Load (Queue.Free'Access);
         First    := Load (Queue.First'Access);
         New_Free := Load (Queue.Free'Access);
         if Old_Free = New_Free then
            Full := First - Old_Free + 1 >= Unsigned_64 (Queue.Size);
            if Full then
               return;
            end if;
            New_Free := Old_Free + 1;
            if True = Exchange_And_Swap
                      (  Queue.New_Free'Access,
                         Old_Free'Access,
                         New_Free
                      )  then
               Queue.Buffer
               (  Positive (Old_Free mod Unsigned_64 (Queue.Size) + 1)
               )  := Element;
               No := Sequence_No (Old_Free);
               Store (Queue.Free'Access, New_Free);
               exit;
            end if;
         end if;
      end loop;
   end Put;

   procedure Put
             (  Queue   : in out FIFO;
                Element : Element_Type;
                Full    : out Boolean
             )  is
      No : Sequence_No;
   begin
      Put (Queue, Element, No, Full);
   end Put;

   procedure Put
             (  Queue   : in out FIFO;
                Element : Element_Type
             )  is
      Full : Boolean;
   begin
      Put (Queue, Element, Full);
      if Full then
         raise Constraint_Error;
      end if;
   end Put;

end Generic_Lock_Free_FIFO;
