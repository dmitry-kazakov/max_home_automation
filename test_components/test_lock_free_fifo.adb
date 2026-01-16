--                                                                    --
--  procedure Test_Lock_Free_FIFO   Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2025       --
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

with Ada.Calendar;    use Ada.Calendar;
with Ada.Text_IO;     use Ada.Text_IO;
with Ada.Exceptions;  use Ada.Exceptions;
with Strings_Edit;    use Strings_Edit;

with Test_Lock_Free_FIFO_Instantiations;
use  Test_Lock_Free_FIFO_Instantiations;

procedure Test_Lock_Free_FIFO is
   Tries       : constant := 1000;
   Consumer_No : constant := 4;
   Producer_No : constant := 4;
   T1, T2 : Duration;

   procedure Report (T : Duration; Times: Positive; Text : String) is
      Line    : String (1..80);
      Pointer : Integer := 1;
   begin
      Pointer := 1;
      Put (Line, Pointer, Text);
      Put
      (  Destination => Line,
         Pointer     => Pointer,
         Value       => Duration'Image (T / Times),
         Field       => 15,
         Justify     => Right
      );
      Put
      (  Destination => Line,
         Pointer     => Pointer,
         Value       => Duration'Image (T),
         Field       => 15,
         Justify     => Right
      );
      Put_Line (Line (1..Pointer - 1));
   end Report;

   use Integer_Sets;

   type Integer_Array is array (Positive range <>) of Long_Integer;

   protected Producer_Count is
      procedure Reset;
      procedure Get (Value : out Positive);
   private
      Count : Natural := 0;
   end Producer_Count;

   protected body Producer_Count is
      procedure Get (Value : out Positive) is
      begin
         Count := Count + 1;
         Value := Count;
      end Get;

      procedure Reset is
      begin
         Count := 0;
      end Reset;
   end Producer_Count;

begin
   declare
      use FIFOs;

      Queue : FIFO (200);

      task type Producer;

      task type Consumer is
         entry Complete;
         entry Get_List
               (  List : in out Integer_Array;
                  Last : out Integer
              );
      end Consumer;

      task body Producer is
         Full  : Boolean;
         Count : Natural;
      begin
         Producer_Count.Get (Count);
         Count := (Count - 1) * Tries;
         for Index in Long_Integer range 1..Tries loop
            loop
               Put (Queue, Index + Long_Integer (Count), Full);
               exit when not Full;
            end loop;
         end loop;
      end Producer;

      task body Consumer is
         Items : Integer_Array (1..Tries);
         Count : Integer := 0;
         Value : Long_Integer;
         Empty : Boolean;
      begin
         for Index in 1..Tries loop
            loop
               Get (Queue, Value, Empty);
               exit when not Empty;
            end loop;
            Count := Count + 1;
            Items (Count) := Value;
         end loop;
         accept Complete;
         select
            accept Get_List
                   (  List : in out Integer_Array;
                      Last : out Integer
                   )  do
               if List'Length >= Count then
                  List (List'First..List'First + Count - 1) :=
                     Items (1..Count);
                  Last := List'First + Count - 1;
               else
                  List := Items (1..List'Length - 1);
                  Last := List'Last;
               end if;
            end Get_List;
         or terminate;
         end select;
      end Consumer;

      Start     : constant Time := Clock;
      Consumers : array (1..Consumer_No) of Consumer;
      Producers : array (1..Producer_No) of Producer;

      Last : Integer;
      List : Integer_Array (1..Tries + 10);
      Set  : Integer_Sets.Set;
   begin
      for Index in Consumers'Range loop
         Consumers (Index).Complete;
      end loop;
      T1 := Clock - Start;
      for Index in Consumers'Range loop
         Consumers (Index).Get_List (List, Last);
         for Item in 1..Last loop
            if Is_In (Set, List (Item)) then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Already in the set"
                  &  Long_Integer'Image (List (Item))
               )  );
            else
               Add (Set, List (Item));
            end if;
         end loop;
      end loop;
      if Get_Size (Set) < Tries * Producer_No then
         Raise_Exception (Data_Error'Identity, "Data loss");
      elsif Get_Size (Set) > Tries * Producer_No then
         Raise_Exception (Data_Error'Identity, "Data overrun");
      end if;
   end;

   Report (T1, Tries * Producer_No, "Lock-free:");
   Producer_Count.Reset;
------------------------------------------------------------------------
   declare

      protected type FIFO (Size : Positive) is
         entry Get (Element : out Long_Integer);
         entry Put (Element : Long_Integer);
      private
         Full  : Boolean := False;
         Empty : Boolean := True;
         Queue : FIFOs.FIFO (Size);
      end FIFO;

      protected body FIFO is
         entry Get (Element : out Long_Integer) when not Empty is
         begin
            Element := FIFOs.Get (Queue);
            Empty   := FIFOs.Is_Empty (Queue);
            Full    := False;
         end Get;

         entry Put (Element : Long_Integer) when not Full is
         begin
            FIFOs.Put (Queue, Element);
            Full  := FIFOs.Is_Full (Queue);
            Empty := False;
         end Put;
      end FIFO;

      Queue : FIFO (200);

      task type Producer;

      task type Consumer is
         entry Complete;
         entry Get_List
               (  List : in out Integer_Array;
                  Last : out Integer
              );
      end Consumer;

      task body Producer is
         Count : Natural;
      begin
         Producer_Count.Get (Count);
         Count := (Count - 1) * Tries;
         for Index in Long_Integer range 1..Tries loop
            Queue.Put (Index + Long_Integer (Count));
         end loop;
      end Producer;

      task body Consumer is
         Items : Integer_Array (1..Tries);
         Count : Integer := 0;
         Value : Long_Integer;
      begin
         for Index in 1..Tries loop
            Queue.Get (Value);
            Count := Count + 1;
            Items (Count) := Value;
         end loop;
         accept Complete;
         select
            accept Get_List
                   (  List : in out Integer_Array;
                      Last : out Integer
                   )  do
               if List'Length >= Count then
                  List (List'First..List'First + Count - 1) :=
                     Items (1..Count);
                  Last := List'First + Count - 1;
               else
                  List := Items (1..List'Length - 1);
                  Last := List'Last;
               end if;
            end Get_List;
         or terminate;
         end select;
      end Consumer;

      Start     : constant Time := Clock;
      Consumers : array (1..Consumer_No) of Consumer;
      Producers : array (1..Producer_No) of Producer;

      Last : Integer;
      List : Integer_Array (1..Tries + 10);
      Set  : Integer_Sets.Set;
   begin
      for Index in Consumers'Range loop
         Consumers (Index).Complete;
      end loop;
      T2 := Clock - Start;
      for Index in Consumers'Range loop
         Consumers (Index).Get_List (List, Last);
         for Item in 1..Last loop
            if Is_In (Set, List (Item)) then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Already in the set"
                  &  Long_Integer'Image (List (Item))
               )  );
            else
               Add (Set, List (Item));
            end if;
         end loop;
      end loop;
      if Get_Size (Set) < Tries * Producer_No then
         Raise_Exception (Data_Error'Identity, "Data loss");
      elsif Get_Size (Set) > Tries * Producer_No then
         Raise_Exception (Data_Error'Identity, "Data overrun");
      end if;
   end;
   Report (T2, Tries * Producer_No, "Blocking :");
exception
   when Error : others =>
      Put_Line ("Error:" & Exception_Information (Error));
end Test_Lock_Free_FIFO;
