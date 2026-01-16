--                                                                    --
--  package Unbounded_Unsigneds     Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2024       --
--                                                                    --
--                                Last revision :  17:48 17 Jun 2025  --
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

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Unbounded_Unsigneds is

   --  S : Unbounded_String;
   --
   --  function "+" (Left : Unbounded_String; Right : String)
   --     return String is
   --  begin
   --     return Ada.Strings.Unbounded.To_String (Left) & Right;
   --  end "+";
   --
   --  procedure Inc is
   --  begin
   --     Append (S,"   ");
   --  end Inc;
   --
   --  procedure Dec is
   --     R : constant String := Ada.Strings.Unbounded.To_String (S);
   --  begin
   --     S := To_Unbounded_String (R (R'First + 3..R'Last));
   --  end Dec;

   Log2_Table : constant array (Half_Word range 0..255) of Natural :=
                   (  0 ..  0 => 0,
                      1 ..  1 => 1,
                      2 ..  3 => 2,
                      4 ..  7 => 3,
                      8 .. 15 => 4,
                     16 .. 31 => 5,
                     32 .. 63 => 6,
                     64 ..127 => 7,
                    128 ..255 => 8
                   );

   --  procedure Check (Value : Unbounded_Unsigned) is
   --  begin
   --     if Value.Length /= 0 and then Value.Value.Data (Value.Length) = 0
   --     then
   --        raise Constraint_Error;
   --     end if;
   --  end Check;

   procedure Deallocate is
      new Ada.Unchecked_Deallocation (Vector, Vector_Ptr);

   procedure Default_Deallocate (Ptr : in out Vector_Ptr) is
   begin
      Deallocate (Ptr);
   end Default_Deallocate;

   procedure Wiping_Deallocate (Ptr : in out Vector_Ptr) is
   begin
      if Ptr /= null then
         declare
            Data : Half_Word_Array renames Ptr.Data;
         begin
            for Index in Data'Range loop
               Data (Index) := 0;
            end loop;
         end;
         Deallocate (Ptr);
      end if;
   end Wiping_Deallocate;

   type Deallocator_Ptr is access procedure (Ptr : in out Vector_Ptr);
   Free : Deallocator_Ptr := Default_Deallocate'Access;
   pragma Atomic (Free);

   function Compute_Hex_Width return Positive is
      Data  : Half_Word := Half_Word'Last;
      Width : Natural   := 0;
   begin
      loop
         Width := Width + 1;
         Data  := Data / 16;
         exit when Data = 0;
      end loop;
      return Width;
   end Compute_Hex_Width;

   Hex_Width : constant Positive := Compute_Hex_Width;
   Figure    : constant String   := "0123456789ABCDEF";

   function Image (Value : Word) return String is
      Result : String (1..Hex_Width * 2);
      Item   : Word := Value;
   begin
      for Index in reverse Result'Range loop
         Result (Index) := Figure (Integer (Item mod 16) + 1);
         Item := Item / 16;
         if Item = 0 then
            return Result (Index..Result'Last);
         end if;
      end loop;
      return Result;
   end Image;

   function Image (Value : Half_Word) return String is
      Result : String (1..Hex_Width);
      Item   : Half_Word := Value;
   begin
      for Index in reverse Result'Range loop
         Result (Index) := Figure (Integer (Item mod 16) + 1);
         Item := Item / 16;
      end loop;
      return Result;
   end Image;

   procedure Dump_Hex
             (  X      : Half_Word_Array;
                Prefix : String  := "";
                Join   : Boolean := False
             )  is
      use Ada.Text_IO;
      Prompt : String := Prefix;
   begin
      if X'Length = 0 then
         Put_Line (Prefix & "0");
      else
         if Join then
            declare
               Item : Digit_Offset := X'Last;
            begin
               if X'Length mod 2 = 0 then
                  for Index in 1..X'Length / 2 loop
                     Put (Prompt);
                     Prompt := (Prompt'Range => ' ');
                     Put (Image (X (Item)));
                     Item := Item - 1;
                     Put (Image (X (Item)));
                     Item := Item - 1;
                     New_Line;
                  end loop;
               else
                  Put (Prompt);
                  Prompt := (Prompt'Range => ' ');
                  Put (Image (Half_Word'(0)));
                  for Index in 1..X'Length / 2 loop
                     Put (Image (X (Item)));
                     New_Line;
                     Item := Item - 1;
                     Put (Image (X (Item)));
                     Item := Item - 1;
                  end loop;
                  Put (Image (X (Item)));
                  New_Line;
               end if;
            end;
         else
            for Index in reverse X'Range loop
               Put (Prompt);
               if Index = X'Last then
                  Prompt := (Prompt'Range => ' ');
               end if;
               Put (Image (X (Index)));
               New_Line;
            end loop;
         end if;
      end if;
   end Dump_Hex;

   procedure Dump_Hex
             (  X      : Unbounded_Unsigned;
                Prefix : String  := "";
                Join   : Boolean := False
             )  is
      use Ada.Text_IO;
   begin
      if X.Length = 0 then
         Put_Line (Prefix & "0");
      else
         Dump_Hex (X.Value.Data (1..X.Length), Prefix, Join);
      end if;
   end Dump_Hex;

   procedure Dump_Hex (X : Signed; Prefix : String := "") is
   begin
      if X.Sign then
         Dump_Hex (X.Value, Prefix & "-");
      else
         Dump_Hex (X.Value, Prefix & "+");
      end if;
   end Dump_Hex;

   procedure Dump (X : Unbounded_Unsigned; Prefix : String := "") is
      use Ada.Text_IO;
      Prompt : String := Prefix;
   begin
      if Is_Zero (X) then
         Put_Line (Prefix & "0");
      else
         Put (Prefix);
         for Index in 1..Get_Length (X) loop
            Put (Prompt);
            if Index = 1 then
               Prompt := (Prompt'Range => ' ');
            end if;
            Put (Half_Word'Image (Get_Digit (X, Index)));
            New_Line;
         end loop;
      end if;
   end Dump;

   procedure Adjust (Object : in out Unbounded_Unsigned) is
   begin
      if Object.Length = 0 then
         Object.Value := null;
      else
         declare
            Dummy : Count_Type;
         begin
            Dummy := Inc (Object.Value.Count'Access);
         end;
      end if;
   end Adjust;

   procedure Add
             (  Accumulator : in out Half_Word_Array;
                Length      : in out Digit_Offset;
                Increment   : Half_Word_Array;
                Carry       : out Half_Word
             )  is
      Sum : Word := 0;
      To  : Digit_Count := Accumulator'First;
   begin
      for Index in Increment'Range loop
         Sum := Sum + Word (Accumulator (To)) + Word (Increment (Index));
         Accumulator (To) := Half_Word (Sum and Half_Word_Mask);
         Sum := Shift_Right (Sum, Bit_Width);
         To := To + 1;
      end loop;
      if Sum = 0 then
         Carry := 0;
         return;
      end if;
      for Index in To..Accumulator'First + Length - 1 loop
         Sum := Sum + Word (Accumulator (Index));
         Accumulator (Index) := Half_Word (Sum and Half_Word_Mask);
         Sum := Shift_Right (Sum, Bit_Width);
         if Sum = 0 then
            Carry := 0;
            return;
         end if;
      end loop;
      if Length < Accumulator'Length then
         Length := Length + 1;
         Accumulator (Length) := Half_Word (Sum);
         Carry := 0;
      else
         Carry := Half_Word (Sum);
      end if;
   end Add;

   procedure Add
             (  Accumulator : in out Unbounded_Unsigned;
                Increment   : Half_Word
             )  is
      Length : Digit_Offset renames Accumulator.Length;
   begin
      if Increment = 0 then
         return;
      elsif Length = 0 then
         Length := 1;
         Allocate (Accumulator.Value, 1);
         Accumulator.Value.Data (1) := Increment;
         return;
      end if;
      Clone (Accumulator.Value, Length);
      declare
         Value : Half_Word_Array renames Accumulator.Value.Data;
         Sum   : Word;
      begin
         Sum := Word (Value (1)) + Word (Increment);
         Value (1) := Half_Word (Sum and Half_Word_Mask);
         Sum := Shift_Right (Sum, Bit_Width);
         if Sum = 0 then
            return;
         end if;
         for Index in 2..Length loop
            Sum := Sum + Word (Value (Index));
            Value (Index) := Half_Word (Sum and Half_Word_Mask);
            Sum := Shift_Right (Sum, Bit_Width);
            if Sum = 0 then
               return;
            end if;
         end loop;
         Length := Length + 1;
         Put (Accumulator.Value, Length, Half_Word (Sum));
      end;
   end Add;

   procedure Add
             (  Accumulator : in out Unbounded_Unsigned;
                Increment   : Half_Word_Array
             )  is
      Carry : Half_Word;
   begin
      if Increment'Length = 0 then
         return;
      elsif Accumulator.Length = 0 then
         Accumulator.Length := Increment'Length;
         Allocate (Accumulator.Value, Increment'Length);
         Accumulator.Value.Data (1..Increment'Length) := Increment;
         return;
      end if;
      Clone (Accumulator.Value, Accumulator.Length);
      if Accumulator.Length < Increment'Length then
         declare
            Old_Length : constant Digit_Offset := Accumulator.Length;
         begin
            for Index in Old_Length + 1..Increment'Length loop
               Put
               (  Accumulator.Value,
                  Index,
                  Increment (Increment'First + Index - 1)
               );
            end loop;
            Accumulator.Length := Increment'Length;
            Add
            (  Accumulator.Value.Data,
               Accumulator.Length,
               Increment
               (  Increment'First
               .. Increment'First + Old_Length - 1
               ),
               Carry
            );
         end;
      else
         Add
         (  Accumulator.Value.Data,
            Accumulator.Length,
            Increment,
            Carry
         );
      end if;
      if Carry /= 0 then
         Accumulator.Length := Accumulator.Length + 1;
         Put (Accumulator.Value, Accumulator.Length, Carry);
      end if;
   end Add;

   procedure Add
             (  Accumulator : in out Unbounded_Unsigned;
                Increment   : Unbounded_Unsigned
             )  is
   begin
      if Increment.Length > 0 then
         if Accumulator.Length = 0 then
            Accumulator := Increment;
         elsif Accumulator.Value = Increment.Value then
            Mul_By_Power_Of_Two (Accumulator, 1);
--          Mul (Accumulator, 2);
         else
            Add
            (  Accumulator,
               Increment.Value.Data (1..Increment.Length)
            );
         end if;
      end if;
   end Add;

   procedure Add
             (  Accumulator : in out Unbounded_Unsigned;
                Increment   : Unbounded_Unsigned;
                Shift       : Digit_Offset
             )  is
      Length : constant Digit_Offset := Increment.Length + Shift;
   begin
      if Shift = 0 then
         Add (Accumulator, Increment);
         return;
      elsif Increment.Length = 0 then
         return;
      elsif Accumulator.Length = 0 then
         Allocate (Accumulator.Value, Length);
         declare
            Data : Half_Word_Array renames Accumulator.Value.Data;
         begin
            Data (Shift + 1..Length) :=
               Increment.Value.Data (1..Increment.Length);
            for Index in 1..Shift loop
               Data (Index) := 0;
            end loop;
         end;
         Accumulator.Length := Length;
         return;
      end if;
      Clone (Accumulator.Value, Accumulator.Length);
      if Accumulator.Length < Length then
         Put (Accumulator.Value, Length, 0);
         declare
            Data : Half_Word_Array renames Accumulator.Value.Data;
         begin
            for Index in Accumulator.Length + 1..Length - 1 loop
               Data (Index) := 0;
            end loop;
         end;
      end if;
      declare
         Sum     : Word := 0;
         Summand : Half_Word_Array renames Accumulator.Value.Data;
         Addend  : Half_Word_Array renames
                      Increment.Value.Data (1..Increment.Length);
         To      : Digit_Count := Summand'First + Shift;
      begin
         for Index in Addend'Range loop
            Sum := Sum + Word (Summand (To)) + Word (Addend (Index));
            Summand (To) := Half_Word (Sum and Half_Word_Mask);
            Sum := Shift_Right (Sum, Bit_Width);
            To  := To + 1;
         end loop;
         while Sum /= 0 and then To <= Accumulator.Length loop
            Sum := Sum + Word (Summand (To));
            Summand (To) := Half_Word (Sum and Half_Word_Mask);
            Sum := Shift_Right (Sum, Bit_Width);
            To  := To + 1;
         end loop;
         if Sum /= 0 then
            Put (Accumulator.Value, To, Half_Word (Sum));
            To := To + 1;
         end if;
         Accumulator.Length := Digit_Count'Max (Accumulator.Length, To - 1);
      end;
   end Add;

   procedure Add
             (  Accumulator : in out Unbounded_Unsigned;
                Increment   : Unbounded_Unsigned;
                Multiplier  : Half_Word;
                Shift       : Digit_Offset
             )  is
   begin
      if Increment.Length = 0 or else Multiplier = 0 then
         return;
      elsif Multiplier = 1 then
         Add (Accumulator, Increment, Shift);
         return;
      end if;
      Clone (Accumulator.Value, Accumulator.Length);
      declare
         Sum    : Word := 0;
         Factor : constant Word := Word (Multiplier);
         Addend : Half_Word_Array renames Increment.Value.Data;
         To     : Digit_Count := Shift + 1;
      begin
         for Index in Accumulator.Length + 1..To - 1 loop
            Put (Accumulator.Value, Index, 0);
         end loop;
         for Index in 1..Increment.Length loop
            Sum := Sum + Word (Addend (Index)) * Factor;
            if To <= Accumulator.Length then
               Sum := Sum + Word (Accumulator.Value.Data (To));
            end if;
            Put
            (  Accumulator.Value,
               To,
               Half_Word (Sum and Half_Word_Mask)
            );
            Sum := Shift_Right (Sum, Bit_Width);
            To  := To + 1;
         end loop;
         while Sum /= 0 and then To <= Accumulator.Length loop
            Sum := Sum + Word (Accumulator.Value.Data (To));
            Put
            (  Accumulator.Value,
               To,
               Half_Word (Sum and Half_Word_Mask)
            );
            Sum := Shift_Right (Sum, Bit_Width);
            To  := To + 1;
         end loop;
         Accumulator.Length :=
            Digit_Offset'Max (Accumulator.Length, To - 1);
         if Sum /= 0 then
            Accumulator.Length := Accumulator.Length + 1;
            Put
            (  Accumulator.Value,
               Accumulator.Length,
               Half_Word (Sum)
            );
         end if;
      end;
   end Add;

   procedure Allocate
             (  Container : in out Vector_Ptr;
                Size      : Digit_Count
             )  is
   begin
      if Container = null then
         Container :=
            new Vector (Digit_Count'Max (Size, Minimal_Size));
      elsif Size > Container.Size     or else
            Load (Container.Count'Access) > 1 then
         declare
            Ptr : Vector_Ptr;
         begin
            Ptr := new Vector (Size);
            Release (Container);
            Container := Ptr;
         exception
            when others =>
               Free (Ptr);
               raise;
         end;
      end if;
   end Allocate;

   procedure Clear_Bit
             (  Left     : in out Unbounded_Unsigned;
                Position : Bit_Position
             )  is
      Power     : constant Bit_Count := Position - 1;
      Bit_Shift : constant Natural := Natural (Power mod Bit_Width);
      Mask      : constant Half_Word := 2 ** Bit_Shift;
      Index     : constant Digit_Offset :=
                           Digit_Offset (Power / Bit_Width + 1);
  begin
      if Index <= Left.Length then
         declare
            Length : Digit_Offset renames Left.Length;
         begin
            if (  (  Shift_Right (Left.Value.Data (Index), Bit_Shift)
                  and
                     1
                  )
               =  1
               )  then
               Clone (Left.Value, Length);
               declare
                  Data : Half_Word_Array renames Left.Value.Data;
               begin
                  Data (Index) := Data (Index) - Mask;
                  if Length = Index then
                     while Length > 0 and then Data (Length) = 0 loop
                        Length := Length - 1;
                     end loop;
                  end if;
               end;
            end if;
         end;
      end if;
   end Clear_Bit;

   procedure Clear_Slice
             (  Left : in out Unbounded_Unsigned;
                From : Bit_Position;
                To   : Bit_Position
             )  is
   begin
      if From > To then
         return;
      end if;
      declare
         MSB : constant Bit_Count := Get_MSB (Left);
      begin
         if From > MSB then
            return;
         elsif From = To then
            Clear_Bit (Left, From);
            return;
         elsif To >= MSB then
            if From = 1 then
               Erase (Left);
            else
               Get_Slice (Left, 1, From - 1);
            end if;
            return;
         end if;
         Clone (Left.Value, Left.Length);
         declare
            First     : constant Digit_Count :=
                           Digit_Count ((From - 1) / Bit_Width + 1);
            Last      : constant Digit_Count :=
                           Digit_Count ((To - 1) / Bit_Width + 1);
            From_At   : constant Natural :=
                           Natural (From - 1) mod Bit_Width;
            To_At     : constant Natural :=
                           Natural (To   - 1) mod Bit_Width;
            From_Mask : constant Half_Word := 2 ** From_At - 1;
            To_Mask   : constant Half_Word :=
                           not (2 ** (To_At + 1) - 1);
            Length    : Digit_Offset renames Left.Length;
            Data      : Half_Word_Array renames
                           Left.Value.Data (1..Length);
         begin
            if First = Last then
               Data (First) := Data (First) and (From_Mask or To_Mask);
            elsif From_At = 0 then
               if To_At = Bit_Width - 1 then
                  for Index in First..Last loop
                     Data (Index) := 0;
                  end loop;
               else
                  for Index in First..Last - 1 loop
                     Data (Index) := 0;
                  end loop;
                  Data (Last) := Data (Last) and To_Mask;
               end if;
            else
               Data (First) := Data (First) and From_Mask;
               for Index in First + 1..Last - 1 loop
                  Data (Index) := 0;
               end loop;
               Data (Last) := Data (Last) and To_Mask;
            end if;
            while Length > 0 and then Data (Length) = 0 loop
               Length := Length - 1;
            end loop;
         end;
      end;
   end Clear_Slice;

   procedure Clone
             (  Container : in out Vector_Ptr;
                Length    : Digit_Count
             )  is
   begin
      if Load (Container.Count'Access) > 1 then
         declare
            Dummy : Count_Type;
         begin
            Dummy := Inc (Container.Count'Access);
         end;
         if Length > Container.Size then
            declare
               Data : Half_Word_Array renames Container.Data;
            begin
               Container := new Vector (Length);
               Container.Data (1..Data'Length) := Data;
            end;
         else
            Container :=
               new Vector'(Length, 1, Container.Data (1..Length));
         end if;
      end if;
   end Clone;

   function Compare
            (  Left  : Half_Word_Array;
               Right : Half_Word_Array
            )  return Precedence is
   begin
      if Left'Length /= Right'Length then
         if Left'Length < Right'Length then
            return Less;
         else
            return Greater;
         end if;
      end if;
      declare
         J : Digit_Offset := Right'Last;
      begin
         for I in reverse Left'Range loop
            if Left (I) /= Right (J) then
               if Left (I) < Right (J) then
                  return Less;
               else
                  return Greater;
               end if;
            end if;
            J := J - 1;
         end loop;
      end;
      return Equal;
   end Compare;

   function Compare (Left : Unbounded_Unsigned; Right : Half_Word)
      return Precedence is
   begin
      if Left.Length = 0 then
         if Right = 0 then
            return Equal;
         else
            return Less;
         end if;
      elsif Left.Length = 1 then
         declare
            Data : constant Half_Word := Left.Value.Data (1);
         begin
            if Data = Right then
               return Equal;
            elsif Data < Right then
               return Less;
            else
               return Greater;
            end if;
         end;
      else
         return Greater;
      end if;
   end Compare;

   function Compare (Left : Half_Word; Right : Unbounded_Unsigned)
      return Precedence is
   begin
      case Compare (Right, Left) is
         when Less =>
            return Greater;
         when Equal =>
            return Equal;
         when Greater =>
            return Less;
      end case;
   end Compare;

   function Compare (Left, Right : Unbounded_Unsigned)
      return Precedence is
   begin
      if Left.Length = 0 then
         if Right.Length = 0 then
            return Equal;
         else
            return Less;
         end if;
      elsif Right.Length = 0 then
         return Greater;
      else
         return Compare
                (  Left.Value.Data  (1..Left.Length),
                   Right.Value.Data (1..Right.Length)
                );
      end if;
   end Compare;

   function Compare
            (  Left       : Unbounded_Unsigned;
               Right      : Half_Word_Array;
               Multiplier : Half_Word;
               Shift      : Digit_Offset
            )  return Precedence is
   begin
      if Right'Length = 0 or else Multiplier = 0 then
         if Left.Length = 0 then
            return Equal;
         else
            return Greater;
         end if;
      elsif Left.Length < Right'Length + Shift then
         return Less;
      end if;
      declare
         Factor : constant Word := Word (Multiplier);
         Length : constant Digit_Offset := Left.Length;
         Data   : Half_Word_Array renames Left.Value.Data;
         Borrow : Word := 0;
         Diff   : Word;
         Count  : Word;
         To     : Digit_Count := Shift + 1;
         Zero   : Boolean := True;
      begin
         for Index in Right'Range loop
            Borrow := Borrow + Word (Right (Index)) * Factor;
            if Word (Data (To)) >= Borrow then
               Zero := Zero and then 0 = Data (To) - Half_Word (Borrow);
               Borrow := 0;
            else
               Diff  := Borrow - Word (Data (To));
               Count := Shift_Right
                        (  Diff + Half_Word_Mask,
                           Bit_Width  -- How much to borrow
                        );
               Borrow := Count * Half_Word_Modulus - Diff;
               Zero := Zero and then
                       0 = Half_Word (Borrow and Half_Word_Mask);
               Borrow := Count;
            end if;
            To := To + 1;
         end loop;
         if Borrow /= 0 then
            for Index in Right'Last  + 1
                      .. Right'First + Length - 1 loop
               exit when To > Length;
               if Word (Data (To)) >= Borrow then
                  Zero :=
                     Zero and then 0 = Data (To) - Half_Word (Borrow);
                  Borrow := 0;
                  exit;
               end if;
               Zero :=
                  Zero and then
                  0 = Word (Data (To)) + Half_Word_Modulus - Borrow;
               Borrow := 1;
               To := To + 1;
            end loop;
         end if;
         if Borrow /= 0 then
            return Less;
         elsif Zero then
            return Equal;
         else
            return Greater;
         end if;
      end;
   end Compare;

   procedure Complement (Left : in out Unbounded_Unsigned) is
      Carry : Word := 1;
   begin
      if Left.Length = 0 then
         return;
      end if;
      Clone (Left.Value, Left.Length);
      declare
         Length : Digit_Offset    renames Left.Length;
         Data   : Half_Word_Array renames Left.Value.Data (1..Length);
      begin
         for Index in Data'Range loop
            Carry := Carry + Word (not Data (Index));
            Data (Index) := Half_Word (Carry and Half_Word_Mask);
            Carry := Shift_Right (Carry, Bit_Width);
         end loop;
         while Length > 0 and then Data (Length) = 0 loop
            Length := Length - 1;
         end loop;
      end;
   end Complement;

   procedure Complement
             (  Left   : in out Unbounded_Unsigned;
                Length : Digit_Count
             )  is
      Carry : Word := 1;
   begin
      if Left.Length = 0 then
         return;
      end if;
      Clone (Left.Value, Left.Length);
      declare
         Data : Half_Word_Array renames Left.Value.Data;
      begin
         for Index in 1..Digit_Offset'Min (Left.Length, Length) loop
            Carry := Carry + Word (not Data (Index));
            Data (Index) := Half_Word (Carry and Half_Word_Mask);
            Carry := Shift_Right (Carry, Bit_Width);
         end loop;
      end;
      for Index in Left.Length + 1..Length loop
         Carry := Carry + Half_Word_Mask;
         Put (Left.Value, Index, Half_Word (Carry and Half_Word_Mask));
         Carry := Shift_Right (Carry, Bit_Width);
      end loop;
      Left.Length := Digit_Offset'Max (Left.Length, Length);
      declare
         Size : Digit_Offset    renames Left.Length;
         Data : Half_Word_Array renames Left.Value.Data (1..Size);
      begin
         while Size > 0 and then Data (Size) = 0 loop
            Size := Size - 1;
         end loop;
      end;
   end Complement;

   procedure Copy
             (  Destination : in out Unbounded_Unsigned;
                Source      : Unbounded_Unsigned
             )  is
   begin
      if Source.Length = 0 then
         Destination.Length := 0;
      elsif Destination.Value = null or else
         Load (Destination.Value.Count'Access) > 1 or else
         Destination.Value.Size < Source.Length then
         Release (Destination.Value);
         Destination.Length := Source.Length;
         Destination.Value :=
            new Vector'
                (  Source.Length,
                   1,
                   Source.Value.Data (1..Source.Length)
                );
      else
         Destination.Length := Source.Length;
         Destination.Value.Data (1..Source.Length) :=
            Source.Value.Data (1..Source.Length);
      end if;
   end Copy;

   procedure Div
             (  Dividend  : in out Unbounded_Unsigned;
                Divisor   : Half_Word;
                Remainder : out Half_Word
             )  is
   begin
      if Divisor = 0 then
         raise Constraint_Error;
      elsif Divisor = 1 or else Dividend.Length = 0 then
         Remainder := 0;
         return;
      elsif Dividend.Length = 1 then
         Clone (Dividend.Value, Dividend.Length);
         declare
            Data : Half_Word renames Dividend.Value.Data (1);
         begin
            Remainder := Data mod Divisor;
            Data := Data / Divisor;
            if Data = 0 then
               Dividend.Length := 0;
            end if;
            return;
         end;
      end if;
      Clone (Dividend.Value, Dividend.Length);
      declare
         Denominator : constant Word := Word (Divisor);
         Length      : Digit_Offset renames Dividend.Length;
         Data        : Half_Word_Array renames Dividend.Value.Data;
         Carry       : Word := 0;
      begin
         for Index in reverse 1..Length loop
            Carry := Word (Data (Index)) + Carry * Half_Word_Modulus;
            Data (Index) := Half_Word (Carry / Denominator);
            Carry := Carry mod Denominator;
         end loop;
         while Length > 0 and then Data (Length) = 0 loop
            Length := Length - 1;
         end loop;
         Remainder := Half_Word (Carry);
      end;
   end Div;

   procedure Div
             (  Dividend : in out Unbounded_Unsigned;
                Divisor  : Half_Word
             )  is
      Remainder : Half_Word;
   begin
      Div (Dividend, Divisor, Remainder);
   end Div;

   procedure Div
             (  Dividend  : in out Unbounded_Unsigned;
                Divisor   : Unbounded_Unsigned;
                Remainder : out Unbounded_Unsigned
             )  is
      function Same_Power (Left, Right : Half_Word) return Boolean is
          pragma Inline (Same_Power);
      begin
          if Left > Right then
             return Left / Right = 1;
          else
             return Right / Left = 1;
          end if;
      end Same_Power;
   begin
      if Divisor.Length = 0 then
         raise Constraint_Error;
      end if;
      if Dividend.Length = 0 then
         Erase (Dividend);
         Erase (Remainder);
         return;
      elsif Divisor.Length = 1 then
         if Divisor.Value.Data (1) = 1 then
            Erase (Remainder);
         elsif Dividend.Length = 1 then
           Clone (Dividend.Value, Dividend.Length);
           declare
              Left  : Half_Word renames Dividend.Value.Data (1);
              Right : constant Half_Word := Divisor.Value.Data  (1);
           begin
              Remainder := From_Half_Word (Left mod Right);
              Left      := Left / Right;
              if Left = 0 then
                 Dividend.Length := 0;
              end if;
           end;
        else
           declare
              Short_Remainder : Half_Word;
           begin
              Div (Dividend, Divisor.Value.Data (1), Short_Remainder);
              Remainder := From_Half_Word (Short_Remainder);
            end;
         end if;
         return;
      elsif Dividend.Length < Divisor.Length then
         Remainder := Dividend;
         Erase (Dividend);
         return;
      elsif Dividend.Length = Divisor.Length then
         case Compare
              (  Dividend.Value.Data (1..Dividend.Length),
                 Divisor.Value.Data  (1..Divisor.Length)
              )  is
            when Less =>
               Remainder := Dividend;
               Erase (Dividend);
               return;
            when Equal =>
               Erase (Remainder);
               Set (Dividend, 1);
               return;
            when Greater =>
               if Same_Power
                  (  Dividend.Value.Data (Dividend.Length),
                     Divisor.Value.Data  (Divisor.Length)
                  )  then
                  Remainder := Dividend;
                  Sub (Remainder, Divisor);
                  Set (Dividend, 1);
                  return;
               end if;
         end case;
      end if;
      declare
         Subtrahend : Unbounded_Unsigned := Divisor;
         Power      : Unbounded_Unsigned := One;
         Shift      : Integer := 1;
         Dummy      : Half_Word;
      begin
         Clone (Dividend.Value, Dividend.Length);
         Remainder := Dividend;
         Erase (Dividend);
         loop
            Mul (Subtrahend, 2);
            if Remainder < Subtrahend then
               Div (Subtrahend, 2, Dummy);
               exit;
            end if;
            Mul (Power, 2);
            Shift := Shift + 1;
         end loop;
         Sub (Remainder, Subtrahend);
         Add (Dividend, Power);
         while Shift > 1 loop
            Shift := Shift - 1;
            Div (Subtrahend, 2, Dummy);
            Div (Power, 2, Dummy);
            if Remainder >= Subtrahend then
               Sub (Remainder, Subtrahend);
               Add (Dividend, Power);
            end if;
         end loop;
      end;
   end Div;

   procedure Div
             (  Dividend : in out Unbounded_Unsigned;
                Divisor  : Unbounded_Unsigned
             )  is
      Remainder : Unbounded_Unsigned;
   begin
      Div (Dividend, Divisor, Remainder);
   end Div;

   procedure Div_By_Power_of_Two
             (  Dividend : in out Half_Word_Array;
                Length   : in out Digit_Offset;
                Power    : Bit_Count
             )  is
      Word_Shift : constant Digit_Offset :=
                            Digit_Offset (Power / Bit_Width);
      Bit_Shift  : constant Natural := Natural (Power mod Bit_Width);
   begin
      if Bit_Shift = 0 then
         Dividend (Dividend'First..Dividend'Last - Word_Shift) :=
            Dividend (Dividend'First + Word_Shift..Dividend'Last);
         Length := Length - Word_Shift;
      else
         declare
            To : Digit_Count := Dividend'First;
         begin
            for From in Dividend'First + 1 + Word_Shift
                     .. Dividend'Last
            loop
               Dividend (To) :=
                  Half_Word
                  (  Shift_Right
                     (  (  Word (Dividend (From - 1))
                        +  Word (Dividend (From)) * Half_Word_Modulus
                        ),
                        Bit_Shift
                     )
                  and
                     Half_Word_Mask
                  );
               To := To + 1;
            end loop;
            Dividend (To) :=
               Half_Word
               (  Shift_Right
                  (  Word (Dividend (Dividend'Last)),
                     Bit_Shift
               )  );
            To := To + 1;
            while To > Dividend'First and then Dividend (To - 1) = 0
            loop
               To := To - 1;
            end loop;
            Length := To - Dividend'First;
         end;
      end if;
      while Length > 0 and then
            Dividend (Dividend'First + Length - 1) = 0 loop
         Length := Length - 1;
      end loop;
   end Div_By_Power_of_Two;

   procedure Div_By_Power_of_Two
             (  Dividend : in out Unbounded_Unsigned;
                Power    : Bit_Count
             )  is
   begin
      if Power = 0 then
         return;
      elsif Dividend.Length = 0 then
         Dividend.Length := 0;
      else
         Clone (Dividend.Value, Dividend.Length);
         Div_By_Power_of_Two
         (  Dividend.Value.Data (1..Dividend.Length),
            Dividend.Length,
            Power
         );
      end if;
   end Div_By_Power_of_Two;

   function Div_By_Power_of_Two
            (  Dividend : Unbounded_Unsigned;
               Power    : Bit_Count
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Set (Result, Dividend);
      Div_By_Power_of_Two (Result, Power);
      return Result;
   end Div_By_Power_of_Two;

   procedure Erase (Left : in out Unbounded_Unsigned) is
   begin
      if Left.Value /= null and then
         Load (Left.Value.Count'Access) > 1 then
         Release (Left.Value); -- If shared give it up
      end if;
      Left.Length := 0;
   end Erase;

   procedure Finalize (Object : in out Unbounded_Unsigned) is
   begin
      Release (Object.Value);
   end Finalize;

   function From_Half_Word (Left : Half_Word)
      return Unbounded_Unsigned is
   begin
      if Left = 0 then
         return
         (  Ada.Finalization.Controlled
         with
            Length => 0,
            Value  => null
         );
      else
         return
         (  Ada.Finalization.Controlled
         with
            Length => 1,
            Value  => new Vector'(1, 1, (1..1 => Left))
         );
      end if;
   end From_Half_Word;

   function From_Word (Left : Word) return Unbounded_Unsigned is
   begin
      if Left = 0 then
         return
         (  Ada.Finalization.Controlled
         with
            Length => 0,
            Value  => null
         );
      elsif Left < Half_Word_Modulus then
         return
         (  Ada.Finalization.Controlled
         with
            Length => 1,
            Value  => new Vector'(1, 1, (1..1 => Half_Word (Left)))
         );
      else
         return
         (  Ada.Finalization.Controlled
         with
            Length => 2,
            Value  =>
               new Vector'
                   (  2,
                      1,
                      (  1 => Half_Word (Left and Half_Word_Mask),
                         2 => Half_Word (Left / Half_Word_Modulus)
         )         )  );
      end if;
   end From_Word;

   function Get_Bit
            (  Left     : Unbounded_Unsigned;
               Position : Bit_Position
            )  return Boolean is
   begin
      if Left.Length = 0 then
         return False;
      else
         declare
            Power : constant Bit_Count := Position - 1;
            Index : constant Digit_Offset :=
                             Digit_Offset (Power / Bit_Width + 1);
         begin
            if Index > Left.Length then
               return False;
            else
               declare
                  N : constant Natural := Natural (Power mod Bit_Width);
               begin
                  return
                  (  (Shift_Right (Left.Value.Data (Index), N) and 1)
                  =  1
                  );
               end;
            end if;
         end;
      end if;
   end Get_Bit;

   function Get_Digit
            (  Left  : Unbounded_Unsigned;
               Index : Digit_Count
            )  return Half_Word is
   begin
      if Index > Left.Length then
         return 0;
      else
         return Left.Value.Data (Index);
      end if;
   end Get_Digit;

   function Get_Length
            (  Left : Unbounded_Unsigned;
               Used : Boolean := True
            )  return Digit_Offset is
   begin
      if Used then
         return Left.Length;
      elsif Left.Value = null then
         return 0;
      else
         return Left.Value.Size;
      end if;
   end Get_Length;

   function Get_MSB (Left : Unbounded_Unsigned) return Bit_Count is
      Length : constant Digit_Offset := Left.Length;
   begin
      if Length = 0 then
         return 0;
      end if;
      declare
         Value  : Half_Word;
         Offset : Natural := 0;
         Data   : constant Half_Word := Left.Value.Data (Length);
      begin
         loop
            Value := Shift_Right (Data, Offset);
            if Value < 256 then
               return
               (  Bit_Count (Log2_Table (Value) + Offset)
               +  Bit_Width * (Bit_Count (Length) - 1)
               );
            end if;
            Offset := Offset + 8;
         end loop;
      end;
   end Get_MSB;

   function Get_Multiplication_Threshold return Digit_Count is
   begin
      return Mul_Threshold;
   end Get_Multiplication_Threshold;

   procedure Get_Slice
             (  Left : in out Unbounded_Unsigned;
                From : Bit_Position;
                To   : Bit_Position
             )  is
   begin
      if From > To or else Left.Length = 0 then
         Erase (Left);
      else
         Clone (Left.Value, Left.Length);
         declare
            First  : constant Digit_Count :=
                              Digit_Count ((From - 1) / Bit_Width + 1);
            Last   : constant Digit_Count :=
                              Digit_Count ((To   - 1) / Bit_Width + 1);
         begin
            if Last > Left.Length then
               --
               -- No need to mask out the highest bits. Just shifting if
               -- necessary
               --
               Left.Length := Left.Length - First + 1;
               if From > 1 then
                  Div_By_Power_Of_Two
                  (  Left.Value.Data (First..Left.Length),
                     Left.Length,
                     From - 1
                  );
               end if;
            else
               --
               -- We must to mask highest bits first and shift then
               --
               declare
                  Highest : constant Natural :=
                                     Natural ((To - 1) mod Bit_Width);
                  Data    : Half_Word_Array renames Left.Value.Data;
                  Length  : Digit_Offset    renames Left.Length;
               begin
                  if Highest + 1 < Bit_Width then
                     Data (Last) := Data (Last) mod 2 ** (Highest + 1);
                  end if;
                  Length := Last;
                  if From > 1 then
                     Div_By_Power_Of_Two
                     (  Data (1..Last),
                        Length,
                        From - 1
                     );
                  end if;
                  loop
                     if Length = 0 then
                        return;
                     end if;
                     exit when Data (Length) /= 0;
                     Length := Length - 1;
                  end loop;
               end;
            end if;
         end;
      end if;
   end Get_Slice;

   function Get_Slice
            (  Left : Unbounded_Unsigned;
               From : Bit_Position;
               To   : Bit_Position
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned := Left;
   begin
      Get_Slice (Result, From, To);
      return Result;
   end Get_Slice;

   function Get_Use_Count (Object : Unbounded_Unsigned)
      return Natural is
   begin
      if Object.Value = null then
         return 0;
      else
         return Natural (Load (Object.Value.Count'Access));
      end if;
   end Get_Use_Count;

   function Get_Wiping_Mode return Boolean is
   begin
      return Free = Wiping_Deallocate'Access;
   end Get_Wiping_Mode;

   function Greatest_Common_Divisor (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned is
      A : Unbounded_Unsigned := Left;
      B : Unbounded_Unsigned := Right;
   begin -- (A, B) -> (B, A mod B)
      loop
         if B.Length = 0 then
            return A;
         end if;
         Modulo (A, B);
         Swap (A, B);
      end loop;
   end Greatest_Common_Divisor;

   function Inverse (Left : Unbounded_Unsigned) return Half_Word is
      A, X, Y : Half_Word;
   begin
      if Is_Even (Left) then
         raise Constraint_Error;
      end if;
      A := Left.Value.Data (1);
      X := (A * 3) ** 2;
      Y := 1 - A * X;
      loop
         X := X * (Y + 1);
         exit when A * X = 1;
         Y := Y * Y;
      end loop;
      return X;
   end Inverse;

   function Inverse (Left : Half_Word) return Half_Word is
      A, X, Y : Half_Word;
   begin
      if (Left and 1) = 0 then
         raise Constraint_Error;
      end if;
      A := Left;
      X := (A * 3) ** 2;
      Y := 1 - A * X;
      loop
         X := X * (Y + 1);
         exit when A * X = 1;
         Y := Y * Y;
      end loop;
      return X;
   end Inverse;

   function Inverse (Left : Word) return Word is
      A, X, Y : Word;
   begin
      if (Left and 1) = 0 then
         raise Constraint_Error;
      end if;
      A := Left;
      X := (A * 3) ** 2;
      Y := 1 - A * X;
      loop
         X := X * (Y + 1);
         exit when A * X = 1;
         Y := Y * Y;
      end loop;
      return X;
   end Inverse;

   procedure Inverse
             (  Left   : Unbounded_Unsigned;
                Count  : Digit_Count;
                Result : out Unbounded_Unsigned
             )  is
      --
      --  Newton's iteration
      --
      --     F(X) = 1/X - N = 0
      --
      --     X <- X - F(X) / F'(X) =
      --        = X - (1/X - N)/(-1/X**2) =
      --        = X + X**2 * (1/X - N) =
      --        = X + X - N * X**2 =
      --        = 2 * X - N * X * X =
      --        = X * (2 - N * X)
      --
      --     if X * N = 1 (mod 2**K) then some M exists that
      --
      --     X * N = 1 + M * 2**K
      --
      --          N * (X * (2 - N * X)) =
      --        = N * X * (2 - N * X)) =
      --        = (1 + M * 2**K) * (2 - 1 - M * 2**K) =
      --        = (1 + M * 2**K) * (1 - M * 2**K) =
      --        = 1 - M * 2**K + M * 2**K - M * M * 2**K * 2**K =
      --        = 1 - M**2 * 2**(2*K) =
      --        = 1 (mod 2**2K)
      --
      X0      : Unbounded_Unsigned renames Result;
      X1      : Unbounded_Unsigned;
      Width   : constant Bit_Count   := Bit_Count (Count * Bit_Width);
      Count_2 : constant Digit_Count := Count * 2;
   begin
      if Is_Zero (Left) then
         raise Constraint_Error;
      end if;
   -- Set (X0, Left.Value.Data (1) and 7);
      declare
         Init : constant Half_Word := Left.Value.Data (1);
      begin
         Set
         (  X0,
            (  Shift_Left
               (  (Shift_Left (Init, 1) or Init) and 4,
                  1
               )
            or Init
         )  );
      end;
      for I in 1..Width + 1 loop
         Mul (X0, Left, X1);      -- X * N
         if X1.Length > 0 then
            declare
               Success : Boolean := True;
               Data    : Half_Word_Array renames
                            X1.Value.Data (1..X1.Length);
            begin
               if Data (1) = 1 then
                  for Index in 2..Count loop
                     if Data (Index) /= 0 then
                        Success := False;
                        exit;
                     end if;
                  end loop;
                  if Success then
                     Modulo (X0, Count);
                     return;
                  end if;
               end if;
            end;
         end if;
         case Compare (X1, 2) is
            when Less =>
               if Is_Zero (X1) then
                  Mul (X0, 2);    -- X * (2 - X * N) = 2 * X
               else               --          ^^^^^  = 0
                  null;           -- X * (2 - X * N) = X
               end if;            --          ^^^^^  = 1
            when Equal =>
               Erase (X0);
            when Greater =>
               Sub (X1, 2);       -- X * N - 2
               Mul (X0, X1);      -- X * (X * N - 2)
               Modulo (X0, Count);
               Sub_From_Power_Of_Half_Word (Count, X0);
--             Complement (X0);
         end case;                -- X * (2 - X * N)
         Modulo (X0, Count_2);
      end loop;
      raise Constraint_Error;
   end Inverse;

   function Inverse
            (  Left  : Unbounded_Unsigned;
               Count : Digit_Count
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Inverse (Left, Count, Result);
      return Result;
   end Inverse;

   procedure Invert_Slice
             (  Left : in out Unbounded_Unsigned;
                From : Bit_Position;
                To   : Bit_Position
             )  is
   --
   -- Set -- Destination bits
   --
   --    Index - The destination/source index
   --    Mask  - Bits to be replaced
   --
      procedure Set (Index : Digit_Count; Mask : Half_Word) is
         pragma Inline (Set);
      begin
         if Index <= Left.Length then
            declare
               Data : Half_Word renames Left.Value.Data (Index);
            begin
               Data := (Data and not Mask) or (Mask and not Data);
            end;
         else
            Put (Left.Value, Index, Mask);
            declare
               Data : Half_Word_Array renames Left.Value.Data;
            begin
               for I in Left.Length + 1..Index - 1 loop
                  Data (I) := 0;
               end loop;
               Left.Length := Index;
            end;
         end if;
      end Set;
   --
   -- Set -- Destination bits in complete half-words
   --
   --    From - The first destination/source index
   --    To   - The last index
   --
      procedure Set (From, To : Digit_Count) is
         pragma Inline (Set);
         Data : Half_Word_Array renames
                   Left.Value.Data (1..Left.Length);
      begin
         for Index in From..To loop
            Data (Index) := not Data (Index);
         end loop;
      end Set;
   begin
      if From > To then
         return;
      elsif From = To then
         if Get_Bit (Left, From) then
            Clear_Bit (Left, From);
         else
            Set_Bit (Left, From);
         end if;
         return;
      end if;
      declare
         First     : constant Digit_Count :=
                        Digit_Count ((From - 1) / Bit_Width + 1);
         Last      : constant Digit_Count :=
                        Digit_Count ((To - 1) / Bit_Width + 1);
         From_At   : constant Natural :=
                        Natural (From - 1) mod Bit_Width;
         To_At     : constant Natural :=
                        Natural (To   - 1) mod Bit_Width;
         From_Mask : constant Half_Word := not (2 ** From_At - 1);
         To_Mask   : constant Half_Word := 2 ** (To_At + 1) - 1;
      begin
         if Left.Length = 0 then
            Allocate (Left.Value, Last);
         else
            Clone (Left.Value, Left.Length);
         end if;
         if First = Last then
            Set (First, From_Mask and To_Mask);
         elsif From_At = 0 then
            if To_At = Bit_Width - 1 then
               if Left.Length < Last then
                  if Left.Length > 0 then
                     Set (First, Left.Length);
                  end if;
                  Put (Left.Value, Last, Half_Word_Mask);
                  declare
                     Data : Half_Word_Array renames
                               Left.Value.Data (1..Last);
                  begin
                     for Index in Left.Length + 1..Last - 1 loop
                        Data (Index) := Half_Word_Mask;
                     end loop;
                  end;
                  Left.Length := Last;
               else
                  Set (First, Last);
               end if;
            else
               Set (Last, To_Mask);
               Set (First, Last - 1);
            end if;
         else
            Set (Last,  To_Mask);
            Set (First, From_Mask);
            Set (First + 1, Last - 1);
         end if;
      end;
      if Left.Length = 0 then
         return;
      end if;
      declare
         Length : Digit_Offset    renames Left.Length;
         Data   : Half_Word_Array renames Left.Value.Data (1..Length);
      begin
         while Length > 0 and then Data (Length) = 0 loop
            Length := Length - 1;
         end loop;
      end;
   end Invert_Slice;

   function Is_Even (Left : Unbounded_Unsigned) return Boolean is
   begin
      return Left.Length = 0 or else (Left.Value.Data (1) and 1) = 0;
   end Is_Even;

   function Is_Mersenne (Left : Unbounded_Unsigned) return Boolean is
   begin
      if Left.Length = 0 then
         return False;
      end if;
      --
      -- Checking if all bits are set, i.e. X = 2 ** K - 1
      --
      declare
         Data : Half_Word_Array renames Left.Value.Data;
      begin
         for I in 1..Left.Length - 1 loop
            if Data (I) /= Half_Word'Last then
               return False;
            end if;
         end loop;
         declare
            Highest : constant Half_Word := Data (Left.Length);
         begin
            return 0 = ((Highest + 1) and Highest);
         end;
      end;
   end Is_Mersenne;

   function Is_Odd (Left : Unbounded_Unsigned) return Boolean is
   begin
      return Left.Length > 0 and then Left.Value.Data (1) mod 2 = 1;
   end Is_Odd;

   function Is_One (Left : Unbounded_Unsigned) return Boolean is
   begin
      return Left.Length = 1 and then Left.Value.Data (1) = 1;
   end Is_One;

   function Is_Power_Of_Two (Left : Unbounded_Unsigned)
      return Boolean is
   begin
      if Left.Length = 0 then
         return False;
      end if;
      declare
         Data : Half_Word_Array renames Left.Value.Data;
      begin
         for I in 1..Left.Length - 1 loop
            if Data (I) /= 0 then
               return False;
            end if;
         end loop;
         declare
            Highest : constant Half_Word := Data (Left.Length);
         begin
            return 0 = ((Highest - 1) and Highest);
         end;
      end;
   end Is_Power_Of_Two;

   function Is_Proth (Left : Unbounded_Unsigned) return Bit_Count is
   begin
      if Left.Length = 0 then
         return 0;
      end if;
      declare
         Data : Half_Word_Array renames
                   Left.Value.Data (1..Left.Length);
         K    : Bit_Count;
         Item : Half_Word := Data (1);
      begin
         if (Item and 1) = 0 then
            return 0;
         elsif Data'Length = 1 and then Item = 1 then
            return 0;
         end if;
         if Item = 1 then
            K := Bit_Width;
            for Index in 2..Data'Last loop
               Item := Data (Index);
               if Item /= 0 then
                  while (Item and 1) = 0 loop
                     K := K + 1;
                     Item := Shift_Right (Item, 1);
                  end loop;
                  exit;
               end if;
               K := K + Bit_Width;
            end loop;
         else
            K := 0;
            loop
               Item := Shift_Right (Item, 1);
               K := K + 1;
               exit when (Item and 1) /= 0;
            end loop;
         end if;
         if Get_MSB (Left) - K - 1 < K then
            return K;
         else
            return 0;
         end if;
      end;
   end Is_Proth;

   function Is_Two (Left : Unbounded_Unsigned) return Boolean is
   begin
      return Left.Length = 1 and then Left.Value.Data (1) = 2;
   end Is_Two;

   function Is_Zero (Left : Unbounded_Unsigned) return Boolean is
   begin
      return Left.Length = 0;
   end Is_Zero;

   function Log2 (Left : Half_Word) return Natural is
      Offset : Natural := 0;
      Value  : Half_Word;
   begin
      if Left = 0 then
         raise Constraint_Error;
      end if;
      loop
         Value := Shift_Right (Left, Offset);
         if Value < 256 then
            return Log2_Table (Value) + Offset - 1;
         end if;
         Offset := Offset + 8;
      end loop;
   end Log2;

   procedure Log2
             (  Left      : Unbounded_Unsigned;
                Power     : out Bit_Count;
                Remainder : out Mod_2_Remainder
             )  is
   begin
      if Left.Length = 0 then
         Power := 0;
         Remainder := -1;
         return;
      end if;
      Power := Get_MSB (Left);
      --
      -- Checking if the modulus has one of simple forms:
      --
      --   2 ** M - 1
      --   2 ** M
      --   2 ** M + 1
      --
      declare
         M        : Bit_Count renames Power;
         Highest  : constant Natural := Natural ((M - 1) mod Bit_Width);
         Length   : constant Digit_Offset := Left.Length;
         All_Bits : Boolean := True;
         Data     : Half_Word_Array renames Left.Value.Data;
      begin
         if Length = 1 then
            if Data (1) - 2 ** Highest = 0 then    -- 2 ** M
               Remainder := 0;
               return;
            elsif Data (1) - 2 ** Highest = 1 then -- 2 ** M + 1
               M := M - 1;
               Remainder := 1;
               return;
            elsif (  Data (1) = Half_Word'Last
                  or else
                     2 ** (Highest + 1) - Data (1) = 1
                  )  then                          -- 2 ** M - 1
               Remainder := -1;
               return;
            end if;
         else
            if Data (1) = 0 then
               for Index in 2..Length - 1 loop
                  if Data (Index) /= 0 then
                     All_Bits := False;
                     exit;
                  end if;
               end loop;
               if All_Bits and then                -- 2 ** M
                  Data (Length) - 2 ** Highest = 0  then
                  Remainder := 0;
                  return;
               end if;
            elsif Data (1) = 1 then
               for Index in 2..Length - 1 loop
                  if Data (Index) /= 0 then
                     All_Bits := False;
                     exit;
                  end if;
               end loop;
               if All_Bits and then               -- 2 ** M + 1
                  Data (Length) - 2 ** Highest = 0 then
                  M := M - 1;
                  Remainder := 1;
                  return;
               end if;
            elsif Data (1) = Half_Word'Last then
               for Index in 2..Length - 1 loop
                  if Data (Index) /= Half_Word'Last then
                     All_Bits := False;
                     exit;
                  end if;
               end loop;
               if All_Bits and then               -- 2 ** M - 1
                  (  Data (Length) = Half_Word'Last
                  or else
                     2 ** (Highest + 1) - Data (Length) = 1
                  )  then
                     -- Modulus = 2 ** M - 1 like above
                  Remainder := -1;
                  return;
               end if;
            end if;
         end if;
      end;
      Remainder := 2;
   end Log2;

   function Max (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned is
   begin
      if Compare (Left, Right) = Greater then
         return Left;
      else
         return Right;
      end if;
   end Max;

   function Min (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned is
   begin
      if Compare (Left, Right) = Less then
         return Left;
      else
         return Right;
      end if;
   end Min;

   procedure Modulo
             (  Left  : in out Unbounded_Unsigned;
                Count : Digit_Count
             )  is
      Length : Digit_Offset renames Left.Length;
   begin
      if Length > Count then
         Length := Count;
         declare
            Data : Half_Word_Array renames Left.Value.Data;
         begin
            while Length > 0 and then Data (Length) = 0 loop
               Length := Length - 1;
            end loop;
         end;
      end if;
   end Modulo;

   function Modulo
            (  Left  : Unbounded_Unsigned;
               Count : Digit_Count
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Set (Result, Left);
      Modulo (Result, Count);
      return Result;
   end Modulo;

--
-- Modulo -- Internal implementation
--
--    Left  - The value
--    Right - The normalized modulus
--
   procedure Modulo
             (  Left  : in out Unbounded_Unsigned;
                Right : Half_Word_Array
             )  is
      Highest    : constant Word := Word (Right (Right'Last) + 1);
      Upper      : Word := 0;
      Borrow     : Half_Word;
      Multiplier : Half_Word;
      Shift      : Digit_Count;
      Length     : Digit_Offset    renames Left.Length;
      Data       : Half_Word_Array renames Left.Value.Data (1..Length);
   begin
      for Index in reverse Right'Length + 1..Length loop
         loop
            if Index < Length then
               Upper :=
                  (  (  Shift_Left (Word (Data (Index + 1)), Bit_Width)
                     or Word (Data (Index))
                     )
                  /  Highest
                  );
            else
               Upper := Word (Data (Index)) / Highest;
            end if;
            if Upper >= Half_Word_Modulus then
               Multiplier := Half_Word (Shift_Right (Upper, Bit_Width));
               Shift      := Index - Right'Length + 1;
               Sub (Left, Right, Multiplier, Shift, Borrow);
               if Borrow /= 0 then
                  raise Program_Error;
               end if;
               Upper :=
                  (  (  Shift_Left (Word (Data (Index + 1)), Bit_Width)
                     or Word (Data (Index))
                     )
                  /  Highest
                  );
            end if;
            Multiplier := Half_Word (Upper and Half_Word_Mask);
            Shift      := Index - Right'Length;
            Sub (Left, Right, Multiplier, Shift, Borrow);
            if Borrow /= 0 then
               raise Program_Error;
            end if;
            exit when Length <= Index;
            Upper := Word (Data (Length));
         end loop;
         if Length = Index then
            Upper := Word (Data (Length));
         else
            Upper := 0;
         end if;
      end loop;
      if Length > Data'Length + 1 then
         raise Program_Error;
      end if;
      loop
         case Compare (Data (1..Length), Right) is
            when Less =>
               return;
            when Equal =>
               Erase (Left);
               return;
            when Greater =>
               null;
         end case;
         if Length > Right'Length then
            Upper :=
               (  (  Shift_Left (Word (Data (Length)), Bit_Width)
                  or Word (Data (Length - 1))
                  )
               /  Highest
               );
         else
            Upper := Word (Data (Length)) / Highest;
         end if;
         if Upper = 0 then
            Sub (Left, Right, Borrow);
            if Borrow /= 0 then
               raise Program_Error;
            end if;
         else
            if Upper >= Half_Word_Modulus then
               Multiplier := Half_Word (Shift_Right (Upper, Bit_Width));
               Sub (Left, Right, Multiplier, 1, Borrow);
               if Borrow /= 0 then
                  raise Program_Error;
               end if;
               if Length > Right'Length then
                  Upper :=
                     (  (  Shift_Left (Word (Data (Length)), Bit_Width)
                        or Word (Data (Length - 1))
                        )
                     /  Highest
                     );
               else
                  Upper := Word (Data (Length)) / Highest;
               end if;
            end if;
            Multiplier := Half_Word (Upper and Half_Word_Mask);
            Sub (Left, Right, Multiplier, 0, Borrow);
            if Borrow /= 0 then
               raise Program_Error;
            end if;
         end if;
         exit when Length < Right'Length;
      end loop;
   end Modulo;

   procedure Modulo
             (  Left  : in out Unbounded_Unsigned;
                Right : Unbounded_Unsigned
             )  is
      Shift : Bit_Count := 0;
   begin
      if Right.Length = 0 then
         raise Constraint_Error;
      elsif Left.Length = 0 then
         Erase (Left);
         return;
      elsif Right.Length = 1 then
         Set (Left, From_Half_Word (Left mod Right.Value.Data (1)));
         return;
      elsif Right.Length > Left.Length then
         return;
      elsif Is_Power_Of_Two (Right) then
         Get_Slice (Left, 1, Get_MSB (Right) - 1);
         return;
      else
         case Compare (Left, Right) is
            when Less =>
               return;
            when Equal =>
               Erase (Left);
               return;
            when Greater =>
               null;
         end case;
      end if;
      declare
         Mask    : constant  := Modulus / 2;
         Highest : Word := Shift_Left
                           (  Word (Right.Value.Data (Right.Length)),
                              Bit_Width
                           )
                        or Word (Right.Value.Data (Right.Length - 1));
      begin
          while (Highest and Mask) = 0 loop
             Highest := Shift_Left (Highest, 1);
             Shift   := Shift + 1;
          end loop;
          if Shift_Right (Highest, Bit_Width) = Half_Word_Mask then
             if Shift = 0 then
                Shift := Bit_Width - 1;
             else
                Shift := Shift - 1;
             end if;
          end if;
      end;
      if Shift = 0 then
         Clone (Left.Value, Left.Length);
         Modulo (Left, Right.Value.Data (1..Right.Length));
      else
         --
         -- Normalize  Left and Right by shifting by Shift.  The shifted
         -- Right is stored in the Right's vector.
         --
         Mul_By_Power_Of_Two (Left, Shift);
         Put (Left.Value, Left.Length + Right.Length + 3, 0);
         declare
            Carry  : Word := 0;
            To     : Digit_Count := Left.Length + 2;
            Source : Half_Word_Array renames
                     Right.Value.Data (1..Right.Length);
            Destination : Half_Word_Array renames
                     Left.Value.Data (To..Left.Length + Right.Length + 3);
         begin
            for Index in Source'Range loop
               declare
                  Shifted : constant Word :=
                            Shift_Left
                            (  Word (Source (Index)),
                               Natural (Shift)
                            );
               begin
                  Destination (To) :=
                     Half_Word (Carry or (Shifted and Half_Word_Mask));
                  Carry := Shift_Right (Shifted, Bit_Width);
               end;
               To := To + 1;
            end loop;
            if Carry /= 0 then
               Destination (To) := Half_Word (Carry);
               To := To + 1;
            end if;
            Modulo (Left, Destination (Left.Length + 2..To - 1));
         end;
         --
         -- Denormalize the result back
         --
         Div_By_Power_Of_Two (Left, Shift);
      end if;
   end Modulo;

   function Modulo_By_Power_Of_Two
            (  Left  : Unbounded_Unsigned;
               Right : Bit_Count
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Set (Result, Left);
      Modulo_By_Power_Of_Two (Result, Right);
      return Result;
   end Modulo_By_Power_Of_Two;

   procedure Modulo_By_Power_Of_Two
             (  Left  : in out Unbounded_Unsigned;
                Right : Bit_Count
             )  is
   begin
      if Right = 0 or else Left.Length = 0 then
         return;
      end if;
      declare
         Length  : Digit_Offset    renames Left.Length;
         Data    : Half_Word_Array renames Left.Value.Data;
         Count   : constant Digit_Offset :=
                      Digit_Offset (Right / Bit_Width + 1);
         Highest : constant Natural := Natural (Right mod Bit_Width);
      begin
         if Highest = 0 then
            if Count > Length then
               return;
            end if;
           Length := Count - 1;
         else
            if Count > Length then
               return;
            end if;
            Length := Count;
            if Highest = Bit_Width - 1 then
               return;
            end if;
            Data (Length) := Data (Length) mod 2 ** Highest;
         end if;
         while Length > 0 and then Data (Length) = 0 loop
            Length := Length - 1;
         end loop;
      end;
   end Modulo_By_Power_Of_Two;

   function Mod_Inv
            (  Left    : Unbounded_Unsigned;
               Modulus : Unbounded_Unsigned
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Mod_Inv (Left, Modulus, Result);
      return Result;
   end Mod_Inv;

   procedure Mod_Inv
             (  Left    : Unbounded_Unsigned;
                Modulus : Unbounded_Unsigned;
                Result  : out Unbounded_Unsigned
             )  is
      A, B, Q   : Unbounded_Unsigned;
      R         : Unbounded_Unsigned renames Result; -- Reuse
      X0, X1, T : Signed;
   begin
      if Is_Zero (Modulus) then
         raise Constraint_Error;
      elsif Is_One (Modulus) then
         Set (Result, 1);
      end if;
      Set (X1.Value, 1);
      Set (A, Left);
      Set (B, Modulus);
      while A > 1 loop
         Swap (A, Q);
         Div (Q, B, R);
         Swap (A, B); -- A = B
         Swap (B, R); -- B = R
         Set (T, X0);
         Mul (X0.Value, Q);
         Sub (X1, X0);
         Swap (X0, X1);
         Swap (X1, T);
      end loop;
      if X1.Sign then
         Set (Result, Modulus);
         Sub (Result, X1.Value);
      else
         Swap (X1.Value, Result);
      end if;
   end Mod_Inv;

   function Mod_Inv_In_Power_Of_Two
            (  Left   : Unbounded_Unsigned;
               Power  : Bit_Position
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Set (Result, Left);
      Mod_Inv_In_Power_Of_Two (Left, Power, Result);
      return Result;
   end Mod_Inv_In_Power_Of_Two;

   procedure Mod_Inv_In_Power_Of_Two
             (  Left   : Unbounded_Unsigned;
                Power  : Bit_Position;
                Result : out Unbounded_Unsigned
             )  is
      --
      --  Newton's iteration
      --
      --     F(X) = 1/X - N = 0
      --
      --     X <- X - F(X) / F'(X) =
      --        = X - (1/X - N)/(-1/X**2) =
      --        = X + X**2 * (1/X - N) =
      --        = X + X - N * X**2 =
      --        = 2 * X - N * X * X =
      --        = X * (2 - N * X)
      --
      --     if X * N = 1 (mod 2**K) then some M exists that
      --
      --     X * N = 1 + M * 2**K
      --
      --          N * (X * (2 - N * X)) =
      --        = N * X * (2 - N * X)) =
      --        = (1 + M * 2**K) * (2 - 1 - M * 2**K) =
      --        = (1 + M * 2**K) * (1 - M * 2**K) =
      --        = 1 - M * 2**K + M * 2**K - M * M * 2**K * 2**K =
      --        = 1 - M**2 * 2**(2*K) =
      --        = 1 (mod 2**2K)
      --
      Count   : constant Digit_Offset :=
                         Digit_Offset (Power / Bit_Width + 1);
      Highest : constant Natural := Natural (Power mod Bit_Width);
      X0      : Unbounded_Unsigned renames Result;
      X1      : Unbounded_Unsigned;
      P       : constant Bit_Position := Power * 2;
      function Is_Inverse return Boolean is
         pragma Inline (Is_Inverse);
         Length : constant Digit_Offset := X1.Length;
      begin
         if Length = 0 then
            return False;
         end if;
         declare
            Data : Half_Word_Array renames X1.Value.Data (1..Length);
         begin
            if Count > Length then
               return Is_One (X1);
            end if;
            if Highest = Bit_Width - 1 then
               for Index in 2..Count loop
                  if Data (Index) /= 0 then
                     return False;
                  end if;
               end loop;
               return Data (1) = 1;
            elsif Count = 1 then
               return Data (1) mod 2 ** Highest = 1;
            else
               if Data (Count) mod 2 ** Highest /= 0 then
                  return False;
               end if;
               for Index in 2..Count - 1 loop
                  if Data (Index) /= 0 then
                     return False;
                  end if;
               end loop;
               return Data (1) = 1;
            end if;
         end;
      end Is_Inverse;
   begin
      if Is_Zero (Left) then
         raise Constraint_Error;
      end if;
   -- Set (X0, Left.Value.Data (1) and 7);
      declare
         Init : constant Half_Word := Left.Value.Data (1);
      begin
         Set
         (  X0,
            (  Shift_Left
               (  (Shift_Left (Init, 1) or Init) and 4,
                  1
               )
            or Init
         )  );
      end;
      for I in 1..Log2 (Half_Word (Power)) + 1 loop
         Mul (X0, Left, X1);      -- X * N
         if Is_Inverse then
            Modulo_By_Power_Of_Two (X0, Power);
            return;
         end if;
         case Compare (X1, 2) is
            when Less =>
               if Is_Zero (X1) then
                  Mul (X0, 2);    -- X * (2 - X * N) = 2 * X
               else               --          ^^^^^  = 0
                  null;           -- X * (2 - X * N) = X
               end if;            --          ^^^^^  = 1
            when Equal =>
               Erase (X0);
            when Greater =>
               Sub (X1, 2);       -- X * N - 2
               Mul (X0, X1);      -- X * (X * N - 2)
               Modulo_By_Power_Of_Two (X0, Power);
               Sub_From_Power_Of_Two (Power, X0);
--             Complement (X0);
         end case;                -- X * (2 - X * N)
         Modulo_By_Power_Of_Two (X0, P);
      end loop;
      raise Constraint_Error;
   end Mod_Inv_In_Power_Of_Two;

   function Mod_Pow (Left, Right, Modulus : Unbounded_Unsigned)
      return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Mod_Pow (Left, Right, Modulus, Result);
      return Result;
   end Mod_Pow;

   procedure Mod_Pow
             (  Left, Right, Modulus : Unbounded_Unsigned;
                Result               : out Unbounded_Unsigned
             )  is
   begin
      if Right.Length <= 1 then
         Mod_Pow (Left, To_Half_Word (Right), Modulus, Result);
         return;
      end if;
      declare
         Base : Unbounded_Unsigned;
         X    : Unbounded_Unsigned;
      begin
         Set (Base, Left);
         Modulo (Base, Modulus);
         if Is_Zero (Base) then
            Erase (Result);
            return;
         end if;
         if Get_Bit (Right, 1) then
            Set (Result, Base);
         else
            Set (Result, 1);
         end if;
         for Bit in 2..Get_MSB (Right) loop
            Square (Base, X);
            Swap   (Base, X);
            Modulo (Base, Modulus);
            if Get_Bit (Right, Bit) then
               Mul (Result, Base, X);
               Swap (Result, X);
               Modulo (Result, Modulus);
            end if;
         end loop;
      end;
   end Mod_Pow;

   function Mod_Pow
            (  Left    : Unbounded_Unsigned;
               Right   : Half_Word;
               Modulus : Unbounded_Unsigned
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Mod_Pow (Left, Right, Modulus, Result);
      return Result;
   end Mod_Pow;

   procedure Mod_Pow
             (  Left    : Unbounded_Unsigned;
                Right   : Half_Word;
                Modulus : Unbounded_Unsigned;
                Result  : out Unbounded_Unsigned
             )  is
      Base  : Unbounded_Unsigned;
      Power : Half_Word := Right;
   begin
      if Is_Zero (Modulus) then
         raise Constraint_Error;
      elsif Is_One (Modulus) then
         Erase (Result);
         return;
      elsif Is_Zero (Left) then
         if Right = 0 then
            Set (Result, 1);
         else
            Erase (Result);
         end if;
         return;
      elsif Is_One (Left) then
         Set (Result, 1);
         return;
      end if;
      Set (Base, Left);
      Modulo (Base, Modulus);
      if Is_Zero (Base) then
         Erase (Result);
         return;
      end if;
      if (Power and 1) = 1 then
         Set (Result, Base);
      else
         Set (Result, 1);
      end if;
      while Power > 1 loop
         Power := Shift_Right (Power, 1);
         Base  := Square (Base);
         Modulo (Base, Modulus);
         if (Power and 1) /= 0 then
            Mul (Result, Base);
            Result := Result mod Modulus;
         end if;
      end loop;
   end Mod_Pow;

   function Mod_Pow_By_Power_Of_Two
            (  Left, Right : Unbounded_Unsigned;
               Modulus     : Bit_Count
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Set (Result, Left);
      Mod_Pow_By_Power_Of_Two (Left, Right, Modulus, Result);
      return Result;
   end Mod_Pow_By_Power_Of_Two;

   procedure Mod_Pow_By_Power_Of_Two
             (  Left    : Unbounded_Unsigned;
                Right   : Unbounded_Unsigned;
                Modulus : Bit_Count;
                Result  : out Unbounded_Unsigned
             )  is
      Base : Unbounded_Unsigned;
      X    : Unbounded_Unsigned;
   begin
      Set (Base, Left);
      Modulo_By_Power_Of_Two (Base, Modulus);
      if Is_Odd (Right) then
         Set (Result, Base);
      else
         Set (Result, 1);
      end if;
      for Bit in 2..Get_MSB (Right) loop
         Square (Base, X);
         Swap (Base, X);
         Modulo_By_Power_Of_Two (Base, Modulus);
         if Get_Bit (Right, Bit) then
            Mul (Result, Base, X);
            Swap (Result, X);
            Modulo_By_Power_Of_Two (Result, Modulus);
         end if;
      end loop;
   end Mod_Pow_By_Power_Of_Two;

   procedure Mod_Pow_By_Power_Of_Two
             (  Left    : Unbounded_Unsigned;
                Right   : Half_Word;
                Modulus : Bit_Count;
                Result  : out Unbounded_Unsigned
             )  is
      Base : Unbounded_Unsigned;
      X    : Unbounded_Unsigned;
      Bits : Half_Word := Right;
   begin
      Set (Base, Left);
      Modulo_By_Power_Of_Two (Base, Modulus);
      if (Right and 1) /= 0 then
         Set (Result, Base);
      else
         Set (Result, 1);
      end if;
      while Bits > 1 loop
         Bits := Shift_Right (Bits, 1);
         Square (Base, X);
         Swap (Base, X);
         Modulo_By_Power_Of_Two (Base, Modulus);
         if (Bits and 1) /= 0 then
            Mul (Result, Base, X);
            Swap (Result, X);
            Modulo_By_Power_Of_Two (Result, Modulus);
         end if;
      end loop;
   end Mod_Pow_By_Power_Of_Two;

   function Mod_Pow_By_Power_Of_Two
            (  Left    : Unbounded_Unsigned;
               Right   : Half_Word;
               Modulus : Bit_Count
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Set (Result, Left);
      Mod_Pow_By_Power_Of_Two (Left, Right, Modulus, Result);
      return Result;
   end Mod_Pow_By_Power_Of_Two;

   function Mod_Pow_Of_Two
            (  Power   : Unbounded_Unsigned;
               Modulus : Unbounded_Unsigned
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Mod_Pow_Of_Two (Power, Modulus, Result);
      return Result;
   end Mod_Pow_Of_Two;

   procedure Mod_Pow_Of_Two
             (  Power   : Unbounded_Unsigned;
                Modulus : Unbounded_Unsigned;
                Result  : out Unbounded_Unsigned
             )  is
      M : Bit_Count;
   begin
      if Is_Zero (Modulus) then
         raise Constraint_Error;
      elsif Is_One (Modulus) then
         Erase (Result);
         return;
      elsif Is_Zero (Power) then
         Set (Result, 1);
         return;
      end if;
      M := Get_MSB (Modulus);
      --
      -- Checking for powers less or equal to modulus
      --
      case Compare (Power, Half_Word (M - 1)) is
         when Less =>
            Set
            (  Result,
               Power_Of_Two (Bit_Count (To_Half_Word (Power)))
            );
            return;
         when Equal => -- 2**N <> Modulus
            if Is_Power_Of_Two (Modulus) then
               Erase (Result);
            else
               Set
               (  Result,
                  Power_Of_Two (Bit_Count (To_Half_Word (Power)))
               );
            end if;
            return;
         when Greater =>
            null;
      end case;
      --
      -- Checking if the modulus has a simple form
      --
      declare
         Remainder : Mod_2_Remainder;
      begin
         Log2 (Modulus, M, Remainder);
         case Remainder is
            when -1 =>
               --  Modulus = 2 ** M - 1
               --     2 ** M         (mod Modulus) =
               --   = 2 ** M - 1 + 1 (mod Modulus) =
               --   = 1              (mod Modulus)
               --
               --  2 ** N = 2 ** (M * (N / M) + N mod M) =
               --         = 2 ** M ** (N / M) * 2 ** (N mod M) =
               --         = 2 ** (N mod M) (mod Modulus)
               --
               Power_Of_Two
               (  Bit_Count (Power mod Half_Word (M)),
                  Result
               );
--             Mod_Pow (Two, Power mod Half_Word (M), Modulus, Result);
               return;
            when 0 =>
               -- Modulus = 2 ** M
               --    2 ** M (mod Modulus) = 0
               Erase (Result);
               return;
            when 1 =>
               --  Modulus = 2 ** M + 1
               --     2 ** M         (mod Modulus) =
               --   = 2 ** M + 1 - 1 (mod Modulus) =
               --   = -1             (mod Modulus)
               --
               --  2 ** N = 2 ** (M * (N / M) + N mod M) =
               --         = 2 ** M ** (N / M) * 2 ** (N mod M) =
               --         =-1 ** (N / M) * 2 ** (N mod M) (mod Modulus)
               --
               declare
                  Q : Unbounded_Unsigned;
                  R : Half_Word;
               begin
                  Set (Q, Power);
                  Div (Q, Half_Word (M), R);
                  if Is_Even (Q) then
--                   Mod_Pow (Two, R, Modulus, Result);
                     Power_Of_Two (Bit_Count (R), Result);
                  else -- -1 = Modulus - 1 = 2 ** M (mod Modulus)
                     Mod_Pow (Two, R + Half_Word (M), Modulus, Result);
                  end if;
                  return;
               end;
            when others =>
               null;
         end case;
      end;
      --
      -- General case, Modulus is  not 2 ** M | 2 ** M + 1 | 2 ** M + 1,
      -- we are using squaring 2 ** M to reach 2 ** N.
      --
      -- 2 ** N = 2 ** (M * (N / M) + N mod M) =
      --        = (2 ** M) ** (N / M) * 2 ** (N mod M)   (mod Modulus)
      --
      --      N / M = Sum { Ai * 2 ** I }
      --               i
      --
      -- 2 ** N = (2 ** M) ** Sum { Ai * 2 ** I } * 2 ** (N mod M) =
      --                       i
      --        = Prod { (2 ** M) ** 2) ** I } * 2 ** (N mod M)
      --            i | Ai=1
      --
      declare
         Base     : Unbounded_Unsigned;
         Exponent : Unbounded_Unsigned;
         X        : Unbounded_Unsigned;
         R        : Half_Word;
         Bits     : Bit_Count := 2 ** (Log2 (Half_Word (M)));
      begin
         if Bits < M then
            Bits := Bits * 2;
         end if;
         Set (Exponent, Power);
         Div (Exponent, Half_Word (Bits), R);
         if (  Set_Bits_Count (Power)
            <  Set_Bits_Count (Exponent) + Set_Bits_Count (R)
            )  then
            --
            -- The number of set  bits will be greater  so that a direct
            -- exponentiation will be faster.
            --
            Mod_Pow (Two, Power, Modulus, Result);
            return;
         end if;
         Power_Of_Two (Bits, Base);
         Sub (Base, Modulus);
         if Is_Odd (Exponent) then
            Mod_Pow (Two, Half_Word (Bits) + R, Modulus, Result);
         else
            Mod_Pow (Two, R, Modulus, Result);
         end if;
         for Bit in 2..Get_MSB (Exponent) loop
            Square (Base, X);
            Swap (Base, X);
            Modulo (Base, Modulus);
            if Get_Bit (Exponent, Bit) then
               Mul (Result, Base);
               Modulo (Result, Modulus);
            end if;
         end loop;
      end;
   end Mod_Pow_Of_Two;

   procedure Mul
             (  Multiplicand : in out Unbounded_Unsigned;
                Multiplier   : Half_Word
             )  is
      Length : Digit_Offset renames Multiplicand.Length;
      Carry  : Word := 0;
   begin
      if Length = 0 or else Multiplier = 1 then
         return;
      elsif Multiplier = 0 then
         Erase (Multiplicand);
         return;
      end if;
      Clone (Multiplicand.Value, Multiplicand.Length);
      declare
         Data : Half_Word_Array renames Multiplicand.Value.Data;
      begin
         for Index in 1..Length loop
            Carry := Word (Data (Index)) * Word (Multiplier) + Carry;
            Data (Index) := Half_Word (Carry and Half_Word_Mask);
            Carry := Shift_Right (Carry, Bit_Width);
         end loop;
      end;
      if Carry /= 0 then
         Length := Length + 1;
         Put (Multiplicand.Value, Length, Half_Word (Carry));
      end if;
   end Mul;

   procedure Mul
             (  Multiplicand : in out Unbounded_Unsigned;
                Multiplier   : Unbounded_Unsigned
             )  is
   begin
      if Multiplicand.Length = 0 or else Multiplier.Length = 0 then
         Erase (Multiplicand);
      elsif Multiplier.Length = 1 then
         Mul (Multiplicand, Multiplier.Value.Data (1));
      elsif Multiplicand.Length = 1 then
         declare
            Data : constant Half_Word := Multiplicand.Value.Data (1);
         begin
            Multiplicand := Multiplier;
            Mul (Multiplicand, Data);
         end;
      else
         Multiplicand :=
            Mul_Karatsuba (Multiplicand, Multiplier, Mul_Threshold);
      end if;
   end Mul;

   procedure Mul
             (  Multiplicand : Unbounded_Unsigned;
                Multiplier   : Half_Word;
                Result       : out Unbounded_Unsigned
             )  is
   begin
      Set (Result, Multiplicand);
      Mul (Result, Multiplier);
   end Mul;

   procedure Mul
             (  Multiplicand : Unbounded_Unsigned;
                Multiplier   : Unbounded_Unsigned;
                Result       : out Unbounded_Unsigned
             )  is
   begin
      if Multiplicand.Length = 0 or else Multiplier.Length = 0 then
         Erase (Result);
      elsif Multiplier.Length = 1 then
         Set (Result, Multiplicand);
         Mul (Result, Multiplier.Value.Data (1));
      elsif Multiplicand.Length = 1 then
         Set (Result, Multiplier);
         Mul (Result, Multiplicand.Value.Data (1));
      else
         Mul_Karatsuba
         (  Multiplicand,
            Multiplier,
            Mul_Threshold,
            Result
         );
      end if;
   end Mul;

   procedure Mul_By_Power_of_Two
             (  Multiplicand : in out Unbounded_Unsigned;
                Power        : Bit_Count
             )  is
      subtype Half_Word_Shift is Natural range 0..Bit_Width - 1;
      procedure Do_Shift
                (  Destination : in out Half_Word_Array;
                   Source      : Half_Word_Array;
                   Shift       : Half_Word_Shift
                )  is
     --
     -- Destination : |____|____|____|____|____|____|
     --                ///////////////////////////
     -- Source:       |____|____|____|____|____|____|
     --
      begin
         if Shift = 0 then
            Destination := Source;
         else
            declare
               To    : Digit_Offset := Destination'Last;
               Carry : Word :=
                       Shift_Left (Word (Source (Source'Last)), Shift);
            begin
               if Carry >= Half_Word_Modulus then
                  Destination (To) :=
                     Half_Word (Shift_Right (Carry, Bit_Width));
                  To := To - 1;
               end if;
               Carry := Carry and Half_Word_Mask;
               for Index in reverse Source'First..Source'Last - 1 loop
                  Carry := Shift_Left (Word (Source (Index)), Shift)
                         + Shift_Left (Carry, Bit_Width);
                  Destination (To) :=
                     Half_Word (Shift_Right (Carry, Bit_Width));
                  Carry := Carry and Half_Word_Mask;
                  To := To - 1;
               end loop;
               if To = Destination'First then
                  Destination (To) := Half_Word (Carry);
               end if;
            end;
         end if;
      end Do_Shift;
   begin
      if Power > 0 and then Multiplicand.Length > 0 then
         Clone (Multiplicand.Value, Multiplicand.Length);
         declare
            Width  : constant Bit_Count :=
                        Get_MSB (Multiplicand) + Power;
            Shift  : constant Digit_Offset :=
                     Digit_Offset (Power / Bit_Width);
            Length : constant Digit_Offset :=
                     Digit_Offset ((Width + Bit_Width - 1) / Bit_Width);
            Data   : Half_Word_Array renames Multiplicand.Value.Data;
         begin
            if Length <= Multiplicand.Value.Size and then
               Load (Multiplicand.Value.Count'Access) = 1 then
               Do_Shift
               (  Data (Shift + 1..Length),
                  Data (1..Multiplicand.Length),
                  Half_Word_Shift (Power mod Bit_Width)
               );
            else
               declare
                  Ptr : Vector_Ptr := new Vector (Length);
               begin
                  Do_Shift
                  (  Ptr.Data (Shift + 1..Length),
                     Data     (1..Multiplicand.Length),
                     Half_Word_Shift (Power mod Bit_Width)
                  );
                  Release (Multiplicand.Value);
                  Multiplicand.Value := Ptr;
               exception
                  when others =>
                     Free (Ptr);
                     raise;
               end;
            end if;
            declare
               Data : Half_Word_Array renames Multiplicand.Value.Data;
            begin
               for Index in 1..Shift loop
                  Data (Index) := 0;
               end loop;
               Multiplicand.Length := Length;
            end;
         end;
      end if;
   end Mul_By_Power_of_Two;

   function Mul_By_Power_of_Two
            (  Multiplicand : Unbounded_Unsigned;
               Power        : Bit_Count
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned := Multiplicand;
   begin
      Mul_By_Power_of_Two (Result, Power);
      return Result;
   end Mul_By_Power_of_Two;

   procedure Mul_Classroom
             (  Left   : Half_Word_Array;
                Right  : Half_Word_Array;
                Result : out Unbounded_Unsigned
             )  is
      Length_1 : constant Digit_Offset := Left'Length;
      Length_2 : constant Digit_Offset := Right'Length;
      Size     : constant Digit_Offset := Length_1 + Length_2;
   begin
      Allocate (Result.Value, Size);
      declare
         Length : Digit_Offset    renames Result.Length;
         Data   : Half_Word_Array renames Result.Value.Data;
      begin
         Length := Size;
         for Index in 1..Length loop
            Data (Index) := 0;
         end loop;
         for Index_1 in Left'Range loop
            declare
               This : constant Word := Word (Left (Index_1));
            begin
               for Index_2 in Right'Range loop
                  declare
                     Carry : Word := This * Word (Right (Index_2));
                  begin
                     for Index in Index_1 - Left'First + 1
                                + Index_2 - Right'First
                                ..Size
                     loop
                        Carry := Word (Data (Index)) + Carry;
                        Data (Index) :=
                           Half_Word (Carry and Half_Word_Mask);
                        Carry := Shift_Right (Carry, Bit_Width);
                        exit when Carry = 0;
                     end loop;
                  end;
               end loop;
            end;
         end loop;
         while Length > 0 and then Data (Length) = 0 loop
            Length := Length - 1;
         end loop;
      end;
   end Mul_Classroom;

   function Mul_Classroom (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      if Left.Length = 0 or else Right.Length = 0 then
         return Result;
      else
         Mul_Classroom
         (  Left.Value.Data  (1..Left.Length),
            Right.Value.Data (1..Right.Length),
            Result
         );
         return Result;
      end if;
   end Mul_Classroom;

   procedure Mul_Classroom
             (  Left, Right : Unbounded_Unsigned;
                Result      : out Unbounded_Unsigned
             )  is
   begin
      if Left.Length = 0 or else Right.Length = 0 then
         Erase (Result);
      else
         Mul_Classroom
         (  Left.Value.Data  (1..Left.Length),
            Right.Value.Data (1..Right.Length),
            Result
         );
      end if;
   end Mul_Classroom;

   function Mul_Karatsuba
            (  Left, Right : Unbounded_Unsigned;
               Threshold   : Digit_Count
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Mul_Karatsuba (Left, Right, Threshold, Result);
      return Result;
   end Mul_Karatsuba;

   procedure Mul_Karatsuba
             (  Left, Right : Unbounded_Unsigned;
                Threshold   : Digit_Count;
                Result      : out Unbounded_Unsigned
             )  is
   begin
      if Left.Length = 0 or else Right.Length = 0 then
         Erase (Result);
      elsif Left.Length + Right.Length <= 2 * Threshold     or
            Left.Length  <= Digit_Offset'Max (1, Threshold) or
            Right.Length <= Digit_Offset'Max (1, Threshold) then
         Mul_Classroom (Left, Right, Result);
      else
         declare
            procedure Multiply
                      (  Left   : Half_Word_Array;
                         Right  : Half_Word_Array;
                         Result : out Unbounded_Unsigned
                      )  is
               L : constant Digit_Offset :=
                      Digit_Offset'Max (Left'Length, Right'Length);
               N : constant Digit_Offset := L / 2;
            begin
               if (  Left'Length + Right'Length <= 2 * Threshold
                  or else
                     Left'Length  <= Digit_Offset'Max (N, Threshold)
                  or else
                     Right'Length <= Digit_Offset'Max (N, Threshold)
                  )  then
                  Mul_Classroom (Left, Right, Result);
               else
                  declare
                     Z0 : Unbounded_Unsigned renames Result;
                     Z1 : Unbounded_Unsigned;
                     Z2 : Unbounded_Unsigned;
                     L1 : Half_Word_Array renames
                             Left (Left'First ..Left'First + N - 1);
                     H1 : Half_Word_Array renames
                             Left (Left'First + N..Left'Last);
                     L2 : Half_Word_Array renames
                             Right (Right'First..Right'First + N - 1);
                     H2 : Half_Word_Array renames
                             Right (Right'First + N..Right'Last);
                  begin
                     Set (Z0, L1);
                     Add (Z0, H1);
                     Set (Z2, L2);
                     Add (Z2, H2);
                     if Z0.Length > 0 and then Z2.Length > 0 then
                        Multiply
                        (  Z0.Value.Data (1..Z0.Length),
                           Z2.Value.Data (1..Z2.Length),
                           Z1
                        );
                     end if;
                     Multiply (L1, L2, Z0);
                     Multiply (H1, H2, Z2);
                     Sub (Z1, Z0);
                     Sub (Z1, Z2);
                     Add (Z0, Z1, N);
                     Add (Z0, Z2, N * 2);
                  end;
               end if;
            end;
         begin
            Multiply
            (  Left.Value.Data  (1..Left.Length),
               Right.Value.Data (1..Right.Length),
               Result
            );
         end;
      end if;
   end Mul_Karatsuba;

   procedure Neg (X : in out Signed) is
      pragma Inline (Neg);
   begin
      if X.Value.Length > 0 then
         X.Sign := not X.Sign;
      end if;
   end Neg;

   function Phi (Left : Unbounded_Unsigned) return Unbounded_Unsigned is
      Result  : Unbounded_Unsigned;
      I, N, R : Unbounded_Unsigned;
   begin
      if Is_Zero (Left) then
         raise Constraint_Error;
      end if;
      Set (Result, Left);
      Set (N, Left);
      Set (I, 2);
      while Compare (Square (I), N) /= Greater loop
         if Is_Zero (N mod I) then
            loop
               Div (N, I);
               exit when not Is_Zero (N mod I);
            end loop;
            Set (R, Result);
            Div (R, I);
            Sub (Result, R);
         end if;
         if Is_Two (I) then
            Set (I, 1);
         end if;
         Add (I, 2);
      end loop;
      if Compare (N, 1) = Greater then
         Set (R, Result);
         Div (R, N);
         Sub (Result, R);
      end if;
      return Result;
   end Phi;

   procedure Power_of_Two
             (  Power  : Bit_Count;
                Result : out Unbounded_Unsigned
             )  is
      Size : constant Digit_Offset :=
                      Digit_Offset (Power / Bit_Width + 1);
   begin
      Allocate (Result.Value, Size);
      declare
         Data : Half_Word_Array renames Result.Value.Data (1..Size);
      begin
         for Index in 1..Data'Last - 1 loop
            Data (Index) := 0;
         end loop;
         Data (Data'Last) :=
            Shift_Left (Half_Word'(1), Natural (Power mod Bit_Width));
      end;
      Result.Length := Size;
   end Power_of_Two;

   function Power_of_Two (Power : Bit_Count)
      return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Power_Of_Two (Power, Result);
      return Result;
   end Power_of_Two;

   procedure Put
             (  Container : in out Vector_Ptr;
                Index     : Digit_Count;
                Element   : Half_Word
             )  is
   begin
      if Container = null then
         Container :=
            new Vector (Digit_Count'Max (Index, Minimal_Size));
      elsif Index > Container.Size then
         declare
            Inc : Digit_Offset;
            Ptr : Vector_Ptr;
         begin
            Inc := Digit_Count'Max
                   (  Minimal_Size,
                      (Container.Size * Increment) / 100
                   );
            Ptr := new Vector
                       (  Digit_Count'Max
                          (  Index,
                             Container.Size + Inc
                       )  );
            Ptr.Data (1..Container.Data'Length) := Container.Data;
            Release (Container);
            Container := Ptr;
         exception
            when others =>
               Free (Ptr);
               raise;
         end;
      end if;
      Container.Data (Index) := Element;
   end Put;

   procedure Release (Container : in out Vector_Ptr) is
   begin
      if Container /= null then
         case Load (Container.Count'Access) is
            when 0 =>
               raise Program_Error;
            when 1 =>
               Free (Container);
            when others =>
               declare
                  Dummy : Count_Type;
               begin
                  Dummy := Dec (Container.Count'Access);
               end;
               Container := null;
         end case;
      end if;
   end Release;

   procedure Replace_Slice
             (  Left  : in out Unbounded_Unsigned;
                Right : Unbounded_Unsigned;
                From  : Bit_Position;
                To    : Bit_Position
             )  is
   begin
      if Right.Length = 0 then
         Clear_Slice (Left, From, To);
         return;
      elsif From > To then
         return;
      elsif From = To then
         if Get_Bit (Right, 1) then
            Set_Bit (Left, From);
         else
            Clear_Bit (Left, From);
         end if;
         return;
      end if;
      declare
         Offset : constant Digit_Offset :=
                     Digit_Offset ((From - 1) / Bit_Width);
         Shift  : constant Natural :=
                     Natural (Bit_Width - (From - 1) mod Bit_Width);
         First  : constant Digit_Count := Offset + 1;
         Source : Half_Word_Array renames
                     Right.Value.Data (1..Right.Length);
      --
      -- Get -- Source half-word
      --
      --    Index - The destination index
      --
      -- Returns :
      --
      --    Source word
      --
         function Get (Index : Digit_Count) return Half_Word is
            pragma Inline (Get);
            From : Digit_Offset := Index - Offset;
         begin
            if Shift = Bit_Width then -- Whole half-word
               if Index <= Source'Last then
                  return Source (From);
               else
                  return 0;
               end if;
           else
               declare
                  Result : Word;
               begin
                  if From <= Source'Last then
                     Result :=
                        Shift_Left (Word (Source (From)), Bit_Width);
                  else
                     Result := 0;
                  end if;
                  From := From - 1;
                  if From in Source'Range then
                     Result := Result or Word (Source (From));
                  end if;
                  return
                     Half_Word
                     (  Shift_Right (Result, Shift) and Half_Word_Mask
                     );
               end;
            end if;
         end Get;
      --
      -- Set -- Destination bits
      --
      --    Index - The destination index
      --    Mask  - Bits to be replaced
      --
         procedure Set (Index : Digit_Count; Mask : Half_Word) is
            pragma Inline (Set);
         begin
            if Index > Left.Length then
               Put (Left.Value, Index, Get (Index) and Mask);
               declare
                  Data : Half_Word_Array renames Left.Value.Data;
               begin
                  for I in Left.Length + 1..Index - 1 loop
                     Data (I) := 0;
                  end loop;
                  Left.Length := Index;
               end;
            else
               declare
                  Data : Half_Word renames Left.Value.Data (Index);
               begin
                  Data := (Data and not Mask) or (Get (Index) and Mask);
               end;
            end if;
         end Set;
      --
      -- Set -- Destination bits in complete half-words
      --
      --    From - The first destination/source index
      --    To   - The last index
      --
         procedure Set (From, To : Digit_Count) is
            pragma Inline (Set);
            Data : Half_Word_Array renames
                      Left.Value.Data (1..Left.Length);
         begin
            for Index in From..To loop
               Data (Index) := Get (Index);
            end loop;
         end Set;

         Last      : constant Digit_Count :=
                        Digit_Count ((To - 1) / Bit_Width + 1);
         From_At   : constant Natural :=
                        Natural (From - 1) mod Bit_Width;
         To_At     : constant Natural :=
                        Natural (To   - 1) mod Bit_Width;
         From_Mask : constant Half_Word := not (2 ** From_At - 1);
         To_Mask   : constant Half_Word := 2 ** (To_At + 1) - 1;
      begin
         if Left.Length = 0 then
            Allocate (Left.Value, Last);
         else
            Clone (Left.Value, Left.Length);
         end if;
         if First = Last then
            Set (First, From_Mask and To_Mask);
         elsif From_At = 0 then
            if To_At = Bit_Width - 1 then
               if Left.Length < Last then
                  Put (Left.Value, Last, 0);
                  declare
                     Data : Half_Word_Array renames
                               Left.Value.Data (1..Last);
                  begin
                     for Index in Left.Length + 1..Last - 1 loop
                        Data (Index) := 0;
                     end loop;
                  end;
                  Left.Length := Last;
               end if;
               Set (First, Last);
            else
               Set (Last, To_Mask);
               Set (First, Last - 1);
            end if;
         else
            Set (Last,  To_Mask);
            Set (First, From_Mask);
            Set (First + 1, Last - 1);
         end if;
      end;
      if Left.Length = 0 then
         return;
      end if;
      declare
         Length : Digit_Offset    renames Left.Length;
         Data   : Half_Word_Array renames Left.Value.Data (1..Length);
      begin
         while Length > 0 and then Data (Length) = 0 loop
            Length := Length - 1;
         end loop;
      end;
   end Replace_Slice;

   procedure Set
             (  Destination : in out Unbounded_Unsigned;
                Source      : Half_Word
             )  is
   begin
      if Source = 0 then
         Destination.Length := 0;
      elsif Destination.Value = null or else
            Load (Destination.Value.Count'Access) > 1 then -- Must clone
         Release (Destination.Value);
         Destination.Length := 1;
         Destination.Value  := new Vector'(1, 1, (1..1 => Source));
      else
         Destination.Length := 1;
         Destination.Value.Data (1) := Source;
      end if;
   end Set;

   procedure Set_Bit
             (  Left     : in out Unbounded_Unsigned;
                Position : Bit_Position
             )  is
      Power : constant Bit_Count := Position - 1;
      Mask  : constant Half_Word := 2 ** Natural (Power mod Bit_Width);
      Index : constant Digit_Offset :=
                       Digit_Offset (Power / Bit_Width + 1);
  begin
      if Left.Length = 0 then
         Allocate (Left.Value, Index);
         Left.Length := Index;
         declare
            Data : Half_Word_Array renames Left.Value.Data;
         begin
            Data (Index) := Mask;
            for I in 1..Index - 1 loop
               Data (I) := 0;
            end loop;
         end;
      elsif Index > Left.Length then
         Clone (Left.Value, Left.Length);
         Put (Left.Value, Index, Mask);
         Left.Length := Index;
         declare
            Data : Half_Word_Array renames Left.Value.Data;
         begin
            for I in Left.Length + 1..Index - 1 loop
               Data (I) := 0;
            end loop;
         end;
      else
         if (Left.Value.Data (Index) and Mask) = 0 then
            Clone (Left.Value, Left.Length);
            declare
               Data : Half_Word renames Left.Value.Data (Index);
            begin
               Data := Data or Mask;
            end;
         end if;
      end if;
   end Set_Bit;

   function Set_Bits_Count (Left : Unbounded_Unsigned)
      return Bit_Count is
   begin
      if Left.Length = 0 then
         return 0;
      end if;
      declare
         Result : Bit_Count := 0;
         Data   : Half_Word_Array renames
                     Left.Value.Data (1..Left.Length);
      begin
         for Index in Data'Range loop
            declare
               Item : Half_Word := Data (Index);
            begin
               while Item > 0 loop
                  if (Item and 1) /= 0 then
                     Result := Result + 1;
                  end if;
                  Item := Shift_Right (Item, 1);
               end loop;
            end;
         end loop;
         return Result;
      end;
   end Set_Bits_Count;

   function Set_Bits_Count (Left : Half_Word) return Bit_Count is
      Result : Bit_Count := 0;
      Item   : Half_Word := Left;
   begin
      while Item > 0 loop
         if (Item and 1) /= 0 then
            Result := Result + 1;
         end if;
         Item := Shift_Right (Item, 1);
      end loop;
      return Result;
   end Set_Bits_Count;

   procedure Set_Word
             (  Destination : in out Unbounded_Unsigned;
                Source      : Word
             )  is
   begin
      if Source < Half_Word_Modulus then
         Set (Destination, Half_Word (Source));
      elsif Destination.Value = null   or else
            Destination.Value.Size = 1 or else
            Load (Destination.Value.Count'Access) > 1 then -- Must clone
         Release (Destination.Value);
         Destination.Length := 2;
         Destination.Value :=
            new Vector'
                (  2,
                   1,
                   (  Half_Word (Source and Half_Word_Mask),
                      Half_Word (Source / Half_Word_Modulus)
                )  );
      else
         Destination.Length := 2;
         Destination.Value.Data (1) :=
            Half_Word (Source and Half_Word_Mask);
         Destination.Value.Data (2) :=
            Half_Word (Source / Half_Word_Modulus);
      end if;
   end Set_Word;

   procedure Set
             (  Destination : in out Unbounded_Unsigned;
                Source      : Half_Word_Array
             )  is
   begin
      if Source'Length = 0 then
         Destination.Length := 0;
      elsif Destination.Value = null or else
            Load (Destination.Value.Count'Access) > 1 or else
            Destination.Value.Size < Source'Length then
         Release (Destination.Value);
         Destination.Value  := new Vector'(Source'Length, 1, Source);
         Destination.Length := Source'Length;
      else
         Destination.Length := Source'Length;
         Destination.Value.Data (1..Source'Length) := Source;
      end if;
   end Set;

   procedure Set
             (  Destination : in out Unbounded_Unsigned;
                Source      : Unbounded_Unsigned
             )  is
   begin
      if Source.Length = 0 then
         Destination.Length := 0;
      elsif Destination.Value = null                  or else
            Load (Destination.Value.Count'Access) > 1 or else
            Destination.Value.Size < Source.Length    then
         Release (Destination.Value);
         Destination.Length := Source.Length;
         Destination.Value  := Source.Value;
         declare
            Dummy : Count_Type;
         begin
            Dummy := Inc (Source.Value.Count'Access);
         end;
      else
         Destination.Length := Source.Length;
         Destination.Value.Data (1..Source.Length) :=
            Source.Value.Data (1..Source.Length);
      end if;
   end Set;

   procedure Set_Multiplication_Threshold (Threshold : Digit_Count) is
   begin
      Mul_Threshold := Threshold;
   end Set_Multiplication_Threshold;

   procedure Set_Wiping_Mode (On : Boolean) is
   begin
      if On then
         Free := Wiping_Deallocate'Access;
      else
         Free := Default_Deallocate'Access;
      end if;
   end Set_Wiping_Mode;

   procedure Set_Slice
             (  Left : in out Unbounded_Unsigned;
                From : Bit_Position;
                To   : Bit_Position
             )  is
      Length : Digit_Offset renames Left.Length;

      procedure Set (Index : Digit_Count; Mask : Half_Word) is
         pragma Inline (Set);
      begin
         if Index <= Length then
            Left.Value.Data (Index) := Left.Value.Data (Index) or Mask;
         else
            Put (Left.Value, Index, Mask);
            declare
               Data : Half_Word_Array renames Left.Value.Data;
            begin
               for I in Length + 1..Index - 1 loop
                  Data (I) := 0;
               end loop;
               Length := Index;
            end;
         end if;
      end Set;
   begin
      if From > To then
         return;
      elsif From = To then
         Set_Bit (Left, From);
         return;
      end if;
      declare
         First     : constant Digit_Count :=
                        Digit_Count ((From - 1) / Bit_Width + 1);
         Last      : constant Digit_Count :=
                        Digit_Count ((To - 1) / Bit_Width + 1);
         From_At   : constant Natural :=
                        Natural (From - 1) mod Bit_Width;
         To_At     : constant Natural :=
                        Natural (To   - 1) mod Bit_Width;
         From_Mask : constant Half_Word := not (2 ** From_At - 1);
         To_Mask   : constant Half_Word := 2 ** (To_At + 1) - 1;
      begin
         if Left.Length = 0 then
            Allocate (Left.Value, Last);
         else
            Clone (Left.Value, Left.Length);
         end if;
         if First = Last then
            Set (First, From_Mask and To_Mask);
         elsif From_At = 0 then
            if To_At = Bit_Width - 1 then
               Set (Last, Half_Word_Mask);
               declare
                  Data : Half_Word_Array renames
                            Left.Value.Data (1..Left.Length);
               begin
                  for Index in First..Last - 1 loop
                     Data (Index) := Half_Word_Mask;
                  end loop;
               end;
            else
               Set (Last, To_Mask);
               declare
                  Data : Half_Word_Array renames
                            Left.Value.Data (1..Left.Length);
               begin
                  for Index in First..Last - 1 loop
                     Data (Index) := Half_Word_Mask;
                  end loop;
               end;
            end if;
         else
            Set (Last, To_Mask);
            declare
               Data : Half_Word_Array renames
                         Left.Value.Data (1..Left.Length);
            begin
               Data (First) := Data (First) or From_Mask;
               for Index in First + 1..Last - 1 loop
                  Data (Index) := Half_Word_Mask;
               end loop;
            end;
         end if;
      end;
   end Set_Slice;

   procedure Shift_Left
             (  Left  : in out Unbounded_Unsigned;
                Shift : Digit_Offset
             )  is
      Length : constant Digit_Offset := Left.Length + Shift;
   begin
      if Shift = 0 or else Left.Length = 0 then
         return;
      elsif Length <= Left.Value.Size and then
            Load (Left.Value.Count'Access) = 1 then
         Clone (Left.Value, Left.Length);
         Left.Value.Data (Shift + 1..Length) :=
            Left.Value.Data (1..Left.Length);
      else
         declare
            Ptr : Vector_Ptr := new Vector (Length);
         begin
            Ptr.Data (Shift + 1..Length) :=
               Left.Value.Data (1..Left.Length);
            Release (Left.Value);
            Left.Value := Ptr;
         exception
            when others =>
               Free (Ptr);
               raise;
         end;
      end if;
      declare
         Data : Half_Word_Array renames Left.Value.Data;
      begin
         for Index in 1..Shift loop
            Data (Index) := 0;
         end loop;
      end;
      Left.Length := Length;
   end Shift_Left;

   procedure Shift_Right
             (  Left  : in out Unbounded_Unsigned;
                Shift : Digit_Offset
             )  is
   begin
      if Shift = 0 or else Left.Length = 0 then
         return;
      elsif Left.Length <= Shift then
         Left.Length := 0;
      else
         Clone (Left.Value, Left.Length);
         declare
            Length : constant Digit_Offset := Left.Length - Shift;
            Data   : Half_Word_Array renames Left.Value.Data;
         begin
            Data (1..Length) := Data (Shift + 1..Left.Length);
            Left.Length := Length;
         end;
      end if;
   end Shift_Right;

   procedure Sqrt
             (  Left      : Word;
                Root      : out Word;
                Remainder : out Word
             )  is
   begin
      case Left is
         when 0 =>
            Root      := 0;
            Remainder := 0;
         when 1 =>
            Root      := 1;
            Remainder := 0;
         when 2 =>
            Root      := 1;
            Remainder := 1;
         when 3 =>
            Root      := 1;
            Remainder := 2;
         when 4 =>
            Root      := 2;
            Remainder := 0;
         when others =>
            --
            -- Newton's method
            --
            --   X0 = N /2
            --
            --   Xk+1 = (Xk + N / Xk) / 2
            --
            declare
               This : Word := Left / 2;
               Next : Word;
            begin
               loop
                  Next := (This + Left / This) / 2;
                  if Next <= This then
                     if This - Next <= 1 and then
                        Next * Next <= Left then
                        Root := Next;
                        exit;
                     end if;
                  elsif Next - This <= 1 then
                     Root := This;
                     exit;
                  end if;
                  This := Next;
               end loop;
               Remainder := Left - Root * Root;
            end;
       end case;
   end Sqrt;

   function Sqrt (Left : Half_Word) return Half_Word is
      Root, Remainder : Word;
   begin
      Sqrt (Word (Left), Root, Remainder);
      return Half_Word (Root);
   end Sqrt;

   procedure Sqrt
             (  Left      : Half_Word;
                Root      : out Half_Word;
                Remainder : out Half_Word
             )  is
   begin
      Sqrt (Word (Left), Word (Root), Word (Remainder));
   end Sqrt;

   procedure Sqrt_Karatsuba
             (  Left      : Unbounded_Unsigned;
                Root      : out Unbounded_Unsigned;
                Remainder : out Unbounded_Unsigned
             )  is
   begin
      case Left.Length is
         when 0 =>
            Erase (Root);
            Erase (Remainder);
            return;
         when 1 =>
            declare
               S, R : Word;
            begin
               Sqrt (Word (Left.Value.Data (1)), S, R);
               Set_Word (Root, S);
               Set_Word (Remainder, R);
               return;
            end;
         when 2 =>
            declare
               S, R : Word;
            begin
               Sqrt
               (  (  Word (Left.Value.Data (2)) * Half_Word_Modulus
                  +  Word (Left.Value.Data (1))
                  ),
                  S,
                  R
               );
               Set_Word (Root, S);
               Set_Word (Remainder, R);
               return;
            end;
         when others =>
            null;
      end case;
      declare
         M : constant Bit_Count := Get_MSB (Left);
         K : constant Bit_Count := (M + 3) / 4;
         Q : Unbounded_Unsigned := Left;
         Shift : Bit_Count := 0;
      begin
         if K * 4 - M > 1 then
            Shift := 2;
            Mul_By_Power_Of_Two (Q, Shift);
         end if;
      --
      -- A0 + A1 * 2**K + A2 * 2**(2*K) + A3 * 2**(3*K)
      --
      -- sqrt (A3 * 2**K + A2) -> S1, R1
      --
      -- div (R1 * 2**K + A1, 2 * S1) -> Q, U
      -- S = S1 * 2**K + Q
      -- R = U * 2**K + A0 - Q * Q
      --
      -- if R < 0 then
      --    R := R + 2 * S - 1;
      --    S := S - 1;
      -- end if;
      --
         declare
            A0  : constant Unbounded_Unsigned :=
                           Get_Slice (Q, 1, K);
            A1  : constant Unbounded_Unsigned :=
                           Get_Slice (Q, K + 1, 2*K);
            A23 : Unbounded_Unsigned := Get_Slice (Q, 2*K + 1, 4*K);
            S1  : Unbounded_Unsigned;
            R1  : Unbounded_Unsigned;
            U   : Unbounded_Unsigned renames A23; -- Resuse
         begin
            Sqrt_Karatsuba (A23, S1, R1);

            Mul_By_Power_Of_Two (R1, K);
            Add (R1, A1);
            Mul_By_Power_Of_Two (S1, 1);
            Div (R1, S1, U);
            Q := R1;

            Mul_By_Power_Of_Two (S1, K - 1); -- Already multiplied by 2
            Add (S1, Q);
            Mul_By_Power_Of_Two (U, K);
            Add (U, A0);
            Mul (Q, Q);

            if U < Q then
               Add (U, S1);
               Add (U, S1);
               Sub (U, 1);
               Sub (S1, 1);
            end if;
            Sub (U, Q);
            if Shift > 0 then
               if S1.Length > 0 and then
                  (S1.Value.Data (1) and 1) = 1 then
                  --
                  -- Fixing remainder upon shift when root is odd
                  --
                  -- N * 4 = S1 * S1 + U
                  -- Let S2 = (S1 - 1) / 2 = S1 / 2 -- The root shifted
                  -- S1 = S2 * 2 + 1
                  -- N * 4 = (S2 * 2 + 1 ) *(S2 * 2 + 1) + U =
                  --       = 4 * S2 ** 2 + 4 * S2 + 1 + U
                  -- N = S2 ** 2 + S2 + (1 + u) / 4
                  --               ^^^^^^^^^^^^^^^^ Remainder
                  -- Thus:
                  --
                  -- U = (U + 1) / 4 + S1 / 2
                  --
                  Div_By_Power_Of_Two (S1, Shift / 2);
                  Add (U, 1);
                  Div_By_Power_Of_Two (U, 2);
                  Add (U, S1);
               else
                  Div_By_Power_Of_Two (S1, Shift / 2);
                  Div_By_Power_Of_Two (U, Shift);
               end if;
            end if;
            Root := S1;
            Remainder := U;
         end;
      end;
   end Sqrt_Karatsuba;

   function Sqrt (Left : Unbounded_Unsigned)
      return Unbounded_Unsigned is
      Root      : Unbounded_Unsigned;
      Remainder : Unbounded_Unsigned;
   begin
      Sqrt (Left, Root, Remainder);
      return Root;
   end Sqrt;

   procedure Sqrt
             (  Left      : Unbounded_Unsigned;
                Root      : out Unbounded_Unsigned;
                Remainder : out Unbounded_Unsigned
             )  is
   begin
      Sqrt_Karatsuba (Left, Root, Remainder);
   end Sqrt;

   function Square (Left : Unbounded_Unsigned)
      return Unbounded_Unsigned is
   begin
      if Left.Length = 0 then
         return Zero;
      elsif Left.Length <= Mul_Threshold then
         return Square_Classroom (Left);
      else
         return Square_Karatsuba (Left, Mul_Threshold);
      end if;
   end Square;

   procedure Square
             (  Left   : Unbounded_Unsigned;
                Result : out Unbounded_Unsigned
             )  is
   begin
      if Left.Length = 0 then
         Erase (Result);
      elsif Left.Length <= Mul_Threshold then
         Square_Classroom (Left, Result);
      else
         Square_Karatsuba (Left, Mul_Threshold, Result);
      end if;
   end Square;

   procedure Square_Classroom
             (  Left   : Half_Word_Array;
                Result : out Unbounded_Unsigned
             )  is
      Size : constant Digit_Count := Left'Length * 2;
   begin
      Allocate (Result.Value, Size);
      declare
         Length : Digit_Offset    renames Result.Length;
         Data   : Half_Word_Array renames Result.Value.Data;
      begin
         Length := Size;
         for Index in 1..Length loop
            Data (Index) := 0;
         end loop;
         for Index_1 in Left'Range loop
            declare
               This : constant Word := Word (Left (Index_1));
            begin
               for Index_2 in Left'First..Index_1 - 1 loop
                  declare
                     Carry : Word := This * Word (Left (Index_2));
                  begin
                     for Index in Index_1 + Index_2 + 1 - Left'First * 2
                                ..Size
                     loop
                        Carry := Word (Data (Index)) + Carry;
                        Data (Index) :=
                           Half_Word (Carry and Half_Word_Mask);
                        Carry := Shift_Right (Carry, Bit_Width);
                        exit when Carry = 0;
                     end loop;
                  end;
               end loop;
            end;
         end loop;
         Mul_By_Power_Of_Two (Result, 1);
         for Index_1 in Left'Range loop
            declare
               This  : constant Word := Word (Left (Index_1));
               Carry : Word := This * This;
            begin
               for Index in (Index_1 - Left'First) * 2 + 1..Size loop
                  Carry := Word (Data (Index)) + Carry;
                  Data (Index) := Half_Word (Carry and Half_Word_Mask);
                  Carry := Shift_Right (Carry, Bit_Width);
                  exit when Carry = 0;
               end loop;
            end;
         end loop;
         while Length > 0 and then Data (Length) = 0 loop
            Length := Length - 1;
         end loop;
      end;
   end Square_Classroom;

   function Square_Classroom (Left : Unbounded_Unsigned)
      return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Square_Classroom (Left, Result);
      return Result;
   end Square_Classroom;

   procedure Square_Classroom
             (  Left   : Unbounded_Unsigned;
                Result : out Unbounded_Unsigned
             )  is
   begin
      if Left.Length = 0 then
         Erase (Result);
      elsif Left.Length = 1 then
         declare
            Value  : constant Word := Word (Left.Value.Data (1));
            Square : constant Word := Value * Value;
         begin
           Set_Word (Result, Square);
         end;
      else
         Square_Classroom (Left.Value.Data (1..Left.Length), Result);
      end if;
   end Square_Classroom;

   function Square_Karatsuba
            (  Left      : Unbounded_Unsigned;
               Threshold : Digit_Count
            )  return Unbounded_Unsigned is
      Result : Unbounded_Unsigned;
   begin
      Square_Karatsuba (Left, Threshold, Result);
      return Result;
   end Square_Karatsuba;

   procedure Square_Karatsuba
             (  Left      : Unbounded_Unsigned;
                Threshold : Digit_Count;
                Result    : out Unbounded_Unsigned
             )  is
   begin
      if Left.Length = 0 then
         Erase (Result);
      elsif Left.Length <= Digit_Offset'Max (1, Threshold) then
         Square_Classroom (Left, Result);
      else
         Allocate (Result.Value, Left.Length * 2);
         declare
            procedure Square
                      (  Left   : Half_Word_Array;
                         Result : out Unbounded_Unsigned
                      )  is
            begin
               if Left'Length <= Digit_Offset'Max (1, Threshold) then
                  Square_Classroom (Left, Result);
               else
                  declare
                     N  : constant Digit_Offset := Left'Length / 2;
                     Z0 : Unbounded_Unsigned renames Result;
                     Z1 : Unbounded_Unsigned;
                     Z2 : Unbounded_Unsigned;
                     L  : Half_Word_Array renames
                             Left (Left'First ..Left'First + N - 1);
                     H  : Half_Word_Array renames
                             Left (Left'First + N..Left'Last);
                  begin
                     Set (Z0, L);
                     Add (Z0, H);           -- Z1 = (L + H) ** 2
                     if Z0.Length > 0 then
                        Square (Z0.Value.Data (1..Z0.Length), Z1);
                     end if;
                     Square (L, Z0);        -- Z0 = L ** 2
                     Square (H, Z2);        -- Z2 = H ** 2
                     Sub (Z1, Z0);
                     Sub (Z1, Z2);          -- Z1 = Z1 - Z0 - Z2
                     Add (Z0, Z1, N);       -- Z0 = Z0 + Z1 * 2**K
                     Add (Z0, Z2, N * 2);   -- Z0 = Z0 + Z2 * 2**(2 * K)
                  end;
               end if;
            end;
         begin
            Square (Left.Value.Data (1..Left.Length), Result);
         end;
      end if;
   end Square_Karatsuba;

   procedure Sub
             (  Minuend    : in out Unbounded_Unsigned;
                Subtrahend : Half_Word
             )  is
   begin
      if Subtrahend = 0 then
         return;
      elsif Minuend.Length = 0 then
         raise Constraint_Error;
      elsif Minuend.Length = 1 then
         declare
            Data : constant Half_Word := Minuend.Value.Data (1);
         begin
            if Data > Subtrahend then
               Clone (Minuend.Value, Minuend.Length);
               Minuend.Value.Data (1) := Data - Subtrahend;
            elsif Data = Subtrahend then
               Minuend.Length := 0;
            else
               raise Constraint_Error;
            end if;
         end;
      else
         Clone (Minuend.Value, Minuend.Length);
         declare
            Length : Digit_Offset renames Minuend.Length;
            Borrow : Word := Word (Subtrahend);
            Data   : Half_Word_Array renames Minuend.Value.Data;
         begin
            for Index in 1..Length loop
               if Word (Data (Index)) >= Borrow then
                  Data (Index) := Data (Index) - Half_Word (Borrow);
                  exit;
               end if;
               Borrow :=
                  Word (Data (Index)) + Half_Word_Modulus - Borrow;
               Data (Index) :=
                  Half_Word (Borrow and Half_Word_Mask);
               Borrow := Shift_Right (Borrow, Bit_Width) + 1;
                exit when Borrow = 0;
            end loop;
            if Data (Length) = 0 then
               Length := Length - 1;
            end if;
         end;
      end if;
   end Sub;
--
-- Sub -- Internal procedure
--
   procedure Sub
             (  Minuend    : in out Unbounded_Unsigned;
                Subtrahend : Half_Word_Array;
                Borrow     : out Half_Word
             )  is
   begin
      if Subtrahend'Length = 0 then
         Borrow := 0;
         return;
      elsif Minuend.Length < Subtrahend'Length then
         if Minuend.Length = 0 and then Subtrahend'Length = 1 then
            Set
            (  Minuend,
               Half_Word
               (  Half_Word_Modulus
               -  Word (Subtrahend (Subtrahend'First))
            )  );
            Borrow := 1;
            return;
         end if;
         Clone (Minuend.Value, Minuend.Length);
         for Index in Minuend.Length + 1..Subtrahend'Length loop
            Put (Minuend.Value, Index, 0);
         end loop;
         Minuend.Length := Subtrahend'Length;
      else
         Clone (Minuend.Value, Minuend.Length);
      end if;
      declare
         Result : Half_Word       renames Borrow;
         Length : Digit_Offset    renames Minuend.Length;
         Left   : Half_Word_Array renames
                     Minuend.Value.Data (1..Minuend.Length);
         To     : Digit_Offset := 1;
         Borrow : Word := 0;
      begin
         for Index in Subtrahend'Range loop
            Borrow := Borrow + Word (Subtrahend (Index));
            if Word (Left (To)) >= Borrow then
               Left (To) := Left (To) - Half_Word (Borrow);
               Borrow := 0;
            else
               Borrow := Word (Left (To)) + Half_Word_Modulus - Borrow;
               Left (To) := Half_Word (Borrow and Half_Word_Mask);
               Borrow := Shift_Right (Borrow, Bit_Width) + 1;
            end if;
            To := To + 1;
         end loop;
         if Borrow /= 0 then
            while To <= Left'Last loop
               if Word (Left (To)) >= Borrow then
                  Left (To) := Left (To) - Half_Word (Borrow);
                  Borrow := 0;
                  exit;
               end if;
               Left (To) :=
                  Half_Word
                  (  Word (Left (To)) + Half_Word_Modulus - Borrow
                  );
               To := To + 1;
               Borrow := 1;
            end loop;
            while Length > 0 and then
                  Left (Length) = Half_Word_Mask loop
               Length := Length - 1;
            end loop;
         end if;
         while Length > 0 and then Left (Length) = 0 loop
            Length := Length - 1;
         end loop;
         Result := Half_Word (Borrow);
      end;
   end Sub;

   procedure Sub
             (  Minuend    : in out Unbounded_Unsigned;
                Subtrahend : Unbounded_Unsigned
             )  is
      Borrow : Half_Word;
   begin
      if Minuend.Length < Subtrahend.Length then
         raise Constraint_Error;
      elsif Subtrahend.Length > 0 then
         Sub
         (  Minuend,
            Subtrahend.Value.Data (1..Subtrahend.Length),
            Borrow
         );
         if Borrow > 0 then
            raise Constraint_Error;
         end if;
      end if;
   end Sub;
--
-- Sub -- Internal procedure
--
   procedure Sub
             (  Minuend    : in out Unbounded_Unsigned;
                Subtrahend : Half_Word_Array;
                Shift      : Digit_Offset
             )  is
   begin
      if Minuend.Length < Subtrahend'Length + Shift then
         raise Constraint_Error;
      end if;
      Clone (Minuend.Value, Minuend.Length);
      declare
         Length : Digit_Offset    renames Minuend.Length;
         Left   : Half_Word_Array renames
                     Minuend.Value.Data (1..Minuend.Length);
         Borrow : Word := 0;
         To     : Digit_Count := Shift + 1;
      begin
         for Index in Subtrahend'Range loop
            Borrow := Borrow + Word (Subtrahend (Index));
            if Word (Left (To)) >= Borrow then
               Left (To) := Left (To) - Half_Word (Borrow);
               Borrow := 0;
            else
               Borrow := Word (Left (To)) + Half_Word_Modulus - Borrow;
               Left (To) := Half_Word (Borrow and Half_Word_Mask);
               Borrow := Shift_Right (Borrow, Bit_Width) + 1;
            end if;
            To := To + 1;
         end loop;
         if Borrow /= 0 then
            if To > Length then
               raise Constraint_Error;
            end if;
            while To <= Left'Last loop
               if Word (Left (To)) >= Borrow then
                  Left (To) := Left (To) - Half_Word (Borrow);
                  Borrow := 0;
                  exit;
               end if;
               Left (To) :=
                  Half_Word
                  (  Word (Left (To)) + Half_Word_Modulus - Borrow
                  );
               Borrow := 1;
               To := To + 1;
            end loop;
            if Borrow /= 0 then
               raise Constraint_Error;
            end if;
         end if;
         while Length > 0 and then Left (Length) = 0 loop
            Length := Length - 1;
         end loop;
      end;
   end Sub;

   procedure Sub
             (  Minuend    : in out Unbounded_Unsigned;
                Subtrahend : Unbounded_Unsigned;
                Shift      : Digit_Offset
             )  is
   begin
      if Shift = 0 then
         Sub (Minuend, Subtrahend);
         return;
      elsif Subtrahend.Length = 0 then
         return;
      elsif Minuend.Length < Subtrahend.Length + Shift then
         raise Constraint_Error;
      elsif Minuend.Value = Subtrahend.Value then -- Aliased, power > 0
         raise Constraint_Error;
      end if;
      Sub
      (  Minuend,
         Subtrahend.Value.Data (1..Subtrahend.Length),
         Shift
      );
   end Sub;
--
-- Sub -- Internal procedure
--
   procedure Sub
             (  Minuend    : in out Unbounded_Unsigned;
                Subtrahend : Half_Word_Array;
                Multiplier : Half_Word;
                Shift      : Digit_Offset;
                Borrow     : out Half_Word
             )  is
      Result : Half_Word renames Borrow;
   begin
      if Subtrahend'Length = 0 or else Multiplier = 0 then
         Result := 0;
         return;
      elsif Minuend.Length < Subtrahend'Length + Shift then
         raise Program_Error;
      end if;
      Clone (Minuend.Value, Minuend.Length);
      declare
         Factor : constant Word := Word (Multiplier);
         Length : Digit_Offset    renames Minuend.Length;
         Left   : Half_Word_Array renames
                     Minuend.Value.Data (1..Minuend.Length);
         Borrow : Word := 0;
         Diff   : Word;
         Count  : Word;
         To     : Digit_Count := Shift + 1;
      begin
         for Index in Subtrahend'Range loop
            Borrow := Borrow + Word (Subtrahend (Index)) * Factor;
            if Word (Left (To)) >= Borrow then
               Left (To) := Left (To) - Half_Word (Borrow);
               Borrow := 0;
            else
               Diff  := Borrow - Word (Left (To));
               Count := Shift_Right
                        (  Diff + Half_Word_Mask,
                           Bit_Width  -- How much to borrow
                        );
               Borrow := Count * Half_Word_Modulus - Diff;
               Left (To) := Half_Word (Borrow and Half_Word_Mask);
               Borrow := Count;
            end if;
            To := To + 1;
         end loop;
         if Borrow /= 0 then
            while To <= Left'Last loop
               if Word (Left (To)) >= Borrow then
                  Left (To) := Left (To) - Half_Word (Borrow);
                  Borrow := 0;
                  To := To + 1;
                  exit;
               end if;
               Left (To) :=
                  Half_Word
                  (  Word (Left (To)) + Half_Word_Modulus - Borrow
                  );
               Borrow := 1;
               To := To + 1;
            end loop;
            while Length > 0 and then
                  Left (Length) = Half_Word_Mask loop
               Length := Length - 1;
            end loop;
         end if;
         while Length > 0 and then Left (Length) = 0 loop
            Length := Length - 1;
         end loop;
         Result := Half_Word (Borrow);
      end;
   end Sub;

   procedure Sub
             (  Minuend    : in out Unbounded_Unsigned;
                Subtrahend : Unbounded_Unsigned;
                Multiplier : Half_Word;
                Shift      : Digit_Offset
             )  is
      Borrow : Half_Word;
   begin
      if Subtrahend.Length = 0 or else Multiplier = 0 then
         Erase (Minuend);
         return;
      elsif Multiplier = 1 then
         Sub (Minuend, Subtrahend, Shift);
         return;
      elsif Minuend.Length < Subtrahend.Length + Shift then
         raise Constraint_Error;
      elsif Shift > 0 and then Minuend.Value = Subtrahend.Value then
         raise Constraint_Error; -- Aliased, power > 0
      end if;
      Sub
      (  Minuend,
         Subtrahend.Value.Data (1..Subtrahend.Length),
         Multiplier,
         Shift,
         Borrow
      );
      if Borrow > 0 then
         raise Constraint_Error;
      end if;
   end Sub;

   procedure Sub_2
             (  Minuend    : Half_Word_Array;
                Subtrahend : in out Unbounded_Unsigned
             )  is
   begin
      if Subtrahend.Length = 0 then
         Set (Subtrahend, Minuend);
         return;
      elsif Minuend'Length < Subtrahend.Length then
         raise Constraint_Error;
      end if;
      Clone (Subtrahend.Value, Subtrahend.Length);
      declare
         Right  : Half_Word_Array renames
                     Subtrahend.Value.Data (1..Subtrahend.Length);
         From   : Digit_Count := Minuend'First;
         Borrow : Word := 0;
      begin
         for Index in Right'Range loop
            Borrow := Borrow + Word (Right (Index));
            if Word (Minuend (From)) >= Borrow then
               Right (Index) := Minuend (From) - Half_Word (Borrow);
               Borrow := 0;
            else
               Borrow :=
                  Word (Minuend (From)) + Half_Word_Modulus - Borrow;
               Right (Index) := Half_Word (Borrow and Half_Word_Mask);
               Borrow := Shift_Right (Borrow, Bit_Width) + 1;
            end if;
            From := From + 1;
         end loop;
         for Index in Subtrahend.Length + 1..Minuend'Length loop
            if Word (Minuend (From)) >= Borrow then
               Put
               (  Subtrahend.Value,
                  Index,
                  Minuend (From) - Half_Word (Borrow)
               );
               Borrow := 0;
            else
               Put
               (  Subtrahend.Value,
                  Index,
                  Half_Word
                  (  Word (Minuend (From)) + Half_Word_Modulus - Borrow
               )  );
               Borrow := 1;
            end if;
            From := From + 1;
            Subtrahend.Length := Index;
         end loop;
         if Borrow /= 0 then
            raise Constraint_Error;
         end if;
      end;
      declare
         Data   : Half_Word_Array renames Subtrahend.Value.Data;
         Length : Digit_Offset    renames Subtrahend.Length;
      begin
         while Length > 0 and then Data (Length) = 0 loop
            Length := Length - 1;
         end loop;
      end;
   end Sub_2;

   procedure Sub_2
             (  Minuend    : Unbounded_Unsigned;
                Subtrahend : in out Unbounded_Unsigned
             )  is
   begin
      if Subtrahend.Length = 0 then
         Set (Subtrahend, Minuend);
      elsif Minuend.Length < Subtrahend.Length then
         raise Constraint_Error;
      elsif Minuend.Value = Subtrahend.Value then
         Subtrahend.Length := 0;
      else
         Sub_2 (Minuend.Value.Data (1..Minuend.Length), Subtrahend);
      end if;
   end Sub_2;

   procedure Sub_From_Power_Of_Two
             (  Power      : Bit_Position;
                Subtrahend : in out Unbounded_Unsigned
             )  is
      Borrow : Word := 0;
   begin
      if Subtrahend.Length = 0 then
         Power_Of_Two (Power, Subtrahend);
         return;
      elsif Power < Get_MSB (Subtrahend) then
         raise Constraint_Error;
      end if;
      Clone (Subtrahend.Value, Subtrahend.Length);
      declare
         Right : Half_Word_Array renames
                    Subtrahend.Value.Data (1..Subtrahend.Length);
      begin
         for Index in Right'Range loop
            Borrow := Borrow + Word (Right (Index));
            if 0 = Borrow then
               Right (Index) := 0;
               Borrow := 0;
            else
               Borrow := Half_Word_Modulus - Borrow;
               Right (Index) := Half_Word (Borrow and Half_Word_Mask);
               Borrow := Shift_Right (Borrow, Bit_Width) + 1;
            end if;
         end loop;
      end;
      if Borrow /= 0 then
         declare
            Last : constant Digit_Offset :=
                   Digit_Offset ((Power + Bit_Width - 1) / Bit_Width);
         begin
            for Index in Subtrahend.Length + 1..Last loop
               Put (Subtrahend.Value, Index, Half_Word_Mask);
            end loop;
            Subtrahend.Length := Last;
         end;
      end if;
      Get_Slice (Subtrahend, 1, Power);
   end Sub_From_Power_Of_Two;

   procedure Sub_From_Power_Of_Half_Word
             (  Count      : Digit_Count;
                Subtrahend : in out Unbounded_Unsigned
             )  is
      Borrow : Word := 0;
   begin
      if Subtrahend.Length = 0 then
         Power_Of_Two (Bit_Count (Count) * Bit_Width, Subtrahend);
         return;
      elsif Count < Subtrahend.Length then
         raise Constraint_Error;
      end if;
      Clone (Subtrahend.Value, Subtrahend.Length);
      declare
         Right : Half_Word_Array renames
                    Subtrahend.Value.Data (1..Subtrahend.Length);
      begin
         for Index in Right'Range loop
            Borrow := Borrow + Word (Right (Index));
            if 0 = Borrow then
               Right (Index) := 0;
               Borrow := 0;
            else
               Borrow := Half_Word_Modulus - Borrow;
               Right (Index) := Half_Word (Borrow and Half_Word_Mask);
               Borrow := Shift_Right (Borrow, Bit_Width) + 1;
            end if;
         end loop;
      end;
      if Borrow /= 0 then
         for Index in Subtrahend.Length + 1..Count loop
            Put (Subtrahend.Value, Index, Half_Word_Mask);
         end loop;
         Subtrahend.Length := Count;
      end if;
      declare
         Length : Digit_Offset    renames Subtrahend.Length;
         Right  : Half_Word_Array renames
                    Subtrahend.Value.Data (1..Length);
      begin
         while Length > 0 and then Right (Length) = 0 loop
            Length := Length - 1;
         end loop;
      end;
   end Sub_From_Power_Of_Half_Word;

   procedure Swap (Left, Right : in out Unbounded_Unsigned) is
      Length : constant Digit_Offset := Left.Length;
      Value  : constant Vector_Ptr   := Left.Value;
   begin
      Left.Length  := Right.Length;
      Left.Value   := Right.Value;
      Right.Length := Length;
      Right.Value  := Value;
   end Swap;

   procedure Truncate
             (  Left  : in out Unbounded_Unsigned;
                Power : out Bit_Count
             )  is
   begin
      if Left.Length = 0 then
         Power := 0;
         return;
      end if;
      declare
         Data : Half_Word_Array renames
                   Left.Value.Data (1..Left.Length);
      begin
         for Index in Data'Range loop
            declare
               This : Half_Word := Data (Index);
            begin
               if This /= 0 then
                  Power := Bit_Count (Index - 1) * Bit_Width;
                  if (This and 1) = 0 then
                     loop
                        Power := Power + 1;
                        This  := This / 2;
                        if (This and 1) /= 0 then
                           Div_By_Power_Of_Two (Left, Power);
                           return;
                        end if;
                     end loop;
                  else
                     Div_By_Power_Of_Two (Left, Power);
                     return;
                  end if;
               end if;
            end;
         end loop;
      end;
   end Truncate;

   procedure Truncate (Left : in out Unbounded_Unsigned) is
      Power : Bit_Count;
   begin
      Truncate (Left, Power);
   end Truncate;

   function "=" (Left, Right : Unbounded_Unsigned) return Boolean is
   begin
      return Compare (Left, Right) = Equal;
   end "=";

   function "=" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Boolean is
   begin
      return Compare (Left, Right) = Equal;
   end "=";

   function "=" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Boolean is
   begin
      return Compare (Left, Right) = Equal;
   end "=";

   function "<" (Left, Right : Unbounded_Unsigned) return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function "<" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function "<" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function "<=" (Left, Right : Unbounded_Unsigned) return Boolean is
   begin
      return Compare (Left, Right) /= Greater;
   end "<=";

   function "<=" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Boolean is
   begin
      return Compare (Left, Right) /= Greater;
   end "<=";

   function "<=" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Boolean is
   begin
      return Compare (Left, Right) /= Greater;
   end "<=";

   function ">" (Left, Right : Unbounded_Unsigned) return Boolean is
   begin
      return Compare (Left, Right) = Greater;
   end ">";

   function ">" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Boolean is
   begin
      return Compare (Left, Right) = Greater;
   end ">";

   function ">" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Boolean is
   begin
      return Compare (Left, Right) = Greater;
   end ">";

   function ">=" (Left, Right : Unbounded_Unsigned) return Boolean is
   begin
      return Compare (Left, Right) /= Less;
   end ">=";

   function ">=" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Boolean is
   begin
      return Compare (Left, Right) /= Less;
   end ">=";

   function ">=" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Boolean is
   begin
      return Compare (Left, Right) /= Less;
   end ">=";

   function "+" (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned is
   begin
      if Left.Length < Right.Length then
         declare
            Result : Unbounded_Unsigned := Right;
         begin
            Add (Result, Left);
            return Result;
         end;
      else
         declare
            Result : Unbounded_Unsigned := Left;
         begin
            Add (Result, Right);
            return Result;
         end;
      end if;
   end "+";

   function "+" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Unbounded_Unsigned is
      Result : Unbounded_Unsigned := Left;
   begin
      Add (Result, Right);
      return Result;
   end "+";

   function "+" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Unbounded_Unsigned is
      Result : Unbounded_Unsigned := Right;
   begin
      Add (Result, Left);
      return Result;
   end "+";

   function "-" (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned is
   begin
      if Right.Length = 0 then
         return Left;
      elsif Left.Length < Right.Length then
         raise Constraint_Error;
      end if;
      declare
         Result : Unbounded_Unsigned := Left;
      begin
         Sub (Result, Right);
         return Result;
      end;
   end "-";

   function "-" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Unbounded_Unsigned is
      Result : Unbounded_Unsigned := Left;
   begin
      Sub (Result, Right);
      return Result;
   end "-";

   function "-" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Half_Word is
      Result : Unbounded_Unsigned := From_Half_Word (Left);
   begin
      if Right.Length > 1 then
         raise Constraint_Error;
      else
         return Left - To_Half_Word (Right);
      end if;
   end "-";

   function "*" (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned is
   begin
      if Left.Length = 0 or else Right.Length = 0 then
         return Zero;
      elsif Right.Length = 1 then
         declare
            Result : Unbounded_Unsigned := Left;
         begin
            Mul (Result, Right.Value.Data (1));
            return Result;
         end;
      elsif Left.Length = 1 then
         declare
            Result : Unbounded_Unsigned := Right;
         begin
            Mul (Result, Left.Value.Data (1));
            return Result;
         end;
      elsif Left.Value = Right.Value then
         return Square (Left);
      else
         return Mul_Karatsuba (Left, Right, Mul_Threshold);
      end if;
   end "*";

   function "*" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Unbounded_Unsigned is
      Result : Unbounded_Unsigned := Left;
   begin
      Mul (Result, Right);
      return Result;
   end "*";

   function "*" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Unbounded_Unsigned is
      Result : Unbounded_Unsigned := Right;
   begin
      Mul (Result, Left);
      return Result;
   end "*";

   function "/" (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned is
   begin
      if Right.Length = 0 then
         raise Constraint_Error;
      elsif Left.Length = 0 then
         return Zero;
      elsif Right.Length = 1 and then Right.Value.Data (1) = 1 then
         return Left;
      elsif Left.Length < Right.Length then
         return Zero;
      elsif Left.Length = 1 and then Right.Length = 1 then
         return
         (  From_Half_Word (Left.Value.Data (1)
         /  Right.Value.Data (1))
         );
      end if;
      case Compare
           (  Left.Value.Data  (1..Left.Length),
              Right.Value.Data (1..Right.Length)
           )  is
         when Less =>
            return Zero;
         when Equal =>
            return One;
         when Greater =>
            declare
               Result    : Unbounded_Unsigned := Left;
               Remainder : Unbounded_Unsigned;
            begin
               Div (Result, Right, Remainder);
               return Result;
            end;
      end case;
   end "/";

   function "/" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Unbounded_Unsigned is
      Result    : Unbounded_Unsigned := Left;
      Remainder : Half_Word;
   begin
      Div (Result, Right, Remainder);
      return Result;
   end "/";

   function "/" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Half_Word is
   begin
      if Right.Length = 0 then
         raise Constraint_Error;
      elsif Left = 0 or else Right.Length > 2 then
         return 0;
      else
         return Left / To_Half_Word (Right);
      end if;
   end "/";

   function "mod" (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned is
      Result : Unbounded_Unsigned := Left;
   begin
      Modulo (Result, Right);
      return Result;
   end "mod";

   function "mod" (Left : Unbounded_Unsigned; Right : Half_Word)
      return Half_Word is
   begin
      if Right = 0 then
         raise Constraint_Error;
      elsif Right = 1 or else Left.Length = 0 then
         return 0;
      elsif Left.Length = 1 then
         return Left.Value.Data (1) mod Right;
      end if;
      declare
         Denominator : constant Word := Word (Right);
         Length      : Digit_Offset    renames Left.Length;
         Data        : Half_Word_Array renames Left.Value.Data;
         Carry       : Word := 0;
      begin
         for Index in reverse 1..Length loop
            Carry :=
               Word (Data (Index)) + Shift_Left (Carry, Bit_Width);
            Carry := Carry mod Denominator;
         end loop;
         return Half_Word (Carry);
      end;
   end "mod";

   function "mod" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Unbounded_Unsigned is
   begin
      return From_Half_Word (Left) mod Right;
   end "mod";

   function "rem" (Left, Right : Unbounded_Unsigned)
      return Unbounded_Unsigned is
   begin
      return Left mod Right;
   end "rem";

   function "rem"
            (  Left  : Unbounded_Unsigned;
               Right : Half_Word
            )  return Half_Word is
   begin
       return Left mod Right;
   end "rem";

   function "rem" (Left : Half_Word; Right : Unbounded_Unsigned)
      return Unbounded_Unsigned is
   begin
      return From_Half_Word (Left) rem Right;
   end "rem";

   function "**" (Left : Unbounded_Unsigned; Right : Bit_Count)
      return Unbounded_Unsigned is
   begin
      if Right = 0 then
         return One;
      elsif Left.Length = 0 then
         return Zero;
      elsif Left.Length = 1 and then Left.Value.Data (1) = 1 then
         return One;
      else
         declare
            Result : Unbounded_Unsigned;
            Power  : Unbounded_Unsigned;
            Temp   : Unbounded_Unsigned;
            Shift  : Bit_Count := Right;
         begin
            Set (Result, 1);
            Set (Power, Left);
            loop
               if Shift mod 2 = 1 then
                  Mul (Result, Power);
               end if;
               Shift := Shift / 2;
               exit when Shift = 0;
               Square (Power, Temp);
               Swap (Power, Temp);
            end loop;
            return Result;
         end;
      end if;
   end "**";

   function To_Half_Word (Left : Unbounded_Unsigned)
      return Half_Word is
   begin
       case Left.Length is
          when 0 =>
             return 0;
          when 1 =>
             return Left.Value.Data (1);
          when others =>
             raise Constraint_Error;
      end case;
   end To_Half_Word;

   function To_Word (Left : Unbounded_Unsigned) return Word is
   begin
       case Left.Length is
          when 0 =>
             return 0;
          when 1 =>
             return Word (Left.Value.Data (1));
          when 2 =>
             return
             (  Word (Left.Value.Data (2)) * Half_Word_Modulus
             +  Word (Left.Value.Data (1))
             );
          when others =>
             raise Constraint_Error;
      end case;
   end To_Word;

   procedure Generic_Replace_Slice
             (  Left  : in out Unbounded_Unsigned;
                Right : Unbounded_Unsigned;
                From  : Bit_Position;
                To    : Bit_Position
             )  is
      procedure Do_It (Source : Half_Word_Array) is
      begin
         declare
            Offset : constant Digit_Offset :=
                        Digit_Offset ((From - 1) / Bit_Width);
            Shift  : constant Natural :=
                        Natural (Bit_Width - (From - 1) mod Bit_Width);
            First  : constant Digit_Count := Offset + 1;
         --
         -- Get -- Source half-word
         --
         --    Index - The destination index
         --
         -- Returns :
         --
         --    Source word
         --
            function Get (Index : Digit_Count) return Half_Word is
               pragma Inline (Get);
               From : Digit_Offset := Index - Offset;
            begin
               if Shift = Bit_Width then -- Whole half-word
                  if Index <= Source'Last then
                     return Source (From);
                  else
                     return 0;
                  end if;
              else
                  declare
                     Result : Word;
                  begin
                     if From <= Source'Last then
                        Result :=
                           Shift_Left (Word (Source (From)), Bit_Width);
                     else
                        Result := 0;
                     end if;
                     From := From - 1;
                     if From in Source'Range then
                        Result := Result or Word (Source (From));
                     end if;
                     return
                        Half_Word
                        (  Shift_Right (Result, Shift)
                        and
                           Half_Word_Mask
                        );
                  end;
               end if;
            end Get;
         --
         -- Set -- Destination bits
         --
         --    Index - The destination index
         --    Mask  - Bits to be replaced
         --
            procedure Set (Index : Digit_Count; Mask : Half_Word) is
               pragma Inline (Set);
            begin
               if Index > Left.Length then
                  Put
                  (   Left.Value,
                      Index,
                      Operation (0, Get (Index)) and Mask
                  );
                  declare
                     Zero : constant Half_Word := Operation (0, 0);
                     Data : Half_Word_Array renames Left.Value.Data;
                  begin
                     for I in Left.Length + 1..Index - 1 loop
                        Data (I) := Zero;
                     end loop;
                     Left.Length := Index;
                  end;
               else
                  declare
                     Data : Half_Word renames Left.Value.Data (Index);
                  begin
                     Data := (Data and not Mask)
                          or (Operation (Data, Get (Index)) and Mask);
                  end;
               end if;
            end Set;
         --
         -- Set -- Destination bits in complete half-words
         --
         --    From - The first destination/source index
         --    To   - The last index
         --
            procedure Set (From, To : Digit_Count) is
               pragma Inline (Set);
               Data : Half_Word_Array renames
                         Left.Value.Data (1..Left.Length);
            begin
               for Index in From..To loop
                  Data (Index) := Operation (Data (Index), Get (Index));
               end loop;
            end Set;

            Last      : constant Digit_Count :=
                           Digit_Count ((To - 1) / Bit_Width + 1);
            From_At   : constant Natural :=
                           Natural (From - 1) mod Bit_Width;
            To_At     : constant Natural :=
                           Natural (To   - 1) mod Bit_Width;
            From_Mask : constant Half_Word := not (2 ** From_At - 1);
            To_Mask   : constant Half_Word := 2 ** (To_At + 1) - 1;
         begin
            if Left.Length = 0 then
               Allocate (Left.Value, Last);
            else
               Clone (Left.Value, Left.Length);
            end if;
            if First = Last then
               Set (First, From_Mask and To_Mask);
            elsif From_At = 0 then
               if To_At = Bit_Width - 1 then
                  if Left.Length < Last then
                     Put (Left.Value, Last, 0);
                     declare
                        Data : Half_Word_Array renames
                                  Left.Value.Data (1..Last);
                     begin
                        for Index in Left.Length + 1..Last - 1 loop
                           Data (Index) := 0;
                        end loop;
                     end;
                     Left.Length := Last;
                  end if;
                  Set (First, Last);
               else
                  Set (Last, To_Mask);
                  Set (First, Last - 1);
               end if;
            else
               Set (Last,  To_Mask);
               Set (First, From_Mask);
               Set (First + 1, Last - 1);
            end if;
         end;
         if Left.Length = 0 then
            return;
         end if;
         declare
            Length : Digit_Offset    renames Left.Length;
            Data   : Half_Word_Array renames Left.Value.Data (1..Length);
         begin
            while Length > 0 and then Data (Length) = 0 loop
               Length := Length - 1;
            end loop;
         end;
      end Do_It;
   begin
      if Right.Length = 0 then
         Do_It ((1..0 => 0));
      elsif From > To then
         null;
      elsif From = To then
         if 0 = Operation
                (  Boolean'Pos (Get_Bit (Left, From)),
                   Boolean'Pos (Get_Bit (Right, 1))
                )  then
            Clear_Bit (Left, From);
         else
            Set_Bit (Left, From);
         end if;
      else
         Do_It (Right.Value.Data (1..Right.Length));
      end if;
   end Generic_Replace_Slice;

   package body Unsigned_Conversions is

      function From_Unbounded_Unsigned (Left : Unbounded_Unsigned)
         return Number is
         Length : Digit_Offset renames Left.Length;
      begin
         if Length = 0 then
            return 0;
         else
            declare
               Value   : Half_Word_Array renames Left.Value.Data;
               Result  : Number := Number (Value (Length));
            begin
               for Index in reverse 1..Length - 1 loop
                  Result := Result * Number (Half_Word_Modulus)
                          + Number (Value (Index));
               end loop;
               return Result;
            end;
         end if;
      end From_Unbounded_Unsigned;

      function To_Unbounded_Unsigned (Left : Number)
         return Unbounded_Unsigned is
      begin
         if Left = 0 then
            return Zero;
         else
            declare
               Ptr    : Vector_Ptr;
               Length : Digit_Count := 1;
            begin
               if Number'Modulus <= Half_Word'Modulus then
                  Ptr := new Vector'(1, 1, (1..1 => Half_Word (Left)));
               else
                  declare
                     Modulus : constant Number :=
                               Number (Half_Word_Modulus);
                     Value   : Number := Left;
                  begin
                     loop
                        Put
                        (  Ptr,
                           Length,
                           Half_Word (Value mod Modulus)
                        );
                        Value := Value / Modulus;
                        exit when Value = 0;
                        Length := Length + 1;
                     end loop;
                  end;
               end if;
               return
               (  Ada.Finalization.Controlled
               with
                  Length => Length,
                  Value  => Ptr
               );
            exception
               when others =>
                  Free (Ptr);
                  raise;
            end;
         end if;
      end To_Unbounded_Unsigned;

   end Unsigned_Conversions;
------------------------------------------------------------------------
   procedure Add (X :in  out Signed; Y : Signed) is
      pragma Inline (Add);
   begin
      if X.Sign = Y.Sign then
         Add (X.Value, Y.Value);
      else
         case Compare (X.Value, Y.Value) is
            when Less =>
               X.Sign := Y.Sign;
               Sub_2 (Y.Value, X.Value);
            when Equal =>
               Erase (X.Value);
               X.Sign := False;
            when Greater =>
               Sub (X.Value, Y.Value);
         end case;
      end if;
   end Add;

   procedure Mul (X : in out Signed; Y : Signed) is
      pragma Inline (Mul);
   begin
      if X.Value.Length = 0 then
         null;
      elsif Y.Value.Length = 0 then
         X.Sign := False;
         Erase (X.Value);
      else
         X.Sign := X.Sign xor Y.Sign;
         Mul (X.Value, Y.Value);
      end if;
   end Mul;

   procedure Mul (X, Y : Signed; Result : out Signed) is
      pragma Inline (Mul);
   begin
      if X.Value.Length = 0 and then Y.Value.Length = 0 then
         Result.Sign := False;
         Erase (Result.Value);
      else
         Result.Sign := X.Sign xor Y.Sign;
         Mul (X.Value, Y.Value, Result.Value);
      end if;
   end Mul;

   procedure Mul_By_Power_Of_Two
             (  X     : in out Signed;
                Power : Bit_Count
             )  is
      pragma Inline (Mul_By_Power_Of_Two);
   begin
      Mul_By_Power_Of_Two (X.Value, Power);
   end Mul_By_Power_Of_Two;

   procedure Set (X : in out Signed; Y : Signed) is
      pragma Inline (Set);
   begin
      X.Sign := Y.Sign;
      Set (X.Value, Y.Value);
   end Set;

   procedure Set (X : in out Signed; Y : Half_Word) is
      pragma Inline (Set);
   begin
      X.Sign := False;
      Set (X.Value, Y);
   end Set;

   procedure Sub (X : in out Signed; Y : Signed) is
      pragma Inline (Sub);
   begin
      if Y.Value.Length = 0 then
         null;
      elsif X.Value.Length = 0 then
         Set (X.Value, Y.Value);
         X.Sign := not Y.Sign;
      elsif X.Sign = Y.Sign then
         case Compare (X.Value, Y.Value) is
            when Greater =>
               Sub (X.Value, Y.Value);
            when Equal =>
               Erase (X.Value);
               X.Sign := False;
            when Less =>
               X.Sign := not Y.Sign;
               Sub_2 (Y.Value, X.Value);
         end case;
      else
         Add (X.Value, Y.Value);
      end if;
   end Sub;

   procedure Sub (X : in out Signed; Y : Half_Word) is
      pragma Inline (Sub);
   begin
      if Y = 0 then
         null;
      elsif X.Value.Length = 0 then
         Set (X.Value, Y);
         X.Sign := True;
      elsif not X.Sign then
         case Compare (X.Value, Y) is
            when Greater =>
               Sub (X.Value, Y);
            when Equal =>
               Erase (X.Value);
               X.Sign := False;
            when Less =>
               X.Sign := True;
               Set (X.Value, Y - To_Half_Word (X.Value));
         end case;
      else
         Add (X.Value, Y);
      end if;
   end Sub;

   procedure Square (X : Signed; Result : out Signed) is
      pragma Inline (Square);
   begin
      Result.Sign := False;
      Square (X.Value, Result.Value);
   end Square;

   procedure Swap (X, Y : in out Signed) is
      pragma Inline (Swap);
      Sign : constant Boolean := X.Sign;
   begin
      X.Sign := Y.Sign;
      Y.Sign := Sign;
      Swap (X.Value, Y.Value);
   end Swap;

end Unbounded_Unsigneds;
