--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.                               Luebeck            --
--        Unbounded_Unsigned_Edit                  Winter, 2024       --
--  Interface                                                         --
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

with Ada.IO_Exceptions;    use Ada.IO_Exceptions;
with Strings_Edit.Fields;  use Strings_Edit.Fields;

package body Strings_Edit.Unbounded_Unsigned_Edit is

   Digit_Count  : array (NumberBase) of Natural   := (others => 0);
   Digit_Factor : array (NumberBase) of Half_Word := (others => 1);

   procedure Get
             (  Source   : String;
                Pointer  : in out Integer;
                Value    : out Unbounded_Unsigned;
                Base     : NumberBase := 10
             )  is
      Radix  : constant Half_Word := Half_Word (Base);
      Count  : Natural   renames Digit_Count  (Base);
      Factor : Half_Word renames Digit_Factor (Base);
      Start  : constant Integer := Pointer;
      Chunk  : Half_Word := 0;
      Index  : Integer   := Pointer;
      Slice  : Natural   := 0;
      Digit  : Half_Word;
      Result : Unbounded_Unsigned;
   begin
      if Index < Source'First then
         raise Layout_Error;
      end if;
      if Index > Source'Last then
         if Index - 1 > Source'Last then
            raise Layout_Error;
         else
            raise End_Error;
         end if;
      end if;
      if Count = 0 then
         declare
            Max : Half_Word := Half_Word'Last;
         begin
            loop
               Count  := Count + 1;
               Max    := Max / Radix;
               Factor := Factor * Radix;
               exit when Max <= Radix;
            end loop;
         end;
      end if;
      while Index <= Source'Last loop
         Digit := Half_Word (GetDigit (Source (Index)));
         exit when Digit >= Radix;
         Index := Index + 1;
         Slice := Slice + 1;
         Chunk := Chunk * Radix + Digit;
         if Count = Slice then
            Mul (Result, Factor);
            Add (Result, Chunk);
            Slice := 0;
            Chunk := 0;
         end if;
      end loop;
      if Slice = 0 then
         if Index = Start then
            raise End_Error;
         end if;
      else
         Mul (Result, Radix ** Slice);
         Add (Result, Chunk);
      end if;
      Pointer := Index;
      Value   := Result;
   end Get;

   Widths : constant array (NumberBase) of Bit_Count :=
                     (   2 => 100,  3 => 157,  4 => 200,  5 => 231,
                         6 => 257,  7 => 279,  8 => 300,  9 => 315,
                        10 => 331, 11 => 344, 12 => 357, 13 => 369,
                        14 => 379, 15 => 389, 16 => 400
                     );

   Half_Word_Digits : constant array (NumberBase) of Natural :=
                     (   2 => Word'Size / 2,
                         3 => Word'Size * 50 / 159,
                         4 => Word'Size / 2 / 2,
                         5 => Word'Size * 50 / 233,
                         6 => Word'Size * 50 / 259,
                         7 => Word'Size * 50 / 281,
                         8 => Word'Size / 2 / 3,
                         9 => Word'Size * 50 / 317,
                        10 => Word'Size * 50 / 333,
                        11 => Word'Size * 50 / 346,
                        12 => Word'Size * 50 / 359,
                        13 => Word'Size * 50 / 371,
                        14 => Word'Size * 50 / 381,
                        15 => Word'Size * 50 / 391,
                        16 => Word'Size / 2 / 4
                     );

   Half_Word_Next : constant array (3..NumberBase'Last) of Half_Word :=
                     (   3 =>  3 ** Half_Word_Digits (3),
                         4 =>  4 ** Half_Word_Digits (4),
                         5 =>  5 ** Half_Word_Digits (5),
                         6 =>  6 ** Half_Word_Digits (6),
                         7 =>  7 ** Half_Word_Digits (7),
                         8 =>  8 ** Half_Word_Digits (8),
                         9 =>  9 ** Half_Word_Digits (9),
                        10 => 10 ** Half_Word_Digits (10),
                        11 => 11 ** Half_Word_Digits (11),
                        12 => 12 ** Half_Word_Digits (12),
                        13 => 13 ** Half_Word_Digits (13),
                        14 => 14 ** Half_Word_Digits (14),
                        15 => 15 ** Half_Word_Digits (15),
                        16 => 16 ** Half_Word_Digits (16)
                     );

   function Get_Width (Width : Bit_Count; Base : NumberBase)
      return Positive is
   begin
      return Positive (Width * 100 / Widths (Base) + 1);
   end Get_Width;

   function Get_Width
            (  Value : Unbounded_Unsigned;
               Base  : NumberBase := 10
            )  return Natural is
   begin
      return Get_Width (Get_MSB (Value), Base);
   end Get_Width;

   function Value
            (  Source : String;
               Base   : NumberBase := 10
            )  return Unbounded_Unsigned is
      Result  : Unbounded_Unsigned;
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, SpaceAndTab);
      Get (Source, Pointer, Result, Base);
      Get (Source, Pointer, SpaceAndTab);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Unbounded_Unsigned;
                Base        : NumberBase := 10;
                Field       : Natural := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      if Is_Zero (Value) then
         Put
         (  Destination,
            Pointer,
            "0",
            Field,
            Justify,
            Fill
         );
         return;
      end if;
      declare
         Out_Field : constant Natural :=
                     Get_Output_Field (Destination, Pointer, Field);
         subtype Output is String (Pointer..Pointer + Out_Field - 1);
         Text   : Output renames
                  Destination (Pointer..Pointer + Out_Field - 1);
         Index  : Integer := Text'Last;
         Figure : Half_Word;
         Accum  : Unbounded_Unsigned := Value;
         Radix  : constant Half_Word := Half_Word (Base);
         Count  : Natural;
      begin
 Outer : loop
            if Base = 2 then
               Figure := Get_Digit (Accum, Get_Length (Accum));
               Shift_Right (Accum, 1);
            else
               Div (Accum, Half_Word_Next (Base), Figure);
            end if;
            Count := 0;
            for Item in 1..Half_Word_Digits (Base) loop
               if Index < Pointer then
                  raise Layout_Error;
               end if;
               Text (Index) :=
                  Figures
                  (  Figures'First + Integer (Figure mod Radix)
                  );
               Count  := Count + 1;
               Figure := Figure / Radix;
               exit Outer when Figure = 0 and then Accum = 0;
               Index  := Index - 1;
            end loop;
            for Item in Count + 1..Half_Word_Digits (Base) loop
               if Index < Pointer then
                  raise Layout_Error;
               end if;
               Text (Index) := '0';
               Index := Index - 1;
            end loop;
         end loop Outer;
         declare
            Length : constant Natural := Text'Last - Index + 1;
         begin
            if Index = Pointer then
               Pointer := Pointer + Length;
            elsif Field = 0 then
               Destination (Pointer..Pointer + Length - 1) :=
                  Destination (Index..Text'Last);
               Pointer := Pointer + Length;
            else
               case Justify is
                  when Left =>
                     Destination (Pointer..Pointer + Length - 1) :=
                        Destination (Index..Text'Last);
                     for Index in Pointer + Length..Text'Last loop
                        Destination (Index) := Fill;
                     end loop;
                  when Center =>
                     declare
                        First : constant Integer :=
                                         Pointer + Length / 2;
                        Next  : constant Integer := First + Length;
                     begin
                        Destination (First..Next - 1) :=
                           Destination (Index..Text'Last);
                        for Position in Pointer..First - 1 loop
                           Destination (Position) := Fill;
                        end loop;
                        for Position in Next..Text'Last loop
                           Destination (Position) := Fill;
                        end loop;
                     end;
                  when Right =>
                     for Position in Pointer..Index - 1 loop
                        Destination (Position) := Fill;
                     end loop;
               end case;
               Pointer := Text'Last + 1;
            end if;
         end;
      end;
   end Put;

   function Image
            (  Value : Unbounded_Unsigned;
               Base  : NumberBase := 10
            )  return String is
   begin
      if Is_Zero (Value) then
         return "0";
      end if;
      declare
         Text    : String (1..Get_Width (Value, Base));
         Pointer : Integer := 1;
      begin
         Put (Text, Pointer, Value, Base);
         return Text (1..Pointer - 1);
      end;
   end Image;

end Strings_Edit.Unbounded_Unsigned_Edit;
