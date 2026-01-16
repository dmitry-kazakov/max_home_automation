--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.                               Luebeck            --
--        Unbounded_Rational_Edit                  Spring, 2025       --
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

with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Strings_Edit.Fields;    use Strings_Edit.Fields;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Unbounded_Integers;     use Unbounded_Integers;
with Unbounded_Unsigneds;    use Unbounded_Unsigneds;

with Strings_Edit.Unbounded_Unsigned_Edit;
use  Strings_Edit.Unbounded_Unsigned_Edit;

package body Strings_Edit.Unbounded_Rational_Edit is

   procedure Get_Exponent
             (  Source   : in String;
                Pointer  : in out Integer;
                Value    : out Integer
             )  is
      Index : Integer := Pointer;
   begin
      Get (Source, Index, SpaceAndTab);
      if (  Index > Source'Last
         or else
            (  Source (Index) /= 'E'
            and
               Source (Index) /= 'e'
         )  )
      then
         Value := 0;
      else
         Index := Index + 1;
         Get (Source, Index, SpaceAndTab);
         Get (Source, Index, Value);
         Pointer := Index;
      end if;
   exception
      when End_Error | Data_Error | Layout_Error =>
         Value := 0;
   end Get_Exponent;

   procedure Get
             (  Source   : String;
                Pointer  : in out Integer;
                Value    : out Unbounded_Rational;
                Base     : NumberBase := 10
             )  is
      Radix        : constant Unbounded_Unsigned :=
                              From_Half_Word (Half_Word (Base));
      After_Length : Integer;
      Before       : Unbounded_Unsigned;
      After        : Unbounded_Unsigned;
      Exponent     : Integer;
      Result       : Unbounded_Rational;
      Sign         : Integer := 0;
      Index        : Integer := Pointer;
   begin
      if (  Pointer < Source'First
         or else
            (  Pointer > Source'Last
            and then
               Pointer - 1 > Source'Last
         )  )
      then
         raise Layout_Error;
      end if;
      if Pointer <= Source'Last then
         case Source (Index) is
            when '-' =>
               Index := Index + 1;
               Sign := -1;
               Get (Source, Index, SpaceAndTab);
            when '+' =>
               Index := Index + 1;
               Sign := 1;
               Get (Source, Index, SpaceAndTab);
            when others =>
               null;
         end case;
      end if;
      begin
         Get (Source, Index, Before, Base);
      exception
         when End_Error =>
            null;
      end;
      if Index + 1 <= Source'Last and then Source (Index) = '.' then
         begin
            Index := Index + 1;
            After_Length := Index;
            Get (Source, Index, After, Base);
         exception
            when End_Error =>
               if Index = Pointer + 1 then
                  raise Data_Error; -- Only decimal point is present
               end if;
         end;
      end if;
      After_Length := Index - After_Length;
      Get_Exponent (Source, Index, Exponent);
      if 0 = Sign then
         if Pointer = Index then
            raise End_Error; -- No fraction part - no number
         end if;
         Sign := 1;
      else
         if Pointer = Index - 1 then
            raise Data_Error; -- Only a sign
         end if;
      end if;
      Result := Compose
                (  Numerator   => After,
                   Denominator => Radix ** Bit_Count (After_Length)
                )
              + Before;
      if Exponent /= 0 then
         if Exponent < 0 then
            Result := Result / Radix ** Bit_Count (-Exponent);
         else
            Result := Result * Radix ** Bit_Count (Exponent);
         end if;
      end if;
      if Sign = -1 then
         Value := -Result;
      else
         Value := Result;
      end if;
      Pointer := Index;
   end Get;

   function Value
            (  Source : String;
               Base   : NumberBase := 10
            )  return Unbounded_Rational is
      Result  : Unbounded_Rational;
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
                Value       : Unbounded_Rational;
                Base        : NumberBase := 10;
                PutPlus     : Boolean    := False;
                Fraction    : Natural    := 6;
                Field       : Natural    := 0;
                Justify     : Alignment  := Left;
                Fill        : Character  := ' '
             )  is
     Out_Field : constant Natural :=
                    Get_Output_Field (Destination, Pointer, Field);
     subtype Output is String (Pointer..Pointer + Out_Field - 1);
     Text  : Output renames
                Destination (Pointer..Pointer + Out_Field - 1);
     Index : Integer := Pointer;
  begin
     if Is_Zero (Value) then
        Put (Text, Index, "0");
     else
        if Is_Negative (Value) then
           Put (Text, Index, "-");
        elsif PutPlus then
           Put (Text, Index, "+");
        end if;
        if Fraction = 0 then
           Put
           (  Text,
              Index,
              Get_Mantissa (Round (abs Value)),
              Base
           );
        else
           declare
              Radix   : constant Unbounded_Unsigned :=
                           From_Half_Word (Half_Word (Base));
              Shifted : constant Unbounded_Rational :=
                           abs Value * Radix ** Bit_Count (Fraction);
              Start   : constant Integer := Index;
           begin
              Put
              (  Text,
                 Index,
                 Get_Mantissa (Round (Shifted)),
                 Base
              );
              declare
                 Length : constant Integer := Index - Start;
              begin
                 if Length > Fraction then
                    declare
                       Point_At : constant Integer := Index - Fraction;
                    begin
                       Put (Text, Index, " ");
                       Text (Point_At + 1..Index - 1) :=
                          Text (Point_At..Index - 2);
                       Text (Point_At) := '.';
                    end;
                 else
                    Index := Start + Fraction + 2;
                    if Index - 1 > Text'Last then
                       raise Layout_Error;
                    end if;
                    Text (Index - Length..Index - 1) :=
                       Text (Start..Start + Length - 1);
                    Text (Start..Start + 1) := "0.";
                    Text (Start + 2..Index - Length - 1) :=
                       (others => '0');
                 end if;
              end;
           end;
        end if;
     end if;
      -- The output is done in Text using Index as the pointer
     Adjust_Output_Field
     (  Destination,
        Pointer,
        Index,
        Out_Field,
        Field,
        Justify,
        Fill
     );
   end Put;

   function Image
            (  Value    : Unbounded_Rational;
               Base     : NumberBase := 10;
               PutPlus  : Boolean    := False;
               Fraction : Natural    := 6
            )  return String is
      Length : Positive := 120;
   begin
      loop
         declare
            Text    : String (1..Length);
            Pointer : Integer := Text'First;
         begin
            Put (Text, Pointer, Value, Base, PutPlus, Fraction);
            return Text (Text'First..Pointer - 1);
         exception
            when Layout_Error =>
               Length := Length * 2;
         end;
      end loop;
   end Image;

end Strings_Edit.Unbounded_Rational_Edit;
