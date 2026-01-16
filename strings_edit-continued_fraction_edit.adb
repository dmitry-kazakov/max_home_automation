--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.                               Luebeck            --
--        Continued_Fraction_Edit                  Spring, 2025       --
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
with Unbounded_Integers;   use Unbounded_Integers;

with Strings_Edit.Unbounded_Integer_Edit;
use  Strings_Edit.Unbounded_Integer_Edit;

package body Strings_Edit.Continued_Fraction_Edit is

   procedure Get
             (  Source   : String;
                Pointer  : in out Integer;
                Value    : out Continued_Fraction;
                Base     : NumberBase := 10
             )  is
      Term  : Unbounded_Integer;
      Index : Integer := Pointer;
   begin
      if (  Pointer < Source'First
         or else
            (  Pointer > Source'Last
            and then
               Pointer - 1 > Source'Last
         )  )
      then
         raise Layout_Error;
      elsif Index > Source'Last or else Source (Index) /= '[' then
         raise End_Error;
      end if;
      Index := Index + 1;
      Erase (Value);
      Get (Source, Index, SpaceAndTab);
      begin
         Get (Source, Index, Term, Base);
         Add (Value, Term);
      exception
         when End_Error =>
            raise Data_Error;
      end;
      Get (Source, Index, SpaceAndTab);
      if Index > Source'Last then
         raise Data_Error;
      end if;
      case Source (Index) is
         when ';' =>
            Index := Index + 1;
         when ']' =>
            Pointer := Index + 1;
            return;
         when others =>
            raise Data_Error;
      end case;
      loop
         Get (Source, Index, SpaceAndTab);
         begin
            Get (Source, Index, Term, Base);
            if Is_Zero (Term) then
               raise Data_Error;
            end if;
            Add (Value, Term);
         exception
            when End_Error =>
               raise Data_Error;
         end;
         Get (Source, Index, SpaceAndTab);
         if Index > Source'Last then
            raise Data_Error;
         end if;
         case Source (Index) is
            when ',' =>
               Index := Index + 1;
            when ']' =>
               Pointer := Index + 1;
               return;
            when others =>
               raise Data_Error;
         end case;
      end loop;
   end Get;

   procedure Get_Rational
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Unbounded_Rational;
                Base    : NumberBase := 10
             )  is
      Fraction : Continued_Fraction;
   begin
      Get (Source, Pointer, Fraction, Base);
      Value := Get_Value (Fraction);
   end Get_Rational;

   procedure Value
             (  Source : String;
                Value  : in out Continued_Fraction;
                Base   : NumberBase := 10
             )  is
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, SpaceAndTab);
      Get (Source, Pointer, Value, Base);
      Get (Source, Pointer, SpaceAndTab);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
   end Value;

   function Value_Rational
            (  Source : String;
               Base   : NumberBase := 10
            )  return Unbounded_Rational is
      Fraction : Continued_Fraction;
   begin
      Value (Source, Fraction, Base);
      return Get_Value (Fraction);
   end Value_Rational;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Continued_Fraction;
                Base        : NumberBase := 10;
                PutPlus     : Boolean    := False;
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
        Put (Text, Index, "[0]");
     else
        Put (Text, Index, "[");
        Put (Text, Index, Get_Term (Value, 1), Base, PutPlus);
        if Get_Length (Value) = 1 then
           Put (Text, Index, "]");
        else
           Put (Text, Index, "; ");
           for Item in 2..Get_Length (Value) - 1 loop
              Put (Text, Index, Get_Term (Value, Item), Base, PutPlus);
              Put (Text, Index, ", ");
           end loop;
           Put
           (  Text,
              Index,
              Get_Term (Value, Get_Length (Value)),
              Base,
              PutPlus
           );
           Put (Text, Index, "]");
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

   procedure Put_Rational
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Unbounded_Rational;
                Base        : NumberBase := 10;
                PutPlus     : Boolean    := False;
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
     Count : Natural := 0;

     function On_Term
              (  Value : Unbounded_Rational;
                 Term  : Unbounded_Integer
              )  return Boolean is
     begin
        case Count is
           when 0 =>
              Put (Text, Index, "[");
           when 1 =>
              Put (Text, Index, "; ");
           when others =>
              Put (Text, Index, ", ");
        end case;
        Count := Count + 1;
        Put (Text, Index, Term, Base, PutPlus);
        return True;
     end On_Term;

     procedure Enumerate is new Generic_Enumerate (On_Term);
  begin
     if Is_Zero (Value) then
        Put (Text, Index, "[0]");
     else
        Enumerate (Value);
        Put (Text, Index, "]");
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
   end Put_Rational;

   function Image
            (  Value    : Continued_Fraction;
               Base     : NumberBase := 10;
               PutPlus  : Boolean    := False
            )  return String is
      Length : Positive := 120;
   begin
      loop
         declare
            Text    : String (1..Length);
            Pointer : Integer := Text'First;
         begin
            Put
            (  Destination => Text,
               Pointer     => Pointer,
               Value       => Value,
               Base        => Base,
               PutPlus     => PutPlus
            );
            return Text (Text'First..Pointer - 1);
         exception
            when Layout_Error =>
               Length := Length * 2;
         end;
      end loop;
   end Image;

   function Image_Rational
            (  Value    : Unbounded_Rational;
               Base     : NumberBase := 10;
               PutPlus  : Boolean    := False
            )  return String is
      Length : Positive := 120;
   begin
      loop
         declare
            Text    : String (1..Length);
            Pointer : Integer := Text'First;
         begin
            Put_Rational
            (  Destination => Text,
               Pointer     => Pointer,
               Value       => Value,
               Base        => Base,
               PutPlus     => PutPlus
            );
            return Text (Text'First..Pointer - 1);
         exception
            when Layout_Error =>
               Length := Length * 2;
         end;
      end loop;
   end Image_Rational;

end Strings_Edit.Continued_Fraction_Edit;
