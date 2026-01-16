--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.                               Luebeck            --
--        Unbounded_Integer_Edit                   Winter, 2024       --
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
with Unbounded_Unsigneds;  use Unbounded_Unsigneds;

with Strings_Edit.Unbounded_Unsigned_Edit;
use  Strings_Edit.Unbounded_Unsigned_Edit;

package body Strings_Edit.Unbounded_Integer_Edit is

   procedure Get
             (  Source   : String;
                Pointer  : in out Integer;
                Value    : out Unbounded_Integer;
                Base     : NumberBase := 10
             )  is
      Mantissa : Unbounded_Unsigned;
      Has_Sign : Boolean := False;
      Negative : Boolean := False;
      Index    : Integer := Pointer;
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
      --
      -- Get sign
      --
      case Source (Index) is
         when '+' =>
            Index    := Index + 1;
            Has_Sign := True;
            Get (Source, Index, SpaceAndTab);
         when '-' =>
            Negative := True;
            Has_Sign := True;
            Index    := Index + 1;
            Get (Source, Index, SpaceAndTab);
         when others =>
            null;
      end case;
      begin
         Get (Source, Index, Mantissa, Base);
      exception
         when End_Error =>
            if Has_Sign then
               raise Data_Error;
            else
               raise;
            end if;
      end;
      Value :=
         Compose (Mantissa, Negative and then not Is_Zero (Mantissa));
      Pointer := Index;
   end Get;

   function Value
            (  Source : String;
               Base   : NumberBase := 10
            )  return Unbounded_Integer is
      Result  : Unbounded_Integer;
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
                Value       : Unbounded_Integer;
                Base        : NumberBase := 10;
                PutPlus     : Boolean := False;
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
         Text  : Output renames
                 Destination (Pointer..Pointer + Out_Field - 1);
         Index : Integer := Pointer;
      begin
         if not Is_Zero (Value) then
            if Is_Negative (Value) then
               Put (Text, Index, "-");
            elsif PutPlus then
               Put (Text, Index, "+");
            end if;
         end if;
         Put (Text, Index, Get_Mantissa (Value), Base);
         Adjust_Output_Field
         (  Destination,
            Pointer,
            Index,
            Out_Field,
            Field,
            Justify,
            Fill
         );
      end;
   end Put;

   function Image
            (  Value   : Unbounded_Integer;
               Base    : NumberBase := 10;
               PutPlus : Boolean    := False
            )  return String is
   begin
      if Is_Zero (Value) then
         return "0";
      end if;
      declare
         Text : String (1..Get_Width (Get_Mantissa (Value), Base) + 1);
         Pointer : Integer := 1;
      begin
         Put (Text, Pointer, Value, Base, PutPlus);
         return Text (1..Pointer - 1);
      end;
   end Image;

end Strings_Edit.Unbounded_Integer_Edit;
