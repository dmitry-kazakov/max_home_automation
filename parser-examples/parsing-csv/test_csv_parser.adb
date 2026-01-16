--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_CSV_Parser                             Luebeck            --
--  Example                                        Winter, 2025       --
--                                                                    --
--                                Last revision :  12:17 04 Jan 2026  --
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

with Parsers.Multiline_Patterns;
with Parsers.Multiline_Source.Text_IO;

with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Parsers.Generic_Source.Patterns.Generic_Variable;

with Strings_Edit;

procedure Test_CSV_Parser is
   package Float_Vectors is
      new Ada.Containers.Vectors (Positive, Float);
   package Integer_Vectors is
      new Ada.Containers.Vectors (Positive, Integer);
   package String_Vectors is
      new Ada.Containers.Indefinite_Vectors (Positive, String);

   Col_1_Data : String_Vectors.Vector;
   Col_2_Data : Integer_Vectors.Vector;
   Col_3_Data : Float_Vectors.Vector;

   use Parsers.Multiline_Patterns;

   procedure Add_1
             (  Value  : String;
                Where  : Location_Subtype;
                Append : Boolean
             )  is
   begin
      Col_1_Data.Append (Strings_Edit.Trim (Value), 1);
   end Add_1;
   procedure Add_2
             (  Value  : String;
                Where  : Location_Subtype;
                Append : Boolean
             )  is
   begin
      Col_2_Data.Append (Integer'Value (Value), 1);
   end Add_2;
   procedure Add_3
             (  Value  : String;
                Where  : Location_Subtype;
                Append : Boolean
             )  is
   begin
      Col_3_Data.Append (Float'Value (Value), 1);
   end Add_3;
   procedure Del_1 (Append : Boolean) is
   begin
      Col_1_Data.Delete_Last;
   end Del_1;
   procedure Del_2 (Append : Boolean) is
   begin
      Col_2_Data.Delete_Last;
   end Del_2;
   procedure Del_3 (Append : Boolean) is
   begin
      Col_3_Data.Delete_Last;
   end Del_3;
   function On_Line_Change
            (  Where : Location_Subtype
            )  return Result_Type is
   begin
      return Matched;
   end On_Line_Change;

   package Col_1 is
      new Parsers.Multiline_Patterns.Generic_Variable (Add_1, Del_1);
   package Col_2 is
      new Parsers.Multiline_Patterns.Generic_Variable (Add_2, Del_2);
   package Col_3 is
      new Parsers.Multiline_Patterns.Generic_Variable (Add_3, Del_3);
   File    : aliased Ada.Text_IO.File_Type;
   SP      : constant Pattern_TYpe := Blank_Or_Empty;
   Pattern : constant Pattern_Type :=
           + (                                             SP &
                Col_1.Append (Field (","))    & SP & "," & SP &
                Col_2.Append (Natural_Number) & SP & "," & SP &
                Col_3.Append (Floating_Point_Number)     & SP &
                End_of_Line                              & NL_or_EOF
             or Failure
             );
   use Ada.Text_IO;
begin
   Open (File, In_File, "test.csv");
   declare
      Source : aliased Parsers.Multiline_Source.Text_IO.Source (File'Access);
      State  : aliased Match_State (100);
   begin
      if Match (Pattern, Source'Access, State'Access) = Matched then
         for Index in 1.. Natural (Col_1_Data.Length) loop
            Put (Col_1_Data.Element (Index) & ",");
            Put (Integer'Image (Col_2_Data.Element (Index)) & ",");
            Put (Float'Image (Col_3_Data.Element (Index)));
            New_Line;
         end loop;
      else
         Put_Line ("Not amtched");
      end if;
   end;
   Close (File);
end Test_CSV_Parser;
