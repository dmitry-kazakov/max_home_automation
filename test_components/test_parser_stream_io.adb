--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Parser_Stream_IO                       Luebeck            --
--  Test program                                   Spring, 2010       --
--                                                                    --
--                                Last revision :  10:32 12 Jul 2025  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of  the  GNU  Library  General  Public  --
--  License  as  published  by  the Free Software Foundation; either  --
--  version 2 of the License, or (at your option) any later version.  --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--                                                                    --
--  This  library is distributed in the hope that it will be useful,  --
--  but WITHOUT ANY WARRANTY; without even the implied  warranty  of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  Library General Public License for more details.                  --
--                                                                    --
--  You  should  have  received  a  copy  of the GNU Library General  --
--  Public  License  along  with  this library; if not, write to the  --
--  Free Software Foundation, Inc.,  675  Mass  Ave,  Cambridge,  MA  --
--  02139, USA.                                                       --
--____________________________________________________________________--

with Ada.Exceptions;                      use Ada.Exceptions;
with Ada.Characters.Latin_1;              use Ada.Characters.Latin_1;
with Ada.Text_IO;                         use Ada.Text_IO;
with Strings_Edit.Streams;                use Strings_Edit.Streams;
with Parsers.Multiline_Source.Stream_IO;  use Parsers.Multiline_Source;

use Parsers.Multiline_Source.Stream_IO;

procedure Test_Parser_Stream_IO is
   Text : aliased String_Stream (1024);
   Data : constant String :=
                   (  "AAA" & LF
                   &  "B" & CR & CR & LF
                   &  LF
                   &  CR & LF
                   &  "C" & CR & "DEFGH" & LF
                   &  EOT
                   );
begin
   Set (Text, Data);
   if Unread (Text) /= Data then
      declare
         S : constant String := Unread (Text);
      begin
         if S'Length /= Data'Length then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Set text length"
               &  Integer'Image (S'Length)
               &  " /="
               &  Integer'Image (Data'Length)
               &  " (expected)"
            )  );
         end if;
         for Index in S'Range loop
            if S (Index) /= Data (Index - S'First + Data'First) then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Set text at"
                  &  Integer'Image (Index)
                  &  " is unexpected"
               )  );
            end if;
         end loop;
         Raise_Exception
         (  Data_Error'Identity,
            "Set text failed"
         );
      end;
   end if;
   declare
      Code : Stream_IO.Source (Text'Access);
   begin
      declare
         Line    : Line_Ptr;
         Pointer : Integer;
         Last    : Integer;
      begin
         Get_Line (Code, Line, Pointer, Last);
         if Pointer /= 1 then
            raise Data_Error;
         end if;
         if Last /= 3 then
            Raise_Exception
            (  Data_Error'Identity,
               "Last =" & Integer'Image (Last) & " /= 3 (expected)"
            );
         end if;
      end;
      if Get_Line (Code) /= "AAA" then
         raise Data_Error;
      end if;
      Next_Line (Code);
      if Get_Line (Code) /= "B" then
         raise Data_Error;
      end if;
      Next_Line (Code);
      if Get_Line (Code) /= "" then
         raise Data_Error;
      end if;
      Next_Line (Code);
      if Get_Line (Code) /= "" then
         raise Data_Error;
      end if;
      Next_Line (Code);
      if Get_Line (Code) /= "C" & CR & "DEFGH" then
         raise Data_Error;
      end if;
      begin
         Next_Line (Code);
         raise Data_Error;
      exception
         when End_Error =>
            null;
      end;
   end;
   Set
   (  Text,
      (  "AAA" & LF
      &  "B" & CR & CR & LF
      &  LF
      &  CR & LF
      &  "C" & CR & "DEFGH"
      & EOT
   )  );
   declare
      Code : Stream_IO.Source (Text'Access);
   begin
      if Get_Line (Code) /= "AAA" then
         raise Data_Error;
      end if;
      Next_Line (Code);
      if Get_Line (Code) /= "B" then
         raise Data_Error;
      end if;
      Next_Line (Code);
      if Get_Line (Code) /= "" then
         raise Data_Error;
      end if;
      Next_Line (Code);
      if Get_Line (Code) /= "" then
         raise Data_Error;
      end if;
      Next_Line (Code);
      if Get_Line (Code) /= "C" & CR & "DEFGH" then
         raise Data_Error;
      end if;
      begin
         Next_Line (Code);
         raise Data_Error;
      exception
         when End_Error =>
            null;
      end;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Parser_Stream_IO;
