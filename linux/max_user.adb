--                                                                    --
--  package MAX_User                Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2019       --
--                                                                    --
--                                Last revision :  13:09 10 Mar 2013  --
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

with Ada.Strings.Maps.Constants;  use Ada.Strings.Maps.Constants;
with Ada.Text_IO;                 use Ada.Text_IO;
with Strings_Edit;                use Strings_Edit;

package body MAX_User is

   function User_Path return UTF8_String is
   begin
      return "~";
   end User_Path;

   procedure Get_Address (Source : String; Pointer : in out Integer) is
   begin
      while Pointer <= Source'Last loop
         case Source (Pointer) is
            when 'A'..'Z'  | 'a'..'z'  | '0'..'9'  |
                 '-' | '.' | '_' | '~' | ':' | '/' |
                 '?' | '#' | '[' | ']' | '@' | '!' |
                 '$' | '&' | ''' | '(' | ')' | '*' |
                 '+' | ';' | ',' | '='             =>
               Pointer := Pointer + 1;
            when others =>
               exit;
         end case;
      end loop;
   end Get_Address;

   function NTP_Servers return Servers_List.Set is
      Result : Servers_List.Set;
   begin
      declare
         File : File_Type;
      begin
         Open (File, In_File, "/etc/ntp.conf");
         declare
            Line    : String (1..1024);
            Last    : Natural;
            Pointer : Integer;
            Start   : Integer;
         begin
            loop
               Get_Line (File, Line, Last);
               if Is_Prefix
                  (  "server",
                     Line (1..Last),
                     Lower_Case_Map
                  )  then
                  Pointer := 7;
                  Get (Line (1..Last), Pointer);
                  while Pointer <= Last loop
                     case Line (Pointer) is
                        when '0'..'9' | '.' =>
                           Pointer := Pointer + 1;
                        when others =>
                           exit;
                     end case;
                  end loop;
                  Start := Pointer;
                  Get_Address (Line (1..Last), Pointer);
                  if Start < Pointer then
                     Result.Add (Line (Start..Pointer - 1));
                  end if;
               end if;
               if Last = Line'Last then
                  Skip_Line (File);
               end if;
            end loop;
         exception
            when others =>
               Close (File);
         end;
      exception
         when others =>
            null;
      end;
      declare
         File : File_Type;
      begin
         Open (File, In_File, "/etc/ntp/step-tickers");
         declare
            Line    : String (1..1024);
            Last    : Natural;
            Pointer : Integer;
            Start   : Integer;
         begin
            loop
               Get_Line (File, Line, Last);
	       Pointer := 1;
               while Pointer <= Last loop
                  case Line (Pointer) is
                     when '0'..'9' | '.' =>
                        Pointer := Pointer + 1;
                     when others =>
                        exit;
                  end case;
               end loop;
               if not Is_Prefix ("#", Line (1..Last), Pointer) then
                  Start := Pointer;
                  Get_Address (Line (1..Last), Pointer);
                  if Start < Pointer then
                     Result.Add (Line (Start..Pointer - 1));
                  end if;
               end if;
               if Last = Line'Last then
                  Skip_Line (File);
               end if;
            end loop;
         exception
            when others =>
               Close (File);
         end;
      exception
         when others =>
            null;
      end;
      declare
         File : File_Type;
      begin
         Open (File, In_File, "/etc/systemd/timesyncd.conf");
         declare
            Line    : String (1..1024);
            Last    : Natural;
            Pointer : Integer;
            Start   : Integer;
         begin
            loop
               Get_Line (File, Line, Last);
               Pointer := 1;
	       if Is_Prefix
                  (  "ntp",
                     Line (1..Last),
                     Pointer,
                     Lower_Case_Map
                  )  then
                  Pointer := Pointer + 3;
                  Get (Line (1..Last), Pointer);
                  if Is_Prefix ("=", Line (1..Last), Pointer) then
                     Pointer := Pointer + 1;
                     Get (Line (1..Last), Pointer);
                     while Pointer <= Last loop
                        Start := Pointer;
                        Get_Address (Line (1..Last), Pointer);
                        if Start < Pointer then
                           Result.Add (Line (Start..Pointer - 1));
                        end if;
                        Get (Line (1..Last), Pointer);
                     end loop;
                  end if;
               end if;
               if Last = Line'Last then
                  Skip_Line (File);
               end if;
            end loop;
         exception
            when others =>
               Close (File);
         end;
      exception
         when others =>
            null;
      end;
      return Result;
   end NTP_Servers;

end MAX_User;
