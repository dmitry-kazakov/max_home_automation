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

with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Ada.Exceptions;           use Ada.Exceptions;
with System.Storage_Elements;  use System.Storage_Elements;

with Interfaces.C;

package body MAX_User is

   CP_UTF8         : constant := 65001;
   MAX_PATH        : constant := 260;
   KEY_QUERY_VALUE : constant := 16#00001#;
   KEY_READ        : constant := 16#20019#;
   ERROR_SUCCESS   : constant := 0;

   subtype BOOL    is Interfaces.C.int;
   subtype DWORD   is Interfaces.C.unsigned_long;
   subtype INT     is Interfaces.C.int;
   subtype LONG    is Interfaces.C.long;
   subtype HRESULT is Interfaces.C.long;
   subtype LSTATUS is LONG;
   subtype UINT    is Interfaces.C.unsigned;
   type LPCSTR     is access constant Character;
   type LPCWSTR    is access constant Wide_Character;
   type LPWSTR     is access all Wide_Character;
   type REGSAM     is new DWORD;

   type HKEY       is new System.Address;

   HKEY_LOCAL_MACHINE : constant HKEY :=
                                 HKEY (To_Address (16#80000002#));

   use type Interfaces.C.int;
   use type Interfaces.C.long;
   use type Interfaces.C.unsigned_long;

   function WideCharToMultiByte
            (  Code_Page      : UINT;
               Flags          : DWORD;
               Wide_Char_Str  : System.Address;
               Wide_Char      : INT;
               Multi_Byte_Str : System.Address := System.Null_Address;
               Multi_Byte     : INT;
               Default_Char   : LPCSTR := null;
               Used_Default_Char : access BOOL := null
            )  return INT;
   pragma Import (C, WideCharToMultiByte,  "WideCharToMultiByte");

   function To_UTF8_String
            (  Text      : System.Address;
               Normalize : Boolean := True
            )  return UTF8_String is
      Length : Interfaces.C.int;
   begin
      Length :=
         WideCharToMultiByte
         (  Code_Page     => CP_UTF8,
            Flags         => 0,
            Wide_Char_Str => Text,
            Wide_Char     => -1, -- All string with NUL
            Multi_Byte    => 0
         );
      if Length > 0 then
         declare
            Result : String (1..Positive (Length) + 1);
            Last   : Integer;
         begin
            Length :=
               WideCharToMultiByte
               (  Code_Page      => CP_UTF8,
                  Flags          => 0,
                  Wide_Char_Str  => Text,
                  Wide_Char      => -1, -- All string with NUL
                  Multi_Byte_Str => Result (Result'First)'Address,
                  Multi_Byte     => Result'Length
               );
            Last := Integer (Length);
            while (  Last > 0
                  and then Result (Last) = Character'Val (0)
                  )  loop
               Last := Last - 1;
            end loop;
            if Normalize then
               for Index in 1..Last loop
                  if Result (Index) = '/' then
                     Result (Index) := '\';
                  end if;
               end loop;
            end if;
--                    if Last > 0 and then Result (Last) = '\' then
--                       Last := Last - 1;
--                    end if;
            return Result (1..Last);
         end;
      else
         return "";
      end if;
   exception
      when Constraint_Error =>
         Raise_Exception (Name_Error'Identity, "Encoding error");
   end To_UTF8_String;

   function RegOpenKeyEx
            (  Key     : HKEY;
               SubKey  : Interfaces.C.char_array;
               Options : DWORD;
               Desired : REGSAM;
               Result  : access HKEY
            )  return LSTATUS;
   pragma Import (Stdcall, RegOpenKeyEx, "RegOpenKeyExA");

   function RegQueryValueEx
            (  Key       : HKEY;
               SubKey    : Interfaces.C.char_array;
               Reserved  : System.Address := System.Null_Address;
               Data_Type : access DWORD   := null;
               Data      : Interfaces.C.char_array;
               Count     : access LONG
            )  return LSTATUS;
   pragma Import (Stdcall, RegQueryValueEx, "RegQueryValueExA");

   function RegCloseKey (Key : HKEY) return LSTATUS;
   pragma Import (Stdcall, RegCloseKey, "RegCloseKey");

   function NTP_Servers return Servers_List.Set is
      Result : LSTATUS;
      List   : Servers_List.Set;
      Key    : aliased HKEY;
      Value  : Interfaces.C.char_array (1..1024);
      Size   : aliased LONG := Value'Length;
   begin
      Result :=
         RegOpenKeyEx
         (  HKEY_LOCAL_MACHINE,
            Interfaces.C.To_C
            (  "SYSTEM\CurrentControlSet\Services\"
            &  "W32Time\Parameters"
            ),
            0,
            KEY_READ,
            Key'Access
         );
      if Result /= ERROR_SUCCESS then
         return List;
      end if;
      Result :=
         RegQueryValueEx
         (  Key    => Key,
            SubKey => Interfaces.C.To_C ("NtpServer"),
            Data   => Value,
            Count  => Size'Access
         );
      if Result /= ERROR_SUCCESS then
         Result := RegCloseKey (Key);
         return List;
      end if;
      Result := RegCloseKey (Key);
      declare
         use type Interfaces.C.size_t;
         Start : Interfaces.C.size_t := 1;

         procedure Truncate (Pointer : Interfaces.C.size_t) is
            Last : Interfaces.C.size_t := Pointer - 1;
         begin
            for Index in reverse Start..Pointer - 1 loop
               if Interfaces.C.To_Ada (Value (Index)) = ',' then
                  Last := Index - 1;
               end if;
            end loop;
            if Start <= Last then
               List.Add
               (  Interfaces.C.To_Ada (Value (Start..Last), False)
               );
            end if;
         end Truncate;
      begin
         for Index in 1..Interfaces.C.size_t (Size) loop
            case Interfaces.C.To_Ada (Value (Index)) is
               when 'A'..'Z'  | 'a'..'z'  | '0'..'9'  |
                    '-' | '.' | '_' | '~' | ':' | '/' |
                    '?' | '#' | '[' | ']' | '@' | '!' |
                    '$' | '&' | ''' | '(' | ')' | '*' |
                    '+' | ';' | ',' | '='             =>
                  null;
               when others =>
                  if Index > Start then
                     Truncate (Index);
                     Start := Index + 1;
                  end if;
            end case;
         end loop;
         Truncate (Interfaces.C.size_t (Size) + 1);
         return List;
      end;
   end NTP_Servers;

   function SHGetFolderPath
            (  Windows : System.Address := System.Null_Address;
               CSIDL   : int            := 16#0028#;
               Token   : System.Address := System.Null_Address;
               Flags   : DWORD          := 0;
               Path    : System.Address
            )  return HRESULT;
   pragma Import (Stdcall, SHGetFolderPath, "SHGetFolderPathW");

   function User_Path return UTF8_String is
      Path : Wide_String (1..MAX_PATH + 20);
   begin
      if 0 /= SHGetFolderPath (Path => Path (1)'Address) then
         return "";
      end if;
      return To_UTF8_String (Path (1)'Address) & "\.MAX";
   exception
      when others =>
         return "";
   end User_Path;

end MAX_User;
