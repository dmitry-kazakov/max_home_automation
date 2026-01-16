--                                                                    --
--  package MAX_Database_Page       Copyright (c)  Dmitry A. Kazakov  --
--     Database page                               Luebeck            --
--  Implementation                                 Winter, 2017       --
--                                                                    --
--                                Last revision :  09:24 30 Nov 2019  --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Calendar;             use Ada.Calendar;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with GLib;                     use GLib;
with GLib.Messages;            use GLib.Messages;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.File_Chooser;         use Gtk.File_Chooser;
with Gtk.Label;                use Gtk.Label;
with Gtk.Recent_Manager_Keys;  use Gtk.Recent_Manager_Keys;
with MAX_IO;                   use MAX_IO;
with MAX_Trace;                use MAX_Trace;
with ODBC.SQLTypes;            use ODBC.SQLTypes;
with Strings_Edit;             use Strings_Edit;
with Strings_Edit.Integers;    use Strings_Edit.Integers;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;

with Ada.Unchecked_Deallocation;
with Interfaces.C;
with Strings_Edit.Symmetric_Serialization;

package body MAX_Database_Page is
   use Strings_Edit.Symmetric_Serialization;

   function Where (Name : String) return String is
   begin
      return " in MAX_Database_Page." & Name;
   end Where;

   function "&" (Left : String; Right : RF_Address) return String is
   begin
      return
         Left & Image (Integer (Right)) & " [" & Image (Right) & "]";
   end "&";

   function "&" (Left : String; Right : Centigrade) return String is
   begin
      return Left & Image (Right);
   end "&";

   function "&" (Left : String; Right : Integer) return String is
   begin
      return Left & Image (Right);
   end "&";

   function "&" (Left : String; Right : Unsigned_8) return String is
   begin
      return Left & Image (Integer (Right));
   end "&";

   function Device (Value : Unsigned_8) return String is
   begin
      case Value is
         when 0 => return "0 [cube]";
         when 1 => return "1 [radiator thermostat]";
         when 2 => return "2 [radiator thermostat plus]";
         when 3 => return "3 [wall thermostat]";
         when 4 => return "4 [shutter contact]";
         when 5 => return "5 [eco button]";
         when others => return Image (Integer (Value));
      end case;
   end Device;

   function Mode (Value : Unsigned_8) return String is
   begin
      case Value is
         when 0 => return "0 [automatic]";
         when 1 => return "1 [manual]";
         when 2 => return "2 [boost]";
         when 3 => return "3 [vacation]";
         when others => return Image (Integer (Value));
      end case;
   end Mode;

   procedure Create
             (  Base   : in out ODBC_Data;
                Widget : not null access MAX_Database_Record'Class;
                Silent : Boolean;
                Failed : out Boolean
             )  is
      type SQL_Type_Info is record
         Data_Type : SQL_Data_Type;
         Name      : Unbounded_String;
      end record;
      type Data_Type_Array is
         array (Positive range <>) of SQL_DATA_TYPE;
      procedure Query_Type
                (  Data : in out SQL_Type_Info;
                   List : Data_Type_Array;
                   Text : String
                )  is
      begin
         for Index in List'Range loop
            begin
               declare
                  Info : constant Type_Info :=
                         Get_Type_Info
                         (  Base.Insert_Status,
                            List (Index)
                         );
               begin
                  Data.Data_Type := Info.Data_Type;
                  Data.Name := To_Unbounded_String (Info.Type_Name);
                  return;
               end;
            exception
               when Constraint_Error => -- The type is not supported
                  null;
            end;
         end loop;
         raise Data_Error with "The database does not support " & Text;
      end Query_Type;

      procedure Report (Text : String; Error : Exception_Occurrence) is
      begin
         if Silent then
            if View /= null then
               View.Trace (Text, Error);
            end if;
         else
            Say (Text & ". " & Exception_Message (Error));
         end if;
      end Report;

      Timestamp : SQL_Type_Info;
      Real      : SQL_Type_Info;
      Small_Int : SQL_Type_Info;
      Int       : SQL_Type_Info;

      B : SQL_DATA_TYPE renames Small_Int.Data_Type;
      I : SQL_DATA_TYPE renames Int.Data_Type;
      R : SQL_DATA_TYPE renames Real.Data_Type;
   begin
      Failed := True;
      begin
         Base.DB :=
            new ODBC_Connection (Base.Environment'Unchecked_Access);
         if DB_Trace and then View /= null then
            View.Trace
            (  "DB: Connecting to ODBC DSN "
            &  Quote (Widget.DSN.Get_Text)
            &  ", user "
            &  Quote (Widget.User.Get_Text)
            &  ", password "
            &  Quote (Widget.Password.Get_Text)
            &  ", autocommit"
            );
         end if;
         Base.DB.Connect
         (  Server_Name => Widget.DSN.Get_Text,
            User_Name   => Widget.User.Get_Text,
            Password    => Widget.Password.Get_Text,
            Auto_Commit => True
         );
         if Base.Insert_Status = null then
            Base.Insert_Status := new ODBC_Command (Base.DB);
         end if;
         if Base.Insert_Minimal_Device = null then
            Base.Insert_Minimal_Device := new ODBC_Command (Base.DB);
         end if;
         if Base.Insert_Shutter_Contact = null then
            Base.Insert_Shutter_Contact := new ODBC_Command (Base.DB);
         end if;
         if Base.Insert_Wall_Thermostat_Short = null then
            Base.Insert_Wall_Thermostat_Short :=
               new ODBC_Command (Base.DB);
         end if;
         if Base.Insert_Wall_Thermostat_Full = null then
            Base.Insert_Wall_Thermostat_Full :=
               new ODBC_Command (Base.DB);
         end if;
         if Base.Insert_Radiator_Thermostat_Short = null then
            Base.Insert_Radiator_Thermostat_Short :=
               new ODBC_Command (Base.DB);
         end if;
         if Base.Insert_Radiator_Thermostat_Full = null then
            Base.Insert_Radiator_Thermostat_Full :=
               new ODBC_Command (Base.DB);
         end if;
      exception
         when Error : others =>
            Report
            (  (" ODBC database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " connection error"
               ),
               Error
            );
           return;
      end;
      begin
         if DB_Trace and then View /= null then
            View.Trace ("DB: ODBC querying capabilities of the driver");
         end if;
         Query_Type
         (  Timestamp,
            (SQL_TYPE_TIMESTAMP, SQL_TIMESTAMP),
            "time stamps"
         );
         Query_Type
         (  Small_Int,
            (  SQL_UTINYINT,
               SQL_STINYINT,
               SQL_USHORT,
               SQL_SSHORT,
               SQL_SHORT,
               SQL_SMALLINT,
               SQL_ULONG,
               SQL_SLONG,
               SQL_LONG,
               SQL_INTEGER,
               SQL_UBIGINT,
               SQL_SBIGINT,
               SQL_BIGINT
            ),
            "byte"
         );
         Query_Type
         (  Int,
            (  SQL_ULONG,
               SQL_SLONG,
               SQL_LONG,
               SQL_INTEGER,
               SQL_UBIGINT,
               SQL_SBIGINT
            ),
            "unsigned"
         );
         Query_Type (Real, (SQL_DOUBLE, SQL_REAL, SQL_FLOAT), "real");
      exception
         when Error : others =>
            Report
            (  (" ODBC database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " capacity error"
               ),
               Error
            );
           return;
      end;
      if Base.DB.Table_Exists (Table_Name) then
         if DB_Trace and then View /= null then
            View.Trace
            (  "DB: Using existing table "
            &  Quote (Table_Name)
            );
         end if;
      else
         if DB_Trace and then View /= null then
            View.Trace
            (  "DB: Creating the table "
            &  Quote (Table_Name)
            );
         end if;
         declare
            Command : constant String :=
                      (  "CREATE TABLE "
                      &  Table_Name
                      &  " ("
                      &  Time_Column &                     -- 1
                            " " & To_String (Timestamp.Name) & ", "
                      &  Address_Column &                  -- 2
                            " " & To_String (Int.Name) & ", "
                      &  Device_Type_Column &              -- 3
                            " " & To_String (Small_Int.Name) & ", "
                      &  Set_Temperature_Column &          -- 4
                            " " & To_String (Real.Name) & ", "
                      &  New_Temperature_Column &          -- 5
                            " " & To_String (Real.Name) & ", "
                      &  Is_Temperature_Column &           -- 6
                            " " & To_String (Real.Name) & ", "
                      &  Offset_Column &                   -- 7
                            " " & To_String (Real.Name) & ", "
                      &  Valve_Column &                    -- 8
                            " " & To_String (Small_Int.Name) & ", "
                      &  Open_Column &                     -- 9
                            " " & To_String (Small_Int.Name) & ", "
                      &  Error_Column &                    -- 10
                            " " & To_String (Small_Int.Name) & ", "
                      &  Duty_Column &                     -- 11
                            " " & To_String (Small_Int.Name) & ", "
                      &  Slots_Column &                    -- 12
                            " " & To_String (Int.Name) & ", "
                      &  Battery_Low_Column &              -- 13
                            " " & To_String (Small_Int.Name) & ", "
                      &  Panel_Locked_Column &             -- 14
                            " " & To_String (Small_Int.Name) & ", "
                      &  Mode_Column &                     -- 15
                            " " & To_String (Small_Int.Name)
                      &  ")"
                      );
         begin
            Base.Insert_Status.Execute (Command);
         exception
            when Error : others =>
               Report
               (  (  " ODBC database "
                  &  Quote (Widget.File_Name.Get_Text)
                  &  " table creation command: "
                  &  Quote (Command)
                  &  " fault"
                  ),
                  Error
               );
               return;
         end;
      end if;
      declare
         Command : constant String :=
                   (  "INSERT INTO "
                   &  Table_Name
                   &  " ("
                   &  Time_Column        & ", "
                   &  Address_Column     & ", "
                   &  Device_Type_Column & ", "
                   &  Error_Column       & ", "
                   &  Duty_Column        & ", "
                   &  Slots_Column
                   &  ") VALUES (Now(),?,?,?,?,?)"
                   );
         Statement : ODBC_Command_Ptr renames Base.Insert_Status;
      begin
         if Statement = null then
            Statement := new ODBC_Command (Base.DB);
         end if;
         Statement.Prepare (Command);
         Statement.Bind_Parameter (1, Base.Address'Access,     I);
         Statement.Bind_Parameter (2, Base.Device_Type'Access, B);
         Statement.Bind_Parameter (3, Base.Error'Access,       B);
         Statement.Bind_Parameter (4, Base.Duty'Access,        B);
         Statement.Bind_Parameter (5, Base.Slots'Access,       I);
      exception
         when Error : others =>
            Report
            (  (  " ODBC database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " command: "
               &  Quote (Command)
               &  " prepare fault"
               ),
               Error
            );
            return;
      end;
      declare
         Command : constant String :=
                   (  "INSERT INTO "
                   &  Table_Name
                   &  " ("
                   &  Time_Column         & ", "
                   &  Address_Column      & ", "
                   &  Device_Type_Column  & ", "
                   &  Error_Column        & ", "
                   &  Battery_Low_Column  & ", "
                   &  Panel_Locked_Column & ", "
                   &  Mode_Column
                   &  ") VALUES (Now(),?,?,?,?,?,?)"
                   );
         Statement : ODBC_Command_Ptr renames
                     Base.Insert_Minimal_Device;
      begin
         if Statement = null then
            Statement := new ODBC_Command (Base.DB);
         end if;
         Statement.Prepare (Command);
         Statement.Bind_Parameter (1, Base.Address'Access,      I);
         Statement.Bind_Parameter (2, Base.Device_Type'Access,  B);
         Statement.Bind_Parameter (3, Base.Error'Access,        B);
         Statement.Bind_Parameter (4, Base.Battery_Low'Access,  B);
         Statement.Bind_Parameter (5, Base.Panel_Locked'Access, B);
         Statement.Bind_Parameter (6, Base.Mode'Access,         B);
      exception
         when Error : others =>
            Report
            (  (  " ODBC database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " command: "
               &  Quote (Command)
               &  " prepare fault"
               ),
               Error
            );
            return;
      end;
      declare
         Command : constant String :=
                   (  "INSERT INTO "
                   &  Table_Name
                   &  " ("
                   &  Time_Column         & ", "
                   &  Address_Column      & ", "
                   &  Device_Type_Column  & ", "
                   &  Error_Column        & ", "
                   &  Battery_Low_Column  & ", "
                   &  Panel_Locked_Column & ", "
                   &  Mode_Column         & ", "
                   &  Open_Column
                   &  ") VALUES (Now(),?,?,?,?,?,?,?)"
                   );
         Statement : ODBC_Command_Ptr renames
                     Base.Insert_Shutter_Contact;
      begin
         if Statement = null then
            Statement := new ODBC_Command (Base.DB);
         end if;
         Statement.Prepare (Command);
         Statement.Bind_Parameter (1, Base.Address'Access,      I);
         Statement.Bind_Parameter (2, Base.Device_Type'Access,  B);
         Statement.Bind_Parameter (3, Base.Error'Access,        B);
         Statement.Bind_Parameter (4, Base.Battery_Low'Access,  B);
         Statement.Bind_Parameter (5, Base.Panel_Locked'Access, B);
         Statement.Bind_Parameter (6, Base.Mode'Access,         B);
         Statement.Bind_Parameter (7, Base.Open'Access,         B);
      exception
         when Error : others =>
            Report
            (  (  " ODBC database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " command: "
               &  Quote (Command)
               &  " prepare fault"
               ),
               Error
            );
            return;
      end;
      declare
         Command : constant String :=
                   (  "INSERT INTO "
                   &  Table_Name
                   &  " ("
                   &  Time_Column            & ", "
                   &  Address_Column         & ", "
                   &  Device_Type_Column     & ", "
                   &  Error_Column           & ", "
                   &  Battery_Low_Column     & ", "
                   &  Panel_Locked_Column    & ", "
                   &  Mode_Column            & ", "
                   &  Set_Temperature_Column & ", "
                   &  New_Temperature_Column
                   &  ") VALUES (Now(),?,?,?,?,?,?,?,?)"
                   );
         Statement : ODBC_Command_Ptr renames
                     Base.Insert_Wall_Thermostat_Short;
      begin
         if Statement = null then
            Statement := new ODBC_Command (Base.DB);
         end if;
         Statement.Prepare (Command);
         Statement.Bind_Parameter (1, Base.Address'Access,         I);
         Statement.Bind_Parameter (2, Base.Device_Type'Access,     B);
         Statement.Bind_Parameter (3, Base.Error'Access,           B);
         Statement.Bind_Parameter (4, Base.Battery_Low'Access,     B);
         Statement.Bind_Parameter (5, Base.Panel_Locked'Access,    B);
         Statement.Bind_Parameter (6, Base.Mode'Access,            B);
         Statement.Bind_Parameter (7, Base.Set_Temperature'Access, R);
         Statement.Bind_Parameter (8, Base.New_Temperature'Access, R);
      exception
         when Error : others =>
            Report
            (  (  " ODBC database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " command: "
               &  Quote (Command)
               &  " prepare fault"
               ),
               Error
            );
            return;
      end;
      declare
         Command : constant String :=
                   (  "INSERT INTO "
                   &  Table_Name
                   &  " ("
                   &  Time_Column            & ", "
                   &  Address_Column         & ", "
                   &  Device_Type_Column     & ", "
                   &  Error_Column           & ", "
                   &  Battery_Low_Column     & ", "
                   &  Panel_Locked_Column    & ", "
                   &  Mode_Column            & ", "
                   &  Set_Temperature_Column & ", "
                   &  New_Temperature_Column & ", "
                   &  Is_Temperature_Column
                   &  ") VALUES (Now(),?,?,?,?,?,?,?,?,?)"
                   );
         Statement : ODBC_Command_Ptr renames
                     Base.Insert_Wall_Thermostat_Full;
      begin
         if Statement = null then
            Statement := new ODBC_Command (Base.DB);
         end if;
         Statement.Prepare (Command);
         Statement.Bind_Parameter (1, Base.Address'Access,         I);
         Statement.Bind_Parameter (2, Base.Device_Type'Access,     B);
         Statement.Bind_Parameter (3, Base.Error'Access,           B);
         Statement.Bind_Parameter (4, Base.Battery_Low'Access,     B);
         Statement.Bind_Parameter (5, Base.Panel_Locked'Access,    B);
         Statement.Bind_Parameter (6, Base.Mode'Access,            B);
         Statement.Bind_Parameter (7, Base.Set_Temperature'Access, R);
         Statement.Bind_Parameter (8, Base.New_Temperature'Access, R);
         Statement.Bind_Parameter (9, Base.Is_Temperature'Access,  R);
      exception
         when Error : others =>
            Report
            (  (  " ODBC database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " command: "
               &  Quote (Command)
               &  " prepare fault"
               ),
               Error
            );
            return;
      end;
      declare
         Command : constant String :=
                   (  "INSERT INTO "
                   &  Table_Name
                   &  " ("
                   &  Time_Column            & ", " -- literal
                   &  Address_Column         & ", " --  1
                   &  Device_Type_Column     & ", " --  2
                   &  Error_Column           & ", " --  3
                   &  Battery_Low_Column     & ", " --  4
                   &  Panel_Locked_Column    & ", " --  5
                   &  Mode_Column            & ", " --  6
                   &  Set_Temperature_Column & ", " --  7
                   &  New_Temperature_Column & ", " --  8
                   &  Offset_Column          & ", " --  9
                   &  Valve_Column                  -- 10
                   &  ") VALUES (Now(),?,?,?,?,?,?,?,?,?,?)"
                   );
         Statement : ODBC_Command_Ptr renames
                     Base.Insert_Radiator_Thermostat_Short;
      begin
         if Statement = null then
            Statement := new ODBC_Command (Base.DB);
         end if;
         Statement.Prepare (Command);
         Statement.Bind_Parameter ( 1, Base.Address'Access,         I);
         Statement.Bind_Parameter ( 2, Base.Device_Type'Access,     B);
         Statement.Bind_Parameter ( 3, Base.Error'Access,           B);
         Statement.Bind_Parameter ( 4, Base.Battery_Low'Access,     B);
         Statement.Bind_Parameter ( 5, Base.Panel_Locked'Access,    B);
         Statement.Bind_Parameter ( 6, Base.Mode'Access,            B);
         Statement.Bind_Parameter ( 7, Base.Set_Temperature'Access, R);
         Statement.Bind_Parameter ( 8, Base.New_Temperature'Access, R);
         Statement.Bind_Parameter ( 9, Base.Offset'Access,          R);
         Statement.Bind_Parameter (10, Base.Valve'Access,           B);
      exception
         when Error : others =>
            Report
            (  (  " ODBC database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " command: "
               &  Quote (Command)
               &  " prepare fault"
               ),
               Error
            );
            return;
      end;
      declare
         Command : constant String :=
                   (  "INSERT INTO "
                   &  Table_Name
                   &  " ("
                   &  Time_Column            & ", " -- literal
                   &  Address_Column         & ", " --  1
                   &  Device_Type_Column     & ", " --  2
                   &  Error_Column           & ", " --  3
                   &  Battery_Low_Column     & ", " --  4
                   &  Panel_Locked_Column    & ", " --  5
                   &  Mode_Column            & ", " --  6
                   &  Set_Temperature_Column & ", " --  7
                   &  New_Temperature_Column & ", " --  8
                   &  Is_Temperature_Column  & ", " --  9
                   &  Offset_Column          & ", " -- 10
                   &  Valve_Column                  -- 11
                   &  ") VALUES (Now(),?,?,?,?,?,?,?,?,?,?,?)"
                   );
         Statement : ODBC_Command_Ptr renames
                     Base.Insert_Radiator_Thermostat_Full;
      begin
         if Statement = null then
            Statement := new ODBC_Command (Base.DB);
         end if;
         Statement.Prepare (Command);
         Statement.Bind_Parameter ( 1, Base.Address'Access,         I);
         Statement.Bind_Parameter ( 2, Base.Device_Type'Access,     B);
         Statement.Bind_Parameter ( 3, Base.Error'Access,           B);
         Statement.Bind_Parameter ( 4, Base.Battery_Low'Access,     B);
         Statement.Bind_Parameter ( 5, Base.Panel_Locked'Access,    B);
         Statement.Bind_Parameter ( 6, Base.Mode'Access,            B);
         Statement.Bind_Parameter ( 7, Base.Set_Temperature'Access, R);
         Statement.Bind_Parameter ( 8, Base.New_Temperature'Access, R);
         Statement.Bind_Parameter ( 9, Base.Is_Temperature'Access,  R);
         Statement.Bind_Parameter (10, Base.Offset'Access,          R);
         Statement.Bind_Parameter (11, Base.Valve'Access,           B);
      exception
         when Error : others =>
            Report
            (  (  " ODBC database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " command: "
               &  Quote (Command)
               &  " prepare fault"
               ),
               Error
            );
            return;
      end;
      Failed := False;
   end Create;

   procedure Create
             (  Base   : in out SQLite_Data;
                Widget : not null access MAX_Database_Record'Class;
                Silent : Boolean;
                Failed : out Boolean
             )  is
      procedure Report (Text : String; Error : Exception_Occurrence) is
      begin
         if Silent then
            if View /= null then
               View.Trace (Text, Error);
            end if;
         else
            Say (Text & ". " & Exception_Message (Error));
         end if;
      end Report;
      use SQLite;
   begin
      Failed := True;
      begin
         if DB_Trace and then View /= null then
            View.Trace
            (  "DB: Opening SQLite database "
            &  Quote (Widget.File_Name.Get_Text)
            );
         end if;
         Base.DB := Open (Widget.File_Name.Get_Text);
      exception
         when Error : others =>
            Report
            (  (" SQLite database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " open error"
               ),
               Error
            );
           return;
      end;
      declare
         Command : constant String :=
                   (  "CREATE TABLE IF NOT EXISTS "
                   &  Table_Name
                   &  " ("
                   &  Time_Column            & " TEXT, "     -- 1
                   &  Address_Column         & " INTEGER, "  -- 2
                   &  Device_Type_Column     & " INTEGER, "  -- 3
                   &  Set_Temperature_Column & " REAL, "     -- 4
                   &  New_Temperature_Column & " REAL, "     -- 5
                   &  Is_Temperature_Column  & " REAL, "     -- 6
                   &  Offset_Column          & " REAL, "     -- 7
                   &  Valve_Column           & " INTEGER, "  -- 8
                   &  Open_Column            & " INTEGER, "  -- 9
                   &  Error_Column           & " INTEGER, "  -- 10
                   &  Duty_Column            & " INTEGER, "  -- 11
                   &  Slots_Column           & " INTEGER, "  -- 12
                   &  Battery_Low_Column     & " INTEGER, "  -- 13
                   &  Panel_Locked_Column    & " INTEGER, "  -- 14
                   &  Mode_Column            & " INTEGER "   -- 15
                   &  ");"
                   );
      begin
         Base.DB.Exec (Command);
      exception
         when Error : others =>
            Report
            (  (  " SQLite database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " table creation command: "
               &  Quote (Command)
               &  " fault"
               ),
               Error
            );
            return;
      end;
      declare
         Command : constant String :=
                   (  "INSERT INTO "
                   &  Table_Name
                   &  " ("
                   &  Time_Column        & ", "
                   &  Address_Column     & ", "
                   &  Device_Type_Column & ", "
                   &  Error_Column       & ", "
                   &  Duty_Column        & ", "
                   &  Slots_Column
                   &  ") VALUES (?,?,?,?,?,?);"
                   );
      begin
         Base.Insert_Status := Prepare (Base.DB, Command);
      exception
         when Error : others =>
            Report
            (  (  " SQLite database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " command: "
               &  Quote (Command)
               &  " prepare fault"
               ),
               Error
            );
            return;
      end;
      declare
         Command : constant String :=
                   (  "INSERT INTO "
                   &  Table_Name
                   &  " ("
                   &  Time_Column         & ", "
                   &  Address_Column      & ", "
                   &  Device_Type_Column  & ", "
                   &  Error_Column        & ", "
                   &  Battery_Low_Column  & ", "
                   &  Panel_Locked_Column & ", "
                   &  Mode_Column
                   &  ") VALUES (?,?,?,?,?,?,?);"
                   );
      begin
         Base.Insert_Minimal_Device := Prepare (Base.DB, Command);
      exception
         when Error : others =>
            Report
            (  (  " SQLite database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " command: "
               &  Quote (Command)
               &  " prepare fault"
               ),
               Error
            );
            return;
      end;
      declare
         Command : constant String :=
                   (  "INSERT INTO "
                   &  Table_Name
                   &  " ("
                   &  Time_Column         & ", "
                   &  Address_Column      & ", "
                   &  Device_Type_Column  & ", "
                   &  Error_Column        & ", "
                   &  Battery_Low_Column  & ", "
                   &  Panel_Locked_Column & ", "
                   &  Mode_Column         & ", "
                   &  Open_Column
                   &  ") VALUES (?,?,?,?,?,?,?,?);"
                   );
      begin
         Base.Insert_Shutter_Contact := Prepare (Base.DB, Command);
      exception
         when Error : others =>
            Report
            (  (  " SQLite database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " command: "
               &  Quote (Command)
               &  " prepare fault"
               ),
               Error
            );
            return;
      end;
      declare
         Command : constant String :=
                   (  "INSERT INTO "
                   &  Table_Name
                   &  " ("
                   &  Time_Column            & ", "
                   &  Address_Column         & ", "
                   &  Device_Type_Column     & ", "
                   &  Error_Column           & ", "
                   &  Battery_Low_Column     & ", "
                   &  Panel_Locked_Column    & ", "
                   &  Mode_Column            & ", "
                   &  Set_Temperature_Column & ", "
                   &  New_Temperature_Column
                   &  ") VALUES (?,?,?,?,?,?,?,?,?);"
                   );
      begin
         Base.Insert_Wall_Thermostat_Short :=
            Prepare (Base.DB, Command);
      exception
         when Error : others =>
            Report
            (  (  " SQLite database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " command: "
               &  Quote (Command)
               &  " prepare fault"
               ),
               Error
            );
            return;
      end;
      declare
         Command : constant String :=
                   (  "INSERT INTO "
                   &  Table_Name
                   &  " ("
                   &  Time_Column            & ", "
                   &  Address_Column         & ", "
                   &  Device_Type_Column     & ", "
                   &  Error_Column           & ", "
                   &  Battery_Low_Column     & ", "
                   &  Panel_Locked_Column    & ", "
                   &  Mode_Column            & ", "
                   &  Set_Temperature_Column & ", "
                   &  New_Temperature_Column & ", "
                   &  Is_Temperature_Column
                   &  ") VALUES (?,?,?,?,?,?,?,?,?,?);"
                   );
      begin
         Base.Insert_Wall_Thermostat_Full := Prepare (Base.DB, Command);
      exception
         when Error : others =>
            Report
            (  (  " SQLite database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " command: "
               &  Quote (Command)
               &  " prepare fault"
               ),
               Error
            );
            return;
      end;
      declare
         Command : constant String :=
                   (  "INSERT INTO "
                   &  Table_Name
                   &  " ("
                   &  Time_Column            & ", " --  1
                   &  Address_Column         & ", " --  2
                   &  Device_Type_Column     & ", " --  3
                   &  Error_Column           & ", " --  4
                   &  Battery_Low_Column     & ", " --  5
                   &  Panel_Locked_Column    & ", " --  6
                   &  Mode_Column            & ", " --  7
                   &  Set_Temperature_Column & ", " --  8
                   &  New_Temperature_Column & ", " --  9
                   &  Offset_Column          & ", " -- 10
                   &  Valve_Column                  -- 11
                   &  ") VALUES (?,?,?,?,?,?,?,?,?,?,?);"
                   );
      begin
         Base.Insert_Radiator_Thermostat_Short :=
            Prepare (Base.DB, Command);
      exception
         when Error : others =>
            Report
            (  (  " SQLite database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " command: "
               &  Quote (Command)
               &  " prepare fault"
               ),
               Error
            );
            return;
      end;
      declare
         Command : constant String :=
                   (  "INSERT INTO "
                   &  Table_Name
                   &  " ("
                   &  Time_Column            & ", " --  1
                   &  Address_Column         & ", " --  2
                   &  Device_Type_Column     & ", " --  3
                   &  Error_Column           & ", " --  4
                   &  Battery_Low_Column     & ", " --  5
                   &  Panel_Locked_Column    & ", " --  6
                   &  Mode_Column            & ", " --  7
                   &  Set_Temperature_Column & ", " --  8
                   &  New_Temperature_Column & ", " --  9
                   &  Is_Temperature_Column  & ", " -- 10
                   &  Offset_Column          & ", " -- 11
                   &  Valve_Column                  -- 12
                   &  ") VALUES (?,?,?,?,?,?,?,?,?,?,?,?);"
                   );
      begin
         Base.Insert_Radiator_Thermostat_Full :=
            Prepare (Base.DB, Command);
      exception
         when Error : others =>
            Report
            (  (  " SQLite database "
               &  Quote (Widget.File_Name.Get_Text)
               &  " command: "
               &  Quote (Command)
               &  " prepare fault"
               ),
               Error
            );
            return;
      end;
      Failed := False;
   end Create;

   procedure Finalize (Base : in out ODBC_Data) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  ODBC_Connection'Class,
                ODBC_Connection_Ptr
             );
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  ODBC_Command'Class,
                ODBC_Command_Ptr
             );
   begin
      if DB_Trace and then View /= null then
         View.Trace ("DB: Finalizing ODBC connection");
      end if;
      Free (Base.Insert_Status);
      Free (Base.Insert_Minimal_Device);
      Free (Base.Insert_Shutter_Contact);
      Free (Base.Insert_Wall_Thermostat_Short);
      Free (Base.Insert_Wall_Thermostat_Full);
      Free (Base.Insert_Radiator_Thermostat_Short);
      Free (Base.Insert_Radiator_Thermostat_Full);
      Free (Base.DB);
      Abstract_Data_Base (Base).Finalize;
      if DB_Trace and then View /= null then
         View.Trace ("DB: ODBC connection finalized");
      end if;
   end Finalize;

   procedure Finalize (Base : in out SQLite_Data) is
   begin
      if DB_Trace and then View /= null then
         View.Trace ("DB: Finalizing SQLite connection");
      end if;
      Abstract_Data_Base (Base).Finalize;
      if DB_Trace and then View /= null then
         View.Trace ("DB: SQLite connection finalized");
      end if;
   end Finalize;

   function Get_Data
            (  Widget  : not null access MAX_Database_Record;
               Address : RF_Address
            )  return not null DB_Record_Ptr is
      Index : constant Integer := Widget.Last.Find (Address);
   begin
      if Index > 0 then
         return Widget.Last.Get (Index);
      else
         return Result : constant not null DB_Record_Ptr :=
                                           new DB_Record
         do
            Widget.Last.Replace (Address, Result);
         end return;
      end if;
   end Get_Data;

   function Gtk_Database_New return MAX_Database is
      Result : constant MAX_Database := new MAX_Database_Record;
      Label  : Gtk_Label;
   begin
      Gtk.Box.Initialize (Result, Orientation_Vertical, 3);
      Result.Set_Border_Width (5);
      Gtk_New (Result.Grid);
      Result.Grid.Set_Row_Spacing (3);
      Result.Grid.Set_Column_Spacing (3);
      Result.Pack_Start (Result.Grid);
      -- Row 1 ---------------------------------------------------------
      Gtk_New (Result.Enable, "Enable");
      Result.Grid.Attach (Result.Enable, 0, 0);
      Result.Enable.Set_Tooltip_Text
      (  "Enable logging into the database"
      );
      Result.Enable.Set_Active
      (  Restore ("enable-logging", "off") = "on"
      );
      -- Row 2 ---------------------------------------------------------
      Gtk_New (Label, "Database interface");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.Enable, Pos_Bottom);

      Gtk_New (Result.Method);
      Result.Method.Append_Text ("ODBC");
      Result.Method.Append_Text ("SQLite");
      Result.Method.Set_Tooltip_Text
      (  "The database connectivity interface. ODBC is Open Database " &
         "Connectivity interface supported by most database "          &
         "management systems. Use this if you plan to store data "     &
         "a large or remote database. SQLite is a single-file based "  &
         "database, e.g. on the local filesystem"
      );
      Result.Grid.Attach_Next_To
      (  Result.Method,
         Label,
         Pos_Right
      );
      -- Row 3 ---------------------------------------------------------
      Gtk_New (Result.DSN);
      Result.Grid.Attach_Next_To
      (  Result.DSN,
         Result.Method,
         Pos_Bottom
      );
      Result.DSN.Set_Hexpand (True);
      Result.DSN.Set_Tooltip_Text
      (  "The data source name (DSN) identifies the database. "   &
         "Refer to the OS documentation about ODBC to configure " &
         "the DSN for your database"
      );
      Gtk_New (Label, "Data source name (DSN)");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.DSN, Pos_Left);
      -- Row 4 ---------------------------------------------------------
      Gtk_New (Result.User);
      Result.Grid.Attach_Next_To
      (  Result.User,
         Result.DSN,
         Pos_Bottom
      );
      Result.User.Set_Tooltip_Text
      (  "The user name to access the data source"
      );
      Gtk_New (Label, "User name");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.User, Pos_Left);
      -- Row 5 ---------------------------------------------------------
      Gtk_New (Result.Password);
      Result.Grid.Attach_Next_To
      (  Result.Password,
         Result.User,
         Pos_Bottom
      );
      Result.Password.Set_Visibility (False);
      Gtk_New (Result.Visible);
      Result.Visible.Set_Tooltip_Text ("Show the password");
      Result.Grid.Attach_Next_To
      (  Result.Visible,
         Result.Password,
         Pos_Right
      );
      Gtk_New (Label, "Password");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.Password, Pos_Left);
      Result.Password.Set_Tooltip_Text
      (  "The password to access the data source"
      );
      -- Row 6 ---------------------------------------------------------
      Gtk_New (Result.File_Name);
      Result.Grid.Attach_Next_To
      (  Result.File_Name,
         Result.Password,
         Pos_Bottom
      );
      Result.File_Name.Set_Tooltip_Text
      (  "The SQLite database file name. An SQLite database is a " &
         "simple file locate on the filesystem"
      );
      Gtk_New (Label, "File name");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.File_Name, Pos_Left);

      SQLite_File_Buttons.Gtk_New (Result.Browse);
      Result.Grid.Attach_Next_To
      (  Result.Browse,
         Result.File_Name,
         Pos_Right
      );
   ---------------------------------------------------------------------
      On_Revert (Result, Result);

      Connect
      (  Result,
         "destroy",
         On_Destroy'Access,
         Result
      );
      Connect
      (  Result.Browse,
         "clicked",
         On_Browse'Access,
         Result
      );
      Connect
      (  Result.Browse,
         "clicked",
         On_Browse'Access,
         Result
      );
      Connect
      (  Result.Enable,
         "toggled",
         On_Toggled'Access,
         Result
      );
      Connect
      (  Result.Visible,
         "toggled",
         On_Visible'Access,
         Result
      );
      Connect
      (  Result.Method,
         "changed",
         On_Method_Changed'Access,
         Result
      );
      return Result;
   end Gtk_Database_New;

   procedure Insert_Minimal_Device
             (  Base    : in out ODBC_Data;
                Address : RF_Address;
                Data    : DB_Record
             )  is
   begin
      Base.Address.Value      := SQLUINTEGER (Address);
      Base.Device_Type.Value  := SQLUTINYINT (Data.Device_Type);
      Base.Error.Value        := SQLUTINYINT (Data.Error);
      Base.Battery_Low.Value  := SQLUTINYINT (Data.Battery_Low);
      Base.Panel_Locked.Value := SQLUTINYINT (Data.Panel_Locked);
      Base.Mode.Value         := SQLUTINYINT (Data.Mode);
      Base.Insert_Minimal_Device.Execute;
   end Insert_Minimal_Device;

   procedure Insert_Minimal_Device
             (  Base    : in out SQLite_Data;
                Address : RF_Address;
                Data    : DB_Record
             )  is
      use Interfaces.C;
      use SQLite;
      This : Statement renames Base.Insert_Minimal_Device;
   begin
      This.Bind (1, Image (Clock, True));
      This.Bind (2, int (Address));
      This.Bind (3, int (Data.Device_Type));
      This.Bind (3, int (Data.Error));
      This.Bind (4, int (Data.Battery_Low));
      This.Bind (5, int (Data.Panel_Locked));
      This.Bind (6, int (Data.Mode));
      Step (This);
      Reset (This);
   end Insert_Minimal_Device;

   procedure Insert_Radiator_Thermostat_Full
             (  Base    : in out ODBC_Data;
                Address : RF_Address;
                Data    : DB_Record
             )  is
   begin
      Base.Address.Value         := SQLUINTEGER (Address);
      Base.Device_Type.Value     := SQLUTINYINT (Data.Device_Type);
      Base.Error.Value           := SQLUTINYINT (Data.Error);
      Base.Battery_Low.Value     := SQLUTINYINT (Data.Battery_Low);
      Base.Panel_Locked.Value    := SQLUTINYINT (Data.Panel_Locked);
      Base.Mode.Value            := SQLUTINYINT (Data.Mode);
      Base.Set_Temperature.Value := SQLDOUBLE   (Data.Set_Temperature);
      Base.New_Temperature.Value := SQLDOUBLE   (Data.New_Temperature);
      Base.Is_Temperature.Value  := SQLDOUBLE   (Data.Is_Temperature);
      Base.Valve.Value           := SQLUTINYINT (Data.Valve);
      Base.Offset.Value          := SQLDOUBLE   (Data.Offset);
      Base.Insert_Radiator_Thermostat_Full.Execute;
   end Insert_Radiator_Thermostat_Full;

   procedure Insert_Radiator_Thermostat_Full
             (  Base    : in out SQLite_Data;
                Address : RF_Address;
                Data    : DB_Record
             )  is
      use Interfaces.C;
      use SQLite;
      This : Statement renames Base.Insert_Radiator_Thermostat_Full;
   begin
      This.Bind ( 1, Image (Clock, True));
      This.Bind ( 2, int (Address));
      This.Bind ( 3, int (Data.Device_Type));
      This.Bind ( 4, int (Data.Error));
      This.Bind ( 5, int (Data.Battery_Low));
      This.Bind ( 6, int (Data.Panel_Locked));
      This.Bind ( 7, int (Data.Mode));
      This.Bind ( 8, double (Data.Set_Temperature));
      This.Bind ( 9, double (Data.New_Temperature));
      This.Bind (10, double (Data.Is_Temperature));
      This.Bind (11, double (Data.Offset));
      This.Bind (12, int (Data.Valve));
      Step (This);
      Reset (This);
   end Insert_Radiator_Thermostat_Full;

   procedure Insert_Radiator_Thermostat_Short
             (  Base    : in out ODBC_Data;
                Address : RF_Address;
                Data    : DB_Record
             )  is
   begin
      Base.Address.Value         := SQLUINTEGER (Address);
      Base.Device_Type.Value     := SQLUTINYINT (Data.Device_Type);
      Base.Error.Value           := SQLUTINYINT (Data.Error);
      Base.Battery_Low.Value     := SQLUTINYINT (Data.Battery_Low);
      Base.Panel_Locked.Value    := SQLUTINYINT (Data.Panel_Locked);
      Base.Mode.Value            := SQLUTINYINT (Data.Mode);
      Base.Set_Temperature.Value := SQLDOUBLE   (Data.Set_Temperature);
      Base.New_Temperature.Value := SQLDOUBLE   (Data.New_Temperature);
      Base.Valve.Value           := SQLUTINYINT (Data.Valve);
      Base.Offset.Value          := SQLDOUBLE   (Data.Offset);
      Base.Insert_Radiator_Thermostat_Short.Execute;
   end Insert_Radiator_Thermostat_Short;

   procedure Insert_Radiator_Thermostat_Short
             (  Base    : in out SQLite_Data;
                Address : RF_Address;
                Data    : DB_Record
             )  is
      use Interfaces.C;
      use SQLite;
      This : Statement renames Base.Insert_Radiator_Thermostat_Short;
   begin
      This.Bind ( 1, Image (Clock, True));
      This.Bind ( 2, int (Address));
      This.Bind ( 3, int (Data.Device_Type));
      This.Bind ( 4, int (Data.Error));
      This.Bind ( 5, int (Data.Battery_Low));
      This.Bind ( 6, int (Data.Panel_Locked));
      This.Bind ( 7, int (Data.Mode));
      This.Bind ( 8, double (Data.Set_Temperature));
      This.Bind ( 9, double (Data.New_Temperature));
      This.Bind (10, double (Data.Offset));
      This.Bind (11, int (Data.Valve));
      Step (This);
      Reset (This);
   end Insert_Radiator_Thermostat_Short;

   procedure Insert_Wall_Thermostat_Full
             (  Base    : in out ODBC_Data;
                Address : RF_Address;
                Data    : DB_Record
             )  is
   begin
      Base.Address.Value         := SQLUINTEGER (Address);
      Base.Device_Type.Value     := SQLUTINYINT (Data.Device_Type);
      Base.Error.Value           := SQLUTINYINT (Data.Error);
      Base.Battery_Low.Value     := SQLUTINYINT (Data.Battery_Low);
      Base.Panel_Locked.Value    := SQLUTINYINT (Data.Panel_Locked);
      Base.Mode.Value            := SQLUTINYINT (Data.Mode);
      Base.Set_Temperature.Value := SQLDOUBLE   (Data.Set_Temperature);
      Base.New_Temperature.Value := SQLDOUBLE   (Data.New_Temperature);
      Base.Is_Temperature.Value  := SQLDOUBLE   (Data.Is_Temperature);
      Base.Insert_Wall_Thermostat_Full.Execute;
   end Insert_Wall_Thermostat_Full;

   procedure Insert_Wall_Thermostat_Full
             (  Base    : in out SQLite_Data;
                Address : RF_Address;
                Data    : DB_Record
             )  is
      use Interfaces.C;
      use SQLite;
      This : Statement renames Base.Insert_Wall_Thermostat_Full;
   begin
      This.Bind ( 1, Image (Clock, True));
      This.Bind ( 2, int (Address));
      This.Bind ( 3, int (Data.Device_Type));
      This.Bind ( 4, int (Data.Error));
      This.Bind ( 5, int (Data.Battery_Low));
      This.Bind ( 6, int (Data.Panel_Locked));
      This.Bind ( 7, int (Data.Mode));
      This.Bind ( 8, double (Data.Set_Temperature));
      This.Bind ( 8, double (Data.New_Temperature));
      This.Bind (10, double (Data.Is_Temperature));
      Step (This);
      Reset (This);
   end Insert_Wall_Thermostat_Full;

   procedure Insert_Wall_Thermostat_Short
             (  Base    : in out ODBC_Data;
                Address : RF_Address;
                Data    : DB_Record
             )  is
   begin
      Base.Address.Value         := SQLUINTEGER (Address);
      Base.Device_Type.Value     := SQLUTINYINT (Data.Device_Type);
      Base.Error.Value           := SQLUTINYINT (Data.Error);
      Base.Battery_Low.Value     := SQLUTINYINT (Data.Battery_Low);
      Base.Panel_Locked.Value    := SQLUTINYINT (Data.Panel_Locked);
      Base.Mode.Value            := SQLUTINYINT (Data.Mode);
      Base.Set_Temperature.Value := SQLDOUBLE   (Data.Set_Temperature);
      Base.New_Temperature.Value := SQLDOUBLE   (Data.New_Temperature);
      Base.Insert_Wall_Thermostat_Short.Execute;
   end Insert_Wall_Thermostat_Short;

   procedure Insert_Wall_Thermostat_Short
             (  Base    : in out SQLite_Data;
                Address : RF_Address;
                Data    : DB_Record
             )  is
      use Interfaces.C;
      use SQLite;
      This : Statement renames Base.Insert_Wall_Thermostat_Short;
   begin
      This.Bind (1, Image (Clock, True));
      This.Bind (2, int (Address));
      This.Bind (3, int (Data.Device_Type));
      This.Bind (4, int (Data.Error));
      This.Bind (5, int (Data.Battery_Low));
      This.Bind (6, int (Data.Panel_Locked));
      This.Bind (7, int (Data.Mode));
      This.Bind (8, double (Data.Set_Temperature));
      This.Bind (9, double (Data.New_Temperature));
      Step (This);
      Reset (This);
   end Insert_Wall_Thermostat_Short;

   procedure Insert_Shutter_Contact
             (  Base    : in out ODBC_Data;
                Address : RF_Address;
                Data    : DB_Record
             )  is
   begin
      Base.Address.Value      := SQLUINTEGER (Address);
      Base.Device_Type.Value  := SQLUTINYINT (Data.Device_Type);
      Base.Error.Value        := SQLUTINYINT (Data.Error);
      Base.Battery_Low.Value  := SQLUTINYINT (Data.Battery_Low);
      Base.Panel_Locked.Value := SQLUTINYINT (Data.Panel_Locked);
      Base.Mode.Value         := SQLUTINYINT (Data.Mode);
      Base.Open.Value         := SQLUTINYINT (Data.Open);
      Base.Insert_Shutter_Contact.Execute;
   end Insert_Shutter_Contact;

   procedure Insert_Shutter_Contact
             (  Base    : in out SQLite_Data;
                Address : RF_Address;
                Data    : DB_Record
             )  is
      use Interfaces.C;
      use SQLite;
      This : Statement renames Base.Insert_Shutter_Contact;
   begin
      This.Bind (1, Image (Clock, True));
      This.Bind (2, int (Address));
      This.Bind (3, int (Data.Device_Type));
      This.Bind (4, int (Data.Error));
      This.Bind (5, int (Data.Battery_Low));
      This.Bind (6, int (Data.Panel_Locked));
      This.Bind (7, int (Data.Mode));
      This.Bind (8, int (Data.Open));
      Step (This);
      Reset (This);
   end Insert_Shutter_Contact;

   procedure Insert_Status
             (  Base    : in out ODBC_Data;
                Address : RF_Address;
                Data    : DB_Record
             )  is
   begin
      Base.Address.Value     := SQLUINTEGER (Address);
      Base.Device_Type.Value := SQLUTINYINT (Data.Device_Type);
      Base.Error.Value       := SQLUTINYINT (Data.Error);
      Base.Duty.Value        := SQLUTINYINT (Data.Duty);
      Base.Slots.Value       := SQLUINTEGER (Data.Error);
      Base.Insert_Status.Execute;
   end Insert_Status;

   procedure Insert_Status
             (  Base    : in out SQLite_Data;
                Address : RF_Address;
                Data    : DB_Record
             )  is
      use Interfaces.C;
      use SQLite;
      This : Statement renames Base.Insert_Status;
   begin
      This.Bind (1, Image (Clock, True));
      This.Bind (2, int (Address));
      This.Bind (3, int (Data.Device_Type));
      This.Bind (4, int (Data.Error));
      This.Bind (5, int (Data.Duty));
      This.Bind (6, int (Data.Slots));
      Step (This);
      Reset (This);
   end Insert_Status;

   function Is_Enabled (Widget : not null access MAX_Database_Record)
      return Boolean is
   begin
      return Widget.Enable.Get_Active;
   end Is_Enabled;

   procedure On_Browse
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Database
             )  is
      Dialog : File_Dialog;
   begin
      Dialog :=
         Gtk_New
         (  "Select or create an SQLite database file",
            Action_Save,
            Widget.File_Name
         );
      Dialog.Show_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Browse")
         )  );
   end On_Browse;

   procedure On_Destroy
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Database
             )  is
      procedure Free is
         new Ada.Unchecked_Deallocation (DB_Record, DB_Record_Ptr);
   begin
      Widget.Base.Initialize;
      for Index in 1..Widget.Last.Get_Size loop
         declare
            This : DB_Record_Ptr := Widget.Last.Get (Index);
         begin
            Free (This);
         end;
      end loop;
   end On_Destroy;

   procedure On_Method_Changed
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Database
             )  is
   begin
      Widget.Set_Enabled;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Method_Changed")
         )  );
   end On_Method_Changed;

   procedure On_Revert
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Database
             )  is
   begin
      Widget.DSN.Set_Text (Restore ("dsn", ""));
      Widget.File_Name.Set_Text (Restore ("sqlite-file", ""));
      begin
         Widget.Method.Set_Active
         (  GInt'Value (Restore ("logging-method", "0"))
         );
      exception
         when Constraint_Error =>
            Widget.Method.Set_Active (0);
      end;
      declare
         Encoded : constant String := Restore ("odbc-credentials", "");
      begin
         if Encoded'Length > 0 then
            declare
               type State is (Have_User, Have_Password, Tail);
               Data     : constant String := Decode (Encoded, Key);
               Part     : State   := Have_User;
               User     : Integer := Data'First - 1;
               Password : Integer := Data'First - 1;
            begin
               for Index in Data'Range loop
                  if Data (Index) = Character'Val (0) then
                     case Part is
                        when Have_User =>
                           User     := Index - 1;
                           Password := User;
                           Part     := Have_Password;
                        when Have_Password =>
                           Password := Index - 1;
                           Part     := Tail;
                           exit;
                        when Tail =>
                           exit;
                     end case;
                  end if;
               end loop;
               case Part is
                  when Have_User => -- Nothing found
                     Widget.User.Set_Text ("");
                     Widget.Password.Set_Text ("");
                  when Have_Password => -- No password
                     Widget.User.Set_Text (Data (Data'First..User));
                     Widget.Password.Set_Text ("");
                  when Tail => -- Everything is here
                     Widget.User.Set_Text (Data (Data'First..User));
                     Widget.Password.Set_Text
                     (  Data (User + 2..Password)
                     );
               end case;
            end;
         else
            Widget.User.Set_Text ("");   -- Ignore decode error
            Widget.Password.Set_Text ("");
         end if;
      exception
         when Constraint_Error | Ada.IO_Exceptions.Data_Error =>
            Widget.User.Set_Text ("");   -- Ignore decode error
            Widget.Password.Set_Text ("");
      end;
      declare
         Failed : Boolean := False;
      begin
         if Widget.Enable.Get_Active then
            case Widget.Method.Get_Active is
               when 0 => -- ODBC
                  Widget.Base.Set (new ODBC_Data);
               when 1 => -- SQLite
                  Widget.Base.Set (new SQLite_Data);
               when others =>
                  Failed := True;
            end case;
            if Failed then
               Widget.Enable.Set_Active (False);
            else
               Widget.Base.Ptr.Create (Widget, True, Failed);
               if Failed then
                  Widget.Base.Invalidate;
                  Widget.Enable.Set_Active (False);
               else
                  Widget.Store;
               end if;
            end if;
         end if;
      end;
      Widget.Set_Enabled;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Revert")
         )  );
   end On_Revert;

   procedure On_Toggled
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Database
             )  is
      Failed : Boolean := False;
   begin
      Widget.Base.Invalidate;
      if Widget.Enable.Get_Active then
         case Widget.Method.Get_Active is
            when 0 => -- ODBC
               Widget.Base.Set (new ODBC_Data);
            when 1 => -- SQLite
               Widget.Base.Set (new SQLite_Data);
            when others =>
               Failed := True;
         end case;
         if Failed then
            Widget.Enable.Set_Active (False);
         else
            Widget.Base.Ptr.Create (Widget, False, Failed);
            if Failed then
               Widget.Base.Invalidate;
               Widget.Enable.Set_Active (False);
            else
               Widget.Store;
            end if;
         end if;
      else
         Widget.Base.Invalidate;
         Widget.Store;
      end if;
      Widget.Set_Enabled;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Toggled")
         )  );
   end On_Toggled;

   procedure On_Visible
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Database
             )  is
   begin
      Widget.Password.Set_Visibility (Widget.Visible.Get_Active);
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Visible")
         )  );
   end On_Visible;

   procedure Set_Enabled
             (  Widget : not null access MAX_Database_Record
             )  is
      Enabled : constant Boolean := Widget.Enable.Get_Active;
      ODBC    : constant Boolean := Widget.Method.Get_Active = 0;
   begin
      Widget.Method.Set_Sensitive     (not Enabled);
      Widget.DSN.Set_Sensitive        (ODBC and not Enabled);
      Widget.File_Name.Set_Sensitive  (not ODBC and not Enabled);
      Widget.User.Set_Sensitive       (ODBC and not Enabled);
      Widget.Password.Set_Sensitive   (ODBC and not Enabled);
      Widget.Visible.Set_Sensitive    (ODBC and not Enabled);
      Widget.Browse.Set_Sensitive     (not ODBC and not Enabled);
   end Set_Enabled;

   procedure Store (Widget : not null access MAX_Database_Record) is
   begin
      if Widget.Enable.Get_Active then
         Store ("enable-logging", "on");
      else
         Store ("enable-logging", "off");
      end if;
      if Widget.Enable.Get_Active then
         Store ("dsn",         Widget.DSN.Get_Text);
         Store ("sqlite-file", Widget.File_Name.Get_Text);
         Store
         (  "logging-method",
            Image (Integer (Widget.Method.Get_Active))
         );
         Store
         (  "odbc-credentials",
            Encode
            (  (  Widget.User.Get_Text
               &  Character'Val (0)
               &  Widget.Password.Get_Text
               &  Character'Val (0)
               ),
               Key
         )  );
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Store")
         )  );
   end Store;

   procedure Update_Data
             (  Logger : not null access MAX_Database_Record;
                Data   : Device_Data;
                Offset : Centigrade
             )  is
   begin
      if not Logger.Base.Is_Valid then
         return;
      end if;
      declare
         Last : DB_Record renames Logger.Get_Data (Data.Address).all;

         function Updated return Boolean is
         begin
            return Result : Boolean := False do
               if Last.Device_Type /= Device_Type'Pos (Data.Kind_Of)
               then
                  Result := True;
                  Last.Device_Type := Device_Type'Pos (Data.Kind_Of);
               end if;
               if Last.Error /= Boolean'Pos (Data.Error) then
                  Result := True;
                  Last.Error := Boolean'Pos (Data.Error);
               end if;
               if Last.Battery_Low /= Boolean'Pos (Data.Battery_Low)
               then
                  Result := True;
                  Last.Battery_Low := Boolean'Pos (Data.Battery_Low);
               end if;
               if (  Last.Panel_Locked
                  /= Boolean'Pos (Data.Panel_Locked)
                  )
               then
                  Result := True;
                  Last.Panel_Locked :=
                     Boolean'Pos (Data.Panel_Locked);
               end if;
               if Last.Mode /= Operating_Mode'Pos (Data.Mode) then
                  Result := True;
                  Last.Mode := Operating_Mode'Pos (Data.Mode);
               end if;
            end return;
         end Updated;

         function Updated_Open return Boolean is
         begin
            return Result : Boolean := False do
               if Last.Open /= Boolean'Pos (Data.Open) then
                  Result := True;
                  Last.Open := Boolean'Pos (Data.Open);
               end if;
            end return;
         end Updated_Open;

         function Updated_Schedule return Boolean is
         begin
            return Result : Boolean := False do
               if Last.Set_Temperature /= Data.Set_Temperature then
                  Result := True;
                  Last.Set_Temperature := Data.Set_Temperature;
               end if;
               if Last.New_Temperature /= Data.New_Temperature then
                  Result := True;
                  Last.New_Temperature := Data.New_Temperature;
               end if;
            end return;
         end Updated_Schedule;

         function Updated_Temperature return Boolean is
         begin
            return Result : Boolean := False do
               if Last.Is_Temperature /= Data.Temperature then
                  Result := True;
                  Last.Is_Temperature := Data.Temperature;
               end if;
            end return;
         end Updated_Temperature;

         function Updated_Valve return Boolean is
            New_Valve : constant Unsigned_8 :=
                Unsigned_8 (Float (Data.Valve_Position) * 100.0);
         begin
            return Result : Boolean := False do
               if Last.Offset /= Offset then
                  Result := True;
                  Last.Offset := Offset;
               end if;
               if Last.Valve /= New_Valve then
                  Result := True;
                  Last.Valve := New_Valve;
               end if;
            end return;
         end Updated_Valve;

      begin
         for Try in 1..Max_Attempts loop
            begin
               case Data.Kind_Of is
                  when Cube | Eco_Button =>
                     if Try > 1 or else Updated then
                        if DB_Trace and then View /= null then
                           View.Trace
                           (  "DB: inserting address="
                           &  Data.Address
                           &  ", device_type="
                           &  Device (Last.Device_Type)
                           &  ", error="
                           &  Last.Error
                           &  ", battery_low="
                           &  Last.Battery_Low
                           &  ", panel_locked="
                           &  Last.Panel_Locked
                           &  ", current_mode="
                           &  Mode (Last.Mode)
                           );
                        end if;
                        Logger.Base.Ptr.Insert_Minimal_Device
                        (  Address => Data.Address,
                           Data    => Last
                        );
                     end if;
                  when Radiator_Thermostat | Radiator_Thermostat_Plus =>
                     if Data.Temperature = Centigrade'First then
                        if Try > 1 or else (  Updated
                                           or Updated_Schedule
                                           or Updated_Valve
                                           )
                        then
                           if DB_Trace and then View /= null then
                              View.Trace
                              (  "DB: inserting address="
                              &  Data.Address
                              &  ", device_type="
                              &  Device (Last.Device_Type)
                              &  ", set_temperature="
                              &  Last.Set_Temperature
                              &  ", new_temperature="
                              &  Last.New_Temperature
                              &  ", offset="
                              &  Last.Offset
                              &  ", valve_position="
                              &  Last.Valve
                              &  ", error="
                              &  Last.Error
                              &  ", battery_low="
                              &  Last.Battery_Low
                              &  ", panel_locked="
                              &  Last.Panel_Locked
                              &  ", current_mode="
                              &  Mode (Last.Mode)
                              );
                           end if;
                           Logger.Base.Ptr.
                           Insert_Radiator_Thermostat_Short
                           (  Address => Data.Address,
                              Data    => Last
                           );
                        end if;
                     else
                        if Try > 1 or else (  Updated
                                           or Updated_Schedule
                                           or Updated_Valve
                                           or Updated_Temperature
                                           )
                        then
                           if DB_Trace and then View /= null then
                              View.Trace
                              (  "DB: inserting address="
                              &  Data.Address
                              &  ", device_type="
                              &  Device (Last.Device_Type)
                              &  ", set_temperature="
                              &  Last.Set_Temperature
                              &  ", new_temperature="
                              &  Last.New_Temperature
                              &  ", is_temperature="
                              &  Last.Is_Temperature
                              &  ", offset="
                              &  Last.Offset
                              &  ", valve_position="
                              &  Last.Valve
                              &  ", error="
                              &  Last.Error
                              &  ", battery_low="
                              &  Last.Battery_Low
                              &  ", panel_locked="
                              &  Last.Panel_Locked
                              &  ", current_mode="
                              &  Mode (Last.Mode)
                              );
                           end if;
                           Logger.Base.Ptr.
                           Insert_Radiator_Thermostat_Full
                           (  Address => Data.Address,
                              Data    => Last
                           );
                        end if;
                     end if;
                  when Wall_Thermostat =>
                     if Data.Temperature = Centigrade'First then
                        if Try > 1 or else (  Updated
                                           or Updated_Schedule
                                           )
                        then
                           if DB_Trace and then View /= null then
                              View.Trace
                              (  "DB: inserting address="
                              &  Data.Address
                              &  ", device_type="
                              &  Device (Last.Device_Type)
                              &  ", set_temperature="
                              &  Last.Set_Temperature
                              &  ", new_temperature="
                              &  Last.New_Temperature
                              &  ", error="
                              &  Last.Error
                              &  ", battery_low="
                              &  Last.Battery_Low
                              &  ", panel_locked="
                              &  Last.Panel_Locked
                              &  ", current_mode="
                              &  Mode (Last.Mode)
                              );
                           end if;
                           Logger.Base.Ptr.
                           Insert_Wall_Thermostat_Short
                           (  Address => Data.Address,
                              Data    => Last
                           );
                        end if;
                     else
                        if Try > 1 or else (  Updated
                                           or Updated_Schedule
                                           or Updated_Temperature
                                           )
                        then
                           if DB_Trace and then View /= null then
                              View.Trace
                              (  "DB: inserting address="
                              &  Data.Address
                              &  ", device_type="
                              &  Device (Last.Device_Type)
                              &  ", set_temperature="
                              &  Last.Set_Temperature
                              &  ", new_temperature="
                              &  Last.New_Temperature
                              &  ", is_temperature="
                              &  Last.Is_Temperature
                              &  ", error="
                              &  Last.Error
                              &  ", battery_low="
                              &  Last.Battery_Low
                              &  ", panel_locked="
                              &  Last.Panel_Locked
                              &  ", current_mode="
                              &  Mode (Last.Mode)
                              );
                           end if;
                           Logger.Base.Ptr.
                           Insert_Wall_Thermostat_Full
                           (  Address => Data.Address,
                              Data    => Last
                           );
                        end if;
                     end if;
                  when Shutter_Contact =>
                     if Try > 1 or else (Updated or Updated_Open) then
                        if DB_Trace and then View /= null then
                           View.Trace
                           (  "DB: inserting address="
                           &  Data.Address
                           &  ", device_type="
                           &  Device (Last.Device_Type)
                           &  ", contact_open="
                           &  Last.Open
                           &  ", error="
                           &  Last.Error
                           &  ", battery_low="
                           &  Last.Battery_Low
                           &  ", panel_locked="
                           &  Last.Panel_Locked
                           &  ", current_mode="
                           &  Mode (Last.Mode)
                           );
                        end if;
                        Logger.Base.Ptr.Insert_Shutter_Contact
                        (  Address => Data.Address,
                           Data    => Last
                        );
                     end if;
                  when Unknown =>
                     null;
               end case;
               exit;
            exception
               when Error : others =>
                  if View /= null then
                     View.Trace ("DB: Database write error", Error);
                     Logger.Base.Invalidate;
                     Logger.Enable.Set_Active (False);
                     View.Trace ("DB: Trying to re-open the database");
                     Logger.Enable.Set_Active (True);
                     if not Logger.Base.Is_Valid then
                        return;
                     end if;
                     if Try = Max_Attempts then
                        View.Trace
                        (  "DB: Giving up after "
                        &  Image (Max_Attempts)
                        &  " attempts"
                        );
                        return;
                     end if;
                     View.Trace
                     (  "DB: Trying again ["
                     &  Image (Try)
                     &  '/'
                     &  Image (Max_Attempts - 1)
                     &  ']'
                     );
                  end if;
            end;
         end loop;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Update_Data")
         )  );
   end Update_Data;

   procedure Update_Status
             (  Logger  : not null access MAX_Database_Record;
                Address : RF_Address;
                Error   : Boolean;
                Duty    : Ratio;
                Slots   : Natural
             )  is
   begin
      if not Logger.Base.Is_Valid then
         return;
      end if;
      declare
         Last      : DB_Record renames Logger.Get_Data (Address).all;
         New_Type  : constant Unsigned_8 := Device_Type'Pos (Cube);
         New_Error : constant Unsigned_8 := Boolean'Pos (Error);
         New_Duty  : constant Unsigned_8 :=
                              Unsigned_8 (Float (Duty) * 100.0);
      begin
         if (  Last.Device_Type = New_Type
            and then
               Last.Error = New_Error
            and then
               Last.Slots = Slots
            and then
               Last.Duty = New_Duty
            )
         then
            return;
         end if;
         Last.Device_Type := New_Type;
         Last.Error       := New_Error;
         Last.Duty        := New_Duty;
         Last.Slots       := Slots;
         for Try in 1..Max_Attempts loop
            begin
               if DB_Trace and then View /= null then
                  View.Trace
                  (  "DB: inserting address="
                  &  Address
                  &  ", device_type="
                  &  Device (Last.Device_Type)
                  &  ", error="
                  &  Last.Error
                  &  ", duty="
                  &  Last.Duty
                  &  ", slots="
                  &  Last.Slots
                  );
               end if;
               Logger.Base.Ptr.Insert_Status (Address, Last);
               exit;
            exception
               when Error : others =>
                  if View /= null then
                     View.Trace ("DB: Database write error", Error);
                     Logger.Base.Invalidate;
                     Logger.Enable.Set_Active (False);
                     View.Trace ("DB: Trying to re-open the database");
                     Logger.Enable.Set_Active (True);
                     if not Logger.Base.Is_Valid then
                        return;
                     end if;
                     if not Logger.Base.Is_Valid then
                        return;
                     end if;
                     if Try = Max_Attempts then
                        View.Trace
                        (  "DB: Giving up after "
                        &  Image (Max_Attempts)
                        &  " attempts"
                        );
                        return;
                     end if;
                     View.Trace
                     (  "DB: Trying again ["
                     &  Image (Try)
                     &  '/'
                     &  Image (Max_Attempts - 1)
                     &  ']'
                     );
                  end if;
            end;
         end loop;
      end;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Update_Status")
         )  );
   end Update_Status;

end MAX_Database_Page;
