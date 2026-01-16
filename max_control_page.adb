--                                                                    --
--  package MAX_Control_Page       Copyright (c)  Dmitry A. Kazakov  --
--     Control page                                Luebeck            --
--  Implementation                                 Winter, 2018       --
--                                                                    --
--                                Last revision :  08:52 04 Aug 2022  --
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

with Ada.Calendar;             use Ada.Calendar;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with GLib;                     use GLib;
with GLib.Messages;            use GLib.Messages;
with GLib.Properties;          use GLib.Properties;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.File_Chooser;         use Gtk.File_Chooser;
with Gtk.File_Filter;          use Gtk.File_Filter;
with Gtk.Missed;               use Gtk.Missed;
with Gtk.Recent_Manager_Keys;  use Gtk.Recent_Manager_Keys;
with MAX_IO;                   use MAX_IO;
with MAX_Trace;                use MAX_Trace;
with Strings_Edit;             use Strings_Edit;
with Strings_Edit.Floats;      use Strings_Edit.Floats;
with Strings_Edit.Integers;    use Strings_Edit.Integers;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;

with Ada.Unchecked_Deallocation;
with Interfaces.C;
with Julia.ELV_MAX_Cube;
with Julia.Load_Julia_Library;
with Py.ELV_MAX_Cube;
with Py.Load_Python_Library;

package body MAX_Control_Page is
   use Interfaces;
   CRLF : constant String := Character'Val (13) & Character'Val (10);

   Python_Script : constant := 0;
   Julia_Script  : constant := 1;
   Edit_Width    : constant := 15;
   Julia_Loaded  : Boolean  := False;

   function Where (Name : String) return String is
   begin
      return " in MAX_Control_Page." & Name;
   end Where;

   function To_String (List : Py.Handle; Delimiter : String := ", ")
      return String is
      Result  : String (1..1024);
      Pointer : Integer := 1;
   begin
      if not Py.Is_Valid (List) then
         return "";
      end if;
      for Index in 0..Py.ssize_t'Pred (Py.List_Size (List)) loop
         if Pointer > 1 then
            Put (Result, Pointer, Delimiter);
         end if;
         Put
         (  Result,
            Pointer,
            Py.As_String (Py.List_GetItem (List, Index))
         );
      end loop;
      return Result (1..Pointer - 1);
   exception
      when Layout_Error =>
         return Result (1..Pointer - 1);
   end To_String;

   procedure Free is
      new Ada.Unchecked_Deallocation (Julia_Worker, Julia_Worker_Ptr);
   procedure Free is
      new Ada.Unchecked_Deallocation (Python_Worker, Python_Worker_Ptr);
   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);

   function Get_Each (Widget : MAX_Control_Record) return Duration is
      Result : Duration;
   begin
      Result := Duration (Float'(Value (Widget.Period.Get_Text)));
      if Result < 0.0 then
         Widget.Period.Set_Text ("0.0");
         return 0.0;
      end if;
      return Result;
   exception
      when others =>
         Widget.Period.Set_Text ("1.0");
         return 1.0;
   end Get_Each;

   function Get_Delay (Widget : MAX_Control_Record) return Duration is
      Result : Duration;
   begin
      Result := Duration (Float'(Value (Widget.Start_Delay.Get_Text)));
      if Result < 0.0 then
         Widget.Start_Delay.Set_Text ("0.0");
         return 0.0;
      end if;
      return Result;
   exception
       when others =>
          Widget.Start_Delay.Set_Text ("1.0");
          return 1.0;
   end Get_Delay;

   function Get_Directory (Widget : MAX_Control_Record) return String is
   begin
      if (  Widget.Python.Path = null
         or else
            Widget.Python.Folder < Widget.Python.Path'First
         )
      then
         return "";
      else
         return Widget.Python.Path
                (  Widget.Python.Path'First
                .. Widget.Python.Folder - 1
                );
      end if;
   end Get_Directory;

   function Get_Julia_File
            (  Widget : MAX_Control_Record
            )  return String is
   begin
      if Widget.Julia.Path = null then
         return "";
      else
         return Trim (Widget.Julia.Path.all);
      end if;
   end Get_Julia_File;

   function Get_Path (Widget : MAX_Control_Record) return String is
   begin
      if Widget.Python.Path = null then
         return "";
      else
         return Widget.Python.Path.all;
      end if;
   end Get_Path;

   function Get_Python (Widget : MAX_Control_Record) return String is
   begin
      return Py.Load_Python_Library.Get_Default_Name;
   end Get_Python;

   function Get_Python_File
            (  Widget : MAX_Control_Record
            )  return String is
   begin
      if Widget.Python.Path = null then
         return "";
      else
         return Widget.Python.Path
                (  Widget.Python.Folder + 1
                .. Widget.Python.Extension - 1
                );
      end if;
   end Get_Python_File;

   function Gtk_Control_New return MAX_Control is
      Result : constant MAX_Control := new MAX_Control_Record;
      Label  : Gtk_Label;
   begin
      Gtk.Box.Initialize (Result, Orientation_Vertical, 6);
      Result.Set_Border_Width (5);
      Gtk_New (Result.Grid);
      Result.Grid.Set_Row_Spacing (3);
      Result.Grid.Set_Column_Spacing (3);
      Result.Pack_Start (Result.Grid);
      -- Row 1 ---------------------------------------------------------
      Gtk_New (Result.Enable, "Enable");
      Result.Grid.Attach (Result.Enable, 0, 0);
      Result.Enable.Set_Tooltip_Text
      (  "Enable user-defined control script"
      );
      Result.Enable.Set_Active
      (  Restore ("enable-control", "off") = "on"
      );
      -- Row 2 ---------------------------------------------------------
      Gtk_New (Label, "Control script");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.Enable, Pos_Bottom);

      Gtk_New (Result.Method);
      Result.Method.Append_Text ("Python 3");

      Result.Method.Append_Text ("Julia 1.6.x");
      Result.Method.Set_Tooltip_Text ("The scripting language to use");
      Result.Grid.Attach_Next_To
      (  Result.Method,
         Label,
         Pos_Right
      );
      begin
         Result.Method.Set_Active
         (  GInt'Value (Restore ("control-method", "0"))
         );
      exception
         when Constraint_Error =>
            Result.Method.Set_Active (0);
      end;
      -- Row 3 ---------------------------------------------------------
      Gtk_New (Result.Library_Name);
      Result.Grid.Attach_Next_To
      (  Result.Library_Name,
         Result.Method,
         Pos_Bottom
      );
      Result.Library_Name.Set_Hexpand (True);

      Gtk_New (Result.Library_Label, "");
      Result.Library_Label.Set_Halign (Align_End);
      Result.Library_Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To
      (  Result.Library_Label,
         Result.Library_Name,
         Pos_Left
      );

      Script_Library_Buttons.Gtk_New (Result.Browse_Library);
      Result.Grid.Attach_Next_To
      (  Result.Browse_Library,
         Result.Library_Name,
         Pos_Right
      );
      -- Row 4 ---------------------------------------------------------
      Gtk_New (Result.File_Name);
      Result.Grid.Attach_Next_To
      (  Result.File_Name,
         Result.Library_Name,
         Pos_Bottom
      );
      Result.File_Name.Set_Hexpand (True);
      Result.File_Name.Set_Text (Restore ("control-file", ""));

      Gtk_New (Label, "Script file name");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.File_Name, Pos_Left);

      Script_File_Buttons.Gtk_New (Result.Browse_File);
      Result.Grid.Attach_Next_To
      (  Result.Browse_File,
         Result.File_Name,
         Pos_Right
      );
      -- Row 5 ---------------------------------------------------------
      Gtk_New (Result.Period);
      Result.Grid.Attach_Next_To
      (  Result.Period,
         Result.File_Name,
         Pos_Bottom
      );
      Result.Period.Set_Tooltip_Text
      (  "The period the script is invoked. "
      &  "The script can run in an infinite loop or "
      &  "it can end after each iteration and invoked again when "
      &  "the specified time interval expires"
      );
      Gtk_New (Label, "Invoke period");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.Period, Pos_Left);
      Gtk_New (Label, "s");
      Label.Set_Halign (Align_Start);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.Period, Pos_Right);
      Result.Period.Set_Text (Restore ("control-period", "10"));
      Result.Period.Set_Width_Chars (Edit_Width);
      if (  Find_Property (Result.Period, "max-width-chars") /= null
         )
      then
         Set_Property
         (  Result.Period,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;
      -- Row 6 ---------------------------------------------------------
      Gtk_New (Result.Start_Delay);
      Result.Grid.Attach_Next_To
      (  Result.Start_Delay,
         Result.Period,
         Pos_Bottom
      );
      Result.Start_Delay.Set_Text
      (  Restore ("control-start-delay", "5")
      );
      Result.Start_Delay.Set_Tooltip_Text
      (  "Initial delay before invoking the script "
      &  "for the first time after the application start"
      );
      Result.Start_Delay.Set_Width_Chars (Edit_Width);
      if (  Find_Property (Result.Start_Delay, "max-width-chars")
         /= null
         )
      then
         Set_Property
         (  Result.Start_Delay,
            Build ("max-width-chars"),
            GInt'(Edit_Width)
         );
      end if;

      Gtk_New (Label, "Start delay");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.Start_Delay, Pos_Left);

      Gtk_New (Label, "s");
      Label.Set_Halign (Align_Start);
      Label.Set_Valign (Align_Center);
      Result.Grid.Attach_Next_To (Label, Result.Start_Delay, Pos_Right);
      -- Row 7 ---------------------------------------------------------
      Gtk_New (Result.Sample_Label, "");
      Result.Grid.Attach (Result.Sample_Label, 0, 7, 6, 1);
      Result.Sample_Label.Set_Halign (Align_Start);
      Result.Sample_Label.Set_Valign (Align_Start);
      Result.Sample_Label.Set_Vexpand (True);
   ---------------------------------------------------------------------
      Result.Update_Method;
      if Result.Enable.Get_Active then
         case Result.Method.Get_Active is
            when Python_Script =>
               Result.Enable.Set_Active
               (  Result.Start_Python
                  (  Interactive => False,
                     Start_Delay => Result.Get_Delay,
                     Each        => Result.Get_Each
               )  );
            when Julia_Script =>
               Result.Enable.Set_Active
               (  Result.Start_Julia
                  (  Interactive => False,
                     Start_Delay => Result.Get_Delay,
                     Each        => Result.Get_Each
               )  );
            when others =>
               null;
         end case;
      end if;
      Connect
      (  Result,
         "destroy",
         On_Destroy'Access,
         Result
      );
      Connect
      (  Result.Browse_File,
         "clicked",
         On_Browse_File'Access,
         Result
      );
      Connect
      (  Result.Browse_Library,
         "clicked",
         On_Browse_Library'Access,
         Result
      );
      Connect
      (  Result.Enable,
         "toggled",
         On_Toggled'Access,
         Result
      );
      Connect
      (  Result.Method,
         "changed",
         On_Method_Changed'Access,
         Result
      );
      Result.Set_Enabled;
      return Result;
   end Gtk_Control_New;

   function Is_Enabled (Widget : not null access MAX_Control_Record)
      return Boolean is
   begin
      return Widget.Enable.Get_Active;
   end Is_Enabled;

   procedure On_Browse_File
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Control
             )  is
      Dialog : File_Dialog;
      Filter : Gtk_File_Filter;
   begin
      case Widget.Method.Get_Active is
         when Python_Script =>
            Dialog :=
               Gtk_New
               (  "Select a Python script file to perform control",
                  Action_Open,
                  Widget.File_Name
               );
            Filter := Gtk_File_Filter_New;
            Filter.Set_Name (Py_Filter_Name);
            Filter.Add_Pattern ("*" & Py_Extension);
         when Julia_Script =>
            Dialog :=
               Gtk_New
               (  "Select a Julia script file to perform control",
                  Action_Open,
                  Widget.File_Name
               );
            Filter := Gtk_File_Filter_New;
            Filter.Set_Name (Jl_Filter_Name);
            Filter.Add_Pattern ("*" & Jl_Extension);
         when others =>
            return;
      end case;
      Dialog.Add_Filter (Filter);
      Dialog.Show_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Browse_File")
         )  );
   end On_Browse_File;

   procedure On_Browse_Library
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Control
             )  is
      Dialog : File_Dialog;
      Filter : Gtk_File_Filter;
   begin
      case Widget.Method.Get_Active is
         when Python_Script =>
            Dialog :=
               Gtk_New
               (  (  "Select the Python dynamic library "
                  &  "file to perform control"
                  ),
                  Action_Open,
                  Widget.Library_Name
               );
            Filter := Gtk_File_Filter_New;
            Filter.Set_Name ("Python dynamic library");
            Filter.Add_Pattern (Py.Load_Python_Library.Get_Extension);
         when Julia_Script =>
            Dialog :=
               Gtk_New
               (  (  "Select the Julia dynamic library "
                  &  "file to perform control"
                  ),
                  Action_Open,
                  Widget.Library_Name
               );
            Filter := Gtk_File_Filter_New;
            Filter.Set_Name ("Julia dynamic library");
            Filter.Add_Pattern (Py.Load_Python_Library.Get_Extension);
         when others =>
            return;
      end case;
      Dialog.Add_Filter (Filter);
      Dialog.Show_All;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Browse_Library")
         )  );
   end On_Browse_Library;

   procedure On_Destroy
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Control
             )  is
   begin
      Widget.Stop_Julia;
      Widget.Stop_Python;
      Free (Widget.Python.Path);
   end On_Destroy;

   procedure On_Method_Changed
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Control
             )  is
   begin
      Widget.Update_Method;
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

   procedure On_Toggled
             (  Object : access Gtk_Widget_Record'Class;
                Widget : MAX_Control
             )  is
   begin
      if Widget.Enable.Get_Active then
         begin
            case Widget.Method.Get_Active is
               when Python_Script =>
                  begin  -- Try to load Python DLL
                     Py.Load (Widget.Library_Name.Get_Text);
                  exception
                     when Error : others =>
                        Say
                        (  "Unable to find Python on the system: "
                        &  Exception_Message (Error)
                        );
                        raise End_Error;
                  end;
                  declare
                     Start_Delay : Duration;
                  begin
                     Start_Delay := Widget.Get_Delay; -- Check only
                  end;
                  if not Widget.Start_Python
                         (  Interactive => True,
                            Start_Delay => 0.0,
                            Each        => Widget.Get_Each
                         )  then
                     raise End_Error;
                  end if;
               when Julia_Script => -- Julia
                  if Julia_Loaded then
                     Say
                     (  "Julia control script was already activated "
                     &  "or attempted to. "
                     &  "You should close and start the application "
                     &  "again in order to be able to activate the "
                     &  "script once more"
                     );
                     raise End_Error;
                  else
                     Julia_Loaded := True;
                     begin  -- Try to load Julia DLL
                        Julia.Load (Widget.Library_Name.Get_Text);
                     exception
                        when Error : others =>
                           Say
                           (  "Unable to find Julia on the system: "
                           &  Exception_Message (Error)
                           );
                           raise End_Error;
                     end;
                     declare
                        Start_Delay : Duration;
                     begin
                        Start_Delay := Widget.Get_Delay; -- Check only
                     end;
                     if not Widget.Start_Julia
                            (  Interactive => True,
                               Start_Delay => 0.0,
                               Each        => Widget.Get_Each
                            )  then
                        raise End_Error;
                     end if;
                  end if;
               when others =>
                  raise End_Error;
            end case;
            Widget.Store;
         exception
            when End_Error =>
               Widget.Enable.Set_Active (False);
         end;
      else
         Widget.Stop_Julia;
         Widget.Stop_Python;
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

   procedure Set_Enabled
             (  Widget : not null access MAX_Control_Record
             )  is
      Enabled : constant Boolean := Widget.Enable.Get_Active;
   begin
      Widget.Method.Set_Sensitive      (not Enabled);
      Widget.File_Name.Set_Sensitive   (not Enabled);
      Widget.Period.Set_Sensitive      (not Enabled);
      Widget.Start_Delay.Set_Sensitive (not Enabled);
      Widget.Browse_File.Set_Sensitive (not Enabled);
      if Py.Load_Python_Library.Is_Loaded then
         Widget.Library_Name.Set_Sensitive   (False);
         Widget.Browse_Library.Set_Sensitive (False);
      else
         Widget.Library_Name.Set_Sensitive   (not Enabled);
         Widget.Browse_Library.Set_Sensitive (not Enabled);
      end if;
   end Set_Enabled;

   procedure Set_Julia_Path
             (  Widget : in out MAX_Control_Record;
                Path   : String
             )  is
   begin
      Free (Widget.Julia.Path);
      Widget.Julia.Path := new String'(Path);
   end Set_Julia_Path;

   procedure Set_Python_Path
             (  Widget : in out MAX_Control_Record;
                Path   : String
             )  is
   begin
      Free (Widget.Python.Path);
      Widget.Python.Path := new String'(Path);
      Widget.Python.Folder := 0;
      for Index in reverse Widget.Python.Path'Range loop
         case Widget.Python.Path (Index) is
            when '/' | '\' =>
               Widget.Python.Folder := Index;
               exit;
            when others =>
               null;
         end case;
      end loop;
      Widget.Python.Extension := Widget.Python.Path'Last + 1;
      for Index in reverse Widget.Python.Folder + 1
                        .. Widget.Python.Path'Last loop
         if Widget.Python.Path (Index) = '.' then
            Widget.Python.Extension := Index;
            exit;
         end if;
      end loop;
   end Set_Python_Path;

   function Start_Julia
            (  Widget      : not null access Max_Control_Record;
               Interactive : Boolean;
               Start_Delay : Duration;
               Each        : Duration
            )  return Boolean is
   begin
      if Widget.Julia.Worker /= null then
         return True;
      end if;
      begin
         Julia.Load (Widget.Library_Name.Get_Text);
      exception
         when Error : others =>
            Trace
            (  (  "Failed to load Julia libarary: "
               &  Exception_Message (Error)
               ),
               Error_Text
            );
            return False;
      end;
      Widget.Set_Julia_Path (Trim (Widget.File_Name.Get_Text));
      if Widget.Julia.Path'Length = 0 then
         Tell ("No Julia script file specified", Interactive);
         return False;
      elsif not File_Test
                (  Widget.Julia.Path.all,
                   File_Test_Exists
                )  then
         Tell
         (  (  "Julia script file "
            &  Quote (Widget.Julia.Path.all)
            &  " does not exist"
            ),
            Interactive
         );
         return False;
      elsif not File_Test
                (  Widget.Julia.Path.all,
                   File_Test_Is_Regular
                )  then
         Tell
         (  (  "Julia script file "
            &  Quote (Widget.Julia.Path.all)
            &  " is not a regular file"
            ),
            Interactive
         );
         return False;
      end if;
      Widget.Julia.Worker :=
         new Julia_Worker (Widget.all'Unchecked_Access);
      declare
          Error   : String (1..1024 * 10);
          Pointer : Integer := 1;
      begin
         Widget.Julia.Worker.Start
         (  Script_File => Widget.Get_Julia_File,
            Start_Delay => Start_Delay,
            Each        => Each,
            Error       => Error,
            Pointer     => Pointer
         );
         if Pointer > 1 then
            Tell (Error (1..Pointer - 1), Interactive);
            while not Widget.Julia.Worker'Terminated loop
               delay 0.01;
            end loop;
            Free (Widget.Julia.Worker);
            return False;
         end if;
      end;
      return True;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            "Julia start fault: " & Exception_Information (Error)
         );
         return False;
   end Start_Julia;

   procedure Stop_Julia (Widget : in out Max_Control_Record) is
   begin
      if Widget.Julia.Worker = null then
         return;
      end if;
      Trace ("Stopping Julia...", Message_Text);
      select
         Widget.Julia.Worker.Stop;
         Trace ("Julia stop completed", Message_Text);
         for Try in 1..10 loop
            exit when Widget.Julia.Worker'Terminated;
            delay 0.1;
         end loop;
      or delay Julia_Stop_Timeout;
         Trace
         (  "Julia stop timed out, stopping forcefully",
            Message_Text
         );
         abort Widget.Julia.Worker.all;
      end select;
      Free (Widget.Julia.Worker);
--    Julia.AtExit_Hook;
   end Stop_Julia;

   function Start_Python
            (  Widget      : not null access Max_Control_Record;
               Interactive : Boolean;
               Start_Delay : Duration;
               Each        : Duration
            )  return Boolean is
   begin
      if Widget.Python.Worker /= null then
         return True;
      end if;
      begin
         Py.Load (Widget.Library_Name.Get_Text);
      exception
         when Error : others =>
            Trace
            (  (  "Failed to load Python libarary: "
               &  Exception_Message (Error)
               ),
               Error_Text
            );
            return False;
      end;
      Widget.Set_Python_Path (Trim (Widget.File_Name.Get_Text));
      if Widget.Python.Path'Length = 0 then
         Tell ("No Python script file specified", Interactive);
         return False;
      elsif not File_Test
                (  Widget.Python.Path.all,
                   File_Test_Exists
                )  then
         Tell
         (  (  "Python script file "
            &  Quote (Widget.Python.Path.all)
            &  " does not exist"
            ),
            Interactive
         );
         return False;
      elsif not File_Test
                (  Widget.Python.Path.all,
                   File_Test_Is_Regular
                )  then
         Tell
         (  (  "Python script file "
            &  Quote (Widget.Python.Path.all)
            &  " is not a regular file"
            ),
            Interactive
         );
         return False;
      end if;
      Widget.Python.Worker :=
         new Python_Worker (Widget.all'Unchecked_Access);
      declare
          Error   : String (1..1024 * 10);
          Pointer : Integer := 1;
      begin
         Widget.Python.Worker.Start
         (  Python_Library   => Widget.Get_Python,
            Script_Directory => Widget.Get_Directory,
            Script_File      => Widget.Get_Python_File,
            Start_Delay      => Start_Delay,
            Each             => Each,
            Error            => Error,
            Pointer          => Pointer
         );
         if Pointer > 1 then
            Tell (Error (1..Pointer - 1), Interactive);
            while not Widget.Python.Worker'Terminated loop
               delay 0.01;
            end loop;
            Free (Widget.Python.Worker);
            return False;
         end if;
      end;
      return True;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            "Python start fault: " & Exception_Information (Error)
         );
         return False;
   end Start_Python;

   procedure Stop_Python (Widget : in out Max_Control_Record) is
   begin
      if Widget.Python.Worker = null then
         return;
      end if;
      Trace ("Stopping Python...", Message_Text);
      if Widget.Python.Executing then
         declare
            Lock : Py.Global_Interpreter_Lock;
         begin
            Py.Request_Abort;
         end;
      end if;
      Trace ("Waiting for Python to complete...", Message_Text);
      select
         Widget.Python.Worker.Stop;
         Trace ("Python stop completed", Message_Text);
         for Try in 1..10 loop
            exit when Widget.Python.Worker'Terminated;
            delay 0.1;
         end loop;
      or delay Python_Stop_Timeout;
         Trace
         (  "Python stop timed out, stopping forcefully",
            Message_Text
         );
         abort Widget.Python.Worker.all;
      end select;
      Free (Widget.Python.Worker);
   end Stop_Python;

   procedure Store (Widget : not null access MAX_Control_Record) is
   begin
      if Widget.Enable.Get_Active then
         Store ("enable-control", "on");
      else
         Store ("enable-control", "off");
      end if;
      if Widget.Enable.Get_Active then
         Store
         (  "control-method",
            Image (Integer (Widget.Method.Get_Active))
         );
         case Widget.Method.Get_Active is
            when Python_Script =>
               Store
               (  "control-python-library",
                  Widget.Library_Name.Get_Text
               );
            when Julia_Script =>
               Store
               (  "control-julia-library",
                  Widget.Library_Name.Get_Text
               );
            when others =>
               null;
         end case;
         Store ("control-file",        Widget.File_Name.Get_Text);
         Store ("control-period",      Widget.Period.Get_Text);
         Store ("control-start-delay", Widget.Start_Delay.Get_Text);
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

   procedure Tell (Error : String; Interactive : Boolean) is
   begin
      if Interactive then
         Say (Error);
      else
         Trace (Error, Error_Text);
      end if;
   exception
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Tell")
         )  );
   end Tell;

   procedure Update_Method
             (  Widget : not null access MAX_Control_Record
             )  is
   begin
      case Widget.Method.Get_Active is
         when Python_Script =>
            Widget.Library_Label.Set_Text ("Python library");
            Widget.Library_Name.Set_Tooltip_Text
            (  "The location of the Python dynamic library. "
            &  "When the name is relative, "
            &  "make sure that the path to is present in "
            &  "the library search path"
            );
            Widget.Library_Name.Set_Text
            (  Restore
               (  "control-python-library",
                  (  Py.Load_Python_Library.Get_Python_Path
                  &  Py.Load_Python_Library.Get_Default_Name
            )  )  );
            Widget.File_Name.Set_Tooltip_Text
            (  "The script file to run. "
            &  "The script must contain function controller, which is "
            &  "normally the script name: controller.py."
            );
            Widget.Sample_Label.Set_Text
            (  "# Sample controller Python script"        & CRLF &
               "import elv_max_cube"                      & CRLF &
               "def controller (*args):"                  & CRLF &
               "   elv_max_cube.trace (""Hello there!"")" & CRLF
            );
         when Julia_Script =>
            Widget.Library_Label.Set_Text ("Julia library");
            Widget.Library_Name.Set_Tooltip_Text
            (  "The location of the Julia dynamic library. "
            &  "It is recommended to use an absolute path "
            &  "to the library. "
            &  "When the name is relative, "
            &  "make sure that the path to is present in "
            &  "the library search path"
            );
            Widget.Library_Name.Set_Text
            (  Restore
               (  "control-julia-library",
                  Julia.Load_Julia_Library.Get_Default_Name
            )  );
            Widget.File_Name.Set_Tooltip_Text
            (  "The script file to run. "
            &  "The script must contain function controller, which is "
            &  "normally the script name: controller.jl."
            );
            Widget.Sample_Label.Set_Text
            (  "# Sample controller Julia script"        & CRLF &
               "controller()"                            & CRLF &
               "   ELV_MAX_Cube.trace(""Hello there!"")" & CRLF &
               "end"                                     & CRLF
            );
         when others =>
            null;
      end case;
   end Update_Method;

   task body Julia_Worker is
      use Julia;
      Exiting    : Boolean := False;
      Wait_First : Duration;
      Period     : Duration;
      Controller : function_t;
   begin
      Julia_Loaded := True;
      --
      -- Inializing Julia, loading extension, loading the script
      --
      accept Start
            (  Script_File : String;
               Start_Delay : Duration;
               Each        : Duration;
               Error       : in out String;
               Pointer     : in out Integer
            )  do
         Wait_First := Start_Delay;
         Period     := Each;
         declare
            procedure Err (Text : String) is
            begin
               Strings_Edit.Put
               (  Error,
                  Pointer,
                  Text
               );
            exception
               when others =>
                  null;
            end Err;
         begin
            Controller := Julia.ELV_MAX_Cube.Init (Script_File);
         exception
            when Error : Name_Error | Julia_Error | End_Error =>
               Err ("Julia start fault:");
               Err (CRLF);
               Err (Exception_Message (Error));
               Exiting := True;
            when Error : others =>
               Err ("Julia start fault:");
               Err (CRLF);
               Err (Exception_Information (Error));
               Exiting := True;
         end;
      end Start;
      --
      -- Running the loop
      --
      if not Exiting then
         begin
            select
               accept Stop;
            or delay Wait_First;
               declare
                  Value : value_t;
               begin
                  loop
                     Trace
                     (  (  "Controlling Julia script "
                        &  Quote (Widget.Julia.Path.all)
                        &  " started at "
                        &  Image (Clock, True)
                        ),
                        Message_Text
                     );
                     Widget.Julia.Executing := True;
                     begin
                        Value := Julia.Call (Controller);
                     exception
                        when Error : Julia.Julia_Error =>
                           Widget.Julia.Executing := False;
                           Trace
                           (  (  "Controlling Julia script "
                              &  Quote (Widget.Julia.Path.all)
                              &  " failed at "
                              &  Image (Clock, True)
                              &  " with error: "
                              &  Exception_Message (Error)
                              ),
                              Error_Text
                           );
                        when others =>
                           Widget.Julia.Executing := False;
                           raise;
                     end;
                     select
                        accept Stop;
                        exit;
                     or delay Period;
                     end select;
                  end loop;
               end;
            end select;
--          Julia.AtExit_Hook;
         exception
            when Error : others =>
--             Julia.AtExit_Hook;
               Trace
               (  (  "Julia worker task failed at "
                  &  Image (Clock, True)
                  &  " with fault: "
                  &  Exception_Information (Error)
                  ),
                  Error_Text
               );
               accept Stop;
         end;
      end if;
   exception
      when End_Error =>
         null;
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Julia worker task failed: "
            &  Exception_Information (Error)
         )  );
   end Julia_Worker;

   task body Python_Worker is
      use Interfaces.C;
      Text        : String (1..1024 *10);
      Pointer     : Integer;
      Exiting     : Boolean  := False;
      Wait_First  : Duration;
      Period      : Duration;
      Module      : Py.Handle;
      Func        : Py.Handle;
      State       : Py.Handle;
      System_Path : Py.Handle;
      Script_Path : Py.Handle;

      procedure Cleanup is
      begin
         Exiting := True;
         begin
            if System_Path.Is_Valid then -- Restore path
               System_Path.Sequence_DelItem
               (  System_Path.Sequence_Index (Script_Path)
               );
            end if;
         exception
            when others =>
               null;
         end;
         System_Path.Invalidate;
         Script_Path.Invalidate;
         Func.Invalidate;
         Module.Invalidate;
         if Py.FinalizeEx < 0 then
            Log
            (  MAX_Domain,
               Log_Level_Critical,
               "Python finalization error"
            );
         end if;
      exception
         when Error : others =>
            Log
            (  MAX_Domain,
               Log_Level_Critical,
               "Python cleanup fault: " & Exception_Information (Error)
            );
      end Cleanup;
   begin
      --
      -- Inializing Python, loading extension, loading the script
      --
      accept Start
            (  Python_Library   : String;
               Script_Directory : String;
               Script_File      : String;
               Start_Delay      : Duration;
               Each             : Duration;
               Error            : in out String;
               Pointer          : in out Integer
            )  do
         Wait_First := Start_Delay;
         Period     := Each;
         Py.ELV_MAX_Cube.Init;
         Py.Initialize;
         declare
            procedure Err (Text : String) is
            begin
               Strings_Edit.Put
               (  Error,
                  Pointer,
                  Text
               );
            exception
               when others =>
                  null;
            end Err;
            No_Error    : Boolean;
            Script_Name : Py.Handle;
         begin
            System_Path := Py.Sys_GetObject ("path");
            Script_Name :=
               Py.Unicode_FromString (Widget.Get_Python_File);
            if System_Path.Is_Valid then
               if Widget.Get_Directory'Length > 0 then
                  Script_Path :=
--                   Py.Unicode_DecodeFSDefault (Widget.Get_Directory);
                     Py.Unicode_FromString (Widget.Get_Directory);
               else
                  Script_Path :=
                     Py.Unicode_FromString (Get_Current_Dir);
               end if;
               System_Path.List_Insert (0, Script_Path);
            end if;
            --
            -- The argument is the module name,  which  is the file name
            -- without extension.  In order to find it  the path  to the
            -- file must be in the system path.
            --
            Py.Import_Import
            (  Script_Name,
               Module,
               Text,
               Pointer,
               No_Error
            );
            if not No_Error then -- Cannot load the module
               Err ("Unable to load Python script ");
               Err (Quote (Widget.Get_Path));
               Err (":");
               Err (CRLF);
               Err (Text (1..Pointer - 1));
               Err ("System path:");
               Err (CRLF);
               Err (To_String (System_Path, ";" & CRLF));
               Cleanup;
               return;
            end if;
            begin
               Func := Py.Object_GetAttrString (Module, Controller);
            exception
               when Error : Py.Python_Error =>
                  Err ("Unable to find method ");
                  Err (Quote (Controller));
                  Err (" in the Python script ");
                  Err (Quote (Widget.Get_Path));
                  Err (":");
                  Err (CRLF);
                  Err (Exception_Message (Error));
                  Err (CRLF);
                  Err ("System path:");
                  Err (CRLF);
                  Err (To_String (System_Path, ";" & CRLF));
                  Cleanup;
                  return;
            end;
            if Py.Callable_Check (Func) = 0 then
               Err ("Method ");
               Err ( Quote (Controller));
               Err (" in the Python script ");
               Err (Quote (Widget.Get_Path));
               Err (" is not callable");
               Err (CRLF);
               Err ("System path:");
               Err (CRLF);
               Err (To_String (System_Path, ";" & CRLF));
               Cleanup;
               return;
            end if;
            System_Path.Invalidate;
            Script_Path.Invalidate;
         exception
            when Error : others =>
               Err ("Python start fault:");
               Err (CRLF);
               Err (Exception_Information (Error));
               Cleanup;
         end;
      end Start;
      --
      -- Running the loop
      --
      if not Exiting then
         begin
            select
               accept Stop;
            or delay Wait_First;
               declare
                  Args     : Py.Handle;
                  Value    : Py.Handle;
                  No_Error : Boolean;
               begin
                  Py.Eval_InitThreads;
                  loop
                     Trace
                     (  (  "Controlling Python script "
                        &  Quote (Widget.Python.Path.all)
                        &  " started at "
                        &  Image (Clock, True)
                        ),
                        Message_Text
                     );
                     if Value.Is_None then
                        Value := Py.No_Value;
                     end if;
                     Args := Py.Tuple_New (1);
                     Args.Tuple_SetItem (0, Value);
                     Widget.Python.Executing := True;
                     Value := Py.Object_CallObject (Func, Args);
                     Widget.Python.Executing := False;
                     Pointer := 1;
                     Py.Error_Traceback (Text, Pointer, No_Error);
                     if No_Error then
                        Trace
                        (  (  "Controlling Python script "
                           &  Quote (Widget.Python.Path.all)
                           &  " completed at "
                           &  Image (Clock, True)
                           ),
                           Message_Text
                        );
                     else
                        Trace
                        (  (  "Controlling Python script "
                           &  Quote (Widget.Python.Path.all)
                           &  " failed at "
                           &  Image (Clock, True)
                           &  " with error: "
                           &  Text (1..Pointer - 1)
                           ),
                           Error_Text
                        );
                     end if;
                     select
                        accept Stop;
                        exit;
                     or delay Period;
                     end select;
                  end loop;
               end;
            end select;
            Cleanup;
         exception
            when Error : others =>
               Cleanup;
               Trace
               (  (  "Python worker task failed at "
                  &  Image (Clock, True)
                  &  " with fault: "
                  &  Exception_Information (Error)
                  ),
                  Error_Text
               );
               accept Stop;
         end;
      end if;
   exception
      when End_Error =>
         null;
      when Error : others =>
         Log
         (  MAX_Domain,
            Log_Level_Critical,
            (  "Python worker task failed: "
            &  Exception_Information (Error)
         )  );
   end Python_Worker;

end MAX_Control_Page;
