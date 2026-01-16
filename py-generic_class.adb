--                                                                    --
--  package Py.Generic_Class        Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2025       --
--                                                                    --
--                                Last revision :  22:30 08 Nov 2025  --
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
with System.Storage_Elements;  use System.Storage_Elements;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with System;

package body Py.Generic_Class is

   type GetSetDef_Array is
      array (Positive range <>) of aliased GetSetDef;
   type GetSetDef_Array_Ptr is access all GetSetDef_Array;
   procedure Free (Ptr : in out GetSetDef_Array_Ptr);

   type MethodDef_Array is
      array (Positive range <>) of aliased MethodDef;
   type MethodDef_Array_Ptr is access all MethodDef_Array;
   procedure Free (Ptr : in out MethodDef_Array_Ptr);

   type Argument_List_Ptr is access Argument_List;
   type Indicator_Ptr     is access Indicator_Array;

   procedure Release is
      new Ada.Unchecked_Deallocation
          (  GetSetDef_Array,
             GetSetDef_Array_Ptr
          );
   procedure Release is
      new Ada.Unchecked_Deallocation
          (  MethodDef_Array,
             MethodDef_Array_Ptr
          );
   procedure Release is
      new Ada.Unchecked_Deallocation
          (  Argument_List,
             Argument_List_Ptr
          );
   procedure Release is
      new Ada.Unchecked_Deallocation
          (  Indicator_Array,
             Indicator_Ptr
          );
   function To_Ptr is new Ada.Unchecked_Conversion (Address, Value_Ptr);

   Members    : GetSetDef_Array_Ptr;
   Methods    : MethodDef_Array_Ptr;
   Profile    : Argument_List_Ptr;
   Indicator  : Indicator_Ptr;
   Shift      : Storage_Offset := 0;
   Base_Count : Argument_Count := 0;
   Base_Size  : Storage_Offset := 0;
   Base_Args  : Base_Init_Arguments;
   On_Init    : On_Initialize_Ptr;

   procedure Add_Member (Member : GetSetDef) is
      Old : GetSetDef_Array_Ptr := Members;
   begin
      if Members = null then
         Members := new GetSetDef_Array (1..2);
      else
         Members := new GetSetDef_Array (1..Old'Length + 1);
         Members (1..Old'Length - 1) := Old (1..Old'Length - 1);
         Release (Old);
      end if;
      Members (Members'Last - 1) := Member;
      Members (Members'Last)     := End_GetSet;
   end Add_Member;

   procedure Add_Method (Method : MethodDef)  is
      Old : MethodDef_Array_Ptr := Methods;
   begin
      if Methods = null then
         Methods := new MethodDef_Array (1..2);
      else
         Methods := new MethodDef_Array (1..Old'Length + 1);
         Methods (1..Old'Length - 1) := Old (1..Old'Length - 1);
         Release (Old);
      end if;
      Methods (Methods'Last - 1) := Method;
      Methods (Methods'Last)     := End_Method;
   end Add_Method;

   procedure Class_Finalize is
   begin
      The_Class := Null_Object;
      Free (Members);
      Free (Methods);
      Release (Profile);
      Release (Indicator);
   end Class_Finalize;

   function Create
            (  Name        : String;
               Doc         : String;
               Constructor : Argument_List := No_Arguments;
               Base        : Handle        := Null_Handle
            )  return Handle is
      Size   : constant int :=
                        int (Object_HeadSize) +
                        Value_Type'Max_Size_In_Storage_Elements;
      Result : Handle;
      Module : Handle;
      C_Name : aliased char_array := To_C (Name);
      C_Doc  : aliased char_array := To_C (Doc);
   begin
      Base_Size := Storage_Offset (Object_HeadSize);
      if The_Class /= Null_Object then
         Raise_Exception
         (  Use_Error'Identity,
            Class_Already_Exists
         );
      elsif Base.Ptr /= Null_Object and then
            not Is_In (Base, Type_Type) then
         Raise_Exception (Use_Error'Identity, Base_Is_Not_A_Type);
      end if;
      Release (Profile);
      Profile := new Argument_List'(Constructor);
      if Members = null then
         Members := new GetSetDef_Array'(1..1 => End_GetSet);
      end if;
      if Methods = null then
         Methods := new MethodDef_Array'(1..1 => End_Method);
      end if;
      declare
         Slots  : array (1..8) of aliased Type_Slot :=
         (  1 => (tp_doc,         (tp_doc,   To_Chars_Ptr
                                             (  C_Doc'Unchecked_Access
            )                     )          ),
            2 => (tp_new,         (tp_new,         Value_New_Ptr)),
            3 => (tp_getset,      (tp_getset,      Members (1)'Access)),
            4 => (tp_methods,     (tp_methods,     Methods (1)'Access)),
            5 => (tp_finalize,    (tp_finalize,    Value_Finalize_Ptr)),
            6 => (tp_str,         (tp_str,         Value_Image_Ptr)),
            7 => (tp_richcompare, (tp_richcompare, Value_Compare_Ptr)),
            8 => End_Slot
         );
      begin
         if Base.Ptr = Null_Object then
            Result.Ptr :=
               Links.Type_FromSpec
               (  (  Basic_Size => Size,
                     Item_Size  => 0,
                     Name  => To_Chars_Ptr (C_Name'Unchecked_Access),
                     Flags => TPFLAGS_DEFAULT  +
                              TPFLAGS_HEAPTYPE +
                              TPFLAGS_BASETYPE,
                     Slots => Slots (Slots'First)'Unchecked_Access
               )  );
         else
            declare
               Bases : constant Handle := Tuple_New (1);
            begin
               Tuple_SetItem (Bases, 0, Base);
               Result.Ptr :=
                  Links.Type_FromSpecWithBases
                  (  (  Basic_Size => Size,
                        Item_Size  => 0,
                        Name  => To_Chars_Ptr (C_Name'Unchecked_Access),
                        Flags => TPFLAGS_DEFAULT  +
                                 TPFLAGS_HEAPTYPE +
                                 TPFLAGS_BASETYPE,
                        Slots => Slots (Slots'First)'Unchecked_Access
                     ),
                     Bases.Ptr
                  );
            end;
         end if;
      end;
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      Object_SetAttrString
      (  Result,
         "__name__",
         Unicode_FromString (Name)
      );
      Module := Import_AddModule (Name);
      Module_AddObject (Module, Name, Result);
      The_Class := Result.Ptr;
      The_Base  := Base.Ptr;
      Add_Hook (Class_Finalize_Ptr);
      return Result;
   exception
      when others =>
         Release (Profile);
         Free (Members);
         Free (Methods);
         raise;
   end Create;

   function Create (Value : Value_Type) return Handle is
      Result : Handle;
   begin
      if The_Class = Null_Object then
         Throw_TypeError (Class_Does_Not_Exist);
      end if;
      Result.Ptr := -- Allocate the object
         Links.Type_GetSlot (The_Class, tp_alloc).tp_alloc
         (  The_Class,
            0
         );
      if Result.Ptr = Null_Object then
         Check_Error;
      else
         declare
            Ptr : Value_Ptr;
         begin
            Value_Handler.Value := System.Address (Result.Ptr)
                                 + Storage_Offset (Object_HeadSize);
            Ptr   := new Value_Type'(Value);
            Shift := Ptr.all'Address - Value_Handler.Value;
         end;
         Links.IncRef (Result.Ptr);
      end if;
      return Result;
   end Create;

   function Extend_Implementation
            (  Name           : String;
               Doc            : String;
               Base           : Handle;
               Constructor    : Argument_List;
               Base_Arguments : Indicator_Array;
               Initialization : Base_Init_Arguments;
               On_Initialize  : On_Initialize_Ptr
            )  return Handle is
      Result : Handle;
      Module : Handle;
      C_Name : aliased char_array := To_C (Name);
      C_Doc  : aliased char_array := To_C (Doc);
   begin
      if Constructor.Length /= Base_Arguments'Length then
         Raise_Exception (Use_Error'Identity, Base_Arguments_Error);
      end if;
      Check_Handle (Base);
      if The_Class /= Null_Object then
         Raise_Exception
         (  Use_Error'Identity,
            Class_Already_Exists
         );
      elsif not Is_In (Base, Type_Type) then
         Raise_Exception (Use_Error'Identity, Base_Is_Not_A_Type);
      end if;
      Release (Profile);
      Release (Indicator);
      Profile   := new Argument_List'(Constructor);
      Indicator := new Indicator_Array (1..Base_Arguments'Length);
      Indicator.all := Base_Arguments;
      if Members = null then
         Members := new GetSetDef_Array'(1..1 => End_GetSet);
      end if;
      if Methods = null then
         Methods := new MethodDef_Array'(1..1 => End_Method);
      end if;
      Base_Size  := Storage_Offset (Get_Basic_Size (Base));
      Base_Count := 0;
      Base_Args  := Initialization;
      for Index in Base_Arguments'Range loop
         if Base_Arguments (Index) then
            Base_Count := Base_Count + 1;
         end if;
      end loop;
      declare
         Slots  : array (1..8) of aliased Type_Slot :=
         (  1 => (tp_doc,         (tp_doc,   To_Chars_Ptr
                                             (  C_Doc'Unchecked_Access
            )                     )          ),
            2 => (tp_init,        (tp_init,     Value_Initialize_Ptr)),
            3 => (tp_getset,      (tp_getset,   Members (1)'Access)),
            4 => (tp_methods,     (tp_methods,  Methods (1)'Access)),
            5 => (tp_finalize,    (tp_finalize, Value_Finalize_Ptr)),
            6 => (tp_str,         (tp_str,      Value_Image_Ex_Ptr)),
            7 => (tp_richcompare, (tp_richcompare,
                                                Value_Compare_Ex_Ptr)),
            8 => End_Slot
         );
         Bases : constant Handle := Tuple_New (1);
      begin
         Tuple_SetItem (Bases, 0, Base);
         Result.Ptr :=
            Links.Type_FromSpecWithBases
            (  (  Basic_Size => int (Base_Size) +
                                Value_Type'Max_Size_In_Storage_Elements,
                  Item_Size  => 0,
                  Name       => To_Chars_Ptr (C_Name'Unchecked_Access),
                  Flags      => TPFLAGS_DEFAULT  +
                                TPFLAGS_HEAPTYPE +
                                TPFLAGS_BASETYPE,
                  Slots      => Slots (Slots'First)'Unchecked_Access
               ),
               Bases.Ptr
            );
      end;
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      Object_SetAttrString
      (  Result,
         "__name__",
         Unicode_FromString (Name)
      );
      Module := Import_AddModule (Name);
      Module_AddObject (Module, Name, Result);
      The_Class := Result.Ptr;
      The_Base  := Base.Ptr;
      On_Init   := On_Initialize;
      Add_Hook (Class_Finalize_Ptr);
      return Result;
   exception
      when others =>
         Release (Profile);
         Release (Indicator);
         Free (Members);
         Free (Methods);
         raise;
   end Extend_Implementation;

   function Extend
            (  Name           : String;
               Doc            : String;
               Base           : Handle;
               Constructor    : Argument_List     := No_Arguments;
               Base_Arguments : Indicator_Array   := (1..0 => False);
               On_Initialize  : On_Initialize_Ptr := null
            )  return Handle is
   begin
      return Extend_Implementation
             (  Name,
                Doc,
                Base,
                Constructor,
                Base_Arguments,
                null,
                On_Initialize
             );
   end Extend;

   function Extend
            (  Name           : String;
               Doc            : String;
               Base           : Handle;
               Constructor    : Argument_List;
               Base_Arguments : Base_Init_Arguments;
               On_Initialize  : On_Initialize_Ptr := null
            )  return Handle is
   begin
      return Extend_Implementation
             (  Name,
                Doc,
                Base,
                No_Arguments,
                (1..0 => False),
                Base_Arguments,
                On_Initialize
             );
   end Extend;

   procedure Free (Ptr : in out MethodDef_Array_Ptr) is
   begin
      if Ptr = null then
         return;
      end if;
      for Index in Ptr'Range loop
         declare
            This : MethodDef renames Ptr (Index);
         begin
            Free (This.Name);
            Free (This.Doc);
         end;
      end loop;
      Release (Ptr);
   end Free;

   procedure Free (Ptr : in out GetSetDef_Array_Ptr) is
   begin
      if Ptr = null then
         return;
      end if;
      for Index in Ptr'Range loop
         declare
            This : GetSetDef renames Ptr (Index);
         begin
            Free (This.Name);
            Free (This.Doc);
         end;
      end loop;
      Release (Ptr);
   end Free;

   function Get (Object : Handle) return Value_Type is
   begin
      if The_Class = Null_Object then
         Raise_Exception
         (  Use_Error'Identity,
            Class_Does_Not_Exist
         );
      else
         Check_Handle (Object);
         if 0 /= Links.Object_IsInstance (Object.Ptr, The_Class) then
            return To_Value_Ptr (Object.Ptr).all;
         else
            Raise_Exception
            (  Constraint_Error'Identity,
               Wrong_Argument_Type
            );
         end if;
      end if;
   end Get;

   function Get (Object : Handle) return Value_Ptr is
   begin
      if The_Class = Null_Object then
         Raise_Exception
         (  Use_Error'Identity,
            Class_Does_Not_Exist
         );
      else
         Check_Handle (Object);
         if 0 /= Links.Object_IsInstance (Object.Ptr, The_Class) then
            return To_Value_Ptr (Object.Ptr);
         else
            Raise_Exception
            (  Constraint_Error'Identity,
               Wrong_Argument_Type
            );
         end if;
      end if;
   end Get;

   procedure Set (Object : Handle; Value : Value_Type) is
   begin
      if The_Class = Null_Object then
         Raise_Exception
         (  Use_Error'Identity,
            Class_Does_Not_Exist
         );
      else
         Check_Handle (Object);
         if 0 /= Links.Object_IsInstance (Object.Ptr, The_Class) then
            To_Value_Ptr (Object.Ptr).all := Value;
         else
            Raise_Exception
            (  Constraint_Error'Identity,
               Wrong_Argument_Type
            );
         end if;
      end if;
   end Set;

   function To_Value_Ptr (Self : Object) return Value_Ptr is
   begin
      return To_Ptr
             (  System.Address (Self)
             +  Shift
             +  Base_Size
             );
   end To_Value_Ptr;

   function Value_Compare
            (  Self  : Object;
               Other : Object;
               Opt   : int
            )  return Object is
      Result : Boolean;
   begin
      if The_Class = Null_Object then
         Throw_TypeError (Class_Does_Not_Exist);
         return Null_Object;
      elsif Other = Null_Object then
         Throw_ValueError (Wrong_Argument_Type);
         return Null_Object;
      else
         case Links.Object_IsInstance (Other, The_Class) is
            when 0 =>
               Links.IncRef (Links.NotImplemented);
               return Links.NotImplemented;
            when 1 =>
               declare
                  This : Value_Type renames To_Value_Ptr (Self).all;
                  That : Value_Type renames To_Value_Ptr (Other).all;
               begin
                  case Opt is
                     when Py_LT =>
                        Result := This < That;
                     when Py_LE =>
                        Result := not (This < That);
                     when Py_EQ =>
                        Result := This = That;
                     when Py_GT =>
                        Result := That < This;
                     when Py_GE =>
                        Result := not (That < This);
                     when others =>
                        Result := This /= That;
                  end case;
               exception
                  when Use_Error =>
                     Links.IncRef (Links.NotImplemented);
                     return Links.NotImplemented;
               end;
            when others =>
               Check_Error;
         end case;
      end if;
      if Result then
         return Links.Bool_FromLong (1);
      else
         return Links.Bool_FromLong (0);
      end if;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_ValueError (Exception_Message (Error));
         return Null_Object;
   end Value_Compare;

   function Value_Compare_Ex
            (  Self  : Object;
               Other : Object;
               Opt   : int
            )  return Object is
      type Threestate is (False, Undefined, True);

      function "and" (Left, Right : Threestate) return Threestate is
      begin
         if Left in Undefined..True then
            return Right;
         elsif Right in Undefined..True then
            return Left;
         else
            return False;
         end if;
      end "and";

      function "or" (Left, Right : Threestate) return Threestate is
      begin
         if Left in False..Undefined then
            return Right;
         elsif Right in False..Undefined then
            return Left;
         else
            return True;
         end if;
      end "or";

      function From_Boolean (Value : Boolean) return Threestate is
      begin
         if Value then
            return True;
         else
            return False;
         end if;
      end From_Boolean;

      function Compare (opt : int) return Threestate is
      begin
         case Links.Object_IsInstance (Other, The_Class) is
            when 0 =>
               return Undefined;
            when 1 =>
               declare
                  This : Value_Type renames To_Value_Ptr (Self).all;
                  That : Value_Type renames To_Value_Ptr (Other).all;
               begin
                  case Opt is
                     when Py_LT =>
                        return From_Boolean (This < That);
                     when Py_LE =>
                        return From_Boolean (not (This < That));
                     when Py_EQ =>
                        return From_Boolean (This = That);
                     when Py_GT =>
                        return From_Boolean (That < This);
                     when Py_GE =>
                        return From_Boolean (not (That < This));
                     when others =>
                        return From_Boolean (This /= That);
                  end case;
               exception
                  when Use_Error =>
                     return Undefined;
               end;
            when others =>
               Check_Error;
               return False;
         end case;
      end Compare;

      Func : richcmpfunc_Ptr;

      function Compare_Parent (opt : int) return Threestate is

      begin
         case Links.Long_AsLong (Func (Self, Other, opt)) is
            when 0 =>
               return False;
            when 1 =>
               return True;
            when others =>
               Err_Clear;
               return Undefined;
         end case;
      end Compare_Parent;

      Result : Threestate;
   begin
      if The_Class = Null_Object then
         Throw_TypeError (Class_Does_Not_Exist);
         return Null_Object;
      elsif Other = Null_Object then
         Throw_ValueError (Wrong_Argument_Type);
         return Null_Object;
      end if;
      Func :=
         Links.Type_GetSlot (The_Base, tp_richcompare).tp_richcompare;
      if Func = null then
         Err_Clear;
         Result := Compare (opt);
      else
         case Links.Object_IsInstance (Other, The_Class) is
            when 1 =>
               case Opt is
                  when Py_EQ =>
                     Result :=
                        Compare_Parent (Py_EQ) and Compare (Py_EQ);
                  when Py_LT =>
                     Result := Compare_Parent (Py_LT);
                     if Result in False..Undefined then
                        Result :=
                           Compare_Parent (Py_EQ) and Compare (Py_LT);
                     end if;
                  when Py_LE =>
                     Result := Compare_Parent (Py_LT);
                     if Result in False..Undefined then
                        Result :=
                           Compare_Parent (Py_EQ) and Compare (Py_LE);
                     end if;
                  when Py_GT =>
                     Result := Compare_Parent (Py_GT);
                     if Result in False..Undefined then
                        Result :=
                           Compare_Parent (Py_EQ) and Compare (Py_GT);
                     end if;
                  when Py_GE =>
                     Result := Compare_Parent (Py_GT);
                     if Result in False..Undefined then
                        Result :=
                           Compare_Parent (Py_EQ) and Compare (Py_GE);
                     end if;
                  when others =>
                     Result :=
                        Compare_Parent (Py_NE) or Compare (Py_NE);
               end case;
            when 0 =>
               case Links.Object_IsInstance (Other, The_Base) is
                  when 1 =>
                     Result := Compare_Parent (opt);
                  when 0 =>
                     Result := Undefined;
                  when others =>
                     Err_Clear;
                     Result := Undefined;
               end case;
            when others =>
               Err_Clear;
               Result := Compare (opt);
         end case;
      end if;
      case Result is
         when Undefined =>
            Links.IncRef (Links.NotImplemented);
            return Links.NotImplemented;
         when True =>
            return Links.Bool_FromLong (1);
         when False =>
            return Links.Bool_FromLong (0);
      end case;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_ValueError (Exception_Message (Error));
         return Null_Object;
   end Value_Compare_Ex;

   procedure Value_Finalize (Self : Object) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Value_Type, Value_Ptr);
      This : Value_Ptr;
   begin
      This := To_Value_Ptr (Self);
      Free (This); -- Ensure Ada finalization
   exception
      when Error : others =>
         Throw_SystemError (Error);
   end Value_Finalize;

   function Value_Image (Self : Object) return Object is
   begin
      declare
         Value : constant String := Image (To_Value_Ptr (Self).all);
      begin
         if Value'Length = 0 then
            return Links.Unicode_FromString ((0..1 => NUL));
         else
            return Links.Unicode_FromStringAndSize
                   (  Value'Address,
                      Value'Length
                   );
         end if;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Value_Image;

   function Value_Image_Ex (Self : Object) return Object is
      This : Handle;
      Str  : Handle;
   begin
      This.Ptr := Self;
      Links.IncRef (This.Ptr);
      Str := Object_Super (This, "__str__");
      Str := Object_CallNoArgs (Str);
      declare
         Value : constant String :=
                 As_String (Str) & Image (To_Value_Ptr (Self).all);
      begin
         if Value'Length = 0 then
            return Links.Unicode_FromString ((0..1 => NUL));
         else
            return Links.Unicode_FromStringAndSize
                   (  Value'Address,
                      Value'Length
                   );
         end if;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Value_Image_Ex;

   function Value_Initialize
            (  Self     : Object;
               Args     : Object;
               Keywords : Object
            )  return int is
      This : Handle;
   begin
      declare
         Objects : constant Object_Array :=
                            Parse (Args, Keywords, Profile.all);
         List    : Actual_Argument_List (Objects'Range);
         No      : ssize_t := 0;
         Tuple   : Handle;
      begin
         for Index in Objects'Range loop
            if Objects (Index) = Null_Object then
               List (Index) := No_Value;
            else
               List (Index).Ptr := Objects (Index);
               Links.IncRef (List (Index).Ptr);
            end if;
         end loop;
         if Base_Args = null then
            Tuple := Tuple_New (ssize_t (Base_Count));
            for Index in List'Range loop
               if Indicator (Index) then
                  Tuple_SetItem (Tuple, No, List (Index));
                  No := No + 1;
               end if;
            end loop;
         else
            declare
               Init : constant Actual_Argument_List :=
                               Base_Args (List);
            begin
               for Index in List'Range loop
                  Tuple_SetItem (Tuple, No, Init (Index));
                  No := No + 1;
               end loop;
            end;
         end if;
         declare
            Base_Init : Handle;
            Result    : Handle;
         begin
            This.Ptr := Self;
            Links.IncRef (This.Ptr);
            Base_Init := Object_Super (This, "__init__");
            if 0 /= Callable_Check (Base_Init) then
               Result := Object_CallObject (Base_Init, Tuple, True);
            end if;
         end;
         declare
            Ptr : Value_Ptr;
         begin
            Value_Handler.Value := System.Address (Self) + Base_Size;
            Ptr   := new Value_Type'(Create (List));
            Shift := Ptr.all'Address - Value_Handler.Value;
         end;
         if On_Init /= null then
            On_Init (This);
         end if;
         return 0;
      end;
   exception
      when Python_Error =>
         return -1;
      when Error : others =>
         Throw_SystemError (Error);
         return -1;
   end Value_Initialize;

   function Value_New
            (  Class    : Object;
               Args     : Object;
               Keywords : Object
            )  return Object is
   begin
      declare
         Objects : constant Object_Array :=
                            Parse (Args, Keywords, Profile.all);
         List    : Actual_Argument_List (Objects'Range);
         Result  : Handle := No_Value;
      begin
         for Index in List'Range loop
            if Objects (Index) = Null_Object then
               List (Index) := No_Value;
            else
               List (Index).Ptr := Objects (Index);
               Links.IncRef (List (Index).Ptr);
            end if;
         end loop;
         Result.Ptr := -- Allocate the object
            Links.Type_GetSlot (Class, tp_alloc).tp_alloc (Class, 0);
         if Result.Ptr = Null_Object then
            Check_Error;
         end if;
         declare
            Ptr : Value_Ptr;
         begin
            Value_Handler.Value := System.Address (Result.Ptr)
                                 + Storage_Offset (Object_HeadSize);
            Ptr   := new Value_Type'(Create (List));
            Shift := Ptr.all'Address - Value_Handler.Value;
         end;
         Links.IncRef (Result.Ptr);
         return Result.Ptr;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Value_New;

end Py.Generic_Class;
