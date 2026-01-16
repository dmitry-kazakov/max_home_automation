--                                                                    --
--  package Py.Generic_Class        Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
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
--
--  Value_Type - The type of Ada object corresponding the instance
--  Create     - The value constructor
--  Image      - The value text representation
--  =          - Comparison, raises Use_Error if not implemented
--  <          - Comparison, raises Use_Error if not implemented
--
generic
   type Value_Type is private;
   with function Create
                 (  Arguments : Actual_Argument_List
                 )  return Value_Type is <>;
   with function Image (Value : Value_Type) return String is <>;
   with function "=" (Left, Right : Value_Type) return Boolean is <>;
   with function "<" (Left, Right : Value_Type) return Boolean is <>;
package Py.Generic_Class is
--
-- Create -- Create new type
--
--    Name        - The type name
--    Doc         - The documentation text
--    Constructor - The parameters list of the type value constructor
--    Base        - The base class to derive from
--
-- When Base is specified it must  a type  created  from the base tagged
-- type of Value_Type.  This form is used to map a hierachy of Ada types
-- onto Python types.
--
-- Returns :
--
--    A handle to the Python type
--
-- Exceptions :
--
--    Use_Error - The type is already created
--
   function Create
            (  Name        : String;
               Doc         : String;
               Constructor : Argument_List := No_Arguments;
               Base        : Handle := Null_Handle
            )  return Handle;
--
-- Create -- A new object from value
--
--    Value - The Ada value
--
-- Returns :
--
--    The object
--
-- Exceptions :
--
--    Use_Error - The type is not uet created
--
   function Create (Value : Value_Type) return Handle;
--
-- Extend -- Create new type by extension of an existing type
--
--    Name           - The type name
--    Doc            - The documentation text
--    Base           - The base class to derive from
--    Constructor    - The parameters list of the type value constructor
--    Base_Arguments - Indicator of arguments passed to the base
--    On_Initialize  - Instance initialization callbak
--
-- The base can by any Python type that supports derivation from.
--
-- Returns :
--
--    A handle to the Python type
--
-- Exceptions :
--
--    Use_Error - The type is already created
--
   type Indicator_Array is
      array (Argument_Position range <>) of Boolean;
   type On_Initialize_Ptr is access procedure (Self : Handle);
   function Extend
            (  Name           : String;
               Doc            : String;
               Base           : Handle;
               Constructor    : Argument_List     := No_Arguments;
               Base_Arguments : Indicator_Array   := (1..0 => False);
               On_Initialize  : On_Initialize_Ptr := null
            )  return Handle;
--
-- Extend -- Create new type by extension of an existing type
--
--    Name           - The type name
--    Doc            - The documentation text
--    Base           - The base class to derive from
--    Constructor    - The parameters list of the type value constructor
--    Base_Arguments - Function to create arguments passed to the base
--    On_Initialize  - Instance initialization callbak
--
-- The base can by any Python type that supports derivation from.
--
-- Returns :
--
--    A handle to the Python type
--
-- Exceptions :
--
--    Use_Error - The type is already created
--
   type Base_Init_Arguments is access function
        (  Argument_List : Actual_Argument_List
        )  return Actual_Argument_List;
   function Extend
            (  Name           : String;
               Doc            : String;
               Base           : Handle;
               Constructor    : Argument_List;
               Base_Arguments : Base_Init_Arguments;
               On_Initialize  : On_Initialize_Ptr := null
            )  return Handle;
--
-- Get -- Get value from object
--
--    Object - The object
--
-- Returns :
--
--    The Ada value stored in the object
--
-- Returns :
--
--    Ada value or an access to it
--
-- Exceptions :
--
--    Constraint_Error - Not a class instance
--
   type Value_Ptr is access Value_Type;
   function Get (Object : Handle) return Value_Type;
   function Get (Object : Handle) return Value_Ptr;
--
-- Set -- Set value into object
--
--    Object - The object
--    Value  - The Ada value to set
--
-- Exceptions :
--
--    Constraint_Error - Not a class instance
--
   procedure Set
             (  Object : Handle;
                Value  : Value_Type
             );
private
   use System.Storage_Elements;

   for Value_Ptr'Storage_Pool use Value_Handler;

   procedure Add_Member (Member : GetSetDef);
   procedure Add_Method (Method : MethodDef);

   procedure Class_Finalize;
   Class_Finalize_Ptr : constant Finalization_Hook_Ptr :=
      Class_Finalize'Access;

   function Value_Compare
            (  Self  : Object;
               Other : Object;
               Opt   : int
            )  return Object;
   pragma Convention (C, Value_Compare);
   Value_Compare_Ptr : constant richcmpfunc_Ptr :=
      Value_Compare'Access;

   function Value_Compare_Ex
            (  Self  : Object;
               Other : Object;
               Opt   : int
            )  return Object;
   pragma Convention (C, Value_Compare_Ex);
   Value_Compare_Ex_Ptr : constant richcmpfunc_Ptr :=
      Value_Compare_Ex'Access;

   procedure Value_Finalize (Self : Object);
   pragma Convention (C, Value_Finalize);
   Value_Finalize_Ptr : constant destructor_Ptr :=
      Value_Finalize'Access;

   function Value_New
            (  Class    : Object;
               Args     : Object;
               Keywords : Object
            )  return Object;
   pragma Convention (C, Value_New);
   Value_New_Ptr : constant newfunc_Ptr := Value_New'Access;

   function Value_Initialize
            (  Self     : Object;
               Args     : Object;
               Keywords : Object
            )  return int;
   pragma Convention (C, Value_Initialize);
   Value_Initialize_Ptr : constant initproc_Ptr :=
      Value_Initialize'Access;

   function Value_Image (Self : Object) return Object;
   pragma Convention (C, Value_Image);
   Value_Image_Ptr : constant reprfunc_Ptr :=
      Value_Image'Access;

   function Value_Image_Ex (Self : Object) return Object;
   pragma Convention (C, Value_Image_Ex);
   Value_Image_Ex_Ptr : constant reprfunc_Ptr :=
      Value_Image_Ex'Access;

   function To_Value_Ptr (Self : Object) return Value_Ptr;
   pragma Inline (To_Value_Ptr);

   The_Class : Object := Null_Object;
   The_Base  : Object := Null_Object;

end Py.Generic_Class;
