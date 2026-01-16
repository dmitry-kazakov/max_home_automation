--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.Patterns.            Luebeck            --
--        Generic_Variable                         Winter, 2025       --
--  Implementation                                                    --
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

package body Parsers.Generic_Source.Patterns.Generic_Variable is

   Initializer    : Finalization_Guard;
   Append_Handler : aliased Variable_Assignment_Handler (True);
   Set_Handler    : aliased Variable_Assignment_Handler (False);

   procedure On_Failure
             (  Hook   : in out Variable_Assignment_Handler;
                Source : in out Source_Type
             )  is
   begin
      Delete (Hook.Append);
   end On_Failure;

   function On_Line_Change
            (  Hook   : access Variable_Assignment_Handler;
               Source : access Source_Type;
               Where  : Location_Type
            )  return Result_Type is
   begin
      return On_Line_Change (Where);
   end On_Line_Change;

   procedure On_Success
             (  Hook   : in out Variable_Assignment_Handler;
                Source : in out Source_Type;
                From   : Integer;
                To     : Integer
             )  is
      Line    : Line_Ptr_Type;
      Pointer : aliased Integer;
      Last    : Integer;
   begin
      Get_Line (Source, Line, Pointer, Last);
      Add
      (  Line (From..To),
         Direct_Link (Source, From, To),
         Hook.Append
      );
   end On_Success;

   procedure Finalize (Object : in out Finalization_Guard) is
   begin
      Append_Handler.On_Failure     := null;
      Append_Handler.On_Line_Change := null;
      Append_Handler.On_Success     := null;
      Set_Handler.On_Failure        := null;
      Set_Handler.On_Line_Change    := null;
      Set_Handler.On_Success        := null;
   end Finalize;

   function Append (Pattern : Pattern_Type) return Pattern_Type is
      Result : Pattern_Ptr;
   begin
      Result := new Hook_Pattern (Append_Handler'Unchecked_Access);
      Hook_Pattern (Result.all).Pattern := Pattern;
      return Ref (Result);
   end Append;

   function Set (Pattern : Pattern_Type) return Pattern_Type is
      Result : Pattern_Ptr;
   begin
      Result := new Hook_Pattern (Set_Handler'Unchecked_Access);
      Hook_Pattern (Result.all).Pattern := Pattern;
      return Ref (Result);
   end Set;

begin
   Append_Handler.On_Failure     := On_Failure_Body;
   Append_Handler.On_Line_Change := On_Line_Change_Body;
   Append_Handler.On_Success     := On_Success_Body;
   Set_Handler.On_Failure        := On_Failure_Body;
   Set_Handler.On_Line_Change    := On_Line_Change_Body;
   Set_Handler.On_Success        := On_Success_Body;
end Parsers.Generic_Source.Patterns.Generic_Variable;
