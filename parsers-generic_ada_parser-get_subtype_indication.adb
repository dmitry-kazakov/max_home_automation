--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Get_Subtype_Indication                   Spring, 2026       --
--  Implementation                                                    --
--                                Last revision :  10:48 02 Jul 2026  --
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

separate (Parsers.Generic_Ada_Parser)
   procedure Get_Subtype_Indication
             (  Context   : in out Ada_Expression;
                Code      : in out Lexers.Lexer_Source_Type;
                Composite : Boolean;
                Not_Null  : in out Boolean;
                Argument  : out Subtype_Indication
             )  is
   Mark : Subtype_Mark;

   function Get (Text : String; Delimited : Boolean := True)
      return Boolean is
      pragma Inline (Get);
      Got_It : Boolean;
   begin
      Get_Blank (Context, Code);
      Get_Delimited (Code, Text, Delimited, Got_It);
      return Got_It;
   end Get;

   procedure Get_Composite_Constraint is
      Mode   : Subtype_Constraint_Mode;
      Result : Tokens.Argument_Token;
      Unset  : Boolean := True;

      procedure Have_Discriminant (Where : Location_Type) is
      begin
         if Unset then
            Mode  := Discriminant_Constraint;
            Unset := False;
         elsif Mode = Index_Constraint then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               "A discrete range is expected in the subtype " &
               "index constraint at "                         &
               Image (Where)
            );
         end if;
      end Have_Discriminant;

      procedure Have_Range (Where : Location_Type) is
      begin
         if Unset then
            Mode  := Index_Constraint;
            Unset := False;
         elsif Mode = Discriminant_Constraint then
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               "A discriminant association is expected in " &
               "the subtype discriminant constraint at "    &
               Image (Where)
            );
         end if;
      end Have_Range;

      function Has_Range (Item : Tokens.Argument_Token)
         return Boolean is
      begin
         if Is_Range (Item) then
            return True;
         elsif Item.Value.all in Expression then
            declare
               This : Expression renames Expression (Item.Value.all);
            begin
               return This.Operation = Left_Bracket and then
                      This.Count = 2                and then
                      Is_Range (This.Operands (1));
            end;
         else
            return False;
         end if;
      end Has_Range;
   begin
      Reset_Pointer (Code);
      Lexers.Parse (Context, Code, Result);
      if Result.Value.all in Expression then
         declare
            This : Expression renames Expression (Result.Value.all);
         begin
            if This.Operation = Left_Bracket then
               declare
                  List : Argument_List renames This.Operands;
               begin
                  for Index in List'Range loop
                     if List (Index).Value.all not in Expression then
                        Have_Discriminant (List (Index).Location);
                     else
                        declare
                           Item : Expression renames
                                  Expression (List (Index).Value.all);
                        begin
                           case Item.Operation is
                              when Associate =>  -- ..., a => b, ...
                                 Have_Discriminant (Item.Location);
                              when Ellipsis =>   -- ..., a..b, ...
                                 Have_Range (Item.Location);
                              when Attribute =>  -- ..., a'b, ...
                                 if Has_Range (Item.Operands (2)) then
                                    Have_Range (Item.Location);
                                 else
                                    Have_Discriminant (Item.Location);
                                 end if;
                              when others =>
                                 Have_Discriminant (Item.Location);
                           end case;
                        end;
                     end if;
                  end loop;
               end;
            else
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "A list of ranges or discriminant associations is " &
                  "expected at "                                      &
                  Image (Result.Location)
               );
            end if;
         end;
      else
         Have_Discriminant (Result.Location);
         Result.Value :=
            new Expression'
                (  Count     => 1,
                   Location  => Result.Location,
                   Operation => Left_Bracket,
                   Operands  => (1 => Result)
                );
      end if;
      if Mode = Index_Constraint then
         Argument := (  Mode       => Subtype_Mode,
                        Mark       => Mark,
                        Not_Null   => Not_Null,
                        Constraint => (Index_Constraint, Result)
                     );
      else
         Argument := (  Mode       => Subtype_Mode,
                        Mark       => Mark,
                        Not_Null   => Not_Null,
                        Constraint => (Discriminant_Constraint, Result)
                     );
      end if;
   end Get_Composite_Constraint;

   procedure Get_Scalar_Constraint is
      Got_It  : Boolean;
      Keyword : Reserved_Word;
   begin
      Get_Reserved_Word (Code, Keyword);
      case Keyword is
         when And_Word =>
            if Not_Null then
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "Null exclusion is not allowed for " &
                  "interface specified at "            &
                  Image (Link (Code))
               );
            end if;
            Reset_Pointer (Code);
            Argument := (  Mode       => Subtype_Mode,
                           Mark       => Mark,
                           Not_Null   => Not_Null,
                           Constraint => (Mode => No_Constraint)
                        );
         when Delta_Word =>
            if Not_Null then
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "Null exclusion is not allowed for " &
                  "delta constraint specified at "     &
                  Image (Link (Code))
               );
            end if;
            declare
               Delta_Value : Tokens.Argument_Token;
               Real_Range  : Tokens.Argument_Token;
            begin
               Get_Blank (Context, Code);
               Lexers.Parse (Context, Code, Delta_Value);
               if Get ("range") then
                  Get_Blank (Context, Code);
                  Lexers.Parse (Context, Code, Real_Range);
                  Argument := (  Mode             => Subtype_Range_Mode,
                                 Not_Null         => Not_Null,
                                 Mark             => Mark,
                                 Range_Constraint => Real_Range,
                                 Constraint =>
                                    (  Mode => Fixed_Point_Constraint,
                                       Delta_Constraint => Delta_Value
                              )     );
               else
                  Argument := (  Mode     => Subtype_Mode,
                                 Not_Null => Not_Null,
                                 Mark     => Mark,
                                 Constraint =>
                                    (  Mode => Fixed_Point_Constraint,
                                       Delta_Constraint => Delta_Value
                              )     );
               end if;
            end;
         when Digits_Word =>
            if Not_Null then
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "Null exclusion is not allowed for " &
                  "digits constraint specified at "    &
                  Image (Link (Code))
               );
            end if;
            declare
               Digits_Value : Tokens.Argument_Token;
               Real_Range   : Tokens.Argument_Token;
            begin
               Get_Blank (Context, Code);
               Lexers.Parse (Context, Code, Digits_Value);
               if Get ("range") then
                  Get_Blank (Context, Code);
                  Lexers.Parse (Context, Code, Real_Range);
                  Argument :=
                     (  Mode             => Subtype_Range_Mode,
                        Not_Null         => Not_Null,
                        Mark             => Mark,
                        Range_Constraint => Real_Range,
                        Constraint =>
                           (  Mode => Floating_Point_Constraint,
                              Digits_Constraint => Digits_Value
                     )     );
               else
                  Argument :=
                     (  Mode       => Subtype_Mode,
                        Not_Null   => Not_Null,
                        Mark       => Mark,
                        Constraint =>
                           (  Mode => Floating_Point_Constraint,
                              Digits_Constraint => Digits_Value
                     )     );
               end if;
            end;
         when Range_Word =>
            if Not_Null then
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "Null exclusion is not allowed for " &
                  "range constraint specified at "     &
                  Image (Link (Code))
               );
            end if;
            if Composite then
               Get_Delimited (Code, "<>)", False, Got_It);
            else
               Got_It := False;
            end if;
            if Got_It then
               Argument :=
                  (  Mode       => Unconstrained_Mode,
                     Not_Null   => Not_Null,
                     Mark       => Mark,
                     Constraint => (Mode => No_Constraint)
                  );
            else
               Get_Blank (Context, Code);
               declare
                  Integer_Range : Tokens.Argument_Token;
               begin
                  Lexers.Parse (Context, Code, Integer_Range);
                  if Integer_Range.Value.all not in Expression then
                     Raise_Exception
                     (  Parsers.Syntax_Error'Identity,
                        "A range is expected at " &
                        Image (Integer_Range.Location)
                     );
                  elsif Expression (Integer_Range.Value.all).Operation /=
                        Ellipsis then
                     Raise_Exception
                     (  Parsers.Syntax_Error'Identity,
                        "'..' is expected at " &
                        Image
                        (  Expression (Integer_Range.Value.all).Location
                     )  );
                  end if;
                  Argument :=
                     (  Mode             => Subtype_Range_Mode,
                        Not_Null         => Not_Null,
                        Mark             => Mark,
                        Range_Constraint => Integer_Range,
                        Constraint       => (Mode => No_Constraint)
                     );
               end;
            end if;
         when With_Word =>
            if Not_Null then
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "Null exclusion is not allowed for " &
                  "record specified at "               &
                  Image (Link (Code))
               );
            end if;
            Reset_Pointer (Code);
            Argument := (  Mode       => Subtype_Mode,
                           Not_Null   => Not_Null,
                           Mark       => Mark,
                           Constraint => (Mode => No_Constraint)
                        );
         when No_Reserved_Word =>
            Argument := (  Mode       => Subtype_Mode,
                           Not_Null   => Not_Null,
                           Mark       => Mark,
                           Constraint => (Mode => No_Constraint)
                        );
         when others =>
            Reset_Pointer (Code);
            Argument := (  Mode       => Subtype_Mode,
                           Not_Null   => Not_Null,
                           Mark       => Mark,
                           Constraint => (Mode => No_Constraint)
                        );
      end case;
   end Get_Scalar_Constraint;

begin
   if not Not_Null then
      Get_Not_Null (Context, Code, Not_Null);
   end if;
   Get_Subtype_Mark
   (  Context  => Context,
      Code     => Code,
      No_Range => True,
      Not_Null => Not_Null,
      Mark     => Mark
   );
   case Mark.Attribute is
      when Class_Attribute | Range_Attribute | Base_Range_Attribute |
           Dimension_Range_Attribute =>
         Argument := (  Mode       => Subtype_Mode,
                        Not_Null   => Not_Null,
                        Mark       => Mark,
                        Constraint => (Mode => No_Constraint)
                     );
      when Base_Attribute =>
         Get_Scalar_Constraint;
      when No_Attribute =>
         if Composite and then Get ("(", False) then
            if Not_Null then
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "Null exclusion is not allowed for " &
                  "composite constraint specified at " &
                  Image (Link (Code))
               );
            end if;
            Get_Composite_Constraint;
         else
            Get_Scalar_Constraint;
         end if;
   end case;
end Get_Subtype_Indication;
