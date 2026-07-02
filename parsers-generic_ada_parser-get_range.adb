--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Ada_Parser.                 Luebeck            --
--        Get_Range                                Spring, 2026       --
--  Separate body implementation                                      --
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
   procedure Get_Range
             (  Context     : in out Ada_Expression;
                Code        : in out Lexers.Lexer_Source_Type;
                Constrained : Boolean;
                Any_Name    : Boolean;
                Argument    : out Subtype_Indication
             )  is
   Result : Tokens.Argument_Token;

   procedure Range_Expected
             (  Where   : Location_Type;
                Message : String
             )  is
   begin
      if not Any_Name then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            Message & Image (Where)
         );
      end if;
      Argument := (  Mode       => Subtype_Mode,
                     Not_Null   => False,
                     Constraint => (Mode => No_Constraint),
                     Mark       => (No_Attribute, Result)
                  );
   end Range_Expected;
begin
   Lexers.Parse (Context, Code, Result);
   if Result.Value.all in Expression then
      declare
         This : Expression renames Expression (Result.Value.all);
      begin
         case This.Operation is
            when Ellipsis => -- a..b
               Argument :=
                  (  Mode       => Range_Mode,
                     Not_Null   => False,
                     Constraint => (Mode => No_Constraint),
                     Mark       => (No_Attribute, Result)
                  );
               return;
            when Attribute => -- a.b.c'Range or a.b.c'Range(d)
               if Is_Range (This.Operands (2)) then
                  Argument :=
                     (  Mode       => Subtype_Mode,
                        Not_Null   => False,
                        Constraint => (Mode => No_Constraint),
                        Mark       => (  Attribute => Range_Attribute,
                                         Name      => This.Operands (1)
                     )                );
                  return;
               elsif This.Operands (2).Value.all in Expression then
                  declare
                     Right : Expression renames
                             Expression (This.Operands (2).Value.all);
                  begin
                     if Right.Operation = Left_Bracket and then
                        Right.Count = 2                and then
                        Is_Range (Right.Operands (1))      then
                        Argument :=
                           (  Mode         => Subtype_Mode,
                              Not_Null     => False,
                              Constraint   => (Mode => No_Constraint),
                              Mark         =>
                              (  Attribute => Dimension_Range_Attribute,
                                 Dimension => Right.Operands (2),
                                 Name      =>
                                   Expression
                                   (  This.Operands (1).Value.all
                                   ) .Operands (1)
                           )  );
                        return;
                     end if;
                  end;
               elsif Any_Name then
                  Argument :=
                     (  Mode       => Subtype_Mode,
                        Not_Null   => False,
                        Constraint => (Mode => No_Constraint),
                        Mark       => (No_Attribute, Result)
                     );
                  return;
               else
                  Raise_Exception
                  (  Parsers.Syntax_Error'Identity,
                     "Range attribute is expected at " &
                     Image (This.Operands (2).Location)
                  );
               end if;
            when Component => -- a.b.c
               null;
            when others =>
               Range_Expected
               (  This.Location,
                  "Instead of a range specification, '" &
                  Image (This.Operation)                &
                  "' is found at "
              );
              return;
         end case;
      end;
   elsif Result.Value.all in Identifier then
      if Equal = Compare
                 (  Identifier (Result.Value.all).Value,
                    "others"
                 )  then
         Range_Expected
         (  Result.Location,
            "A range specification is expected at "
         );
         return;
      end if;
   elsif Result.Value.all in Universal_Value'Class or else
         Result.Value.all in Literal'Class then
      Argument := (  Mode       => Subtype_Mode,
                     Not_Null   => False,
                     Constraint => (Mode => No_Constraint),
                     Mark       => (No_Attribute, Result)
                  );
      return;
   else
      Range_Expected
      (  Result.Location,
         "A range specification is expected at "
      );
      return;
   end if;
   declare
      Value  : Tokens.Argument_Token;
      Got_It : Boolean;
   begin
      Get_Blank (Context, Code);
      Get_Delimited (Code, "range", True, Got_It);
      if not Got_It then
         Argument :=
            (  Mode         => Subtype_Mode,
               Not_Null     => False,
               Constraint   => (Mode => No_Constraint),
               Mark         => (  Attribute => No_Attribute,
                                  Name      => Result
            )                  );
         return;
      end if;
      if not Constrained then
         Get_Delimited (Code, "<>", False, Got_It);
         if Got_It then
            Argument :=
               (  Mode       => Unconstrained_Mode,
                  Not_Null   => False,
                  Constraint => (Mode => No_Constraint),
                  Mark       => (  Attribute => No_Attribute,
                                   Name      => Result
               )                );
            return;
         end if;
      end if;
      Lexers.Parse (Context, Code, Value);
      Argument :=
         (  Mode             => Subtype_Range_Mode,
            Not_Null         => False,
            Constraint       => (Mode => No_Constraint),
            Range_Constraint => Value,
            Mark             => (  Attribute => No_Attribute,
                                   Name      => Result
         )                     );
    end;
end Get_Range;
