--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Patterns_Instantiations                Luebeck            --
--  Interface                                      Summer, 2025       --
--                                                                    --
--                                Last revision :  21:31 04 Jan 2026  --
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

with Parsers.Generic_Source.Patterns.Generic_User_Pattern;
with Parsers.Generic_Source.Patterns.Generic_Parametrized_User_Pattern;
with Parsers.Multiline_Patterns;
with Parsers.String_Patterns;

package Test_Patterns_Instantiations is

   function Match_Identifier
            (  Line    : String;
               Pointer : access Integer
            )  return Boolean;

   package Identifier_Patterns is
      new Parsers.String_Patterns.Generic_User_Pattern
          (  Match_Identifier,
             True
          );

   type Number_Base is range 2..16;
   function Match_Number
            (  Line    : String;
               Pointer : access Integer;
               Base    : Number_Base
            )  return Boolean;

   package Number_Patterns is
      new Parsers.String_Patterns.Generic_Parametrized_User_Pattern
          (  Number_Base,
             Match_Number,
             True
          );

   function Match_Normalized
            (  Line    : String;
               Pointer : access Integer;
               Text    : String
            )  return Boolean;

   package Normalized_String_Patterns is
      new Parsers.String_Patterns.Generic_Parametrized_User_Pattern
          (  String,
             Match_Normalized,
             True
          );

   type Matrix is
      array (Positive range <>, Positive range <>) of Natural;
   type Matrix_Variable (Rows, Columns : Positive) is
      new Parsers.Multiline_Patterns.Abstract_User_Variable with
   record
      Row    : Positive := 1;
      Column : Positive := 1;
      Value  : Matrix (1..Rows, 1..Columns);
   end record;
   procedure Add
             (  Variable : in out Matrix_Variable;
                Source   : in out Parsers.Multiline_Patterns.
                                  Source_Subtype;
                Value    : String;
                Where    : Parsers.Multiline_Patterns.Location_Subtype;
                Append   : Boolean
             );
   procedure Delete
             (  Variable : in out Matrix_Variable;
                Source   : in out Parsers.Multiline_Patterns.
                                  Source_Subtype;
                Append   : Boolean
             );
   function On_Line_Change
            (  Variable   : access Matrix_Variable;
               Source     : access Parsers.Multiline_Patterns.
                                   Source_Subtype;
               Where      : Parsers.Multiline_Patterns.Location_Subtype
            )  return Parsers.Multiline_Patterns.Result_Type;

end Test_Patterns_Instantiations;
