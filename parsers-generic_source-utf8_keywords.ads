--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.                     Luebeck            --
--        UTF8_Keywords                            Summer, 2025       --
--  Interface                                                         --
--                                Last revision :  11:48 10 Aug 2025  --
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
--  This generic package is used for matching keywords.  An  enumeration
--  type  is  the  generic parameter of the package. The literals of the
--  type are the keywords to match. For example: 
--
--     with Parsers.Multiline_Source; -- Muilti-line sources
--     ...
--     type Color_Type is (Red, Blue, White, Green);
--     package Colors is
--        new Parsers.Multiline_Source.Code.UTF8_Keywords (Color_Type);
--     ...
--     loop -- Parsing loop
--        ...
--        Colors.Get (Code, Color, Got_It);
--        if not Got_It then
--           ... -- This is probably a syntax error
--        else
--           case Color is
--              when Red  => -- "red" was matched
--                 ...
--              when Blue => -- "blue" was matched
--                 ...
--
with Tables.UTF8_Names;

generic
   type Keyword is (<>);
package Parsers.Generic_Source.UTF8_Keywords is
--
-- Get -- Get a keyword from the source
--
--    Code   - The source code
--    Token  - The token matched
--    Got_It - Set to false if no token was matched
--
-- This   procedure   matches   a   keyword   in   Code.   Matching   is
-- case-insensitive.  When  matched  the keyword value is set into Token
-- and Got_It is set to True. The source cursor is then advanced  behind
-- the text matched. The longest possible token is always matched.  When
-- no token matches the source Got_It is set to False. 
--
   procedure Get
             (  Code   : in out Source_Type;
                Token  : out Keyword;
                Got_It : out Boolean
             );
private
   package Keyword_Raw_Tables is new Tables (Keyword);
   procedure Check_Spelling (Name : String);
   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean;

   package Keyword_Tables is new Keyword_Raw_Tables.UTF8_Names;

end Parsers.Generic_Source.UTF8_Keywords;
