--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--    Test_Parsers                                 Luebeck            --
--  Instantiation                                  Summer, 2025       --
--                                                                    --
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

with Parsers.Generic_Ada_Parser.Generic_Dot;
with Parsers.Generic_Ada_Parser.Generic_Text_IO;
with Parsers.Multiline_Source;

package Test_Parsers is

   package Ada_Parsers is
      new Parsers.Generic_Ada_Parser (Parsers.Multiline_Source.Code);

   package Dot is new Ada_Parsers.Generic_Dot;
   package IO  is new Ada_Parsers.Generic_Text_IO;

end Test_Parsers;
