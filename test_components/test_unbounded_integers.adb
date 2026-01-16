--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Unbounded_Integers                    Luebeck            --
--  Test                                           Winter, 2024       --
--                                                                    --
--                                Last revision :  17:53 15 Jan 2025  --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Text_IO;          use Ada.Text_IO;
with Strings_Edit.Quoted;  use Strings_Edit.Quoted;
with Unbounded_Integers;   use Unbounded_Integers;

with Strings_Edit.Unbounded_Integer_Edit;
use  Strings_Edit.Unbounded_Integer_Edit;

with Unbounded_Unsigneds;

procedure Test_Unbounded_Integers is
   use Strings_Edit;

begin
   declare
      X : Unbounded_Integer;
   begin
      X := To_Unbounded_Integer (0);
      if not Is_Zero (X) then
         Raise_Exception
         (  Data_Error'Identity,
            "Is_Zero is False when True expected"
         );
      end if;
   end;
   declare
      procedure Check (Value : Integer) is
         X : Unbounded_Integer;
      begin
         X := To_Unbounded_Integer (Value);
         if To_Integer (X) /= Value then
            Raise_Exception
            (  Data_Error'Identity,
               (  "To_Integer "
               &  Integer'Image (To_Integer (X))
               &  " /= "
               &  Integer'Image (Value)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (0);
      Check (1);
      Check (-1);
      Check (Integer'First);
      Check (Integer'Last);
   end;
   declare
      Text    : String (1..15) := (others => '#');
      Pointer : Integer  := 1;
      X       : constant Unbounded_Integer :=
                         To_Unbounded_Integer (12345);
   begin
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => X,
         Field       => 10,
         Justify     => Right,
         Fill        => '*'
      );
      if Pointer /= 11 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Put"
            &  Integer'Image (Pointer)
            &  " /= 11  (expected)"
         )  );
      elsif Text /= "*****12345#####" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Put "
            &  Quote (Text)
            &  " /= ""*****12345#####"" (expected)"
         )  );
      end if;
   end;
   declare
      Text    : String (1..15) := (others => '#');
      Pointer : Integer  := 1;
      X       : Unbounded_Integer := To_Unbounded_Integer (12345);
   begin
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => X,
         Field       => 10,
         Justify     => Left,
         Fill        => '*'
      );
      if Pointer /= 11 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Put"
            &  Integer'Image (Pointer)
            &  " /= 11  (expected)"
         )  );
      elsif Text /= "12345*****#####" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Put "
            &  Quote (Text)
            &  " /= ""12345*****#####"" (expected)"
         )  );
      end if;
      Pointer := 1;
      Get (Text, Pointer, X);
      if Pointer /= 6 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get"
            &  Integer'Image (Pointer)
            &  " /= 6  (expected)"
         )  );
      elsif X /= To_Unbounded_Integer (12345) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get "
            &  Image (X)
            &  " /= 1234 (expected)"
         )  );
      end if;
   end;
   declare
      Text    : String (1..15) := (others => '#');
      Pointer : Integer  := 1;
      X       : constant Unbounded_Integer :=
                         To_Unbounded_Integer (12345);
   begin
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => X,
         Field       => 10,
         Justify     => Center,
         Fill        => '*'
      );
      if Pointer /= 11 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Put"
            &  Integer'Image (Pointer)
            &  " /= 11  (expected)"
         )  );
      elsif Text /= "**12345***#####" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Put "
            &  Quote (Text)
            &  " /= ""**12345***#####"" (expected)"
         )  );
      end if;
   end;
   declare
      procedure Check (W : Integer) is
         X : Unbounded_Integer;
      begin
         X := To_Unbounded_Integer (W);
         if Image (X) /= Trim (Integer'Image (W)) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Image "
               &  Quote (Image (X))
               &  " /= "
               &  Quote (Trim (Integer'Image (W)))
               &  " (expected)"
            )  );
         elsif Value (Image (X)) /= X then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Value (Image (X)) "
               &  Quote (Image (Value (Image (X)))
               &  " /= "
               &  Quote (Image (X)))
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (0);
      Check (1);
      Check (Integer'Last);
   end;
   declare
      X : Unbounded_Integer := To_Unbounded_Integer (Integer'Last);
      Y : Unbounded_Integer;
   begin
      X := X + One;
      declare
         Text    : constant String := Image (X);
         Pointer : Integer := Text'First;
      begin
         Get (Text, Pointer, Y);
         if Pointer /= Text'Last + 1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Get"
               &  Integer'Image (Pointer)
               &  " /="
               &  Integer'Image (Integer (Text'Last + 1))
               &  " (expected)"
            )  );
         elsif Y /= X then
            Raise_Exception
            (  Data_Error'Identity,
               "Get X /= Y"
            );
         end if;
      end;
   end;
   declare
      X : Unbounded_Integer := To_Unbounded_Integer (Integer'Last);
      Y : Unbounded_Integer := To_Unbounded_Integer (Integer'Last);
   begin
      X := X + One;
      Y := Y + One + One;
      Y := Y - X;
      if Y /= One then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Y - X = "
            &  Image (Y)
            &  " /= 1 (expected)"
         )  );
      end if;
      Y := To_Unbounded_Integer (Integer'Last);
      Y := Y + One + One;
      if X - Y /= -One then
         Raise_Exception
         (  Data_Error'Identity,
            (  "X - Y "
            &  Image (X - Y)
            &  " /= -1 (expected)"
         )  );
      end if;
   end;
   declare
      X : Unbounded_Integer := To_Unbounded_Integer (Integer'Last);
      Y : Unbounded_Integer := To_Unbounded_Integer (Integer'Last);
      Q : Unbounded_Integer;
      R : Unbounded_Integer;
   begin
      X := X + One;
      Y := Y + Two;
      Q := Y / X;
      R := Y mod X;
      if Q /= One then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Y / X /= 1 ("
            &  Image (Q)
            &  ")"
         )  );
      elsif R /= One then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Y mod X /= 1 ("
            &  Image (R)
            &  ")"
         )  );
      end if;
      X := Value ("45D9291E6091938000002407A5B35DB",  16);
      Y := Value ("945D9291E6091938000002407A5B35DB", 16);
      Q := X / Y;
      R := X mod Y;
      if Q /= Zero then
         Raise_Exception
         (  Data_Error'Identity,
            "X / Y /= 0"
         );
      elsif R /= Value ("5802768382061651856745051576390596059") then
         Raise_Exception
         (  Data_Error'Identity,
            (  "X mod Y "
            &  Image (R)
            &  " /= "
            &  "5802768382061651856745051576390596059"
         )  );
      end if;
   end;
   declare
      procedure Check (X, Y, Q, R : Unbounded_Integer) is
         X1 : Unbounded_Integer;
         R1 : Unbounded_Integer;
      begin
         X1 := X / Y;
         R1 := X mod Y;
         if X1 /= Q then
            Raise_Exception
            (  Data_Error'Identity,
               (  "X / Y result "
               &  Image (X1)
               &  " /= "
               &  Image (Q)
               &  " (expected)"
            )  );
         elsif R1 /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "X mod Y remainder "
               &  Image (R1)
               &  " /= "
               &  Image (R)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check
      (  X => Value ("945D9291E6091938000002407A5B35DB", 16),
         Y => Value ("BAE8E9AD6E68BBE0025C768141D369EF", 16),
         Q => Value ("0"),
         R => Value ("197211599775089537554893268256760215003")
      );
      Check
      (  X => Value ("3138550867693340381888917894716767200"),
         Y => Value ("45673289045999998889999999768888"),
         Q => Value ("68717"),
         R => Value ("19464319358458164787910598090504")
      );
      Check
      (  X => Value ("313855086769334038191789471"),
         Y => Value ("13"),
         Q => Value ("24142698982256464476291497"),
         R => Value ("10")
      );
      Check
      (  X => Value ("BAE8E9AD6E68BBE0025C768141D369EF", 16),
         Y => Value ("945D9291E6091938000002407A5B35DB", 16),
         Q => Value ("1"),
         R => Value ("51234159854164627202334092808366339092")
      );
      Check
      (  X => Value (  "764576885433975432078965356789005612343" &
                       "26478098"
                    ),
         Y => Value (  "145674686357948906756438976521900875"
                    ),
         Q => Value ("524852261260"),
         R => Value ("86785721234646786180837077003875598")
      );
      Check
      (  X => Value (  "145674686357948906756438976521900875"
                    ),
         Y => Value (  "764576885433975432078965356789005612343" &
                       "26478098"
                    ),
         Q => Value ("0"),
         R => Value ("145674686357948906756438976521900875")
      );
      Check
      (  X => Value (  "999999999999999999999999999999999999999" &
                       "999999999999999999999999999999999999999" &
                       "999999999999999999999999999999999999999" &
                       "99999999999"
                    ),
         Y => Value (  "888888888888888888888888888888888888888" &
                       "888888888888888888888888888888888888888" &
                       "8888888888888888888888888888888888888"
                    ),
         Q => Value ("11250000000000"),
         R => Value ("9999999999999")
      );
   end;
   declare
      procedure Check (X, Y, R : Unbounded_Integer) is
         X1 : Unbounded_Integer;
      begin
         X1 := X + Y;
         if X1 /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "X + Y result "
               &  Image (X1)
               &  " /= "
               &  Image (R)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check
      (  X => Value ("76457688543397543207896535678900561234326478098"),
         Y => Value ("145674686357948906756438976521900875"),
         R => Value ("76457688543543217894254484585657000210848378973")
      );
      Check
      (  X => Value ("145674686357948906756438976521900875"),
         Y => Value ("76457688543397543207896535678900561234326478098"),
         R => Value ("76457688543543217894254484585657000210848378973")
      );
      Check
      (  X => Value (  "999999999999999999999999999999999999999" &
                       "999999999999999999999999999999999999999" &
                       "999999999999999999999999999999999999999" &
                       "99999999999"
                    ),
         Y => Value (  "888888888888888888888888888888888888888" &
                       "888888888888888888888888888888888888888" &
                       "8888888888888888888888888888888888888"
                    ),
         R => Value (  "100000000000008888888888888888888888888" &
                       "888888888888888888888888888888888888888" &
                       "888888888888888888888888888888888888888" &
                       "888888888887"
      )             );
      Check
      (  X => Value (  "888888888888888888888888888888888888888" &
                       "888888888888888888888888888888888888888" &
                       "8888888888888888888888888888888888888"
                    ),
         Y => Value (  "999999999999999999999999999999999999999" &
                       "999999999999999999999999999999999999999" &
                       "999999999999999999999999999999999999999" &
                       "99999999999"
                    ),
         R => Value (  "100000000000008888888888888888888888888" &
                       "888888888888888888888888888888888888888" &
                       "888888888888888888888888888888888888888" &
                       "888888888887"
      )             );
   end;
   declare
      procedure Check (X, Y : Unbounded_Integer; R : String) is
         X1 : Unbounded_Integer;
      begin
         X1 := X - Y;
         if X1 /= Value (R) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "X - Y result "
               &  Image (X1)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check
      (  X => Value ("F5D0EC9F256464FA0A2F1360DA9B9B030000000000000002",
                    16
                    ),
         Y => Value ("9B755A4B9543BFF9648AA5B46ABC4006", 16),
         R => "602739349290069372851406611842622822218117237045601945" &
              "1900"
      );
      Check
      (  X => Value ("76457688543397543207896535678900561234326478098"),
         Y => Value ("145674686357948906756438976521900875"),
         R => "76457688543251868521538586772144122257804577223"
      );
      Check
      (  X => Value ("145674686357948906756438976521900875"),
         Y => Value ("76457688543397543207896535678900561234326478098"),
         R => "-76457688543251868521538586772144122257804577223"
      );
      Check
      (  X => Value (  "999999999999999999999999999999999999999" &
                       "999999999999999999999999999999999999999" &
                       "999999999999999999999999999999999999999" &
                       "99999999999"
                    ),
         Y => Value (  "888888888888888888888888888888888888888" &
                       "888888888888888888888888888888888888888" &
                       "8888888888888888888888888888888888888"
                    ),
         R => (  "999999999999911111111111111111111111111111111" &
                 "111111111111111111111111111111111111111111111" &
                 "11111111111111111111111111111111111111"
      )       );
   end;
   declare
      procedure Check (X, Y, R : Unbounded_Integer) is
         R1 : Unbounded_Integer;
      begin
         R1 := X mod Y;
         if R1 /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Remainder "
               &  Image (R1)
               &  " /= "
               &  Image (R)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check
      (  X => Value (  "764576885433975432078965356789005612343" &
                       "26478098"
                    ),
         Y => Value (  "145674686357948906756438976521900875"
                    ),
         R => Value ("86785721234646786180837077003875598")
      );
      Check
      (  X => Value ("145674686357948906756438976521900875"),
         Y => Value (  "764576885433975432078965356789005612343" &
                       "26478098"
                    ),
         R => Value ("145674686357948906756438976521900875")
      );
      Check
      (  X => Value (  "999999999999999999999999999999999999999" &
                       "999999999999999999999999999999999999999" &
                       "999999999999999999999999999999999999999" &
                       "99999999999"
                    ),
         Y => Value (  "888888888888888888888888888888888888888" &
                       "888888888888888888888888888888888888888" &
                       "8888888888888888888888888888888888888"
                    ),
         R => Value ("9999999999999")
      );
   end;
   declare
      procedure Check (X, Y, Q : Unbounded_Integer) is
         Q1 : Unbounded_Integer;
      begin
         Q1 := X / Y;
         if Q1 /= Q then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Division result "
               &  Image (Q1)
               &  " /= "
               &  Image (Q)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check
      (  X => Value (  "764576885433975432078965356789005612343" &
                       "26478098"
                    ),
         Y => Value (  "145674686357948906756438976521900875"
                    ),
         Q => Value ("524852261260")
      );
      Check
      (  X => Value (  "145674686357948906756438976521900875"
                    ),
         Y => Value (  "764576885433975432078965356789005612343" &
                       "26478098"
                    ),
         Q => Value ("0")
      );
      Check
      (  X => Value (  "999999999999999999999999999999999999999" &
                       "999999999999999999999999999999999999999" &
                       "999999999999999999999999999999999999999" &
                       "99999999999"
                    ),
         Y => Value (  "888888888888888888888888888888888888888" &
                       "888888888888888888888888888888888888888" &
                       "8888888888888888888888888888888888888"
                    ),
         Q => Value ("11250000000000")
      );
   end;
   declare
      use Unbounded_Unsigneds;
      procedure Check (X, Y : String; P : Bit_Count) is
         V : Unbounded_Integer;
      begin
         V := Mul_By_Power_of_Two (Value (X), P);
         if V /= Value (Y) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Mul_By_Power_of_Two "
               &  Image (V)
               &  " /= "
               &  Y
               &  " Power ="
               &  Bit_Count'Image (P)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check
      (  X => "170141183460469231731687303715884105728",
         Y => "340282366920938463463374607431768211456",
         P => 1
      );
      Check
      (  X => "1",
         Y => "2",
         P => 1
      );
      Check
      (  X => "1",
         Y => "1",
         P => 0
      );
      Check
      (  X => "1",
         Y => "4",
         P => 2
      );
   end;
   declare
      procedure Check (X, Y, R : String) is
         R1 : constant Unbounded_Integer := Value (X) * Value (Y);
      begin
         if R1 /= Value (R) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "X * Y "
               &  Image (R1)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check
      (  X => "31385508676933403819178947116038332080511777222320172" &
              "56448",
         Y => "170141183460469231740910675752738881536",
         R => "53399675898022752062770344885171707750656887684133516" &
              "9450077898121965072455024533507718804144128"
      );
      Check
      (  X => "76457688543397543207896535678900561234326478098",
         Y => "145674686357948906756438976521900875",
         R => "11137949798213180507215220338863835286860369130971713" &
              "936218199876323616603014535750"
      );
      Check (X => "1",     Y => "1",    R => "1");
      Check (X => "14485", Y => "6667", R => "96571495");
      Check
      (  X => "145674686357948906756438976521900875",
         Y => "76457688543397543207896535678900561234326478098",
         R => "11137949798213180507215220338863835286860369130971713" &
              "936218199876323616603014535750"
      );
      Check
      (  X => "99999999999999999999999999999999999999999999999999999" &
              "99999999999999999999999999999999999999999999999999999" &
              "99999999999999999999999",
         Y => "88888888888888888888888888888888888888888888888888888" &
              "88888888888888888888888888888888888888888888888888888" &
              "8888888888888888888888888888888888888",
         R => "88888888888888888888888888888888888888888888888888888" &
              "88888888888888888888888888888888888888888888888888888" &
              "88888888888888888888887999999999999991111111111111111" &
              "11111111111111111111111111111111111111111111111111111" &
              "11111111111111111111111111111111111111111111111111111" &
              "1111112"
      );
   end;
   declare
      use Unbounded_Unsigneds;
      procedure Check (X : String; Y : Bit_Count; R : String) is
         R1 : constant Unbounded_Integer := Value (X) ** Y;
      begin
         if R1 /= Value (R) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Exponentiation "
               &  X
               &  " **"
               &  Bit_Count'Image (Y)
               &  " = "
               &  Image (R1)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (X => "2", Y => 2, R => "4");
      Check
      (  X => "31385508676933403819178947116038332080511777222320172",
         Y => 13,
         R => "286734026683655703493776259591969684499921075255782004" &
              "687685343772602933906445009149309261737368063699430258" &
              "415546006584885968466387992469286872835367252855952252" &
              "951759674062830796074593144067265373340709024798812231" &
              "929572458278999201182139657794076133194273863190085164" &
              "048180589748966097479936520133675629777337507731461242" &
              "999161732312045226234856181019625203192189839819291269" &
              "183739220389375939342468140521083772698570480987943022" &
              "400887072938474704453869128590727726015075031730626118" &
              "314546882485709316806308925902438692173319060850187361" &
              "172521687637309075171864959827505444705274213129647277" &
              "873407857171503189116677606786224896832321891472925466" &
              "94769847142191868340639435904253952"
      );
   end;
   declare
      procedure Check (X, Y, R : Unbounded_Integer) is
         R1 : Unbounded_Integer;
      begin
         R1 := X mod Y;
         if R1 /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "mod result "
               &  Image (R1)
               &  " /= "
               &  Image (R)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check
      (  X => Value ("313855086769334038191789471"),
         Y => Value ("13"),
         R => Value ("10")
      );
      Check
      (  X => Value (  "764576885433975432078965356789005612343" &
                       "26478098"
                    ),
         Y => Value (  "145674686357948906756438976521900875"
                    ),
         R => Value ("86785721234646786180837077003875598")
      );
      Check
      (  X => Value (  "145674686357948906756438976521900875"
                    ),
         Y => Value (  "764576885433975432078965356789005612343" &
                       "26478098"
                    ),
         R => Value ("145674686357948906756438976521900875")
      );
      Check
      (  X => Value (  "999999999999999999999999999999999999999" &
                       "999999999999999999999999999999999999999" &
                       "999999999999999999999999999999999999999" &
                       "99999999999"
                    ),
         Y => Value (  "888888888888888888888888888888888888888" &
                       "888888888888888888888888888888888888888" &
                       "8888888888888888888888888888888888888"
                    ),
         R => Value ("9999999999999")
      );
   end;
   declare
      procedure Check (X, Y, R : String) is
         R1 : constant Unbounded_Integer :=
                 Greatest_Common_Divisor (Value (X), Value (Y));
         R2 : constant Unbounded_Integer :=
                 Greatest_Common_Divisor (Value (Y), Value (X));
      begin
         if R1 /= Value (R) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "GCD "
               &  Image (R1)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         elsif R2 /= Value (R) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "GCD "
               &  Image (R2)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check
      (  X => "48",
         Y => "18",
         R => "6"
      );
      Check
      (  X => "3138550867693340381888917894716767200",
         Y => "45673289045999998889999999768888",
         R => "8"
      );
      Check
      (  X => "313855086769334038191789471",
         Y => "13",
         R => "1"
      );
   end;
--end if;
   declare
      procedure Check (X, Y, Q, R : String) is
         Q1 : constant Unbounded_Integer := Value (X) / Value (Y);
         R1 : constant Unbounded_Integer := Value (X) rem Value (Y);
      begin
         if Q1 /= Value (Q) then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " / "
               &  Y
               &  " = "
               &  Image (Q1)
               &  " /= "
               &  Q
               &  " (expected)"
            )  );
         elsif R1 /= Value (R) then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " rem "
               &  Y
               &  " = "
               &  Image (R1)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (X => "10", Y => "5", Q => "2", R => "0");
      Check (X => "11", Y => "5", Q => "2", R => "1");
      Check (X => "12", Y => "5", Q => "2", R => "2");
      Check (X => "13", Y => "5", Q => "2", R => "3");
      Check (X => "14", Y => "5", Q => "2", R => "4");

      Check (X => "-10", Y => "5", Q => "-2", R =>  "0");
      Check (X => "-11", Y => "5", Q => "-2", R => "-1");
      Check (X => "-12", Y => "5", Q => "-2", R => "-2");
      Check (X => "-13", Y => "5", Q => "-2", R => "-3");
      Check (X => "-14", Y => "5", Q => "-2", R => "-4");

      Check (X => "10", Y => "-5", Q => "-2", R => "0");
      Check (X => "11", Y => "-5", Q => "-2", R => "1");
      Check (X => "12", Y => "-5", Q => "-2", R => "2");
      Check (X => "13", Y => "-5", Q => "-2", R => "3");
      Check (X => "14", Y => "-5", Q => "-2", R => "4");

      Check (X => "-10", Y => "-5", Q => "2", R =>  "0");
      Check (X => "-11", Y => "-5", Q => "2", R => "-1");
      Check (X => "-12", Y => "-5", Q => "2", R => "-2");
      Check (X => "-13", Y => "-5", Q => "2", R => "-3");
      Check (X => "-14", Y => "-5", Q => "2", R => "-4");
   end;
   declare
      procedure Check (X, Y, Q, R : String) is
         Q1 : constant Unbounded_Integer := Value (X) / Value (Y);
         R1 : constant Unbounded_Integer := Value (X) mod Value (Y);
      begin
         if Q1 /= Value (Q) then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " / "
               &  Y
               &  " = "
               &  Image (Q1)
               &  " /= "
               &  Q
               &  " (expected)"
            )  );
         elsif R1 /= Value (R) then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " mod "
               &  Y
               &  " = "
               &  Image (R1)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (X => "10", Y => "5", Q => "2", R => "0");
      Check (X => "11", Y => "5", Q => "2", R => "1");
      Check (X => "12", Y => "5", Q => "2", R => "2");
      Check (X => "13", Y => "5", Q => "2", R => "3");
      Check (X => "14", Y => "5", Q => "2", R => "4");

      Check (X => "-10", Y => "5", Q => "-2", R => "0");
      Check (X => "-11", Y => "5", Q => "-2", R => "4");
      Check (X => "-12", Y => "5", Q => "-2", R => "3");
      Check (X => "-13", Y => "5", Q => "-2", R => "2");
      Check (X => "-14", Y => "5", Q => "-2", R => "1");

      Check (X => "10", Y => "-5", Q => "-2", R =>  "0");
      Check (X => "11", Y => "-5", Q => "-2", R => "-4");
      Check (X => "12", Y => "-5", Q => "-2", R => "-3");
      Check (X => "13", Y => "-5", Q => "-2", R => "-2");
      Check (X => "14", Y => "-5", Q => "-2", R => "-1");

      Check (X => "-10", Y => "-5", Q => "2", R =>  "0");
      Check (X => "-11", Y => "-5", Q => "2", R => "-1");
      Check (X => "-12", Y => "-5", Q => "2", R => "-2");
      Check (X => "-13", Y => "-5", Q => "2", R => "-3");
      Check (X => "-14", Y => "-5", Q => "2", R => "-4");
   end;
   declare
      procedure Check (X, Y, R : String) is
         R1 : constant Unbounded_Integer := Value (X) + Value (Y);
      begin
         if R1 /= Value (R) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "X + Y "
               &  Image (R1)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check
      (  X => "31385508676933403819178947116038332080511777222320172",
         Y => "170141183460469231740910675752738881536",
         R => "31385508676933573960362407585270072991187529961201708"
      );
      Check
      (  X => "31385508676933403819178947116038332080511777222320172",
         Y => "-170141183460469231740910675752738881536",
         R => "31385508676933233677995486646806591169836024483438636"
      );
      Check
      (  X => "-31385508676933403819178947116038332080511777222320172",
         Y => "170141183460469231740910675752738881536",
         R => "-31385508676933233677995486646806591169836024483438636"
      );
      Check
      (  X => "-31385508676933403819178947116038332080511777222320172",
         Y => "-170141183460469231740910675752738881536",
         R => "-31385508676933573960362407585270072991187529961201708"
      );
   end;
   declare
      procedure Check (X, Y, R : String) is
         R1 : constant Unbounded_Integer := Value (X) - Value (Y);
      begin
         if R1 /= Value (R) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "X - Y "
               &  Image (R1)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check
      (  X => "31385508676933403819178947116038332080511777222320172",
         Y => "170141183460469231740910675752738881536",
         R => "31385508676933233677995486646806591169836024483438636"
      );
      Check
      (  X => "31385508676933403819178947116038332080511777222320172",
         Y => "-170141183460469231740910675752738881536",
         R => "31385508676933573960362407585270072991187529961201708"
      );
      Check
      (  X => "-31385508676933403819178947116038332080511777222320172",
         Y => "170141183460469231740910675752738881536",
         R => "-31385508676933573960362407585270072991187529961201708"
      );
      Check
      (  X => "-31385508676933403819178947116038332080511777222320172",
         Y => "-170141183460469231740910675752738881536",
         R => "-31385508676933233677995486646806591169836024483438636"
      );
   end;
   declare
      package Long_Integer_Conversions is
         new Signed_Conversions (Long_Integer);
      use Long_Integer_Conversions;
      procedure Check (X : Long_Integer) is
         Y : constant Unbounded_Integer := To_Unbounded_Integer (X);
      begin
         if Image (Y) /= Trim (Long_Integer'Image (X)) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "To_Unbounded_Integer "
               &  Image (Y)
               &  " /= "
               &  Trim (Long_Integer'Image (X))
               &  " (expected)"
            )  );
         elsif From_Unbounded_Integer (Y) /= X then
            Raise_Exception
            (  Data_Error'Identity,
               (  "From_Unbounded_Integer "
               &  Image (Y)
               &  " /= "
               &  Trim (Long_Integer'Image (X))
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (0);
      Check (1);
      Check (-1);
      Check (Long_Integer'First);
      Check (Long_Integer'Last);
   end;
   declare
      package Word_Conversions is
         new Unsigned_Conversions (Unbounded_Unsigneds.Word);
      use Word_Conversions;
      use Unbounded_Unsigneds;
      procedure Check (X : Word) is
         Y : constant Unbounded_Integer := To_Unbounded_Integer (X);
      begin
         if Image (Y) /= Trim (Word'Image (X)) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "To_Unbounded_Integer "
               &  Image (Y)
               &  " /= "
               &  Trim (Word'Image (X))
               &  " (expected)"
            )  );
         elsif From_Unbounded_Integer (Y) /= X then
            Raise_Exception
            (  Data_Error'Identity,
               (  "From_Unbounded_Integer "
               &  Image (Y)
               &  " /= "
               &  Trim (Word'Image (X))
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Check (0);
      Check (1);
      Check (-1);
      Check (Word'First);
      Check (Word'Last);
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Unbounded_Integers;
