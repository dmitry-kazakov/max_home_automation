--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Unbounded_Unsigneds                    Luebeck            --
--  Test                                           Winter, 2024       --
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

with Ada.Calendar;                  use Ada.Calendar;
with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Text_IO;                   use Ada.Text_IO;
with Job_Servers;                   use Job_Servers;
with Strings_Edit.Quoted;           use Strings_Edit.Quoted;
with Unbounded_Unsigneds;           use Unbounded_Unsigneds;
with Unbounded_Unsigneds.Barrett;   use Unbounded_Unsigneds.Barrett;
with Unbounded_Unsigneds.Primes;    use Unbounded_Unsigneds.Primes;
with Unbounded_Unsigneds.Parallel;  use Unbounded_Unsigneds.Parallel;

with Strings_Edit.Unbounded_Unsigned_Edit;
use  Strings_Edit.Unbounded_Unsigned_Edit;

with Strings_Edit.Integers;

with Strings_Edit.Lexicographical_Order;
use  Strings_Edit.Lexicographical_Order;

with Unbounded_Unsigneds.Montgomery;
use  Unbounded_Unsigneds.Montgomery;

with Ada.Unchecked_Conversion;

procedure Test_Unbounded_Unsigneds is
   use Strings_Edit;

   function Compute_Hex_Width return Positive is
      Data  : Half_Word := Half_Word'Last;
      Width : Natural   := 0;
   begin
      loop
         Width := Width + 1;
         Data  := Data / 16;
         exit when Data = 0;
      end loop;
      return Width;
   end Compute_Hex_Width;

   Hex_Width : constant Positive := Compute_Hex_Width;
   Figure    : constant String := "0123456789ABCDEF";

   function Hex_Image (Value : Half_Word) return String is
      Result : String (1..Hex_Width);
      Item   : Half_Word := Value;
   begin
      for Index in reverse Result'Range loop
         Result (Index) := Figure (Integer (Item mod 16) + 1);
         Item := Item / 16;
      end loop;
      return Result;
   end Hex_Image;

   List : constant array (2..1_000) of Primality_Test_Outcome :=
          (    2 |   3 |   5 |   7 |  11 |  13 |  17 |  19 |  23 |
              29 |  31 |  37 |  41 |  43 |  47 |  53 |  59 |  61 |
              67 |  71 |  73 |  79 |  83 |  89 |  97 | 101 | 103 |
             107 | 109 | 113 | 127 | 131 | 137 | 139 | 149 | 151 |
             157 | 163 | 167 | 173 | 179 | 181 | 191 | 193 | 197 |
             199 | 211 | 223 | 227 | 229 | 233 | 239 | 241 | 251 |
             257 | 263 | 269 | 271 | 277 | 281 | 283 | 293 | 307 |
             311 | 313 | 317 | 331 | 337 | 347 | 349 | 353 | 359 |
             367 | 373 | 379 | 383 | 389 | 397 | 401 | 409 | 419 |
             421 | 431 | 433 | 439 | 443 | 449 | 457 | 461 | 463 |
             467 | 479 | 487 | 491 | 499 | 503 | 509 | 521 | 523 |
             541 | 547 | 557 | 563 | 569 | 571 | 577 | 587 | 593 |
             599 | 601 | 607 | 613 | 617 | 619 | 631 | 641 | 643 |
             647 | 653 | 659 | 661 | 673 | 677 | 683 | 691 | 701 |
             709 | 719 | 727 | 733 | 739 | 743 | 751 | 757 | 761 |
             769 | 773 | 787 | 797 | 809 | 811 | 821 | 823 | 827 |
             829 | 839 | 853 | 857 | 859 | 863 | 877 | 881 | 883 |
             887 | 907 | 911 | 919 | 929 | 937 | 941 | 947 | 953 |
             967 | 971 | 977 | 983 | 991 | 997 => Prime,
             others => Composite
          );

   Fibonacci_Numbers : constant array (0..250) of Unbounded_Unsigned :=
      (  Value ("0"),
         Value ("1"),
         Value ("1"),
         Value ("2"),
         Value ("3"),
         Value ("5"),
         Value ("8"),
         Value ("13"),
         Value ("21"),
         Value ("34"),
         Value ("55"),
         Value ("89"),
         Value ("144"),
         Value ("233"),
         Value ("377"),
         Value ("610"),
         Value ("987"),
         Value ("1597"),
         Value ("2584"),
         Value ("4181"),
         Value ("6765"),
         Value ("10946"),
         Value ("17711"),
         Value ("28657"),
         Value ("46368"),
         Value ("75025"),
         Value ("121393"),
         Value ("196418"),
         Value ("317811"),
         Value ("514229"),
         Value ("832040"),
         Value ("1346269"),
         Value ("2178309"),
         Value ("3524578"),
         Value ("5702887"),
         Value ("9227465"),
         Value ("14930352"),
         Value ("24157817"),
         Value ("39088169"),
         Value ("63245986"),
         Value ("102334155"),
         Value ("165580141"),
         Value ("267914296"),
         Value ("433494437"),
         Value ("701408733"),
         Value ("1134903170"),
         Value ("1836311903"),
         Value ("2971215073"),
         Value ("4807526976"),
         Value ("7778742049"),
         Value ("12586269025"),
         Value ("20365011074"),
         Value ("32951280099"),
         Value ("53316291173"),
         Value ("86267571272"),
         Value ("139583862445"),
         Value ("225851433717"),
         Value ("365435296162"),
         Value ("591286729879"),
         Value ("956722026041"),
         Value ("1548008755920"),
         Value ("2504730781961"),
         Value ("4052739537881"),
         Value ("6557470319842"),
         Value ("10610209857723"),
         Value ("17167680177565"),
         Value ("27777890035288"),
         Value ("44945570212853"),
         Value ("72723460248141"),
         Value ("117669030460994"),
         Value ("190392490709135"),
         Value ("308061521170129"),
         Value ("498454011879264"),
         Value ("806515533049393"),
         Value ("1304969544928657"),
         Value ("2111485077978050"),
         Value ("3416454622906707"),
         Value ("5527939700884757"),
         Value ("8944394323791464"),
         Value ("14472334024676221"),
         Value ("23416728348467685"),
         Value ("37889062373143906"),
         Value ("61305790721611591"),
         Value ("99194853094755497"),
         Value ("160500643816367088"),
         Value ("259695496911122585"),
         Value ("420196140727489673"),
         Value ("679891637638612258"),
         Value ("1100087778366101931"),
         Value ("1779979416004714189"),
         Value ("2880067194370816120"),
         Value ("4660046610375530309"),
         Value ("7540113804746346429"),
         Value ("12200160415121876738"),
         Value ("19740274219868223167"),
         Value ("31940434634990099905"),
         Value ("51680708854858323072"),
         Value ("83621143489848422977"),
         Value ("135301852344706746049"),
         Value ("218922995834555169026"),
         Value ("354224848179261915075"),
         Value ("573147844013817084101"),
         Value ("927372692193078999176"),
         Value ("1500520536206896083277"),
         Value ("2427893228399975082453"),
         Value ("3928413764606871165730"),
         Value ("6356306993006846248183"),
         Value ("10284720757613717413913"),
         Value ("16641027750620563662096"),
         Value ("26925748508234281076009"),
         Value ("43566776258854844738105"),
         Value ("70492524767089125814114"),
         Value ("114059301025943970552219"),
         Value ("184551825793033096366333"),
         Value ("298611126818977066918552"),
         Value ("483162952612010163284885"),
         Value ("781774079430987230203437"),
         Value ("1264937032042997393488322"),
         Value ("2046711111473984623691759"),
         Value ("3311648143516982017180081"),
         Value ("5358359254990966640871840"),
         Value ("8670007398507948658051921"),
         Value ("14028366653498915298923761"),
         Value ("22698374052006863956975682"),
         Value ("36726740705505779255899443"),
         Value ("59425114757512643212875125"),
         Value ("96151855463018422468774568"),
         Value ("155576970220531065681649693"),
         Value ("251728825683549488150424261"),
         Value ("407305795904080553832073954"),
         Value ("659034621587630041982498215"),
         Value ("1066340417491710595814572169"),
         Value ("1725375039079340637797070384"),
         Value ("2791715456571051233611642553"),
         Value ("4517090495650391871408712937"),
         Value ("7308805952221443105020355490"),
         Value ("11825896447871834976429068427"),
         Value ("19134702400093278081449423917"),
         Value ("30960598847965113057878492344"),
         Value ("50095301248058391139327916261"),
         Value ("81055900096023504197206408605"),
         Value ("131151201344081895336534324866"),
         Value ("212207101440105399533740733471"),
         Value ("343358302784187294870275058337"),
         Value ("555565404224292694404015791808"),
         Value ("898923707008479989274290850145"),
         Value ("1454489111232772683678306641953"),
         Value ("2353412818241252672952597492098"),
         Value ("3807901929474025356630904134051"),
         Value ("6161314747715278029583501626149"),
         Value ("9969216677189303386214405760200"),
         Value ("16130531424904581415797907386349"),
         Value ("26099748102093884802012313146549"),
         Value ("42230279526998466217810220532898"),
         Value ("68330027629092351019822533679447"),
         Value ("110560307156090817237632754212345"),
         Value ("178890334785183168257455287891792"),
         Value ("289450641941273985495088042104137"),
         Value ("468340976726457153752543329995929"),
         Value ("757791618667731139247631372100066"),
         Value ("1226132595394188293000174702095995"),
         Value ("1983924214061919432247806074196061"),
         Value ("3210056809456107725247980776292056"),
         Value ("5193981023518027157495786850488117"),
         Value ("8404037832974134882743767626780173"),
         Value ("13598018856492162040239554477268290"),
         Value ("22002056689466296922983322104048463"),
         Value ("35600075545958458963222876581316753"),
         Value ("57602132235424755886206198685365216"),
         Value ("93202207781383214849429075266681969"),
         Value ("150804340016807970735635273952047185"),
         Value ("244006547798191185585064349218729154"),
         Value ("394810887814999156320699623170776339"),
         Value ("638817435613190341905763972389505493"),
         Value ("1033628323428189498226463595560281832"),
         Value ("1672445759041379840132227567949787325"),
         Value ("2706074082469569338358691163510069157"),
         Value ("4378519841510949178490918731459856482"),
         Value ("7084593923980518516849609894969925639"),
         Value ("11463113765491467695340528626429782121"),
         Value ("18547707689471986212190138521399707760"),
         Value ("30010821454963453907530667147829489881"),
         Value ("48558529144435440119720805669229197641"),
         Value ("78569350599398894027251472817058687522"),
         Value ("127127879743834334146972278486287885163"),
         Value ("205697230343233228174223751303346572685"),
         Value ("332825110087067562321196029789634457848"),
         Value ("538522340430300790495419781092981030533"),
         Value ("871347450517368352816615810882615488381"),
         Value ("1409869790947669143312035591975596518914"),
         Value ("2281217241465037496128651402858212007295"),
         Value ("3691087032412706639440686994833808526209"),
         Value ("5972304273877744135569338397692020533504"),
         Value ("9663391306290450775010025392525829059713"),
         Value ("15635695580168194910579363790217849593217"),
         Value ("25299086886458645685589389182743678652930"),
         Value ("40934782466626840596168752972961528246147"),
         Value ("66233869353085486281758142155705206899077"),
         Value ("107168651819712326877926895128666735145224"),
         Value ("173402521172797813159685037284371942044301"),
         Value ("280571172992510140037611932413038677189525"),
         Value ("453973694165307953197296969697410619233826"),
         Value ("734544867157818093234908902110449296423351"),
         Value ("1188518561323126046432205871807859915657177"),
         Value ("1923063428480944139667114773918309212080528"),
         Value ("3111581989804070186099320645726169127737705"),
         Value ("5034645418285014325766435419644478339818233"),
         Value ("8146227408089084511865756065370647467555938"),
         Value ("13180872826374098837632191485015125807374171"),
         Value ("21327100234463183349497947550385773274930109"),
         Value ("34507973060837282187130139035400899082304280"),
         Value ("55835073295300465536628086585786672357234389"),
         Value ("90343046356137747723758225621187571439538669"),
         Value ("146178119651438213260386312206974243796773058"),
         Value ("236521166007575960984144537828161815236311727"),
         Value ("382699285659014174244530850035136059033084785"),
         Value ("619220451666590135228675387863297874269396512"),
         Value ("1001919737325604309473206237898433933302481297"),
         Value ("1621140188992194444701881625761731807571877809"),
         Value ("2623059926317798754175087863660165740874359106"),
         Value ("4244200115309993198876969489421897548446236915"),
         Value ("6867260041627791953052057353082063289320596021"),
         Value ("11111460156937785151929026842503960837766832936"),
         Value ("17978720198565577104981084195586024127087428957"),
         Value ("29090180355503362256910111038089984964854261893"),
         Value ("47068900554068939361891195233676009091941690850"),
         Value ("76159080909572301618801306271765994056795952743"),
         Value ("123227981463641240980692501505442003148737643593"),
         Value ("199387062373213542599493807777207997205533596336"),
         Value ("322615043836854783580186309282650000354271239929"),
         Value ("522002106210068326179680117059857997559804836265"),
         Value ("844617150046923109759866426342507997914076076194"),
         Value ("1366619256256991435939546543402365995473880912459"),
         Value ("2211236406303914545699412969744873993387956988653"),
         Value ("3577855662560905981638959513147239988861837901112"),
         Value ("5789092068864820527338372482892113982249794889765"),
         Value ("9366947731425726508977331996039353971111632790877"),
         Value ("15156039800290547036315704478931467953361427680642"),
         Value ("24522987531716273545293036474970821924473060471519"),
         Value ("39679027332006820581608740953902289877834488152161"),
         Value ("64202014863723094126901777428873111802307548623680"),
         Value ("103881042195729914708510518382775401680142036775841"),
         Value ("168083057059453008835412295811648513482449585399521"),
         Value ("271964099255182923543922814194423915162591622175362"),
         Value ("440047156314635932379335110006072428645041207574883"),
         Value ("712011255569818855923257924200496343807632829750245"),
         Value ("1152058411884454788302593034206568772452674037325128"),
         Value ("1864069667454273644225850958407065116260306867075373"),
         Value ("3016128079338728432528443992613633888712980904400501"),
         Value ("4880197746793002076754294951020699004973287771475874"),
         Value ("7896325826131730509282738943634332893686268675876375")
      );
   Mersenne_Powers : constant array (1..52) of Bit_Position :=
      (         2,        3,        5,         7,       13,       17,
               19,       31,       61,        89,      107,      127,
              521,      607,     1279,      2203,     2281,     3217,
             4253,     4423,     9689,      9941,    11213,    19937,
            21701,    23209,    44497,     86243,   110503,   132049,
           216091,   756839,   859433,   1257787,  1398269,  2976221,
          3021377,  6972593, 13466917,  20996011, 24036583, 25964951,
         30402457, 32582657, 37156667,  42643801, 43112609, 57885161,
         74207281, 77232917, 82589933, 136279841
      );
begin
   Put_Line ("Half_Word_Size" & Integer'Image (Half_Word'Size));
   declare
      T : Unbounded_Unsigned := From_Half_Word (456);
   begin
      declare
         S : Montgomery_Number :=
             From_Unbounded_Unsigned (From_Half_Word (123));
      begin
         if Get_Use_Count (S) /= 1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Finalization error in From_Unbounded_Unsigned, "
               &  "Count ="
               &  Integer'Image (Get_Use_Count (S))
               &  " /= 1 (expected)"
            )  );
         end if;
         Swap_Unchecked (T, S);
         if Get_Use_Count (T) /= 1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error in Swap T.Count ="
               &  Integer'Image (Get_Use_Count (T))
               &  " /= 1 (expected)"
            )  );
         elsif Get_Use_Count (S) /= 1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error in Swap S.Count ="
               &  Integer'Image (Get_Use_Count (S))
               &  " /= 1 (expected)"
            )  );
         end if;
      end;
      if Get_Use_Count (T) /= 1 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Swap Count ="
            &  Integer'Image (Get_Use_Count (T))
            &  " /= 1 (expected)"
         )  );
      end if;
   end;
   declare
      X : Unbounded_Unsigned;
   begin
      Put_Line ("Get_MSB test");
      X := From_Half_Word (1);
      if Get_MSB (X) /= 1 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get_MSB (1) ="
            &  Bit_Count'Image (Get_MSB (X))
            &  " /= 1 (expected)"
         )  );
      end if;
      X := From_Half_Word (2);
      if Get_MSB (X) /= 2 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get_MSB (2) ="
            &  Bit_Count'Image (Get_MSB (X))
            &  " /= 2 (expected)"
         )  );
      end if;
      X := From_Half_Word (3);
      if Get_MSB (X) /= 2 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get_MSB (3) ="
            &  Bit_Count'Image (Get_MSB (X))
            &  " /= 2 (expected)"
         )  );
      end if;
      X := From_Half_Word (4);
      if Get_MSB (X) /= 3 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get_MSB (4) ="
            &  Bit_Count'Image (Get_MSB (X))
            &  " /= 3 (expected)"
         )  );
      end if;
      X := From_Half_Word (5);
      if Get_MSB (X) /= 3 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get_MSB (5) ="
            &  Bit_Count'Image (Get_MSB (X))
            &  " /= 3 (expected)"
         )  );
      end if;
      X := From_Half_Word (6);
      if Get_MSB (X) /= 3 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get_MSB (6) ="
            &  Bit_Count'Image (Get_MSB (X))
            &  " /= 3 (expected)"
         )  );
      end if;
      X := From_Half_Word (7);
      if Get_MSB (X) /= 3 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get_MSB (7) ="
            &  Bit_Count'Image (Get_MSB (X))
            &  " /= 3 (expected)"
         )  );
      end if;
      X := From_Half_Word (8);
      if Get_MSB (X) /= 4 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get_MSB (8) ="
            &  Bit_Count'Image (Get_MSB (X))
            &  " /= 4 (expected)"
         )  );
      end if;
      X := From_Half_Word (Half_Word'Last);
      if (  Word (2) ** Natural (Get_MSB (X) - 1)
         /= Half_Word_Modulus / 2
         )  then
         Raise_Exception
         (  Data_Error'Identity,
            (  "2**Get_MSB (Half_Word'Last) ="
            &  Word'Image (2 ** Natural (Get_MSB (X) - 1))
            &  " /="
            &  Word'Image (Half_Word_Modulus / 2)
            &  " (expected)"
         )  );
      end if;--12345678901234567890123456789012345678901234567890
      X := Value
           (  "10000000000000000000000000000000000000000000000000" &
              "00000000000000000000000000000000000000000000000000" &
              "00000000000000000000001100000101",
               2
           );
      if Get_MSB (X) /= 132 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get_MSB () ="
            &  Bit_Count'Image (Get_MSB (X))
            &  " /= 132 (expected)"
         )  );
      end if;
   end;
   declare
      S : Montgomery_Number;
   begin
      declare
         T : constant Unbounded_Unsigned := From_Half_Word (123);
      begin
         Set (S, From_Unbounded_Unsigned (T));
      end;
      if Get_Use_Count (S) /= 1 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Finalization/Set error in From_Unbounded_Unsigned, "
            &  "Count ="
            &  Integer'Image (Get_Use_Count (S))
            &  " /= 1 (expected)"
         )  );
      end if;
   end;
   declare
      T : Unbounded_Unsigned;
   begin
      declare
         S : constant Montgomery_Number :=
             From_Unbounded_Unsigned (From_Half_Word (123));
      begin
         if Get_Use_Count (S) /= 1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Finalization error in From_Unbounded_Unsigned, "
               &  "Count ="
               &  Integer'Image (Get_Use_Count (S))
               &  " /= 1 (expected)"
            )  );
         end if;
         T := To_Unbounded_Unsigned (S);
      end;
      if Get_Use_Count (T) /= 1 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Finalization error in To_Unbounded_Unsigned, "
            &  "Count ="
            &  Integer'Image (Get_Use_Count (T))
            &  " /= 1 (expected)"
         )  );
      end if;
   end;
   declare
      procedure Check (X : Word) is
         R : Word;
      begin
         R := Inverse (X);
         if X * R  /= 1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Wrong inverse"
               &  Word'Image (X)
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Inverve word test");
      for I in Word range 1..100 loop
         Check (I * 2 - 1);
      end loop;
      Check (Half_Word_Modulus - 3);
      Check (Half_Word_Modulus - 1);
      Check (Half_Word_Modulus + 1);
      Check (Half_Word_Modulus + 3);
      Check (not 0 - 2);
      Check (not 0);
   end;
   declare
      X : Unbounded_Unsigned;
   begin
      Put_Line ("From_Half_Word vs. Is_Zero test");
      X := From_Half_Word (0);
      if not Is_Zero (X) then
         Raise_Exception
         (  Data_Error'Identity,
            "Is_Zero is False when True expected"
         );
      end if;
   end;
   declare
      procedure Check
                (  X    : String;
                   E    : Boolean;
                   Base : Positive := 10
                )  is
         N : constant Unbounded_Unsigned := Value (X, Base);
         R : constant Boolean := Is_Mersenne (N);
      begin
         if R /= E then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Is_Mersenne "
               &  X
               &  " is "
               &  Boolean'Image (R)
               &  " /= "
               &  Boolean'Image (E)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Is_Mersenne test");
      Check ("0", False);
      Check ("1", True);
      Check ("2", False);
      Check ("3", True);
      Check ("4", False);
      Check ("5", False);
      Check ("6", False);
      Check ("7", True);
      Check (Image (From_Half_Word (Half_Word'Last - 1)), False);
      Check (Image (From_Half_Word (Half_Word'Last)),     True);
      Check (Image (From_Half_Word (Half_Word'Last) + 1), False);
      Check
      (  "0000000000000001" &
         "FFFFFFFFFFFFFFF",
         True,
         16
      );
      Check
      (  "0000000000000001" &
         "FFFFFFFFFFFFFFF",
         True,
         16
      );
      Check
      (  "0000000000000003" &
         "FFFFFFFFFFFFFFF",
         True,
         16
      );
   end;
   declare
      procedure Check (Value : Half_Word) is
         X : Unbounded_Unsigned;
      begin
         X := From_Half_Word (Value);
         if To_Half_Word (X) /= Value then
            Raise_Exception
            (  Data_Error'Identity,
               (  "To_Half_Word "
               &  Half_Word'Image (To_Half_Word (X))
               &  " /= "
               &  Half_Word'Image (Value)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("From_Half_Word vs. To_Half_Word test");
      Check (0);
      Check (1);
      Check (Half_Word'Last);
   end;
   declare
      procedure Check
                (  X, Y : String;
                   R    : Strings_Edit.Lexicographical_Order.Precedence;
                   Base : Positive := 10
                )  is
         Left   : constant Unbounded_Unsigned := Value (X, Base);
         Right  : constant Unbounded_Unsigned := Value (Y, Base);
         Result : constant Precedence := Compare (Left, Right);
      begin
         if Result /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " compare "
               &  Y
               &  " is "
               &  Precedence'Image (Result)
               &  " /= "
               &  Precedence'Image (R)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Compare test");
      Check
      (  "0000000000000001" &
         "40FA38275AF08F0F",
         "0000000000000001" &
         "40FA38275AF08F0F",
         Equal,
         16
      );
      Check
      (  "0000000001FFFFFF" &
         "FFFFFFFFFFFFFFFF",
         "F0DE2448E18D242D" &
         "416B89E77E8269F5",
         Less,
         16
      );
   end;
   declare
      X : Unbounded_Unsigned := From_Half_Word (Half_Word'Last);
   begin
      Put_Line ("Half_Word Get_Length, Add, Sub test");
      if Get_Length (X) /= 1 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get_Length"
            &  Digit_Offset'Image (Get_Length (X))
            &  " /= 1 (expected)"
         )  );
      end if;
      Add (X, 1);
      if Get_Length (X) /= 2 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get_Length"
            &  Digit_Offset'Image (Get_Length (X))
            &  " /= 2 (expected)"
         )  );
      end if;
      Sub (X, 1);
      if Get_Length (X) /= 1 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get_Length"
            &  Digit_Offset'Image (Get_Length (X))
            &  " /= 1 (expected)"
         )  );
      elsif To_Half_Word (X) /= Half_Word'Last then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Add, Sub"
            &  Half_Word'Image (To_Half_Word (X))
            &  " /="
            &  Half_Word'Image (Half_Word'Last)
            &  " (expected)"
         )  );
      end if;
      Add (X, 999);
      if Get_Length (X) /= 2 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get_Length"
            &  Digit_Offset'Image (Get_Length (X))
            &  " /= 2 (expected)"
         )  );
      end if;
      Sub (X, 999);
      if Get_Length (X) /= 1 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get_Length"
            &  Digit_Offset'Image (Get_Length (X))
            &  " /= 1 (expected)"
         )  );
      elsif To_Half_Word (X) /= Half_Word'Last then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Add, Sub"
            &  Half_Word'Image (To_Half_Word (X))
            &  " /="
            &  Half_Word'Image (Half_Word'Last)
            &  " (expected)"
         )  );
      end if;
   end;
   declare
      procedure Check
                (  X, Y  : String;
                   Power : Digit_Offset;
                   Base  : Natural := 10
                )  is
         Left   : constant Unbounded_Unsigned := Value (X, Base);
         Right  : constant Unbounded_Unsigned := Value (Y, Base);
         Result : Unbounded_Unsigned := Left;
         R      : Unbounded_Unsigned := Right;
      begin
         Shift_Left (R, Power);
         Add (R, Left);
         Add (Result, Right, Power);
         if Result /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " + "
               &  Y
               &  " * Half_Modulus **"
               &  Digit_Offset'Image (Power)
               &  " /= "
               &  Image (R, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Add with shift test");
      Check
      (  X => "6666666666677777" &
              "8782234599999999" &
              "0000000000233456",
         Y => "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         Power => 0,
         Base  => 16
      );
      Check
      (  X => "2000000000000000" &
              "0000000000000000",
         Y => "1000000000000000",
         Power => 6,
         Base  => 16
      );
      Check
      (  X => "D666666666677777" &
              "6666667885666666" &
              "2222222222222226" &
              "9999786543232222" &
              "8782234599999999" &
              "0000000000233456",
         Y => "FFFFFFFFFFFFFFFF",
         Power => 0,
         Base  => 16
      );
      Check
      (  X => "D666666666677777" &
              "6666667885666666" &
              "2222222222222226" &
              "9999786543232222" &
              "8782234599999999" &
              "0000000000233456",
         Y => "FFFFFFFFFFFFFFFF",
         Power => 5,
         Base  => 16
      );
      Check
      (  X => "6666666666677777" &
              "6666667885666666" &
              "2222222222222226" &
              "9999786543232222" &
              "8782234599999999" &
              "0000000000233456",
         Y => "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         Power => 5,
         Base  => 16
      );
      Check
      (  X => "0",
         Y => "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         Power => 5,
         Base  => 16
      );
   end;
   declare
      procedure Check
                (  X, Y : String;
                   R    : String;
                   Base : Natural := 10
                )  is
         Left   : constant Unbounded_Unsigned := Value (X, Base);
         Right  : constant Unbounded_Unsigned := Value (Y, Base);
         Result : Unbounded_Unsigned := Left;
      begin
         Add (Result, Right);
         if Result /= Value (R, Base) then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " + "
               &  Y
               &  " is "
               &  Image (Result, Base)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Add test 1");
      Check
      (  X => "6666666666677777" &
              "6666667885666666" &
              "2222222222222226" &
              "9999786543232222" &
              "8782234599999999" &
              "0000000000233456",
         Y => "FFFFFFFFFFFFFFFE" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "0000000000000001",
         R => "6666666666677777" &
              "6666667885666667" &
              "2222222222222225" &
              "9999786543232222" &
              "8782234599999998" &
              "0000000000233457",
         Base  => 16
      );
   end;
   declare
      procedure Check
                (  P    : Bit_Count;
                   R    : String;
                   Base : Natural := 10
                )  is
         Result : Unbounded_Unsigned;
      begin
         Power_Of_Two (P, Result);
         if Result /= Value (R, Base) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "2 **"
               &  Bit_Count'Image (P)
               &  " is "
               &  Image (Result, Base)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Power_Of_Two test");
      Check (0, "1");
      Check (10, "1024");
      Check (63, "8000000000000000", 16);
      Check
      (  64,
         "0000000000000001" &
         "0000000000000000",
         16
      );
   end;
   declare
      procedure Check
                (  X, Y  : String;
                   M     : Half_Word;
                   Power : Digit_Offset;
                   Base  : Natural := 10
                )  is
         Left   : constant Unbounded_Unsigned := Value (X, Base);
         Right  : constant Unbounded_Unsigned := Value (Y, Base);
         Result : Unbounded_Unsigned := Left;
         R      : Unbounded_Unsigned;
      begin
         Set_Word (R, Half_Word_Modulus);
         R := R ** Bit_Count (Power);
         Mul (R, Right);
         Mul (R, M);
         Add (R, Left);
         Add (Result, Right, M, Power);
         if Result /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " + "
               &  Y
               &  " *"
               &  Half_Word'Image (M)
               &  " * Half_Modulus **"
               &  Digit_Offset'Image (Power)
               &  " is "
               &  Image (Result, Base)
               &  " /= "
               &  Image (R, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Add x multiplier + shift test");
      Check
      (  X => "6666666666677777" &
              "6666667885666666" &
              "2222222222222226" &
              "9999786543232222" &
              "8782234599999999" &
              "0000000000233456",
         Y => "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         M => Half_Word'Last,
         Power => 0,
         Base  => 16
      );
      Check
      (  X => "2000000000000000" &
              "0000000000000000",
         Y => "1000000000000000",
         M => 2,
         Power => 0,
         Base  => 16
      );
      Check
      (  X => "2000000000000000" &
              "0000000000000000",
         Y => "1000000000000000",
         M => 2,
         Power => 6,
         Base  => 16
      );
      Check
      (  X => "6666666666677777" &
              "6666667885666666" &
              "2222222222222226" &
              "9999786543232222" &
              "8782234599999999" &
              "0000000000233456",
         Y => "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         M => Half_Word'Last,
         Power => 5,
         Base  => 16
      );
      Check
      (  X => "D666666666677777" &
              "6666667885666666" &
              "2222222222222226" &
              "9999786543232222" &
              "8782234599999999" &
              "0000000000233456",
         Y => "FFFFFFFFFFFFFFFF",
         M => Half_Word'Last,
         Power => 5,
         Base  => 16
      );
   end;
   declare
      procedure Check
                (  X, Y   : String;
                   M      : Word;
                   Power  : Digit_Offset;
                   Base   : Natural := 10
                )  is
         Left   : constant Unbounded_Unsigned := Value (X, Base);
         Right  : constant Unbounded_Unsigned := Value (Y, Base);
         R_1    : Unbounded_Unsigned := Left;
         R_2    : Unbounded_Unsigned;
         Except : Boolean := False;
      begin
         if M >= Word (Half_Word'Last) then -- Cannot run this test
            return;
         end if;
         begin
            Set (R_2, Right);
            Mul (R_2, Half_Word (M));
            Shift_Left (R_2, Power);
            Sub_2 (Left, R_2);
         exception
            when Constraint_Error =>
               Except := True;
         end;
         Sub (R_1, Right, Half_Word (M), Power);
         if Except then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " - "
               &  Y
               &  " *"
               &  Word'Image (M)
               &  " * Half_Modulus **"
               &  Digit_Offset'Image (Power)
               &  " did not raise "
               &  " (expected)"
            )  );
         elsif R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " - "
               &  Y
               &  " *"
               &  Word'Image (M)
               &  " * Half_Modulus **"
               &  Digit_Offset'Image (Power)
               &  " /= "
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      exception
         when Constraint_Error =>
            if not Except then
               Raise_Exception
               (  Data_Error'Identity,
                  (  X
                  &  " - "
                  &  Y
                  &  " *"
                  &  Word'Image (M)
                  &  " * Half_Modulus **"
                  &  Digit_Offset'Image (Power)
                  &  " unexpectedly raised"
               )  );
            end if;
      end Check;
   begin
      Put_Line ("Sub x multiplier + shift test");
      Check
      (  X => "2000000000000000" &
              "0000000000000000",
         Y => "1000000000000000",
         M     => 2,
         Power => 0,
         Base  => 16
      );
      Check
      (  X => "00000001F41BF473" &
              "6C371BAED3D500F8" &
              "ADC8B152A28A08FC" &
              "0E922B6111AC704C" &
              "7EE2408DB0FD86ED" &
              "FCE476DF40768675" &
              "FA7277CC65DAE784" &
              "CDD2385B67568144" &
              "80DD4CEBBC7BC758" &
              "43C2173973358E20" &
              "1E7F27F882CFFFFE",
         Y => "000000007FFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         M => 33561760236,
         Power => 0,
         Base  => 16
      );
      Check
      (  X => "FFFFFFFA0BE40B8B" &
              "FFFFFFFF2C2AFF07" &
              "52374EAD5D75F703" &
              "F16DD49EEE538FB3" &
              "811DBF724F027912" &
              "031B8920BF89798A" &
              "058D88339A25187B" &
              "322DC7A498A97EBB" &
              "7F22B314438438A7" &
              "BC3DE8C48CCA71E7" &
              "E180D808A4C1C8A6",
         Y => "000000007FFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         M => 4958832804,
         Power => 0,
         Base  => 16
      );
      Check
      (  X => "000000000000003B" &
              "18E4857300C7AFE8",
         Y => "0000000000000003" &
              "7514FB07E5802D80",
         M     => 19,
         Power => 0,
         Base  => 16
      );
      Check
      (  X => "1000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000",
         Y => "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         M     => Word (Half_Word'Last),
         Power => 0,
         Base  => 16
      );
      Check
      (  X => "D666666666677777" &
              "6666667885666666" &
              "2222222222222226" &
              "9999786543232222" &
              "8782234599999999" &
              "0000000000233456",
         Y => "6666666666677777",
         M     => 2,
         Power => 0,
         Base  => 16
      );
      Check
      (  X => "6666666666677777" &
              "6666667885666666" &
              "2222222222222226" &
              "9999786543232222" &
              "8782234599999999" &
              "0000000000233456",
         Y => "222",
         M     => 2,
         Power => 0,
         Base  => 16
      );
      Check
      (  X => "2000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000",
         Y => "0000000000000001" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000",
         M     => 2305843009213693952,
         Power => 47,
         Base  => 16
      );
      Check
      (  X => "00000000001E43B7" &
              "5F1CA468AF8D242D" &
              "3249AE305F425A0E",
         Y => "0000000001FFFFFF" &
              "FFFFFFFFFFFFFFFF",
         M     => 31322371,
         Power => 1,
         Base  => 16
      );
      Check
      (  X => "2000000000000000" &
              "0000000000000000",
         Y => "1000000000000000",
         M     => 2,
         Power => 1,
         Base  => 16
      );
      Check
      (  X => "D666666666677777" &
              "6666667885666666" &
              "2222222222222226" &
              "9999786543232222" &
              "8782234599999999" &
              "0000000000233456",
         Y => "6666666666677777",
         M     => 2,
         Power => 5,
         Base  => 16
      );
      Check
      (  X => "1000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000",
         Y => "0000000000000001",
         M     => 2,
         Power => 3,
         Base  => 16
      );
      Check
      (  X => "1000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000",
         Y => "FFFFFFFFFFFFFFFF",
         M     => 2,
         Power => 3,
         Base  => 16
      );
      Check
      (  X => "1000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000",
         Y => "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         M     => Word (Half_Word'Last),
         Power => 2,
         Base  => 16
      );
      Check
      (  X => "1000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000",
         Y => "FFFFFFFFFFFFFFFF",
         M     => Word (Half_Word'Last),
         Power => 3,
         Base  => 16
      );
   end;
   declare
      procedure Check
                (  X, Y  : String;
                   Power : Digit_Offset;
                   Base  : Natural := 10
                )  is
         Left   : constant Unbounded_Unsigned := Value (X, Base);
         Right  : constant Unbounded_Unsigned := Value (Y, Base);
         R_1    : Unbounded_Unsigned := Left;
         R_2    : Unbounded_Unsigned := Right;
         Except : Boolean := False;
      begin
         begin
            Shift_Left (R_2, Power);
            Sub_2 (Left, R_2);
         exception
            when Constraint_Error =>
               Except := True;
         end;
         Sub (R_1, Right, Power);
         if Except then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " - "
               &  Y
               &  " * Half_Modulus **"
               &  Digit_Offset'Image (Power)
               &  " did not raised (expected)"
            )  );
         elsif R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " - "
               &  Y
               &  " * Half_Modulus **"
               &  Digit_Offset'Image (Power)
               &  " /= "
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      exception
         when Constraint_Error =>
            if not Except then
               Raise_Exception
               (  Data_Error'Identity,
                  (  X
                  &  " - "
                  &  Y
                  &  " * Half_Modulus **"
                  &  Digit_Offset'Image (Power)
                  &  " unexpectedly raised"
               )  );
            end if;
      end Check;
   begin
      Put_Line ("Sub + shift test");
      Check
      (  X => "6666666666677777" &
              "6666667885666666" &
              "2222222222222226" &
              "9999786543232222" &
              "8782234599999999" &
              "0000000000233456",
         Y => "222",
         Power => 2,
         Base  => 16
      );
      Check
      (  X => "1000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000",
         Y => "0000000000000001",
         Power => 3,
         Base  => 16
      );
      Check
      (  X => "6666666666677777" &
              "6666667885666666" &
              "2222222222222226" &
              "9999786543232222" &
              "8782234599999999" &
              "0000000000233456",
         Y => "6666666666677777",
         Power => 5,
         Base  => 16
      );
      Check
      (  "2000000000000000" &
         "0000000000000000",
         "1000000000000000",
         1,
         16
      );
      Check
      (  "2000000000000000" &
         "0000000000000000",
         "2000000000000000",
         1,
         16
      );
   end;
   declare
      X, Y      : Unbounded_Unsigned;
      Remainder : Half_Word;
   begin
      Put_Line ("Div test 1");
      X := From_Half_Word (1);
      if 1 /= X mod 20 then
         Raise_Exception
         (  Data_Error'Identity,
            "Div 1 mod 20 /=" & Half_Word'Image (X mod 20)
         );
      end if;
      Div (X, 20, Remainder);
      if not Is_Zero (X) then
         Raise_Exception
         (  Data_Error'Identity,
            "Div 1 20 is not zero"
         );
      elsif Remainder /= 1 then
         Raise_Exception
         (  Data_Error'Identity,
            "Div 1 20 mod /=" & Half_Word'Image (Remainder)
         );
      end if;
      X := From_Half_Word (123);
      if 123 mod 20 /= X mod 20 then
         Raise_Exception
         (  Data_Error'Identity,
            "123 mod 20 /=" & Half_Word'Image (123 mod 20)
         );
      end if;
      Div (X, 20, Remainder);
      if To_Half_Word (X) /= 123 / 20 then
         Raise_Exception
         (  Data_Error'Identity,
            "Div 123 20 is not" & Half_Word'Image (123 / 20)
         );
      elsif Remainder /= 123 mod 20 then
         Raise_Exception
         (  Data_Error'Identity,
            "Div 123 20 mod /=" & Half_Word'Image (123 mod 20)
         );
      end if;
      X := From_Half_Word (Half_Word'Last);
      Add (X, 1);
      if Half_Word (Half_Word_Modulus mod 20) /= X mod 20 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Div Modulus mod 20 /="
            &  Word'Image (Half_Word_Modulus mod 20)
         )  );
      end if;
      Div (X, 20, Remainder);
      if To_Half_Word (X) /= Half_Word (Half_Word_Modulus / 20) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Div Modulus 20 is not"
            &  Half_Word'Image (Half_Word (Half_Word_Modulus / 20) / 20)
         )  );
      elsif Remainder /= Half_Word (Half_Word_Modulus mod 20) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Div Modulus 20 mod /="
            &  Word'Image (Half_Word_Modulus mod 20)
         )  );
      end if;
      X := Value
           (  "00008661D87CDE3E" &
              "E23FF73E18B5BE89" &
              "33D27B72337C70FE" &
              "912CC9E083FD0DF7" &
              "66FE30D43275C21C" &
              "FC81AFEFF7BDEDBB" &
              "89875B5D25EF8B86" &
              "ECAF962113273D96" &
              "363EBC5E605F6154" &
              "89F51EA1B8431BE6" &
              "B288851174EE47A3" &
              "03F09E3C619D9A4A" &
              "6D19F507CE7A8AE2" &
              "24BA6A01A236B2D5" &
              "7CA445847FA5CCAA" &
              "026157C76A6D770B",
              16
           );
      Set (Y, X);
      Div (Y, 1024, Remainder);
      if X /= (Y * 1024 + Remainder) then
         Raise_Exception
         (  Data_Error'Identity,
            "Div by 1024 error"
         );
      end if;
   end;
   declare
      Text    : String (1..15) := (others => '#');
      Pointer : Integer  := 1;
      X       : constant Unbounded_Unsigned :=
                         From_Half_Word (12345);
   begin
      Put_Line ("Put test 1");
      Put (Text, Pointer, X, 10, 10, Right, '*');
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
      Text    : String (1..30) := (others => '#');
      Pointer : Integer  := 1;
      X       : Unbounded_Unsigned := From_Half_Word (12345);
   begin
      Put_Line ("Get test 1");
      Put (Text, Pointer, X, 2, 20, Left, '*');
      if Pointer /= 21 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Put"
            &  Integer'Image (Pointer)
            &  " /= 21  (expected)"
         )  );    -- 123456789012345678901234567890
      elsif Text /= "11000000111001******##########" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Put "
            &  Quote (Text)
            &  " /= ""11000000111001*******########"" (expected)"
         )  );
      end if;
      Pointer := 1;
      Get (Text, Pointer, X, 2);
      if Pointer /= 15 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get"
            &  Integer'Image (Pointer)
            &  " /= 15  (expected)"
         )  );
      elsif X /= From_Half_Word (12345) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get "
            &  Image (X)
            &  " /= 12345 (expected)"
         )  );
      end if;
   end;
   declare
      Text    : String (1..15) := (others => '#');
      Pointer : Integer  := 1;
      X       : Unbounded_Unsigned := From_Half_Word (12345);
   begin
      Put_Line ("Get test 2");
      Put (Text, Pointer, X, 10, 10, Left, '*');
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
      elsif X /= From_Half_Word (12345) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Get "
            &  Image (X)
            &  " /= 12345 (expected)"
         )  );
      end if;
   end;
   declare
      Text    : String (1..15) := (others => '#');
      Pointer : Integer  := 1;
      X       : constant Unbounded_Unsigned :=
                         From_Half_Word (12345);
   begin
      Put_Line ("Put test 2");
      Put (Text, Pointer, X, 10, 10, Center, '*');
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
      procedure Check (W : Word) is
         X : Unbounded_Unsigned;
      begin
         X := From_Word (W);
         if Image (X) /= Trim (Word'Image (W)) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Image "
               &  Quote (Image (X))
               &  " /= "
               &  Quote (Trim (Word'Image (W)))
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
      Put_Line ("Image test 1");
      Check (0);
      Check (1);
      Check (1000);
      Check (10001);
      Check (Word (Half_Word'Last));
      Check (Word (Half_Word'Last + 1));
   end;
   declare
      procedure Check (W : String) is
         X : Unbounded_Unsigned;
      begin
         X := Value (W);
         if Image (X) /= W then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Image "
               &  Quote (Image (X))
               &  " /= "
               &  Quote (W)
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
      Put_Line ("Image test 2");
      Check ("345");
      Check ("1208925819614629174706176");
      Check ("1208000000000000000000076");
   end;
   declare
      X : Unbounded_Unsigned := From_Half_Word (Half_Word'Last);
   begin
      Add (X, 1);
   end;
   declare
      X : Unbounded_Unsigned := From_Half_Word (Half_Word'Last);
      Y : Unbounded_Unsigned;
   begin
      Put_Line ("Get test 3");
      Add (X, 1);
      declare
         Text    : constant String  := Image (X);
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
      X : Unbounded_Unsigned := From_Half_Word (Half_Word'Last);
   begin
      Put_Line ("Value test");
      Add (X, 1);
      if (  Image (X)
         /= Trim (Word'Image (Half_Word_Modulus))
         )  then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Image "
            &  Quote (Image (X))
            &  " /= "
            &  Quote (Trim (Word'Image (Word (Half_Word'Last) + 1)))
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
   end;
   declare
      package Conversions is
         new Unbounded_Unsigneds.Unsigned_Conversions (Word);
      use Conversions;
      procedure Check (X : Word) is
         Y : constant Unbounded_Unsigned := To_Unbounded_Unsigned (X);
      begin
         if From_Unbounded_Unsigned (Y) /= X then
            Raise_Exception
            (  Data_Error'Identity,
               (  "From_Unbounded_Unsigned "
               &  Image (Y)
               &  " /= "
               &  Word'Image (X)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Unsigned_Conversions test");
      Check (0);
      Check (1);
      Check (Half_Word'Modulus);
      Check (Half_Word'Modulus * 2);
      Check (Half_Word'Modulus * 3);
   end;
   declare
      X : Unbounded_Unsigned := From_Half_Word (Half_Word'Last);
      Y : Unbounded_Unsigned := From_Half_Word (Half_Word'Last);
   begin
      Put_Line ("Sub test 1");
      Add (X, 1);
      Add (Y, 2);
      Sub (Y, X);
      if Y /= One then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Sub Y X "
            &  Image (Y)
            &  " /= 1 (expected)"
         )  );
      end if;
      Y := From_Half_Word (Half_Word'Last);
      Add (Y, 2);
      begin
         Sub (X, Y);
         Raise_Exception
         (  Data_Error'Identity,
            "Sub X Y not raising"
         );
      exception
         when Constraint_Error =>
            null;
      end;
   end;
   declare
      X : Unbounded_Unsigned := From_Half_Word (Half_Word'Last);
      Y : Unbounded_Unsigned := From_Half_Word (Half_Word'Last);
      R : Unbounded_Unsigned;
   begin
      Put_Line ("Div test 2");
      Add (X, 1);
      Add (Y, 2);
      Div (Y, X, R);
      if Y /= One then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Div Y X /= 1 ("
            &  Image (Y)
            &  ")"
         )  );
      elsif R /= One then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Div Y X remainder /= 1 ("
            &  Image (R)
            &  ")"
         )  );
      end if;
      X := Value ("45D9291E6091938000002407A5B35DB",  16);
      Y := Value ("945D9291E6091938000002407A5B35DB", 16);
      Div (X, Y, R);
      if X /= Value ("0") then
         Raise_Exception
         (  Data_Error'Identity,
            "Div X, Y /= " & "0"
         );
      elsif R /= Value ("5802768382061651856745051576390596059") then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Div X, Y remainder "
            &  Image (R)
            &  " /= "
            &  "5802768382061651856745051576390596059"
         )  );
      end if;
   end;
   declare
      procedure Check (S : String; Hex : String) is
         X : Unbounded_Unsigned;
         function Hex_Image (Index : Digit_Offset) return String is
         begin
            if Index = Get_Length (X) then
               declare
                  Result : constant String :=
                                    Hex_Image (Get_Digit (X, Index));
                  First  : Integer := Result'First;
               begin
                  while Result (First) = '0' loop
                     First := First + 1;
                  end loop;
                  return Result (First..Result'Last);
               end;
            else
               return Hex_Image (Index + 1) &
                      Hex_Image (Get_Digit (X, Index));
            end if;
         end Hex_Image;
      begin
         X := Value (S);
         declare
            Text : constant String := Hex_Image (Index => 1);
         begin
            if Text /= Hex then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Value "
                  &  Text
                  &  " /= "
                  &  Hex
                  &  " (expected)"
               )  );
            end if;
         end;
      end Check;
   begin
      Put_Line ("Value 16-base test");
      Check
      (  "999999999999999999999999999999999999999" &
         "999999999999999999999999999999999999999" &
         "999999999999999999999999999999999999999" &
         "99999999999",
         "24EE91F2603A6337F19BCCDB0DAC404DC08D3CF" &
         "F5EC2374E42F0F1538FD03DF99092E953E00FFF" &
         "FFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
      );
      Check
      (  "999999999999999999999999999999999999999",
         "2F050FE938943ACC45F65567FFFFFFFFF"
      );
      Check ("1", "1");
      Check ("9", "9");
      Check ("10", "A");
      Check ("16", "10");
   end;
   declare
      procedure Check (X, Y, Q, R : Unbounded_Unsigned) is
         X1 : Unbounded_Unsigned := X;
         R1 : Unbounded_Unsigned;
      begin
         Div (X1, Y, R1);
         if X1 /= Q then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Div result "
               &  Image (X1)
               &  " /= "
               &  Image (Q)
               &  " (expected)"
            )  );
         elsif R1 /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Div remainder "
               &  Image (R1)
               &  " /= "
               &  Image (R)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Div test 3");
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
      procedure Check (X, Y, R : Unbounded_Unsigned) is
         X1 : Unbounded_Unsigned := X;
      begin
         Add (X1, Y);
         if X1 /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Add result "
               &  Image (X1)
               &  " /= "
               &  Image (R)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Add test 2");
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
      procedure Check
                (  X, Y : Unbounded_Unsigned;
                   R    : String
                )  is
      begin
         declare
            X1 : Unbounded_Unsigned := X;
         begin
            Sub (X1, Y);
            if R = "" then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Sub result "
                  &  Image (X1)
                  &  " does not raise (expected)"
               )  );
            elsif X1 /= Value (R) then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Sub result "
                  &  Image (X1)
                  &  " /= "
                  &  R
                  &  " (expected)"
               )  );
            end if;
         exception
            when Constraint_Error =>
               if R /= "" then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Sub raises "
                     &  " /= "
                     &  R
                     &  " (expected)"
                  )  );
               end if;
         end;
         declare
            Y1 : Unbounded_Unsigned := Y;
         begin
            Sub_2 (X, Y1);
            if R = "" then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Sub_2 result "
                  &  Image (Y1)
                  &  " does not raise (expected)"
               )  );
            elsif Y1 /= Value (R) then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Sub_2 result "
                  &  Image (Y1)
                  &  " /= "
                  &  R
                  &  " (expected)"
               )  );
            end if;
         exception
            when Constraint_Error =>
               if R /= "" then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Sub_2 raises "
                     &  " /= "
                     &  R
                     &  " (expected)"
                  )  );
               end if;
         end;
      end Check;
   begin
      Put_Line ("Sub test 2");
      Check
      (  X => Value
              (  "0000000000000001" &
                 "03DAC8BD658BEEF3" &
                 "1B224CC5C3BE0505" &
                 "D2CE53FFF8824EAD" &
                 "A7EE8EF9FEE76FCC" &
                 "3C654B5228583216" &
                 "643CC8840C95A5A8" &
                 "E43729797D503CA3" &
                 "082A3BCFBFDBBFA8" &
                 "9017D18CCFC66A24" &
                 "9A40BEE9139F529A" &
                 "0A3D657C7D4696DD" &
                 "73114BAD1829C9D6" &
                 "0A0F06373370EFB3" &
                 "2CCCB0936BFC23D6" &
                 "BD3B64C67C91A406" &
                 "832BA827F1303586",
                 16
              ),
         Y => Value
              (  "7226F4ECD4497143" &
                 "CFEF5D414FE9D1AA" &
                 "2711763CFDC7A5CA" &
                 "8C1E8BFCE0FF4B04" &
                 "2B9FF9937F7C318C" &
                 "0CE68FDB210B4307" &
                 "75E739101DF35ED3" &
                 "8440217B9BCE7E92" &
                 "1A11BA3128E24FAD" &
                 "3F19FEA162D6C9AC" &
                 "FAFC2D34F42C8057" &
                 "A0C3E65F6D8B9DA9" &
                 "FD1C5030BA027BFF" &
                 "22ED2F1667B76839" &
                 "45AE472420CB034D" &
                 "C4C42C367FA09606",
                 16
              ),
         R => "10231574043388634874930631956381912097501780290181011" &
              "04301085940488622563215383027149251936104341862386601" &
              "23389277424328590896516309250459870243698326978841570" &
              "61417238156683868933366423702655887010682049615389235" &
              "66760049110388542101472744785336772529858086500338666" &
              "36597752683349258813561895382805611866464128"
      );
      Check
      (  X => Value ("76457688543397543207896535678900561234326478098"),
         Y => Value ("145674686357948906756438976521900875"),
         R => "76457688543251868521538586772144122257804577223"
      );
      Check
      (  X => Value ("F5D0EC9F256464FA0A2F1360DA9B9B030000000000000002",
                    16
                    ),
         Y => Value ("9B755A4B9543BFF9648AA5B46ABC4006", 16),
         R => "602739349290069372851406611842622822218117237045601945" &
              "1900"
      );
      Check
      (  X => Value ("145674686357948906756438976521900875"),
         Y => Value ("76457688543397543207896535678900561234326478098"),
         R => ""
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
         R => ""
      );
   end;
   declare
      procedure Check
                (  P : Bit_Position;
                   X : String;
                   Base   : Positive := 16;
                   Except : Boolean  := False
                )  is
         X1 : Unbounded_Unsigned := Value (X, Base);
         R  : Unbounded_Unsigned;
      begin
         Sub_From_Power_Of_Two (P, X1);
         if Except then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Sub_From_Power_Of_Two result "
               &  Image (X1, Base)
               &  " does not raise (expected)"
            )  );
         end if;
         R := Power_Of_Two (P) - Value (X, Base);
         if X1 /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Sub_From_Power_Of_Two result "
               &  Image (X1, Base)
               &  " /= "
               &  Image (R, Base)
               &  " (expected)"
            )  );
         end if;
      exception
         when Constraint_Error =>
            if not Except then
               R := Power_Of_Two (P) - Value (X, Base);
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Sub_From_Power_Of_Two raises "
                  &  " /= "
                  &  Image (R, Base)
                  &  " (expected)"
               )  );
            end if;
      end Check;
   begin
      Put_Line ("Sub_From_Power_Of_Two test");
      Check
      (  P => 128,
         X => "F001000000000010" &
              "FF00000000000010",
         Base => 16
      );
      Check
      (  P => 127,
         X => "4001000000000010" &
              "FF00000000000010",
         Base => 16
      );
      Check
      (  P => 80,
         X => "0000000000000010" &
              "FF00000000000010",
         Base => 16
      );
      Check
      (  P => 64,
         X => "FF00000000000010",
         Base => 16
      );
      Check
      (  P => 63,
         X => "7F00000000000000",
         Base => 16
      );
      Check
      (  P => 63,
         X => "0F00000000000000",
         Base => 16
      );
      Check
      (  P => 2,
         X => "4",
         Except => True
      );
      Check
      (  P => 2,
         X => "3"
      );
      Check
      (  P => 2,
         X => "2"
      );
      Check
      (  P => 2,
         X => "1"
      );
      Check
      (  P => 2,
         X => "0"
      );
   end;
   declare
      procedure Check
                (  P : Digit_Count;
                   X : String;
                   Base   : Positive := 16;
                   Except : Boolean  := False
                )  is
         X1 : Unbounded_Unsigned := Value (X, Base);
         R  : Unbounded_Unsigned;
      begin
         Sub_From_Power_Of_Half_Word (P, X1);
         if Except then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Sub_From_Power_Of_Half_Word result "
               &  Image (X1, Base)
               &  " does not raise (expected)"
            )  );
         end if;
         R := (  Power_Of_Two (Bit_Count (P) * Half_Word'Size)
              -  Value (X, Base)
              );
         if X1 /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Sub_From_Power_Of_Half_Word result "
               &  Image (X1, Base)
               &  " /= "
               &  Image (R, Base)
               &  " (expected)"
            )  );
         end if;
      exception
         when Constraint_Error =>
            if not Except then
               R := (  Power_Of_Two (Bit_Count (P) * Half_Word'Size)
                    -  Value (X, Base)
                    );
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Sub_From_Power_Of_Half_Word raises "
                  &  " /= "
                  &  Image (R, Base)
                  &  " (expected)"
               )  );
            end if;
      end Check;
   begin
      Put_Line ("Sub_From_Power_Of_Half_Word test");
      Check
      (  P => 4,
         X => "F001000000000010" &
              "FF00000000000010",
         Base => 16
      );
      Check
      (  P => 3,
         X => "FF00000000000010",
         Base => 16
      );
      Check
      (  P => 1,
         X => "F001000000000010" &
              "FF00000000000010",
         Except => True
      );
   end;
   declare
      procedure Check
                (  X, Y, R : String;
                   Base    : Positive := 10
                )  is
         Left   : Unbounded_Unsigned := Value (X, Base);
         Right  : constant Unbounded_Unsigned := Value (Y, Base);
         Result : constant Unbounded_Unsigned := Value (R, Base);
      begin
         Modulo (Left, Right);
         if Result /= Left then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Modulo ("
               &  X
               &  ", "
               &  Y
               &  " is "
               &  Image (Left, Base)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Modulo test");
      Check
      (  "0F00000000000000" &
         "0000000000000001" &
         "0000000000000000" &
         "0000000000000001",
         "0000000000000001" &
         "0000000000000000" &
         "0000000000000000",
         "1",
         16
      );
      Check
      (  "468A94087E5D3474" &
         "96F69DF69ACD1D46" &
         "2C92AF9C7748A181" &
         "15F26820728979AC" &
         "1E5400254F33EC7C" &
         "F71CCC5ACCBE3BEC" &
         "1F2D3E76366A3E22" &
         "702E539B0452A20E" &
         "BE6EF9DA1B72224C" &
         "911D53FB9E8C41D8" &
         "3E9F9A46E850DDE6" &
         "2770FA2CCACD35AE" &
         "505329B375080B5B" &
         "FFA2CF100A85598A" &
         "76825B29A9B1BE62" &
         "8B916961D620BEEE" &
         "74773C5723E6DF96" &
         "D2F08604D93EE21D" &
         "B5FCE42E1F976224" &
         "950228861C24F9E9" &
         "842F596ED98D842E" &
         "6B9584BF6B5C411C" &
         "D17CDB249DCD052E" &
         "B84203EC94D71A0A" &
         "11FA6A498E656B6D" &
         "DDA7B542F2A291C5" &
         "DECFAFC3FD3336FC" &
         "305CF8CEAC8184FC" &
         "814F5DDC3443E3BA" &
         "2AEA71B36DC26AD4" &
         "5C356984B2185F47" &
         "FD9F6FE324361000",
         "8661D87CDE3EE23F" &
         "F73E18B5BE8933D2" &
         "7B72337C70FE912C" &
         "C9E083FD0DF766FE" &
         "30D43275C21CFC81" &
         "AFEFF7BDEDBB8987" &
         "5B5D25EF8B86ECAF" &
         "962113273D96363E" &
         "BC5E605F615489F5" &
         "1EA1B8431BE6B288" &
         "851174EE47A303F0" &
         "9E3C619D9A4A6D19" &
         "F507CE7A8AE224BA" &
         "6A01A236B2D57CA4" &
         "45847FA5CCAA0261" &
         "57C76A6D770B4C41",
         "1",
         16
      );
      Check
      (  "2FF448A14ECAD4BA" &
         "F76D7050C720B2B4" &
         "A61F31B8BC32F7D3" &
         "4591AEF904D3709C" &
         "4EA808AD6980E7C5" &
         "C7E0DD20E2D0EE13" &
         "132B12A5A3D065EB" &
         "22FBECE0D691EEF3" &
         "C0F2427E14267691" &
         "050B31B85DDC252B" &
         "FC89ABCD8EF0FCA1" &
         "0EC1855DA80EE7A2" &
         "43AA54AE02DCFD45" &
         "1E5AD86A8197A0C3" &
         "098B45FA56685642" &
         "B012FBEB599D9DAC" &
         "EB8109B34C73F0EF" &
         "8C4DED094B8A5007" &
         "C02B4334AE435478" &
         "CDF40A00E6D240C1" &
         "53253305F4455287" &
         "84F1E4B623E268F2" &
         "78647F86028CE51C" &
         "13AC55AB43355027" &
         "78B3BBF7C3222CCD" &
         "3BB8E58CAF724AF6" &
         "01F369CC47B02259" &
         "6DA8092CB9DE6DC7" &
         "3492233B49D2CAF3" &
         "83CBFA980A3A044C" &
         "B9B99AF453BDE3C2" &
         "26FB81E0A9FE2FE1",
         "8661D87CDE3EE23F" &
         "F73E18B5BE8933D2" &
         "7B72337C70FE912C" &
         "C9E083FD0DF766FE" &
         "30D43275C21CFC81" &
         "AFEFF7BDEDBB8987" &
         "5B5D25EF8B86ECAF" &
         "962113273D96363E" &
         "BC5E605F615489F5" &
         "1EA1B8431BE6B288" &
         "851174EE47A303F0" &
         "9E3C619D9A4A6D19" &
         "F507CE7A8AE224BA" &
         "6A01A236B2D57CA4" &
         "45847FA5CCAA0261" &
         "57C76A6D770B4C41",
         "15206CEAD90A9DB5" &
         "737C418C8F7D23B7" &
         "D5A332F29CE8EEC2" &
         "5D468F867208B798" &
         "EA1801D8E8855E83" &
         "60A0DAF4A0B1078B" &
         "0644FB7CE5878397" &
         "3D79F0F2457F1253" &
         "049B7ECA0723544D" &
         "CAE212C228231C58" &
         "FE4D3E30D84F956F" &
         "5682CF703721EE2D" &
         "AE564A2F0C73B562" &
         "D36F6ACFFAC72FD8" &
         "C57E1F6DF8F1A119" &
         "BC71D93A70087843",
         16
      );
      Check
      (  "0000000000D039A6" &
         "FBF25547AEDF2AC5",
         "0000000000000001" &
         "FBF25547AEDF2AC5",
         "0000000000000001" &
         "47892B4FBB60D2F6",
         16
      );
      Check
      (  "11",
         "6",
         "5"
      );
      Check
      (  "11",
         "5",
         "1"
      );
      Check
      (  "0000000000D039A6" &
         "FBF25547AEDF2AC5",
         "0000000000000001" &
         "0380E6C4955FBA37",
         "E5FEBA616DF7C20F",
         16
      );
      Check
      (  "0000000000D039A6" &
         "FBF25547AEDF2AC5",
         "0000000010000000",
         "000000000EDF2AC5",
         16
      );
      Check
      (  "B8F8E35C527E1D55" &
         "1773DAE0FE42C58E",
         "0000000000000001" &
         "0380E6C4955FBA37",
         "0",
         16
      );
      Check
      (  "00000000000AFF73" &
         "6BF0886C2C63D3EB" &
         "864F1CEFD492B600" &
         "3B2D70F147E39F91" &
         "624C09BB27E454BE" &
         "FB38A82D2933CB04" &
         "61A2E2B3B00D0A9C" &
         "EDC3C88D5927ECD8" &
         "07F22CF8F3990E5C" &
         "ABBA6C571D9EB37F" &
         "F24190B62C52C291" &
         "BF894CC7C8633ADE" &
         "BA2531591F2A9CC9" &
         "FFCD20E317FF13F8" &
         "D1B5A86180D36FD1" &
         "6BAC5DA27A3B0477" &
         "9E1CC1A54FE8A9FD" &
         "5A73359CA1701824" &
         "6BD00C04339CBF0A" &
         "62DDE85DD994F709" &
         "59A5FA478D551651" &
         "2370795649D0710F" &
         "F76F5136980437F2" &
         "9AC7F80BAFA958A9",
         "8661D87CDE3EE23F" &
         "F73E18B5BE8933D2" &
         "7B72337C70FE912C" &
         "C9E083FD0DF766FE" &
         "30D43275C21CFC81" &
         "AFEFF7BDEDBB8987" &
         "5B5D25EF8B86ECAF" &
         "962113273D96363E" &
         "BC5E605F615489F5" &
         "1EA1B8431BE6B288" &
         "851174EE47A303F0" &
         "9E3C619D9A4A6D19" &
         "F507CE7A8AE224BA" &
         "6A01A236B2D57CA4" &
         "45847FA5CCAA0261" &
         "57C76A6D770B4C41",
         "09F623F751A33EE7" &
         "04EC4B3CEAEC06A4" &
         "BC532536BF6573BE" &
         "99A6F3C521F43CFE" &
         "7C19B60435B64D55" &
         "198E57434A171DFE" &
         "0D1C98CBA46502E4" &
         "F37E2DD3812EDC05" &
         "E0E0E169246FA491" &
         "4D6CC01DB1E323B9" &
         "FB95490F37381B46" &
         "E97CBD2DEAC30663" &
         "A3634C88AC539B54" &
         "EAF04423DD2F951F" &
         "0C89E7B33A17150B" &
         "BD36460ED93E531D",
         16
      );
   end;
   declare
      procedure Check
                (  X    : String;
                   Y, R : Half_Word;
                   Base : Positive := 10
                )  is
         Left : constant Unbounded_Unsigned := Value (X, Base);
         R1   : Half_Word;
      begin
         R1 := Left mod Y;
         if R1 /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " mod"
               &  Half_Word'Image (Y)
               &  " is"
               &  Half_Word'Image (R1)
               &  " /="
               &  Half_Word'Image (R)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Mod test 1");
      Check
      (  X => "1000000000000000" &
              "0000000000000000" &
              "0000000000000000",
         Y => 100,
         R => 56,
         Base => 16
      );
      Check
      (  X => "1000000000000000" &
              "0000000000000000" &
              "0000000000000000",
         Y => 256,
         R => 0,
         Base => 16
      );
      Check
      (  X => "123",
         Y => 100,
         R => 23
      );
   end;
   declare
      procedure Check
                (  X    : String;
                   Bit  : Bit_Position;
                   R    : String;
                   Base : Positive := 10
                )  is
         Left : constant Unbounded_Unsigned := Value (X, Base);
         R_1  : constant Unbounded_Unsigned := Value (R, Base);
         R_2  : Unbounded_Unsigned := Left;
      begin
         Set_Bit (R_2, Bit);
         if R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Set_Bit "
               &  X
               &  Bit_Position'Image (Bit)
               &  " is "
               &  Image (R_2, Base)
               &  " /="
               &  Image (R_1, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Set_Bit test");
      Check
      (  X    => "8000000000000000",
         Bit  => 64,
         R    => "8000000000000000",
         Base => 16
      );
      Check
      (  X    => "8000000000000000",
         Bit  => 65,
         R    => "0000000000000001" &
                 "8000000000000000",
         Base => 16
      );
      Check
      (  X    => "0",
         Bit  => 65,
         R    => "0000000000000001" &
                 "0000000000000000",
         Base => 16
      );
   end;
   declare
      procedure Check
                (  X    : String;
                   From : Bit_Position;
                   To   : Bit_Position;
                   Base : Positive := 10
                )  is
         Left : constant Unbounded_Unsigned := Value (X, Base);
         R_1  : Unbounded_Unsigned;
         R_2  : Unbounded_Unsigned;
      begin
         Set (R_1, Left);
         Clear_Slice (R_1, From, To);
         Set (R_2, Left);
         for Bit in From..To loop
            Clear_Bit (R_2, Bit);
         end loop;
         if R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Clear_Slice "
               &  X
               &  Bit_Position'Image (From)
               &  " .."
               &  Bit_Position'Image (To)
               &  " is "
               &  Image (R_1, Base)
               &  " /="
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Clear_Bits test");
      Check
      (  X => "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         From => 12,
         To   => 120,
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         From => 12,
         To   => 192,
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         From => 12,
         To   => 131,
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         From => 1,
         To   => 127,
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         From => 1,
         To   => 128,
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF",
         From => 2,
         To   => 63,
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF",
         From => 2,
         To   => 64,
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF",
         From => 1,
         To   => 64,
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF",
         From => 1,
         To   => 1,
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF",
         From => 64,
         To   => 1,
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF",
         From => 64,
         To   => 128,
         Base => 16
      );
   end;
   declare
      procedure Check
                (  X    : String;
                   From : Bit_Position;
                   To   : Bit_Position;
                   Base : Positive := 10
                )  is
         Left : constant Unbounded_Unsigned := Value (X, Base);
         R_1  : Unbounded_Unsigned;
         R_2  : Unbounded_Unsigned;
      begin
         Set (R_1, Left);
         Set_Slice (R_1, From, To);
         Set (R_2, Left);
         for Bit in From..To loop
            Set_Bit (R_2, Bit);
         end loop;
         if R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Set_Slice "
               &  X
               &  Bit_Position'Image (From)
               &  " .."
               &  Bit_Position'Image (To)
               &  " is "
               &  Image (R_1, Base)
               &  " /="
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Set_Bits test");
      Check
      (  X    => "0",
         From => 66,
         To   => 129,
         Base => 16
      );
      Check
      (  X    => "8000000000000000",
         From => 64,
         To   => 128,
         Base => 16
      );
      Check
      (  X    => "8000000000000000" &
                 "0000000000000000" &
                 "0000000000000000",
         From => 12,
         To   => 120,
         Base => 16
      );
      Check
      (  X    => "8000000000000000" &
                 "0000000000000000" &
                 "0000000000000000",
         From => 12,
         To   => 192,
         Base => 16
      );
      Check
      (  X    => "8000000000000000" &
                 "0000000000000000" &
                 "0000000000000000",
         From => 12,
         To   => 131,
         Base => 16
      );
      Check
      (  X    => "8000000000000000" &
                 "0000000000000000" &
                 "0000000000000000",
         From => 1,
         To   => 127,
         Base => 16
      );
      Check
      (  X    => "8000000000000000" &
                 "0000000000000000" &
                 "0000000000000000",
         From => 1,
         To   => 128,
         Base => 16
      );
      Check
      (  X    => "8000000000000000",
         From => 2,
         To   => 63,
         Base => 16
      );
      Check
      (  X    => "8000000000000000",
         From => 2,
         To   => 64,
         Base => 16
      );
      Check
      (  X    => "8000000000000000",
         From => 1,
         To   => 64,
         Base => 16
      );
      Check
      (  X    => "8000000000000000",
         From => 1,
         To   => 1,
         Base => 16
      );
      Check
      (  X    => "8000000000000000",
         From => 64,
         To   => 1,
         Base => 16
      );
   end;
   declare
      procedure Check
                (  X, Y : String;
                   From : Bit_Position;
                   To   : Bit_Position;
                   Base : Positive := 10
                )  is
         Left  : constant Unbounded_Unsigned := Value (X, Base);
         Right : constant Unbounded_Unsigned := Value (Y, Base);
         R_1   : Unbounded_Unsigned;
         R_2   : Unbounded_Unsigned;
      begin
         Set (R_2, Left);
         for Bit in From..To loop
            if Get_Bit (Right, Bit - From + 1) then
               Set_Bit (R_2, Bit);
            else
               Clear_Bit (R_2, Bit);
            end if;
         end loop;
         Set (R_1, Left);
         Replace_Slice (R_1, Right, From, To);
         if R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Replace_Slice "
               &  X
               &  Bit_Position'Image (From)
               &  " .."
               &  Bit_Position'Image (To)
               &  " with "
               &  Y
               &  " is "
               &  Image (R_1, Base)
               &  " /="
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Replace_Slice test");
      Check
      (  X    => "0",
         Y    => "123456789ABCDEFE" &
                 "DCBA987654321012",
         From => 4,
         To   => 4,
         Base => 16
      );
      Check
      (  X    => "8000000000000000" &
                 "0000000000000000" &
                 "000000FFFFFFFFF1",
         Y    => "0",
         From => 4,
         To   => 124,
         Base => 16
      );
      Check
      (  X    => "8000000000000000" &
                 "0000000000000000" &
                 "0000000000000001",
         Y    => "123456789ABCDEFE" &
                 "DCBA987654321012",
         From => 4,
         To   => 124,
         Base => 16
      );

      Check
      (  X    => "8000000000000000" &
                 "0000000000000000" &
                 "0000000000000000" &
                 "0000000000000001",
         Y    => "123456789ABCDEFE" &
                 "DCBA987654321012" &
                 "3456789ABCDEF012",
         From => 4,
         To   => 188,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "123456789ABCDEFE" &
                 "DCBA987654321012",
         From => 4,
         To   => 124,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "123456789ABCDEFE" &
                 "123456789ABCDEFE",
         From => 1,
         To   => 127,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "123456789ABCDEFE" &
                 "123456789ABCDEFE",
         From => 140,
         To   => 207,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "123456789ABCDEFE" &
                 "DCBA987654321012",
         From => 1,
         To   => 127,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "123456789ABCDEFE" &
                 "DCBA987654321012",
         From => 1,
         To   => 128,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "123456789ABCDEFE" &
                 "DCBA987654321012",
         From => 1,
         To   => 64,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "123456789ABCDEFE" &
                 "DCBA987654321012",
         From => 1,
         To   => 24,
         Base => 16
      );
      Check
      (  X    => "FFFFFFF",
         Y    => "ABCD",
         From => 2,
         To   => 25,
         Base => 16
      );
      Check
      (  X    => "FFFFFFF",
         Y    => "ABCD",
         From => 1,
         To   => 24,
         Base => 16
      );
      Check
      (  X    => "F",
         Y    => "ABCD",
         From => 1,
         To   => 24,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "ABCD",
         From => 1,
         To   => 24,
         Base => 16
      );
   end;
   declare
      procedure Do_Slice is new Generic_Replace_Slice ("xor");
      procedure Check
                (  X, Y : String;
                   From : Bit_Position;
                   To   : Bit_Position;
                   Base : Positive := 10
                )  is
         Left  : constant Unbounded_Unsigned := Value (X, Base);
         Right : constant Unbounded_Unsigned := Value (Y, Base);
         R_1   : Unbounded_Unsigned;
         R_2   : Unbounded_Unsigned;
      begin
         Set (R_1, Left);
         Do_Slice (R_1, Right, From, To);
         Set (R_2, Left);
         for Bit in reverse From..To loop
            if Get_Bit (Left, Bit) xor Get_Bit (Right, Bit - From + 1)
            then
               Set_Bit (R_2, Bit);
            else
               Clear_Bit (R_2, Bit);
            end if;
         end loop;
         if R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Generic_Replace_Slice xor "
               &  X
               &  Bit_Position'Image (From)
               &  " .."
               &  Bit_Position'Image (To)
               &  " with "
               &  Y
               &  " is "
               &  Image (R_1, Base)
               &  " /="
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Generic_Replace_Slice test");
      Check
      (  X    => "F0000000110010000",
         Y    => "00000000001010000",
         From => 1,
         To   => 24,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "FFFFFFFFFFFFFFFF" &
                 "FFFFFFFFFFFFFFFF",
         From => 4,
         To   => 4,
         Base => 16
      );
      Check
      (  X    => "8000000000000000" &
                 "0000000000000000" &
                 "000000FFFFFFFFF1",
         Y    => "0",
         From => 4,
         To   => 124,
         Base => 16
      );
      Check
      (  X    => "8000000000000000" &
                 "0000000000000000" &
                 "0000000000000001",
         Y    => "FFFFFFFFFFFFFFFF" &
                 "FFFFFFFFFFFFFFFF",
         From => 4,
         To   => 124,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "FFFFFFFFFFFFFFFF" &
                 "FFFFFFFFFFFFFFFF",
         From => 4,
         To   => 124,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "FFFFFFFFFFFFFFFF" &
                 "FFFFFFFFFFFFFFFF",
         From => 1,
         To   => 127,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "123456789ABCDEFE" &
                 "DCBA987654321012",
         From => 1,
         To   => 127,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "123456789ABCDEFE" &
                 "DCBA987654321012",
         From => 1,
         To   => 128,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "123456789ABCDEFE" &
                 "DCBA987654321012",
         From => 1,
         To   => 64,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "123456789ABCDEFE" &
                 "DCBA987654321012",
         From => 1,
         To   => 24,
         Base => 16
      );
      Check
      (  X    => "FFFFFFF",
         Y    => "ABCD",
         From => 2,
         To   => 25,
         Base => 16
      );
      Check
      (  X    => "FFFFFFF",
         Y    => "ABCD",
         From => 1,
         To   => 24,
         Base => 16
      );
      Check
      (  X    => "F",
         Y    => "ABCD",
         From => 1,
         To   => 24,
         Base => 16
      );
      Check
      (  X    => "0",
         Y    => "ABCD",
         From => 1,
         To   => 24,
         Base => 16
      );
   end;
   declare
      procedure Check
                (  X    : String;
                   From : Bit_Position;
                   To   : Bit_Position;
                   Base : Positive := 10
                )  is
         Left : constant Unbounded_Unsigned := Value (X, Base);
         R_1  : Unbounded_Unsigned;
         R_2  : Unbounded_Unsigned;
      begin
         Set (R_1, Left);
         Invert_Slice (R_1, From, To);
         Set (R_2, Left);
         for Bit in reverse From..To loop
            if Get_Bit (R_2, Bit) then
               Clear_Bit (R_2, Bit);
            else
               Set_Bit (R_2, Bit);
            end if;
         end loop;
         if R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Invert_Slice "
               &  X
               &  Bit_Position'Image (From)
               &  " .."
               &  Bit_Position'Image (To)
               &  " is "
               &  Image (R_1, Base)
               &  " /= "
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Invert_Slice test");
      Check
      (  X    => "0",
         From => 4,
         To   => 124,
         Base => 16
      );
      Check
      (  X    => "8000000000000000" &
                 "0000000000000000" &
                 "0000000000000001",
         From => 4,
         To   => 124,
         Base => 16
      );
      Check
      (  X    => "0",
         From => 1,
         To   => 127,
         Base => 16
      );
      Check
      (  X    => "0",
         From => 3,
         To   => 127,
         Base => 16
      );
      Check
      (  X    => "0",
         From => 1,
         To   => 128,
         Base => 16
      );
      Check
      (  X    => "0",
         From => 1,
         To   => 64,
         Base => 16
      );
      Check
      (  X    => "0",
         From => 1,
         To   => 24,
         Base => 16
      );
      Check
      (  X    => "FFFFFFF",
         From => 2,
         To   => 25,
         Base => 16
      );
      Check
      (  X    => "FFFFFFF",
         From => 1,
         To   => 24,
         Base => 16
      );
      Check
      (  X    => "F",
         From => 1,
         To   => 24,
         Base => 16
      );
      Check
      (  X    => "0",
         From => 1,
         To   => 24,
         Base => 16
      );
   end;
   declare
      procedure Check
                (  X    : String;
                   Base : Positive := 10
                )  is
         Left : Unbounded_Unsigned := Value (X, Base);
         R    : Unbounded_Unsigned;
      begin
         Set_Bit
         (  R,
            Bit_Count (Get_Length (Left)) * Half_Word'Size + 1
         );
         Sub (R, Left);
         Clear_Bit
         (  R,
            Bit_Count (Get_Length (Left)) * Half_Word'Size + 1
         );
         Complement (Left);
         if Left /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Complement "
               &  X
               &  " is "
               &  Image (Left, Base)
               &  " /="
               &  Image (R, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Complement test");
      Check
      (  X => "FFFFFFFFFFFFFFFF",
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF" &
              "0000000000000001",
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFF0" &
              "00000001E99233A1",
         Base => 16
      );
      Check
      (  X => "000000000000000F" &
              "FFFFFFFE166DCC5F",
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF" &
              "00000001E99233A1",
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         Base => 16
      );
      Check
      (  X => "0000000000000000",
         Base => 16
      );
      Check
      (  X => "0000000000000001",
         Base => 16
      );
      Check
      (  X => "0000000000000001" &
              "0000000000000000",
         Base => 16
      );
   end;
   declare
      procedure Check
                (  X      : String;
                   Length : Digit_Count;
                   Base   : Positive := 10
                )  is
         Left : constant Unbounded_Unsigned := Value (X, Base);
         R_1  : Unbounded_Unsigned := Left;
         R_2  : Unbounded_Unsigned := Left;
         Y    : Unbounded_Unsigned;
         From : constant Bit_Position := 1;
         To   : constant Bit_Offset :=
                         Bit_Count (Length * Half_Word'Size);
      begin
         Complement (R_1, Length);
         Y := Get_Slice (Left, From, To);
         Invert_Slice (Y, From, To);
         Add (Y, 1);
         Replace_Slice (R_2, Y, From, To);
         if R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Complement "
               &  X
               &  " is "
               &  Image (R_1, Base)
               &  " /="
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Complement 2**K test");
      Check
      (  X => "00001123499FFFF0" &
              "FFFFFFFFFFFFFFF0" &
              "00000001E99233A1",
         Length => 2,
         Base   => 16
      );
      Check
      (  X => "0000000000000001",
         Length => 2,
         Base   => 16
      );
      Check
      (  X => "000000000000000F" &
              "FFFFFFFE166DCC5F",
         Length => 2,
         Base   => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF" &
              "00000001E99233A1",
         Length => 2,
         Base   => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF" &
              "0000000000000001",
         Length => 2,
         Base   => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF",
         Length => 1,
         Base   => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         Length => 2,
         Base   => 16
      );
      Check
      (  X => "0000000000000000",
         Length => 2,
         Base   => 16
      );
      Check
      (  X => "0000000000000001",
         Length => 1,
         Base   => 16
      );
      Check
      (  X => "0000000000000001" &
              "0000000000000000",
         Length => 2,
         Base   => 16
      );
   end;
   declare
      procedure Check
                (  X    : String;
                   R    : Boolean;
                   Base : Positive := 10
                )  is
         Left : constant Unbounded_Unsigned := Value (X, Base);
      begin
         if R /= Is_Power_Of_Two (Left) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Is_Power_Of_Two "
               &  X
               &  " is "
               &  Boolean'Image (Is_Power_Of_Two (Left))
               &  " /="
               &  Boolean'Image (R)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Is_Power_Of_Two test");
      Check
      (  X => "FFFFFFFFFFFFFFF0" &
              "00000001E99233A1",
         R => False,
         Base => 16
      );
      Check
      (  X => "0000000000004000" &
              "0000000000000000",
         R => True,
         Base => 16
      );
      Check
      (  X => "0000000000000000",
         R => False,
         Base => 16
      );
      Check
      (  X => "0000000000000001",
         R => True,
         Base => 16
      );
      Check
      (  X => "8000000000000000",
         R => True,
         Base => 16
      );
      Check
      (  X => "A000000000000000" &
              "0000000000000000",
         R => False,
         Base => 16
      );
      Check
      (  X => "8000000000000000" &
              "0000000000000000",
         R => True,
         Base => 16
      );
      Check
      (  X => "0000000000000002",
         R => True,
         Base => 16
      );
      Check
      (  X => "0000000000000003",
         R => False,
         Base => 16
      );
   end;
   declare
      procedure Check
                (  X    : String;
                   P    : Bit_Count;
                   R    : String;
                   Base : Positive := 10
                )  is
         Left : constant Unbounded_Unsigned := Value (X, Base);
      begin
         if Modulo_By_Power_Of_Two (Left, P) /= Value (R, Base) then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " mod 2 **"
               &  Bit_Count'Image (P)
               &  " is "
               &  Image (Modulo_By_Power_Of_Two (Left, P), Base)
               &  " /="
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Modulo_By_Power_Of_Two test");
      Check
      (  X => "C0F8DEB7B35847FF" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000001",
         P => 448,
         R => "1",
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFF0" &
              "00000001E99233A1",
         P => 124,
         R => "0FFFFFFFFFFFFFF0" &
              "00000001E99233A1",
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFF0" &
              "00000001E99233A1",
         P => 64,
         R => "00000001E99233A1",
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFF0" &
              "00000001E99233A1",
         P => 4,
         R => "0000000000000001",
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFF0" &
              "00000001E99233A1",
         P => 400,
         R => "FFFFFFFFFFFFFFF0" &
              "00000001E99233A1",
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFF0" &
              "00000001E99233A1",
         P => 128,
         R => "FFFFFFFFFFFFFFF0" &
              "00000001E99233A1",
         Base => 16
      );
   end;
   declare
      procedure Check (X, M : String; Base : Positive := 10) is
         Left    : constant Unbounded_Unsigned := Value (X, Base);
         Modulus : constant Unbounded_Unsigned := Value (M, Base);
         R       : Unbounded_Unsigned;
         T       : Half_Word;
      begin
         R := Mod_Inv (Left, Modulus);
         if (Left * R) mod Modulus /= 1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Wrong inverse "
               &  X
               &  " in "
               &  M
               &  " is "
               &  Image (R, Base)
            )  );
         end if;
         if Is_Odd (Modulus) then
            T := Inverse (Modulus);
            R := Modulus * T;
            if Get_Digit (R, 1) /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Wrong 2**K inverse "
                  &  M
                  &  " in Half_Word_M is"
                  &  Half_Word'Image (T)
               )  );
            end if;
         end if;
      end Check;
   begin
      Put_Line ("Mod_Inv test");
      Check
      (  X => "F000000000000000" &
              "0000000000000001",
         M => "0000000000000001" &
              "0000000000000000" &
              "0000000000000000",
         Base => 16
      );
      Check (X =>  "3", M => "11");
      Check (X => "42", M => "2017");
   end;
   declare
      procedure Check
                (  X    : String;
                   P    : Bit_Position;
                   Base : Positive := 10
                )  is
         Left : constant Unbounded_Unsigned := Value (X, Base);
         R    : Unbounded_Unsigned;
      begin
         R := Mod_Inv_In_Power_Of_Two (Left, P);
         if Modulo_By_Power_Of_Two (Left * R, P) /= 1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Wrong inverse "
               &  X
               &  " in 2 **"
               &  Bit_Position'Image (P)
               &  " is "
               &  Image (R, Base)
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Mod_Inv_In_Power_Of_Two test");
      Check
      (  X  => "000000003FFFFFFF" &
               "FFFFFFFFFFFFFFFF" &
               "FFFFFFFFFFFFFFFF" &
               "FFFFFFFFFFFFFFFF" &
               "FFFFFFFFFFFFFFFF" &
               "FFFFFFFFFFFFFFFF" &
               "FFFFFFFFFFFFFFEF",
         P    => 448,
         Base => 16
      );
      Check
      (  X    => "0000000001FFFFFF" &
                 "FFFFFFFFFFFFFFFF",
         P    => 128,
         Base => 16
      );
      Check (X => "3", P => 64);
      Check
      (  X    => "F000000000000000" &
                 "0000000000000001",
         P    => 128,
         Base => 16
      );
      Check (X => "47", P => 3);
      Check (X => "3",  P => 3);
   end;
   declare
      procedure Check
                (  X    : String;
                   P    : Digit_Count;
                   Base : Positive := 10
                )  is
         Left : constant Unbounded_Unsigned := Value (X, Base);
         R    : Unbounded_Unsigned;
      begin
         R := Inverse (Left, P);
         if Modulo (Left * R, P) /= 1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Wrong inverse "
               &  X
               &  " in 2 ** (Half_Word_Modulus *"
               &  Digit_Count'Image (P)
               &  ") is "
               &  Image (R, Base)
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Inverse in Half_Word_Modulus * K test");
      Check
      (  X  => "000000003FFFFFFF" &
               "FFFFFFFFFFFFFFFF" &
               "FFFFFFFFFFFFFFFF" &
               "FFFFFFFFFFFFFFFF" &
               "FFFFFFFFFFFFFFFF" &
               "FFFFFFFFFFFFFFFF" &
               "FFFFFFFFFFFFFFEF",
         P    => 7,
         Base => 16
      );
      Check
      (  X    => "0000000001FFFFFF" &
                 "FFFFFFFFFFFFFFFF",
         P    => 2,
         Base => 16
      );
      Check (X => "3", P => 64);
      Check
      (  X    => "F000000000000000" &
                 "0000000000000001",
         P    => 2,
         Base => 16
      );
   end;
   declare
      procedure Check (X, Y, R : String; Base : Positive := 10) is
         R1 : Unbounded_Unsigned;
      begin
         R1 := Value (X, Base) mod Value (Y, Base);
         if R1 /= Value (R, Base) then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " mod "
               &  Y
               &  " is "
               &  Image (R1, Base)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Mod test 2");
      Check
      (  X => "3FFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFF0000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0001FFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         Y => "000000007FFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         R => "0",
         Base => 16
      );
      Check
      (  X => "8000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000" &
              "0000000000000000",
         Y => "0000000000000001" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         R => "2000000000000000",
         Base => 16
      );
      Check
      (  X => "0003BBE2061E43B7" &
              "5F1CA468ADAF332A" &
              "3249AE305F425A0E",
         Y => "0000000001FFFFFF" &
              "FFFFFFFFFFFFFFFF",
         R => "00000000018D242D" &
              "416B89DFED948E65",
         Base => 16
      );
      Check
      (  X => "52219444070657625334587635535831219128998212452369189" &
              "01921167416419769539857787284244134059674987791704450" &
              "53357219631418993786719092896803631618043925682638972" &
              "97848827185499917018079506719185915721403500592797311" &
              "31881594196988563728361673421722933087484039543529018" &
              "52035642024370059304557233988891799014503343469488440" &
              "89389297345281509513047029978972671641173465151334822" &
              "15295125079861999338571077708469177799426457431591189" &
              "57217248367043905936319748237550094520674504208530837" &
              "54683416692527551648604413477538499180818470596650760" &
              "68984129185940459168283756106592464231840627751129991" &
              "50206172392431297837246097308511903252956622805412865" &
              "91769004380431105141713509884910115658450883900333759" &
              "77425399608182096851426875623920074535795677299913952" &
              "56699805775897135553415567045292136442139895777424891" &
              "47716176725853261163453069745299384650106148169784389" &
              "14394742203080037064728374599115252858211885774081606" &
              "90315522951458068463354171428220365223949985950890732" &
              "88173661192513362652994989799804539973460088731240885" &
              "92249337278296250891645352365597165827754037841109232" &
              "85873186648442456409760158728501220463308455437074192" &
              "53920596490226149092866948882405156304295150065120673" &
              "35948633366082457555658014603908690167180451219023541" &
              "70201577095168",
         Y => "17976931348623159077293051907890247336179769789423065" &
              "72734300811577326758055009631327084773224075360211201" &
              "13879871393357658789768814416622492847430639474124377" &
              "76789342486548527630221960124609411945308295208500576" &
              "88381506823424628814739131105408272371633505106845862" &
              "98239947245938479716304835356329624224137216",
         R => "0"
      );
      Check
      (  X => "000000000000003B" &
              "18E4857300C7AFE8",
         Y => "0000000000000003" &
              "7514FB07E5802D80",
         R => "527FD9ECC344AA68",
         Base => 16
      );
      Check
      (  X => "999999999999999999999999999999999999999" &
              "999999999999999999999999999999999999999" &
              "999999999999999999999999999999999999999" &
              "99999999999",
         Y => "888888888888888888888888888888888888888" &
              "888888888888888888888888888888888888888" &
              "8888888888888888888888888888888888888",
         R => "9999999999999"
      );
      Check
      (  X => "145674686357948906756438976521900875",
         Y => "76457688543397543207896535678900561234326478098",
         R => "145674686357948906756438976521900875"
      );
      Check
      (  X => "1000000000000000" &
              "0000000000000000" &
              "0000000000000000",
         Y => "100",
         R => "0",
         Base => 16
      );
      Check
      (  X => "76457688543397543207896535678900561234326478098",
         Y => "145674686357948906756438976521900875",
         R => "86785721234646786180837077003875598"
      );
      Check
      (  X => "10",
         Y => "7",
         R => "3"
      );
      Check
      (  X => "10",
         Y => "5",
         R => "0"
      );
      Check
      (  X => "0000000000001B8C" &
              "96780FB363DFF216" &
              "25A9F48824F07AA4" &
              "4E390B4886E2F705" &
              "ED0DE76C166DCC5E" &
              "F3E65E4C8BE40B8C" &
              "93C8E4512C2AFF07" &
              "52374EAD5D75F703" &
              "F16DD49EEE538FB3" &
              "811DBF724F027912" &
              "031B520792995A23" &
              "3DCDA4074ED12F6A" &
              "E84CD25BFC37682A" &
              "715CC508696869CF" &
              "8F625008A4FDB542" &
              "E180D8077D300002",
         Y => "000000007FFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         R => "000000002C2AFF07" &
              "52374EAD5D75F703" &
              "F16DD49EEE538FB3" &
              "811DBF724F027912" &
              "031B8920BF89798A" &
              "058D88339A25187B" &
              "322DC7A498A97EBB" &
              "7F22B314438438A7" &
              "BC3DE8C68CCA71DB" &
              "F948EF20A4C1C8A4",
         Base => 16
      );
   end;
   declare
      procedure Check (N : String; R : Primality_Test_Outcome) is
         Result : constant Primality_Test_Outcome :=
                           Lucas_Lehmer (Value (N));
      begin
         if Result /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Lucas-Lehmer "
               &  N
               &  " is "
               &  Primality_Test_Outcome'Image (Result)
               &  " /= "
               &  Primality_Test_Outcome'Image (R)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Lucas_Lehmer test");
      for I in 1..15 loop -- A reasonably large numbers
         Check
         (  Image (Power_Of_Two (Mersenne_Powers (I)) - 1),
            Prime
         );
      end loop;
      Check (Image (Power_Of_Two (16) - One), Composite);
      Check (Image (Power_Of_Two (32) - One), Composite);
      Check (Image (Power_Of_Two (64) - One), Composite);
      Check ("6", Composite);
      Check ("5", Undecided);
      Check (Image (Power_Of_Two (64)), Composite);
      Check ("2047", Composite);
      Check ("8388607", Composite);
      Check ("536870911", Composite);
      Check ("137438953471", Composite);
      Check ("2199023255551", Composite);
      Check ("8796093022207", Composite);
      Check ("140737488355327", Composite);
      Check ("147573952589676412927", Composite);
   end;
   declare
      procedure Check (N : String; R : Primality_Test_Outcome) is
         Domain : constant Montgomery_Domain := Create (Value (N));
         Result : constant Primality_Test_Outcome :=
                           Lucas_Lehmer (Domain);
      begin
         if Result /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Montgomery Lucas-Lehmer "
               &  N
               &  " is "
               &  Primality_Test_Outcome'Image (Result)
               &  " /= "
               &  Primality_Test_Outcome'Image (R)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Lucas_Lehmer test");
      for I in 1..15 loop -- A reasonably large numbers
         Check
         (  Image (Power_Of_Two (Mersenne_Powers (I)) - 1),
            Prime
         );
      end loop;
      Check (Image (Power_Of_Two (16) - One), Composite);
      Check (Image (Power_Of_Two (32) - One), Composite);
      Check (Image (Power_Of_Two (64) - One), Composite);
      Check ("5", Undecided);
      Check ("2047", Composite);
      Check ("8388607", Composite);
      Check ("536870911", Composite);
      Check ("137438953471", Composite);
      Check ("2199023255551", Composite);
      Check ("8796093022207", Composite);
      Check ("140737488355327", Composite);
      Check ("147573952589676412927", Composite);
   end;
   declare
      procedure Check (X, Y, Q : String; Base : Positive := 10) is
         Q1 : Unbounded_Unsigned;
      begin
         Q1 := Value (X, Base) / Value (Y, Base);
         if Q1 /= Value (Q, Base) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Division result "
               &  Image (Q1)
               &  " /= "
               &  Q
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("/-operator test");
      Check
      (  X => "76457688543397543207896535678900561234326478098",
         Y => "145674686357948906756438976521900875",
         Q => "524852261260"
      );
      Check
      (  X => "145674686357948906756438976521900875",
         Y => "76457688543397543207896535678900561234326478098",
         Q => "0"
      );
      Check
      (  X => "999999999999999999999999999999999999999" &
              "999999999999999999999999999999999999999" &
              "999999999999999999999999999999999999999" &
              "99999999999",
         Y => "888888888888888888888888888888888888888" &
              "888888888888888888888888888888888888888" &
              "8888888888888888888888888888888888888",
         Q => "11250000000000"
      );
   end;
   declare
      X : Unbounded_Unsigned := From_Half_Word (1);
   begin
      Put_Line ("Shift_Left test");
      Shift_Left (X, 1);
      if Word'Value (Image (X)) /= Word (Half_Word'Last) + 1 then
         Raise_Exception
            (  Data_Error'Identity,
               (  "Shift_Left (X, 1) "
               &  Image (X)
               &  " /="
               &  Word'Image (Word (Half_Word'Last) + 1)
               &  " (expected)"
            )  );
      end if;
   end;
   declare
      procedure Check
                (  X, Y : String;
                   P    : Bit_Count;
                   Base : Integer := 10
                )  is
         V : Unbounded_Unsigned;
      begin
         V := Value (X, Base);
         Mul_By_Power_of_Two (V, P);
         if V /= Value (Y, Base) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "shift "
               &  Image (V, Base)
               &  " /= "
               &  Y
               &  " Power ="
               &  Bit_Count'Image (P)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Mul_By_Power_of_Two test");
      Check
      (  X => "E96686B6EC95D6AD0000000000000004",
         Y => "74B3435B764AEB56800000000000000200000000000000000",
         P => 67,
         Base => 16
      );
      Check
      (  X => "E96686B6EC95D6AD0000000000000004",
         Y => "E96686B6EC95D6AD00000000000000040000000000000000",
         P => 64,
         Base => 16
      );
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
      Check
      (  X => "1",
         Y => "18446744073709551616",
         P => 64
      );
      Check
      (  X => "1",
         Y => "9223372036854775808",
         P => 63
      );
   end;
   declare
      procedure Check
                (  X    : String;
                   P    : Digit_Offset;
                   Base : Integer := 10
                )  is
         R_1, R_2 : Unbounded_Unsigned;
      begin
         R_1 := Value (X, Base);
         R_2 := R_1;
         Shift_Right (R_1, P);
         Div_By_Power_Of_Two (R_2, Bit_Count (P) * Half_Word'Size);
         if R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "shift right "
               &  Image (R_1, Base)
               &  " /= "
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Div_By_Power_Of_Two test 1");
      Check
      (  X     => "E96686B6EC95D6AD0000000000000004",
         P    => 1,
         Base => 16
      );
      Check
      (  X    => "E96686B6EC95D6AD0000000000000004",
         P    => 0,
         Base => 16
      );
      Check
      (  X => "170141183460469231731687303715884105728",
         P => 1
      );
      Check
      (  X => "170141183460469231731687303715884105728",
         P => 2
      );
   end;
   declare
      procedure Check
                (  X    : String;
                   P    : Bit_Count;
                   R    : String;
                   Base : Positive := 10
                )  is
         R1 : Unbounded_Unsigned := Value (X, Base);
      begin
         Div_By_Power_of_Two (R1, P);
         if R1 /= Value (R, Base) then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " / 2 **"
               &  Bit_Count'Image (P)
               &  " = "
               &  Image (R1, Base)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Div_By_Power_of_Two test 2");
      Check
      (  X => "ABE3B536BB85A620" &
              "ABE3B536BB85A620",
         P => 64,
         R => "ABE3B536BB85A620",
         Base => 16
      );
      Check
      (  X => "456734567798706546565645367888991111124445325",
         P => 148,
         R => "1"
      );
      Check
      (  X => "456734567798706546565645367888991111124445325",
         P => 144,
         R => "20"
      );
      Check
      (  X => "456734567798706546565645367888991111124445325",
         P => 150,
         R => "0"
      );
      Check
      (  X => "456734567798706546565645367888991111124445325",
         P => 132,
         R => "83888"
      );
      Check
      (  X => "456734567798706546565645367888991111124445325",
         P => 129,
         R => "671111"
      );
      Check
      (  X => "456734567798706546565645367888991111124445325",
         P => 128,
         R => "1342222"
      );
      Check
      (  X => "456734567798706546565645367888991111124445325",
         P => 127,
         R => "2684444"
      );
      Check
      (  X => "456734567798706546565645367888991111124445325",
         P => 100,
         R => "360300044599415"
      );
      Check
      (  X => "456734567798706546565645367888991111124445325",
         P => 64,
         R => "24759630532829277720392072"
      );
      Check
      (  X => "456734567798706546565645367888991111124445325",
         P => 63,
         R => "49519261065658555440784144"
      );
      Check
      (  X => "456734567798706546565645367888991111124445325",
         P => 3,
         R => "57091820974838318320705670986123888890555665"
      );
      Check
      (  X => "456734567798706546565645367888991111124445325",
         P => 2,
         R => "114183641949676636641411341972247777781111331"
      );
      Check
      (  X => "456734567798706546565645367888991111124445325",
         P => 1,
         R => "228367283899353273282822683944495555562222662"
      );
      Check
      (  X => "18446744073709551616",
         P => 65,
         R => "0"
      );
      Check
      (  X => "18446744073709551616",
         P => 64,
         R => "1"
      );
      Check
      (  X => "18446744073709551616",
         P => 63,
         R => "2"
      );
      Check
      (  X => "18446744073709551616",
         P => 62,
         R => "4"
      );
      Check
      (  X => "18446744073709551616",
         P => 60,
         R => "16"
      );
      Check
      (  X => "18446744073709551616",
         P => 3,
         R => "2305843009213693952"
      );
      Check
      (  X => "18446744073709551616",
         P => 2,
         R => "4611686018427387904"
      );
      Check
      (  X => "18446744073709551616",
         P => 1,
         R => "9223372036854775808"
      );
      Check
      (  X => "456734567798706546565645367888991111124445325",
         P => 0,
         R => "456734567798706546565645367888991111124445325"
      );
      Check (X => "1",  P => 1, R => "0");
      Check (X => "2",  P => 1, R => "1");
      Check (X => "0",  P => 2, R => "0");
      Check (X => "1",  P => 0, R => "1");
      Check (X => "2",  P => 0, R => "2");
      Check (X => "2",  P => 2, R => "0");
      Check (X => "3",  P => 0, R => "3");
      Check (X => "3",  P => 1, R => "1");
      Check (X => "3",  P => 2, R => "0");
      Check (X => "63", P => 2, R => "15");
      Check (X => "64", P => 2, R => "16");
   end;
   declare
      procedure Check (X, Y : String; P : Digit_Offset) is
         R_1 : Unbounded_Unsigned;
         R_2 : Unbounded_Unsigned;
      begin
         R_1 := Value (X);
         Add (R_1, Value (Y), P);
         R_2 := Value (Y);
         Shift_Left (R_2, P);
         Add (R_2, Value (X));
         if R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "X + Y * R**N "
               &  Image (R_1)
               &  " /= "
               &  Image (R_2)
               &  " Power ="
               &  Digit_Offset'Image (P)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Add + shift test");
      Check
      (  X => "76457688543397543207896535678900561234326478098",
         Y => "145674686357948906756438976521900875",
         P => 1
      );
      Check
      (  X => "340282366920938463463374607431768211455",
         Y => "18446744073709551615",
         P => 1
      );
      Check (X => "1", Y => "1", P => 1);
      Check (X => "14485", Y => "6667", P => 2);
      Check
      (  X => "145674686357948906756438976521900875",
         Y => "76457688543397543207896535678900561234326478098",
         P => 8
      );
      Check
      (  X => "99999999999999999999999999999999999999999999999999999" &
              "99999999999999999999999999999999999999999999999999999" &
              "99999999999999999999999",
         Y => "88888888888888888888888888888888888888888888888888888" &
              "88888888888888888888888888888888888888888888888888888" &
              "8888888888888888888888888888888888888",
         P => 17
      );
   end;
   declare
      procedure Check (X, Y, R : String) is
         R1 : constant Unbounded_Unsigned :=
                       Mul_Classroom (Value (X), Value (Y));
      begin
         if R1 /= Value (R) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Classroom multiplication result "
               &  Image (R1)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Mul_Classroom test");
      Check
      (  X => "57896044618658097711785492504343953926634992332820282" &
              "019728792003956564819968",
         Y => "57896044618658097711785492504343953926634992332820282" &
              "019728792003956564819968",
         R => "33519519824856492748935062495514615318698414551480983" &
              "44430890360930441007518386744200468574541725856922507" &
              "964546621512713438470702986642486608412251521024"
      );
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
      procedure Check (X, Y : String) is
         R1 : constant Unbounded_Unsigned :=
                       Mul_Karatsuba (Value (X), Value (Y), 1);
         R2 : constant Unbounded_Unsigned :=
                       Mul_Classroom (Value (X), Value (Y));
      begin
         if R1 /= R2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Karatsuba multiplication result "
               &  Image (R1)
               &  " /= "
               &  Image (R2)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Mul_Karatsuba test");
      Check
      (  X => "99999999999999999999999999999999999999999999999999999" &
              "99999999999999999999999999999999999999999999999999999" &
              "99999999999999999999999",
         Y => "88888888888888888888888888888888888888888888888888888" &
              "88888888888888888888888888888888888888888888888888888" &
              "8888888888888888888888888888888888888"
      );
      Check
      (  X => "31385508676933403819178947116038332080511777222320172" &
              "56448",
         Y => "170141183460469231740910675752738881536"
      );
      Check
      (  X => "76457688543397543207896535678900561234326478098",
         Y => "145674686357948906756438976521900875"
      );
      Check
      (  X => "31385508676933403819178947116038332080511777222320172" &
              "56448",
         Y => "170141183460469231731687303715884105728"
      );
      Check
      (  X => "170141183460469231731687303715884105728",
         Y => "9223372036854775808"
      );
      Check
      (  X => "170141183460469231731687303715884105728",
         Y => "170141183460469231731687303715884105728"
      );
      Check (X => "1", Y => "1");
      Check (X => "14485", Y => "6667");
      Check
      (  X => "18446744073709551616",
         Y => "18446744073709551616"
      );
      Check
      (  X => "145674686357948906756438976521900875",
         Y => "76457688543397543207896535678900561234326478098"
      );
   end;
   declare
      Server : Job_Server (6); -- Job server with 6 workers

      procedure Check (X, Y : String) is
         Left  : constant Unbounded_Unsigned := Value (X);
         Right : constant Unbounded_Unsigned := Value (Y);
         R_1   : Unbounded_Unsigned;
         R_2   : constant Unbounded_Unsigned :=
                    Mul_Classroom (Left, Right);
      begin
         Mul_Karatsuba (Left, Right, Server, 1, R_1);
         if R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Parallel Karatsuba multiplication result "
               &  Image (R_1)
               &  " /= "
               &  Image (R_2)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Parallel Mul_Karatsuba test");
      Check
      (  X => "99999999999999999999999999999999999999999999999999999" &
              "99999999999999999999999999999999999999999999999999999" &
              "99999999999999999999999",
         Y => "88888888888888888888888888888888888888888888888888888" &
              "88888888888888888888888888888888888888888888888888888" &
              "8888888888888888888888888888888888888"
      );
      Check
      (  X => "31385508676933403819178947116038332080511777222320172" &
              "56448",
         Y => "170141183460469231740910675752738881536"
      );
      Check
      (  X => "76457688543397543207896535678900561234326478098",
         Y => "145674686357948906756438976521900875"
      );
      Check
      (  X => "31385508676933403819178947116038332080511777222320172" &
              "56448",
         Y => "170141183460469231731687303715884105728"
      );
      Check
      (  X => "170141183460469231731687303715884105728",
         Y => "9223372036854775808"
      );
      Check
      (  X => "170141183460469231731687303715884105728",
         Y => "170141183460469231731687303715884105728"
      );
      Check (X => "1", Y => "1");
      Check (X => "14485", Y => "6667");
      Check
      (  X => "18446744073709551616",
         Y => "18446744073709551616"
      );
      Check
      (  X => "145674686357948906756438976521900875",
         Y => "76457688543397543207896535678900561234326478098"
      );
   end;
   declare
      Server : Job_Server (6); -- Job server with 6 workers

      procedure Check (X, Y : String; Base : Positive := 10) is
         Left  : constant Unbounded_Unsigned := Value (X, Base);
         Right : constant Unbounded_Unsigned := Value (Y, Base);
         Cache : Unbounded_Unsigned_Array (1..3);
         R_1   : Unbounded_Unsigned;
         R_2   : constant Unbounded_Unsigned :=
                    Mul_Classroom (Left, Right);
      begin
         Mul (Left, Right, Server, Cache, 1, R_1);
         if R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Parallel multiplication result "
               &  Image (R_1)
               &  " /= "
               &  Image (R_2)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Parallel Mul test");
      Check
      (  X => "005E471D635E17F8" &
              "E0F3F3000794E16C" &
              "65A601B241F01722" &
              "8D9F1A2EE90E0F4E" &
              "43B2E44A937B9B2A" &
              "63998B5846D14646" &
              "8C830DD33517133A" &
              "FDA58C5AB311562E" &
              "E2D7650B85A88EEA" &
              "E90F13639BADADEC" &
              "87104ABD28A3C3B2" &
              "23CBC6D3F3C53BE9" &
              "C085F7BFBE5234B0" &
              "526B42F156A5E21E" &
              "1784DB85294F5641" &
              "0E75C55847DDB515" &
              "5A95E85B0DCF2F79" &
              "9DB1644BBB8F2F29" &
              "111AA517B59444D6" &
              "46A3DAF6E46E64DC",
         Y => "54E022D3E733FA09" &
              "DA23EE99F0426D39" &
              "97F38EB8099B59AF" &
              "3AFD02D51F307061" &
              "F8E1655731C58996" &
              "C948B74EACCC1145" &
              "3F9B8C0B6EDA40A0" &
              "D61AF094DCF8FD90" &
              "FB84F24B4D7F689C" &
              "88C6D832044730B8" &
              "F27A700F1F185D8B" &
              "2129ABDD047EFC2D" &
              "7CB91D54CE26361F" &
              "A25B4E5AB024587A" &
              "7F99F5EA6589047B" &
              "9E841AD79C193C3F",
         Base => 16
      );
      Check
      (  X => "6CDA768965435940" &
              "1A45B5DEC4646488" &
              "8DA9658AAD044C79" &
              "A25E7408D619CB05" &
              "6D83689EB9A90A7A" &
              "F03018C636CD6369" &
              "EDE88E315D6B39F1" &
              "3D9CC68A473D5D43" &
              "CAE4DEE1DC026220" &
              "A41AD736AC4BE866" &
              "70CBA1352916F42E" &
              "254ADB273120B8B2" &
              "20E894905F5991D0" &
              "C1FB195BE77F8A13" &
              "2F72810E9A01F8DB",
         Y => "6CDA768965435940" &
              "1A45B5DEC4646488" &
              "8DA9658AAD044C79" &
              "A25E7408D619CB05" &
              "6D83689EB9A90A7A" &
              "F03018C636CD6369" &
              "EDE88E315D6B39F1" &
              "3D9CC68A473D5D43" &
              "CAE4DEE1DC026220" &
              "A41AD736AC4BE866" &
              "70CBA1352916F42E" &
              "254ADB273120B8B2" &
              "20E894905F5991D0" &
              "C1FB195BE77F8A13" &
              "2F72810E9A01F8DB" &
              "F8A9C0B79ADE1B3D",
         Base => 16
      );
      Check
      (  X => "1000000000000000" &
              "0100000000000000" &
              "0010000000000000" &
              "0001000000000000" &
              "0000100000000000",
         Y => "0000000000000001" &
              "0000000000000000",
         Base => 16
      );
      Check
      (  X => "99999999999999999999999999999999999999999999999999999" &
              "99999999999999999999999999999999999999999999999999999" &
              "99999999999999999999999",
         Y => "88888888888888888888888888888888888888888888888888888" &
              "88888888888888888888888888888888888888888888888888888" &
              "8888888888888888888888888888888888888"
      );
      Check
      (  X => "31385508676933403819178947116038332080511777222320172" &
              "56448",
         Y => "170141183460469231740910675752738881536"
      );
      Check
      (  X => "76457688543397543207896535678900561234326478098",
         Y => "145674686357948906756438976521900875"
      );
      Check
      (  X => "31385508676933403819178947116038332080511777222320172" &
              "56448",
         Y => "170141183460469231731687303715884105728"
      );
      Check
      (  X => "170141183460469231731687303715884105728",
         Y => "9223372036854775808"
      );
      Check
      (  X => "170141183460469231731687303715884105728",
         Y => "170141183460469231731687303715884105728"
      );
      Check (X => "1", Y => "1");
      Check (X => "14485", Y => "6667");
      Check
      (  X => "18446744073709551616",
         Y => "18446744073709551616"
      );
      Check
      (  X => "145674686357948906756438976521900875",
         Y => "76457688543397543207896535678900561234326478098"
      );
   end;
   declare
      procedure Check (X : String; Y : Bit_Count; R : String) is
         R1 : constant Unbounded_Unsigned := Value (X) ** Y;
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
      Put_Line ("**-operator test");
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
      procedure Check (X, Y, R : String; Base : Positive := 10) is
         R1 : Unbounded_Unsigned;
      begin
         R1 := Value (X, Base) mod Value (Y, Base);
         if R1 /= Value (R, Base) then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " mod "
               &  Y
               &  " is "
               &  Image (R1, Base)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("mod-operator test");
      Check
      (  X => "999999999999999999999999999999999999999" &
              "999999999999999999999999999999999999999" &
              "999999999999999999999999999999999999999" &
              "99999999999",
         Y => "888888888888888888888888888888888888888" &
              "888888888888888888888888888888888888888" &
              "8888888888888888888888888888888888888",
         R => "9999999999999"
      );
      Check
      (  X => "145674686357948906756438976521900875",
         Y => "76457688543397543207896535678900561234326478098",
         R => "145674686357948906756438976521900875"
      );
      Check
      (  X => "76457688543397543207896535678900561234326478098",
         Y => "145674686357948906756438976521900875",
         R => "86785721234646786180837077003875598"
      );
      Check
      (  X => "313855086769334038191789471",
         Y => "13",
         R => "10"
      );
   end;
   declare
      procedure Check (X, Y, R : String) is
         R1 : constant Unbounded_Unsigned :=
                 Greatest_Common_Divisor (Value (X), Value (Y));
         R2 : constant Unbounded_Unsigned :=
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
      Put_Line ("Greatest_Common_Divisor test");
      Check
      (  X => "3138550867693340381888917894716767200",
         Y => "45673289045999998889999999768888",
         R => "8"
      );
      Check
      (  X => "48",
         Y => "18",
         R => "6"
      );
      Check
      (  X => "313855086769334038191789471",
         Y => "13",
         R => "1"
      );
   end;
   declare
      procedure Check (X : Bit_Count; R : String) is
         R1 : constant Unbounded_Unsigned := Power_of_Two (X);
      begin
         if R1 /= Value (R) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "2 **"
               &  Bit_Count'Image (X)
               &  " = "
               &  Image (R1)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Power_of_Two test");
      Check (X => 0,  R => "1");
      Check (X => 1,  R => "2");
      Check (X => 2,  R => "4");
      Check (X => 3,  R => "8");
      Check (X => 63, R => "9223372036854775808");
      Check (X => 64, R => "18446744073709551616");
   end;
   declare
      procedure Check
                (  X        : String;
                   Position : Bit_Position;
                   R        : Boolean;
                   Base     : NumberBase := 10
                )  is
         R1 : constant Boolean := Get_Bit (Value (X, Base), Position);
      begin
         if R1 /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Get bit "
               &  X
               &  ","
               &  Bit_Position'Image (Position)
               &  " = "
               &  Boolean'Image (R1)
               &  " /= "
               &  Boolean'Image (R)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Get_Bit test");
      Check (X => "0", Position => 0 + 1, R => False);
      Check (X => "1", Position => 0 + 1, R => True);
      Check (X => "2", Position => 0 + 1, R => False);
      Check (X => "3", Position => 0 + 1, R => True);
      Check (X => "3", Position => 1 + 1, R => True);
      Check (X => "4", Position => 2 + 1, R => True);
      Check
      (  X        => "1024",
         Position => 10 + 1,
         R        => True
      );
      Check
      (  X        => "65536",
         Position => 16 + 1,
         R        => True
      );
      Check
      (  X        => "4294967296",
         Position => 32 + 1,
         R        => True
      );
      Check
      (  X        => "4611686018427387904",
         Position => 62 + 1,
         R        => True
      );
      Check
      (  X        => "9223372036854775808",
         Position => 62 + 1,
         R        => False
      );
      Check
      (  X        => "9223372036854775808",
         Position => 63 + 1,
         R        => True
      );
      Check
      (  X        => "9223372036854775808",
         Position => 64 + 1,
         R        => False
      );
      Check
      (  X        => "18446744073709551616",
         Position => 64 + 1,
         R        => True
      );
      Check
      (  X        => "18446744073709551616",
         Position => 65 + 1,
         R        => False
      );
      Check
      (  X        => "36893488147419103232",
         Position => 65 + 1,
         R        => True
      );
      Check
      (  X        => "170141183460469231731687303715884105728",
         Position => 127 + 1,
         R        => True
      );
      Check
      (  X        => "170141183460469231731687303715884105728",
         Position => 128 + 1,
         R        => False
      );
      Check
      (  X        => "340282366920938463463374607431768211456",
         Position => 128 + 1,
         R        => True
      );
   end;
   declare
      procedure Check
                (  X, Y     : String;
                   Position : Bit_Position;
                   Base     : NumberBase := 10
                )  is
         R : Unbounded_Unsigned := Value (X, Base);
      begin
         Set_Bit (R, Position);
         if R /= Value (Y, Base) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Set bit "
               &  X
               &  ","
               &  Bit_Position'Image (Position)
               &  " = "
               &  Image (R, Base)
               &  " /= "
               &  Y
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Set_Bit test");
      Check
      (  X => "0",
         Y => "2",
         Position => 2,
         Base => 16
      );
      Check
      (  X => "2",
         Y => "2",
         Position => 2,
         Base => 16
      );
      Check
      (  X => "8",
         Y => "10",
         Position => 2
      );
      Check
      (  X => "0",
         Y => "0000000000000001" &
              "0000000000000000",
         Position => 65,
         Base => 16
      );
   end;
   declare
      procedure Check
                (  X, Y     : String;
                   Position : Bit_Position;
                   Base     : NumberBase := 10
                )  is
         R : Unbounded_Unsigned := Value (X, Base);
      begin
         Clear_Bit (R, Position);
         if R /= Value (Y, Base) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Set bit "
               &  X
               &  ","
               &  Bit_Position'Image (Position)
               &  " = "
               &  Image (R, Base)
               &  " /= "
               &  Y
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Clear_Bit test");
      Check
      (  X => "0",
         Y => "0",
         Position => 2,
         Base => 16
      );
      Check
      (  X => "2",
         Y => "0",
         Position => 2,
         Base => 16
      );
      Check
      (  X => "FFFFFFFFFFFFFFFF" &
              "FFFFFFFFFFFFFFFF",
         Y => "FFFFFFFFFFFFFFFE" &
              "FFFFFFFFFFFFFFFF",
         Position => 65,
         Base => 16
      );
   end;
   declare
      procedure Check
                (  X        : String;
                   From, To : Bit_Position;
                   R        : String;
                   Base     : NumberBase := 10
                )  is
         R1 : constant Unbounded_Unsigned :=
                       Get_Slice (Value (X, Base), From, To);
      begin
         if R1 /= Value (R, Base) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "("
               &  Bit_Position'Image (From)
               &  " .."
               &  Bit_Position'Image (To)
               &  ") = "
               &  Image (R1, Base)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Get_Slice test");
      Check
      (  "ABE3B536BB85A620" &
         "22C23FD2E6550130",
         65,
         128,
         "ABE3B536BB85A620",
         16
      );
      Check
      (  "ABE3B536BB85A620" &
         "22C23FD2E6550130" &
         "3500D11B596ABE52" &
         "FA83E73D4571125D" &
         "4F1E30CECD25368C" &
         "4288BA7723D181F8" &
         "8F50DC218DF35944" &
         "5E2F302FB0AA44FA" &
         "CB1089939ECB1B1F" &
         "ADAE92F7C5C37657" &
         "D7F7FBDEF6DDC4C3" &
         "186A193AE10E7E40" &
         "64F041FE86FBB37F" &
         "3DB919BE387F4896" &
         "FB9F0C5ADF4499E9" &
         "4330EC3E6F1F711F",
         513,
         1024,
         "ABE3B536BB85A620" &
         "22C23FD2E6550130" &
         "3500D11B596ABE52" &
         "FA83E73D4571125D" &
         "4F1E30CECD25368C" &
         "4288BA7723D181F8" &
         "8F50DC218DF35944" &
         "5E2F302FB0AA44FA",
         16
      );
      Check
      (  X    => "800000000000000000000000000000305",
         From => 67,
         To   => 132,
         R    => "20000000000000000",
         Base => 16
      );
      Check
      (  X    => "800000000000000000000000000000305",
         From => 1,
         To   => 33,
         R    => "305",
         Base => 16
      );
      Check
      (  X    => "800000000000000000000000000000305",
         From => 34,
         To   => 66,
         R    => "0",
         Base => 16
      );
      Check
      (  X    => "682",
         From => 1,
         To   => 1,
         R    => "0"
      );
      Check
      (  X    => "101010101010101010101010101010101010101010101010101" &
                 "010101010101010101010101010101010101010101010101010" &
                 "10101010101010101010",
         From => 2,
         To   => 102,
         R    => "101010101010101010101010101010101010101010101010101" &
                 "01010101010101010101010101010101010101010101010101",
         Base => 2
      );
      Check
      (  X    => "101010101010101010101010101010101010101010101010101" &
                 "010101010101010101010101010101010101010101010101010" &
                 "101010101010101010",
         From => 2,
         To   => 101,
         R    => "101010101010101010101010101010101010101010101010101" &
                 "010101010101010101010101010101010101010101010101",
         Base => 2
      );
      Check
      (  X    => "101010101010101010101010101010101010101010101010101" &
                 "010101010101010101010101010101010101010101010101010" &
                 "101010101010101010",
         From => 2,
         To   => 100,
         R    => "101010101010101010101010101010101010101010101010101" &
                 "010101010101010101010101010101010101010101010101",
         Base => 2
      );
      Check
      (  X    => "101010101010101010101010101010101010101010101010101" &
                 "010101010101010101010101010101010101010101010101010" &
                 "101010101010101010",
         From => 2,
         To   => 64,
         R    => "101010101010101010101010101010101010101010101010101" &
                 "010101010101",
         Base => 2
      );
      Check
      (  X    => "101010101010101010101010101010101010101010101010101" &
                 "010101010101010101010101010101010101010101010101010" &
                 "101010101010101010",
         From => 1,
         To   => 63,
         R    => "0101010101010101010101010101010101010101010101010101" &
                 "01010101010",
         Base => 2
      );
      Check
      (  X    => "101010101010101010101010101010101010101010101010101" &
                 "0101010101010",
         From => 60,
         To   => 63,
         R    => "101",
         Base => 2
      );
      Check
      (  X    => "101010101010101010101010101010101010101010101010101" &
                 "0101010101010",
         From => 61,
         To   => 61,
         R    => "0",
         Base => 2
      );
      Check
      (  X    => "101010101010101010101010101010101010101010101010101" &
                 "0101010101010",
         From => 63,
         To   => 63,
         R    => "0",
         Base => 2
      );
      Check
      (  X    => "101010101010101010101010101010101010101010101010101" &
                 "0101010101010",
         From => 3,
         To   => 4,
         R    => "10",
         Base => 2
      );
      Check
      (  X    => "1010101010",
         From => 3,
         To   => 4,
         R    => "10",
         Base => 2
      );
      Check
      (  X    => "1010101010",
         From => 2,
         To   => 4,
         R    => "101",
         Base => 2
      );
      Check
      (  X    => "1010101010",
         From => 2,
         To   => 3,
         R    => "1",
         Base => 2
      );
      Check
      (  X    => "1010101010",
         From => 2,
         To   => 2,
         R    => "1",
         Base => 2
      );
      Check
      (  X    => "1010101010",
         From => 1,
         To   => 9,
         R    => "10101010",
         Base => 2
      );
      Check
      (  X    => "1010101010",
         From => 1,
         To   => 65,
         R    => "1010101010",
         Base => 2
      );
      Check
      (  X    => "1010101010",
         From => 1,
         To   => 64,
         R    => "1010101010",
         Base => 2
      );
      Check
      (  X    => "1010101010",
         From => 1,
         To   => 11,
         R    => "1010101010",
         Base => 2
      );
      Check
      (  X    => "1010101010",
         From => 1,
         To   => 10,
         R    => "1010101010",
         Base => 2
      );
      Check
      (  X    => "1010101010",
         From => 1,
         To   => 4,
         R    => "1010",
         Base => 2
      );
      Check
      (  X    => "1010101010",
         From => 1,
         To   => 3,
         R    => "10",
         Base => 2
      );
      Check
      (  X    => "1010101010",
         From => 1,
         To   => 2,
         R    => "10",
         Base => 2
      );
      Check
      (  X    => "682",
         From => 10,
         To   => 1,
         R    => "0"
      );
   end;
   declare
      procedure Check (X : String; Base : Positive := 10) is
         V  : constant Unbounded_Unsigned := Value (X, Base);
         R1 : Unbounded_Unsigned;
         R2 : Unbounded_Unsigned;
      begin
         Sqrt (V, R1, R2);
         if R1 * R1 > V then
            Raise_Exception
            (  Data_Error'Identity,
               (  "sqrt "
               &  Image (R1)
               &  " too large, "
               &  Image (R1 * R1)
               &  " > "
               &  X
               &  " (expected)"
            )  );
         elsif (R1 + One) * (R1 + One) <= V then
            Raise_Exception
            (  Data_Error'Identity,
               (  "sqrt "
               &  Image (R1)
               &  " too small, "
               &  Image ((R1 + One) * (R1 + One))
               &  " > "
               &  X
               &  " (expected)"
            )  );
         elsif V - R1 * R1 /= R2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "sqrt "
               &  X
               &  " actual remainder = "
               &  Image (V - R1 * R1)
               &  " /= "
               &  Image (R2)
               &  " (obtained)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Sqrt test");
      Check
      (  "ABE3B536BB85A620" &
         "22C23FD2E6550130" &
         "3500D11B596ABE52" &
         "FA83E73D4571125D" &
         "4F1E30CECD25368C" &
         "4288BA7723D181F8" &
         "8F50DC218DF35944" &
         "5E2F302FB0AA44FA" &
         "CB1089939ECB1B1F" &
         "ADAE92F7C5C37657" &
         "D7F7FBDEF6DDC4C3" &
         "186A193AE10E7E40" &
         "64F041FE86FBB37F" &
         "3DB919BE387F4896" &
         "FB9F0C5ADF4499E9" &
         "4330EC3E6F1F711F",
         16
      );
      Check
      (  "1820000000000000000000000000000000000000000000" &
         "000000400000000000000",
         16
      );
      Check ("773");
      Check ("2722258935367507707706996859454145692421");
      for I in Half_Word range 0..256 loop
         Check (Trim (Half_Word'Image (I)));
      end loop;
      Check ("340282366920938463463374607431768211455");
      Check ("340282366920938463463374607431768211454");
      Check ("340282366920938463463374607431768211453");
      Check ("340282366920938463463374607431768211452");
      Check ("340282366920938463463374607431768211451");
      Check ("340282366920938463463374607431768211450");
      Check ("73786976294838206464");
      Check ("73786976294838206463");
      Check ("36893488147419103232");
      Check ("18446744073709551618");
      Check ("18446744073709551616");
      Check ("18446744073709551617");
      Check ("4294967296");
      Check ("4294967300");
   end;
   declare
      procedure Check (X : String) is
         V : constant Unbounded_Unsigned := Value (X);
         R : constant Unbounded_Unsigned := Square_Classroom (V);
      begin
         if R /= V * V then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Square classroom "
               &  X
               &  " is "
               &  Image (R)
               &  " /= "
               &  Image (V * V)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Square_Classroom test");
      Check ("0");
      Check ("1");
      Check ("3");
      Check ("2147483648");
      Check ("340282366920938463463374607431768211455");
      Check ("170141183460469231731687303715884105728");
      Check ("4611686018427387904");
      Check ("1125899906842624");
      Check ("4294967296");
   end;
   declare
      procedure Check (X : String) is
         V : constant Unbounded_Unsigned := Value (X);
         R : constant Unbounded_Unsigned := Square_Karatsuba (V, 1);
      begin
         if R /= V * V then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Square Karatsuba "
               &  X
               &  " is "
               &  Image (R)
               &  " /= "
               &  Image (V * V)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Square_Karatsuba test");
      Check ("170141183460469231731687303715884105728");
      Check ("340282366920938463463374607431768211455");
      Check
      (  "99999999999999999999999999999999999999999999999999999999999"
      &  "99999999999999999999999999999999999999999999999999999999999"
      &  "99999999999999999999999999999999999999999999999999999999999"
      &  "99999999999"
      );
   end;
   declare
      Server : Job_Server (6);

      procedure Check (X : String) is
         V : constant Unbounded_Unsigned := Value (X);
         R : Unbounded_Unsigned;
      begin
         Square_Karatsuba (V, Server, 1, R);
         if R /= V * V then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Parallel square Karatsuba "
               &  X
               &  " is "
               &  Image (R)
               &  " /= "
               &  Image (V * V)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Parallel Square_Karatsuba test");
      Check ("170141183460469231731687303715884105728");
      Check ("340282366920938463463374607431768211455");
      Check
      (  "99999999999999999999999999999999999999999999999999999999999"
      &  "99999999999999999999999999999999999999999999999999999999999"
      &  "99999999999999999999999999999999999999999999999999999999999"
      &  "99999999999"
      );
   end;
   declare
      procedure Check (X, Y, M, R : String) is
         R1 : constant Unbounded_Unsigned :=
                       Mod_Pow (Value (X), Value (Y), Value (M));
      begin

         if R1 /= Value (R) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Mod_Pow "
               &  X
               &  " ** "
               &  Y
               &  " mod "
               &  M
               &  " is "
               &  Image (R1)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Mod_Pow test 1");
      Check ("4",  "123",  "1", "0");
      Check ("2",    "5", "13", "6");
      Check ("89", "100", "13", "3");
   end;
   declare
      procedure Check (X, Y : String; M : Bit_Count; R : String) is
         R1 : constant Unbounded_Unsigned :=
              Mod_Pow_By_Power_Of_Two (Value (X), Value (Y), M);
      begin

         if R1 /= Value (R) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Mod_Pow "
               &  X
               &  " ** "
               &  Y
               &  " mod 2 **"
               &  Bit_Count'Image (M)
               &  " is "
               &  Image (R1)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Mod_Pow_By_Power_Of_Two test");
      Check
      (  X => "251",
         Y => "71",
         M => 13,
         R => "6867"
      );
   end;
   declare
      procedure Check (X : String; Y : Half_Word; M, R : String) is
         R1 : constant Unbounded_Unsigned :=
                       Mod_Pow (Value (X), Y, Value (M));
      begin
         if R1 /= Value (R) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Half_Word Mod_Pow "
               &  X
               &  " **"
               &  Half_Word'Image (Y)
               &  " mod "
               &  M
               &  " is "
               &  Image (R1)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Mod_Pow test 2");
      Check ("2", 5, "13", "6");
   end;
   declare
      procedure Check
                (  X : String; P : Natural; R : Half_Word
                )  is
         Result : constant Half_Word := Value (X) mod 2 ** P;
      begin
         if Result /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  X
               &  " mod 2 **"
               &  Integer'Image (2 ** P)
               &  " is"
               &  Half_Word'Image (Result)
               &  " /="
               &  Half_Word'Image (R)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("mod 2**K test");
      Check ("17", 3, 1);
      Check ("17678231", 2, 3);
      Check ("9223372036854775808", 4,  0);
   end;
   declare
      procedure Check
                (  X, M : String; R: Integer
                )  is
         Result : constant Integer :=
                  Jacobi_Symbol (Value (X), Value (M));
      begin
         if Result /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Jacobi "
               &  X
               &  "/"
               &  M
               &  " is"
               &  Integer'Image (Result)
               &  " /= "
               &  Integer'Image (R)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Jacobi_Symbol test");
      Check (  "17",    "7", -1);
      Check (  "20",   "19",  1);
      Check (  "20",   "59",  1);
      Check (  "30",   "59", -1);
      Check (  "30",   "57",  0);
      Check ("1001", "9907", -1);
   end;
   declare
      procedure Check (N : Half_Word; E : Boolean) is
         Result : constant Boolean := Is_Prime (N) = Prime;
      begin
         if Result /= E then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Is prime"
               &  Half_Word'Image (N)
               &  " is "
               &  Boolean'Image (Result)
               &  " /= "
               &  Boolean'Image (E)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Scalar Is_Prime test");
      Check (6, False);
      Check (5, True);
      Check (4, False);
      for N in List'Range loop
         Check (Half_Word (N), List (N) = Prime);
      end loop;
      Check (12889,    True);
      Check (12888,    False);
      Check (211597,   True);
      Check (88160341, True);
   end;
   declare
      procedure Check (X, R : String; Base : Positive := 10) is
         Left : Unbounded_Unsigned := Value (X, Base);
      begin
         Truncate (Left);
         if Left /= Value (R, Base) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Truncate "
               &  X
               &  " is "
               &  Image (Left, Base)
               &  " /= "
               &  R
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Truncate test");
      Check
      (  X => "00000456689AC000" &
              "0000000000000000",
         R => "000000001159A26B",
         Base => 16
      );
      Check
      (  X => "00000000000000F2" &
              "0000000000000000",
         R => "0000000000000079",
         Base => 16
      );
      Check
      (  X => "9000000000000001" &
              "0000000000000000",
         R => "9000000000000001",
         Base => 16
      );
      Check
      (  X => "0000000000000001" &
              "0000000000000000",
         R => "1",
         Base => 16
      );
      Check
      (  X => "10",
         R => "5"
      );
      Check
      (  X => "0",
         R => "0"
      );
      Check
      (  X => "1",
         R => "1"
      );
   end;
   declare
      procedure Check
                (  Left, Right, M : String;
                   Base : Positive := 10
                )  is
         Domain  : constant Montgomery_Domain :=
                            Create (Value (M, Base));
         X, Y, R_1 : Montgomery_Number;
         R_2       : Unbounded_Unsigned;
      begin
         X := From_Unbounded_Unsigned (Value (Left,  Base));
         Y := From_Unbounded_Unsigned (Value (Right, Base));
         if To_Domain (Domain, From_Domain (Domain, X)) /= X then
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid value (Left) " & Left
            );
         end if;
         if To_Domain (Domain, From_Domain (Domain, Y)) /= Y then
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid value (Right) " & Right
            );
         end if;
         Mod_Mul_CIOS (Domain, X, Y, R_1);
         R_2 := (  (  To_Unbounded_Unsigned (X)
                   *  To_Unbounded_Unsigned (Y)
                   *  Reducer_Inverse (Domain)
                   )
                mod
                   Domain.Modulus
                );
         if To_Unbounded_Unsigned (R_1) /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Montgomery Mod_Mul_CIOS product "
               &  Image (To_Unbounded_Unsigned (R_1), Base)
               &  " /= "
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Montgomery Mod_Mul_CIOS test");
      Check
      (  Left  => "6CDA768965435940" &
                  "1A45B5DEC4646488" &
                  "8DA9658AAD044C79" &
                  "A25E7408D619CB05" &
                  "6D83689EB9A90A7A" &
                  "F03018C636CD6369" &
                  "EDE88E315D6B39F1" &
                  "3D9CC68A473D5D43" &
                  "CAE4DEE1DC026220" &
                  "A41AD736AC4BE866" &
                  "70CBA1352916F42E" &
                  "254ADB273120B8B2" &
                  "20E894905F5991D0" &
                  "C1FB195BE77F8A13" &
                  "2F72810E9A01F8DB" &
                  "F8A9C0B79ADE1B3D",
         Right => "0DDB8A96AA06B891" &
                  "49235C65CD089F6C" &
                  "2B5AA94695C90B72" &
                  "7C875C4AAC3760C",
         M     => "8661D87CDE3EE23F" &
                  "F73E18B5BE8933D2" &
                  "7B72337C70FE912C" &
                  "C9E083FD0DF766FE" &
                  "30D43275C21CFC81" &
                  "AFEFF7BDEDBB8987" &
                  "5B5D25EF8B86ECAF" &
                  "962113273D96363E" &
                  "BC5E605F615489F5" &
                  "1EA1B8431BE6B288" &
                  "851174EE47A303F0" &
                  "9E3C619D9A4A6D19" &
                  "F507CE7A8AE224BA" &
                  "6A01A236B2D57CA4" &
                  "45847FA5CCAA0261" &
                  "57C76A6D770B4C41",
         Base  => 16
      );
      Check
      (  Left  => "10",
         Right => "10",
         M     => "11"
      );
      Check
      (  Left  => "000000000000000F" & -- 2
                  "FFFFFFFFFFFFFFFF" &
                  "E000000000000001",
         Right => "000000000000000F" & -- 2
                  "FFFFFFFFFFFFFFFF" &
                  "E000000000000001",
         M     => "10000000000000000" &
                  "00000000000000001",
         Base => 16
      );
      Check
      (  Left  => "E2150A85FE767750" &
                  "324C3334A920248D",
         Right => "E2150A85FE767750" &
                  "324C3334A920248D",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "0000000000000001",
         Right => "0000000000000001",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "51",
         Right => "108",
         M     => "111"
      );
      Check
      (  Left  => "10",
         Right => "9",
         M     => "11"
      );
      Check
      (  Left  => "5",
         Right => "3",
         M     => "11"
      );
      Check
      (  Left  => "1000000000000000",
         Right => "2000000000000000",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "1000800000000000",
         Right => "2000800000000000",
         M     => "F000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "8000000000000000",
         Right => "8000000000000000",
         M     => "0000000000000001" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "8000000000000000",
         Right => "8000000000000000",
         M     => "0000000979EFD66A" &
                  "D4419169C5B34413",
         Base  => 16
      );
      Check
      (  Left  => "000000009F809326" &
                  "D2CFD366E15EF201",
         Right => "000000006BFAF030" &
                  "96C5324C4136716B",
         M     => "0000000979EFD66A" &
                  "D4419169C5B34413",
         Base  => 16
      );
      Check
      (  Left  => "540019781128412936473322405310",
         Right => "515692107665463680305819378593",
         M     => "750791094644726559640638407699"
      );
   end;
   declare
      procedure Check
                (  Left, Right, M : String;
                   Base : Positive := 10
                )  is
         Domain  : constant Montgomery_Domain :=
                            Create (Value (M, Base));
         X, Y, R_1 : Montgomery_Number;
         R_2       : Unbounded_Unsigned;
      begin
         X := From_Unbounded_Unsigned (Value (Left,  Base));
         Y := From_Unbounded_Unsigned (Value (Right, Base));
         if To_Domain (Domain, From_Domain (Domain, X)) /= X then
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid value (Left) " & Left
            );
         end if;
         if To_Domain (Domain, From_Domain (Domain, Y)) /= Y then
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid value (Right) " & Right
            );
         end if;
         R_2 := (  (  To_Unbounded_Unsigned (X)
                   *  To_Unbounded_Unsigned (Y)
                   *  Reducer_Inverse (Domain)
                   )
                mod
                   Domain.Modulus
                );
         Mod_Mul_Dusse_Kaliski (Domain, X, Y, R_1);
         if To_Unbounded_Unsigned (R_1) /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Montgomery Mod_Mul_Dusse_Kaliski product "
               &  Image (To_Unbounded_Unsigned (R_1), Base)
               &  " /= "
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Montgomery Mod_Mul_Dusse_Kaliski test");
      Check
      (  Left  => "8000000000000000",
         Right => "8000000000000000",
         M     => "0000000000000001" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "0000000000000001",
         Right => "0000000000000001",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "6CDA768965435940" &
                  "1A45B5DEC4646488" &
                  "8DA9658AAD044C79" &
                  "A25E7408D619CB05" &
                  "6D83689EB9A90A7A" &
                  "F03018C636CD6369" &
                  "EDE88E315D6B39F1" &
                  "3D9CC68A473D5D43" &
                  "CAE4DEE1DC026220" &
                  "A41AD736AC4BE866" &
                  "70CBA1352916F42E" &
                  "254ADB273120B8B2" &
                  "20E894905F5991D0" &
                  "C1FB195BE77F8A13" &
                  "2F72810E9A01F8DB" &
                  "F8A9C0B79ADE1B3D",
         Right => "0DDB8A96AA06B891" &
                  "49235C65CD089F6C" &
                  "2B5AA94695C90B72" &
                  "7C875C4AAC3760C",
         M     => "8661D87CDE3EE23F" &
                  "F73E18B5BE8933D2" &
                  "7B72337C70FE912C" &
                  "C9E083FD0DF766FE" &
                  "30D43275C21CFC81" &
                  "AFEFF7BDEDBB8987" &
                  "5B5D25EF8B86ECAF" &
                  "962113273D96363E" &
                  "BC5E605F615489F5" &
                  "1EA1B8431BE6B288" &
                  "851174EE47A303F0" &
                  "9E3C619D9A4A6D19" &
                  "F507CE7A8AE224BA" &
                  "6A01A236B2D57CA4" &
                  "45847FA5CCAA0261" &
                  "57C76A6D770B4C41",
         Base  => 16
      );
      Check
      (  Left  => "10",
         Right => "10",
         M     => "11"
      );
      Check
      (  Left  => "000000000000000F" & -- 2
                  "FFFFFFFFFFFFFFFF" &
                  "E000000000000001",
         Right => "000000000000000F" & -- 2
                  "FFFFFFFFFFFFFFFF" &
                  "E000000000000001",
         M     => "10000000000000000" &
                  "00000000000000001",
         Base => 16
      );
      Check
      (  Left  => "E2150A85FE767750" &
                  "324C3334A920248D",
         Right => "E2150A85FE767750" &
                  "324C3334A920248D",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "51",
         Right => "108",
         M     => "111"
      );
      Check
      (  Left  => "10",
         Right => "9",
         M     => "11"
      );
      Check
      (  Left  => "5",
         Right => "3",
         M     => "11"
      );
      Check
      (  Left  => "1000000000000000",
         Right => "2000000000000000",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "1000800000000000",
         Right => "2000800000000000",
         M     => "F000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "8000000000000000",
         Right => "8000000000000000",
         M     => "0000000979EFD66A" &
                  "D4419169C5B34413",
         Base  => 16
      );
      Check
      (  Left  => "000000009F809326" &
                  "D2CFD366E15EF201",
         Right => "000000006BFAF030" &
                  "96C5324C4136716B",
         M     => "0000000979EFD66A" &
                  "D4419169C5B34413",
         Base  => 16
      );
      Check
      (  Left  => "540019781128412936473322405310",
         Right => "515692107665463680305819378593",
         M     => "750791094644726559640638407699"
      );
   end;
   declare
      procedure Check
                (  Left, Right, M : String;
                   Base : Positive := 10
                )  is
         Server : Job_Server (6);
         Cache  : Unbounded_Unsigned_Array (1..4);
         Domain : constant Montgomery_Domain :=
                           Create (Value (M, Base));
         X, Y, R_1 : Montgomery_Number;
         R_2       : Unbounded_Unsigned;
      begin
         X := From_Unbounded_Unsigned (Value (Left,  Base));
         Y := From_Unbounded_Unsigned (Value (Right, Base));
         Mod_Mul (Domain, X, Y, Server, Cache, 1, R_1);
         R_2 := (  (  To_Unbounded_Unsigned (X)
                   *  To_Unbounded_Unsigned (Y)
                   *  Reducer_Inverse (Domain)
                   )
                mod
                   Domain.Modulus
                );
         if To_Unbounded_Unsigned (R_1) /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Parallel Montgomery Mod_Mul product "
               &  Image (To_Unbounded_Unsigned (R_1), Base)
               &  " /= "
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Parallel Montgomery Mod_Mul test");
      Check
      (  Left  => "6CDA768965435940" &
                  "1A45B5DEC4646488" &
                  "8DA9658AAD044C79" &
                  "A25E7408D619CB05" &
                  "6D83689EB9A90A7A" &
                  "F03018C636CD6369" &
                  "EDE88E315D6B39F1" &
                  "3D9CC68A473D5D43" &
                  "CAE4DEE1DC026220" &
                  "A41AD736AC4BE866" &
                  "70CBA1352916F42E" &
                  "254ADB273120B8B2" &
                  "20E894905F5991D0" &
                  "C1FB195BE77F8A13" &
                  "2F72810E9A01F8DB" &
                  "F8A9C0B79ADE1B3D",
         Right => "0DDB8A96AA06B891" &
                  "49235C65CD089F6C" &
                  "2B5AA94695C90B72" &
                  "7C875C4AAC3760C",
         M     => "8661D87CDE3EE23F" &
                  "F73E18B5BE8933D2" &
                  "7B72337C70FE912C" &
                  "C9E083FD0DF766FE" &
                  "30D43275C21CFC81" &
                  "AFEFF7BDEDBB8987" &
                  "5B5D25EF8B86ECAF" &
                  "962113273D96363E" &
                  "BC5E605F615489F5" &
                  "1EA1B8431BE6B288" &
                  "851174EE47A303F0" &
                  "9E3C619D9A4A6D19" &
                  "F507CE7A8AE224BA" &
                  "6A01A236B2D57CA4" &
                  "45847FA5CCAA0261" &
                  "57C76A6D770B4C41",
         Base  => 16
      );
      Check
      (  Left  => "10",
         Right => "10",
         M     => "11"
      );
      Check
      (  Left  => "000000000000000F" & -- 2
                  "FFFFFFFFFFFFFFFF" &
                  "E000000000000001",
         Right => "000000000000000F" & -- 2
                  "FFFFFFFFFFFFFFFF" &
                  "E000000000000001",
         M     => "10000000000000000" &
                  "00000000000000001",
         Base => 16
      );
      Check
      (  Left  => "E2150A85FE767750" &
                  "324C3334A920248D",
         Right => "E2150A85FE767750" &
                  "324C3334A920248D",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "0000000000000001",
         Right => "0000000000000001",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "51",
         Right => "108",
         M     => "111"
      );
      Check
      (  Left  => "10",
         Right => "9",
         M     => "11"
      );
      Check
      (  Left  => "5",
         Right => "3",
         M     => "11"
      );
      Check
      (  Left  => "1000000000000000",
         Right => "2000000000000000",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "1000800000000000",
         Right => "2000800000000000",
         M     => "F000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "8000000000000000",
         Right => "8000000000000000",
         M     => "0000000000000001" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "8000000000000000",
         Right => "8000000000000000",
         M     => "0000000979EFD66A" &
                  "D4419169C5B34413",
         Base  => 16
      );
      Check
      (  Left  => "000000009F809326" &
                  "D2CFD366E15EF201",
         Right => "000000006BFAF030" &
                  "96C5324C4136716B",
         M     => "0000000979EFD66A" &
                  "D4419169C5B34413",
         Base  => 16
      );
      Check
      (  Left  => "540019781128412936473322405310",
         Right => "515692107665463680305819378593",
         M     => "750791094644726559640638407699"
      );
   end;
   declare
      procedure Check
                (  Left, Right, M : String;
                   Base : Positive := 10
                )  is
         Reducer  : constant Barrett_Reducer :=
                             Create (Value (M, Base));
         X, Y          : Unbounded_Unsigned;
         R_1, R_2, R_3 : Unbounded_Unsigned;
      begin
         X := Value (Left,  Base);
         Y := Value (Right, Base);
         R_2 := X * Y mod Reducer.Modulus;
         Reduce (Reducer, X * Y, R_1);
         R_3 := Mod_Mul (Reducer, X, Y);
         if R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Barrett product reduction "
               &  Image (R_1, Base)
               &  " /= "
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
         if R_3 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Barrett product "
               &  Image (R_1, Base)
               &  " /= "
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Barrett reduction and multiplication test");
      Check
      (  Left  => "6CDA768965435940" &
                  "1A45B5DEC4646488" &
                  "8DA9658AAD044C79" &
                  "A25E7408D619CB05" &
                  "6D83689EB9A90A7A" &
                  "F03018C636CD6369" &
                  "EDE88E315D6B39F1" &
                  "3D9CC68A473D5D43" &
                  "CAE4DEE1DC026220" &
                  "A41AD736AC4BE866" &
                  "70CBA1352916F42E" &
                  "254ADB273120B8B2" &
                  "20E894905F5991D0" &
                  "C1FB195BE77F8A13" &
                  "2F72810E9A01F8DB" &
                  "F8A9C0B79ADE1B3D",
         Right => "0DDB8A96AA06B891" &
                  "49235C65CD089F6C" &
                  "2B5AA94695C90B72" &
                  "7C875C4AAC3760C",
         M     => "8661D87CDE3EE23F" &
                  "F73E18B5BE8933D2" &
                  "7B72337C70FE912C" &
                  "C9E083FD0DF766FE" &
                  "30D43275C21CFC81" &
                  "AFEFF7BDEDBB8987" &
                  "5B5D25EF8B86ECAF" &
                  "962113273D96363E" &
                  "BC5E605F615489F5" &
                  "1EA1B8431BE6B288" &
                  "851174EE47A303F0" &
                  "9E3C619D9A4A6D19" &
                  "F507CE7A8AE224BA" &
                  "6A01A236B2D57CA4" &
                  "45847FA5CCAA0261" &
                  "57C76A6D770B4C41",
         Base  => 16
      );
      Check
      (  Left  => "10",
         Right => "10",
         M     => "11"
      );
      Check
      (  Left  => "000000000000000F" & -- 2
                  "FFFFFFFFFFFFFFFF" &
                  "E000000000000001",
         Right => "000000000000000F" & -- 2
                  "FFFFFFFFFFFFFFFF" &
                  "E000000000000001",
         M     => "10000000000000000" &
                  "00000000000000001",
         Base => 16
      );
      Check
      (  Left  => "E2150A85FE767750" &
                  "324C3334A920248D",
         Right => "E2150A85FE767750" &
                  "324C3334A920248D",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "0000000000000001",
         Right => "0000000000000001",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "51",
         Right => "108",
         M     => "111"
      );
      Check
      (  Left  => "10",
         Right => "9",
         M     => "11"
      );
      Check
      (  Left  => "5",
         Right => "3",
         M     => "11"
      );
      Check
      (  Left  => "1000000000000000",
         Right => "2000000000000000",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "1000800000000000",
         Right => "2000800000000000",
         M     => "F000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "8000000000000000",
         Right => "8000000000000000",
         M     => "0000000000000001" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "8000000000000000",
         Right => "8000000000000000",
         M     => "0000000979EFD66A" &
                  "D4419169C5B34413",
         Base  => 16
      );
      Check
      (  Left  => "000000009F809326" &
                  "D2CFD366E15EF201",
         Right => "000000006BFAF030" &
                  "96C5324C4136716B",
         M     => "0000000979EFD66A" &
                  "D4419169C5B34413",
         Base  => 16
      );
      Check
      (  Left  => "540019781128412936473322405310",
         Right => "515692107665463680305819378593",
         M     => "750791094644726559640638407699"
      );
   end;
   declare
      procedure Check
                (  Left, M : String;
                   Base : Positive := 10
                )  is
         Domain  : constant Montgomery_Domain :=
                            Create (Value (M, Base));
         X, R_1  : Montgomery_Number;
         R_2     : Unbounded_Unsigned;
      begin
         X := From_Unbounded_Unsigned (Value (Left, Base));
         Mod_Square_CIOS (Domain, X, R_1);
         R_2 := (  (  To_Unbounded_Unsigned (X)
                   *  To_Unbounded_Unsigned (X)
                   *  Reducer_Inverse (Domain)
                   )
                mod
                   Domain.Modulus
                );
         if To_Unbounded_Unsigned (R_1) /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Montgomery CIOS square "
               &  Image (To_Unbounded_Unsigned (R_1), Base)
               &  " /= "
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Montgomery Mod_Square_CIOS test");
      Check
      (  Left  => "000000000000000F" & -- 2
                  "FFFFFFFFFFFFFFFF" &
                  "E000000000000001",
         M     => "10000000000000000" &
                  "00000000000000001",
         Base => 16
      );
      Check
      (  Left  => "51",
         M     => "111"
      );
      Check
      (  Left  => "0000000000000001",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "10",
         M     => "11"
      );
      Check
      (  Left  => "5",
         M     => "11"
      );
      Check
      (  Left  => "2000000000000000",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "540019781128412936473322405310",
         M     => "750791094644726559640638407699"
      );
   end;
   declare
      procedure Check
                (  Left, M : String;
                   Base : Positive := 10
                )  is
         Domain  : constant Montgomery_Domain :=
                            Create (Value (M, Base));
         X, R_1  : Montgomery_Number;
         R_2     : Unbounded_Unsigned;
      begin
         X := From_Unbounded_Unsigned (Value (Left, Base));
         Mod_Square_Dusse_Kaliski (Domain, X, R_1);
         R_2 := (  (  To_Unbounded_Unsigned (X)
                   *  To_Unbounded_Unsigned (X)
                   *  Reducer_Inverse (Domain)
                   )
                mod
                   Domain.Modulus
                );
         if To_Unbounded_Unsigned (R_1) /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Montgomery Square_Dusse-Kaliski square "
               &  Image (To_Unbounded_Unsigned (R_1), Base)
               &  " /= "
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Montgomery Mod_Square_Dusse_Kaliski test");
      Check
      (  Left  => "000000000000000F" & -- 2
                  "FFFFFFFFFFFFFFFF" &
                  "E000000000000001",
         M     => "10000000000000000" &
                  "00000000000000001",
         Base => 16
      );
      Check
      (  Left  => "51",
         M     => "111"
      );
      Check
      (  Left  => "0000000000000001",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "10",
         M     => "11"
      );
      Check
      (  Left  => "5",
         M     => "11"
      );
      Check
      (  Left  => "2000000000000000",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "540019781128412936473322405310",
         M     => "750791094644726559640638407699"
      );
   end;
   declare
      procedure Check
                (  Left, Right, M : String;
                   Base : Positive := 10
                )  is
         Domain  : constant Montgomery_Domain :=
                            Create (Value (M, Base));
         X_1      : constant Unbounded_Unsigned := Value (Left,  Base);
         X_2      : constant Unbounded_Unsigned := Value (Right, Base);
         R_1, R_2 : Montgomery_Number;
         R_Mul    : Unbounded_Unsigned;
         X_Mul    : Unbounded_Unsigned;
      begin
         R_1 := To_Domain (Domain, X_1);
         if (  To_Unbounded_Unsigned (R_1)
            /= X_1 * Domain.Reducer mod Domain.Modulus
            )  then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Montgomery domain transformation "
               &  Image (To_Unbounded_Unsigned (R_1), Base)
               &  " /= "
               &  Image (X_1 * Domain.Reducer mod Domain.Modulus, Base)
               &  " (expected)"
            )  );
         end if;
         if From_Domain (Domain, R_1) /= X_1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Montgomery domain reduce "
               &  Image (To_Unbounded_Unsigned (R_1), Base)
               &  " -> "
               &  Image (From_Domain (Domain, R_1), Base)
               &  " /= "
               &  Image (X_1, Base)
               &  " (expected)"
            )  );
         end if;
         R_2 := To_Domain (Domain, X_2);
         if From_Domain (Domain, R_2) /= X_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Montgomery domain reduce of "
               &  Image (To_Unbounded_Unsigned (R_2), Base)
               &  " -> "
               &  Image (From_Domain (Domain, R_2), Base)
               &  " /= "
               &  Image (X_2, Base)
               &  " (expected)"
            )  );
         end if;
         R_Mul := Mul (Domain, X_1, X_2);
         X_Mul := (X_1 * X_2) mod Domain.Modulus;
         if X_Mul /= R_Mul then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Montgomery domain reduce of product "
               &  Image (R_Mul, Base)
               &  " /= "
               &  Image (X_Mul, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Montgomery Mul test");
      Check
      (  Left  => "2",
         Right => "2",
         M     => "10000000000000000" &
                  "00000000000000001",
         Base => 16
      );
      Check
      (  Left  => "0000000000000001",
         Right => "0000000000000001",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "51",
         Right => "108",
         M     => "111"
      );
      Check
      (  Left  => "10",
         Right => "9",
         M     => "11"
      );
      Check
      (  Left  => "5",
         Right => "3",
         M     => "11"
      );
      Check
      (  Left  => "1000000000000000",
         Right => "2000000000000000",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "1000800000000000",
         Right => "2000800000000000",
         M     => "F000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "540019781128412936473322405310",
         Right => "515692107665463680305819378593",
         M     => "750791094644726559640638407699"
      );
   end;
   declare
      procedure Check
                (  Left, Right, M : String;
                   Base : Positive := 10
                )  is
         Domain : constant Montgomery_Domain :=
                           Create (Value (M, Base));
         X_1    : constant Unbounded_Unsigned := Value (Left,  Base);
         X_2    : constant Unbounded_Unsigned := Value (Right, Base);
         R_1    : Montgomery_Number;
         R_Pow  : Unbounded_Unsigned;
         X_Pow  : Unbounded_Unsigned;
      begin
         R_1 := To_Domain (Domain, X_1);
         if (  To_Unbounded_Unsigned (R_1)
            /= X_1 * Domain.Reducer mod Domain.Modulus
            )  then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Montgomery domain transformation "
               &  Image (To_Unbounded_Unsigned (R_1), Base)
               &  " /= "
               &  Image (X_1 * Domain.Reducer mod Domain.Modulus, Base)
               &  " (expected)"
            )  );
         end if;
         if From_Domain (Domain, R_1) /= X_1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Montgomery domain reduce "
               &  Image (To_Unbounded_Unsigned (R_1), Base)
               &  " -> "
               &  Image (From_Domain (Domain, R_1), Base)
               &  " /= "
               &  Image (X_1, Base)
               &  " (expected)"
            )  );
         end if;
         R_Pow := Pow (Domain, X_1, X_2);
         X_Pow := Mod_Pow (X_1, X_2, Domain.Modulus);
         if X_Pow /= R_Pow then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Montgomery domain exponent "
               &  Left
               &  " ** "
               &  Right
               &  " is "
               &  Image (R_Pow, Base)
               &  " /= "
               &  Image (X_Pow, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Montgomery Pow test");
      Check
      (  Left  => "2",
         Right => "00000000000000002" &
                  "00000000000000000",
         M     => "11",
         Base => 16
      );
      Check
      (  Left  => "2",
         Right => "00000000000000002",
         M     => "10000000000000000" &
                  "00000000000000001",
         Base => 16
      );
      Check
      (  Left  => "2",
         Right => "02198761F378FB88" &
                  "FFDCF862D6FA24CF" &
                  "49EDC8CDF1C3FA44" &
                  "B327820FF437DD9B" &
                  "F8C350C9D70873F2" &
                  "06BFBFDEF7B6EE26" &
                  "1D6D7497BE2E1BB2" &
                  "BE58844C9CF658D8" &
                  "FAF179817D855227" &
                  "D47A86E10C6F9ACA" &
                  "221445D3B91E8C0F" &
                  "C278F186766929B4" &
                  "67D41F39EA2B8892" &
                  "E9A80688DACB55F2" &
                  "911611FE9732A809" &
                  "855F1DA9B5DC2D31",
         M     => "8661D87CDE3EE23F" &
                  "F73E18B5BE8933D2" &
                  "7B72337C70FE912C" &
                  "C9E083FD0DF766FE" &
                  "30D43275C21CFC81" &
                  "AFEFF7BDEDBB8987" &
                  "5B5D25EF8B86ECAF" &
                  "962113273D96363E" &
                  "BC5E605F615489F5" &
                  "1EA1B8431BE6B288" &
                  "851174EE47A303F0" &
                  "9E3C619D9A4A6D19" &
                  "F507CE7A8AE224BA" &
                  "6A01A236B2D57CA4" &
                  "45847FA5CCAA0261" &
                  "57C76A6D770B4C41",
         Base => 16
      );
      Check
      (  Left  => "5",
         Right => "2",
         M     => "11"
      );
      Check
      (  Left  => "5",
         Right => "3",
         M     => "11"
      );
      Check
      (  Left  => "5",
         Right => "4",
         M     => "11"
      );
      Check
      (  Left  => "5",
         Right => "2",
         M     => "11"
      );
      Check
      (  Left  => "0000000000000001",
         Right => "0000000000000001",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "51",
         Right => "108",
         M     => "111"
      );
      Check
      (  Left  => "10",
         Right => "9",
         M     => "11"
      );
      Check
      (  Left  => "1000000000000000",
         Right => "2000000000000000",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "1000800000000000",
         Right => "2000800000000000",
         M     => "F000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "540019781128412936473322405310",
         Right => "515692107665463680305819378593",
         M     => "750791094644726559640638407699"
      );
   end;
   declare
      procedure Check
                (  Left, M : String;
                   Base : Positive := 10
                )  is
         Reducer  : constant Barrett_Reducer :=
                             Create (Value (M, Base));
         X        : Unbounded_Unsigned;
         R_1, R_2 : Unbounded_Unsigned;
      begin
         X := Value (Left, Base);
         R_2 := Square (X) mod Reducer.Modulus;
         Mod_Square (Reducer, X, R_1);
         if R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Barrett modular square "
               &  Image (R_1, Base)
               &  " /= "
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Barrett Mod_Square test");
      Check
      (  Left  => "000000000000000F" & -- 2
                  "FFFFFFFFFFFFFFFF" &
                  "E000000000000001",
         M     => "10000000000000000" &
                  "00000000000000001",
         Base => 16
      );
      Check
      (  Left  => "51",
         M     => "111"
      );
      Check
      (  Left  => "0000000000000001",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "10",
         M     => "11"
      );
      Check
      (  Left  => "5",
         M     => "11"
      );
      Check
      (  Left  => "2000000000000000",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "540019781128412936473322405310",
         M     => "750791094644726559640638407699"
      );
   end;
   declare
      procedure Check
                (  Left, Right, M : String;
                   Base : Positive := 10
                )  is
         Reducer  : constant Barrett_Reducer :=
                             Create (Value (M, Base));
         X_1      : constant Unbounded_Unsigned := Value (Left,  Base);
         X_2      : constant Unbounded_Unsigned := Value (Right, Base);
         R_1, R_2 : Unbounded_Unsigned;
      begin
         R_2 := Mod_Pow (X_1, X_2, Reducer.Modulus);
         R_1 := Mod_Pow (Reducer, X_1, X_2);
         if R_1 /= R_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Barrett modular exponent "
               &  Left
               &  " ** "
               &  Right
               &  " is "
               &  Image (R_1, Base)
               &  " /= "
               &  Image (R_2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Barrett Mod_Pow test");
      Check
      (  Left  => "2",
         Right => "00000000000000002" &
                  "00000000000000000",
         M     => "11",
         Base => 16
      );
      Check
      (  Left  => "2",
         Right => "00000000000000002",
         M     => "10000000000000000" &
                  "00000000000000001",
         Base => 16
      );
      Check
      (  Left  => "2",
         Right => "02198761F378FB88" &
                  "FFDCF862D6FA24CF" &
                  "49EDC8CDF1C3FA44" &
                  "B327820FF437DD9B" &
                  "F8C350C9D70873F2" &
                  "06BFBFDEF7B6EE26" &
                  "1D6D7497BE2E1BB2" &
                  "BE58844C9CF658D8" &
                  "FAF179817D855227" &
                  "D47A86E10C6F9ACA" &
                  "221445D3B91E8C0F" &
                  "C278F186766929B4" &
                  "67D41F39EA2B8892" &
                  "E9A80688DACB55F2" &
                  "911611FE9732A809" &
                  "855F1DA9B5DC2D31",
         M     => "8661D87CDE3EE23F" &
                  "F73E18B5BE8933D2" &
                  "7B72337C70FE912C" &
                  "C9E083FD0DF766FE" &
                  "30D43275C21CFC81" &
                  "AFEFF7BDEDBB8987" &
                  "5B5D25EF8B86ECAF" &
                  "962113273D96363E" &
                  "BC5E605F615489F5" &
                  "1EA1B8431BE6B288" &
                  "851174EE47A303F0" &
                  "9E3C619D9A4A6D19" &
                  "F507CE7A8AE224BA" &
                  "6A01A236B2D57CA4" &
                  "45847FA5CCAA0261" &
                  "57C76A6D770B4C41",
         Base => 16
      );
      Check
      (  Left  => "5",
         Right => "2",
         M     => "11"
      );
      Check
      (  Left  => "5",
         Right => "3",
         M     => "11"
      );
      Check
      (  Left  => "5",
         Right => "4",
         M     => "11"
      );
      Check
      (  Left  => "5",
         Right => "2",
         M     => "11"
      );
      Check
      (  Left  => "0000000000000001",
         Right => "0000000000000001",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "51",
         Right => "108",
         M     => "111"
      );
      Check
      (  Left  => "10",
         Right => "9",
         M     => "11"
      );
      Check
      (  Left  => "1000000000000000",
         Right => "2000000000000000",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "1000800000000000",
         Right => "2000800000000000",
         M     => "F000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "540019781128412936473322405310",
         Right => "515692107665463680305819378593",
         M     => "750791094644726559640638407699"
      );
   end;
   declare
      procedure Check
                (  Left, Right, M : String;
                   Base : Positive := 10
                )  is
         Server : Job_Server (8);
         Cache  : Unbounded_Unsigned_Array (1..8);
         Domain : constant Montgomery_Domain :=
                           Create (Value (M, Base));
         X_1    : constant Unbounded_Unsigned := Value (Left,  Base);
         X_2    : constant Unbounded_Unsigned := Value (Right, Base);
         R_Pow  : Unbounded_Unsigned;
         X_Pow  : Unbounded_Unsigned;
      begin
         X_Pow := Mod_Pow (X_1, X_2, Domain.Modulus);
         Pow (Domain, X_1, X_2, Server, Cache, 1, R_Pow);
         if X_Pow /= R_Pow then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Parallel Montgomery domain exponent "
               &  Left
               &  " ** "
               &  Right
               &  " is "
               &  Image (R_Pow, Base)
               &  " /= "
               &  Image (X_Pow, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Parallel Montgomery Pow test");
      Check
      (  Left  => "2",
         Right => "02198761F378FB88" &
                  "FFDCF862D6FA24CF" &
                  "49EDC8CDF1C3FA44" &
                  "B327820FF437DD9B" &
                  "F8C350C9D70873F2" &
                  "06BFBFDEF7B6EE26" &
                  "1D6D7497BE2E1BB2" &
                  "BE58844C9CF658D8" &
                  "FAF179817D855227" &
                  "D47A86E10C6F9ACA" &
                  "221445D3B91E8C0F" &
                  "C278F186766929B4" &
                  "67D41F39EA2B8892" &
                  "E9A80688DACB55F2" &
                  "911611FE9732A809" &
                  "855F1DA9B5DC2D31",
         M     => "8661D87CDE3EE23F" &
                  "F73E18B5BE8933D2" &
                  "7B72337C70FE912C" &
                  "C9E083FD0DF766FE" &
                  "30D43275C21CFC81" &
                  "AFEFF7BDEDBB8987" &
                  "5B5D25EF8B86ECAF" &
                  "962113273D96363E" &
                  "BC5E605F615489F5" &
                  "1EA1B8431BE6B288" &
                  "851174EE47A303F0" &
                  "9E3C619D9A4A6D19" &
                  "F507CE7A8AE224BA" &
                  "6A01A236B2D57CA4" &
                  "45847FA5CCAA0261" &
                  "57C76A6D770B4C41",
         Base => 16
      );
      Check
      (  Left  => "2",
         Right => "00000000000000002" &
                  "00000000000000000",
         M     => "11",
         Base => 16
      );
      Check
      (  Left  => "2",
         Right => "00000000000000002",
         M     => "10000000000000000" &
                  "00000000000000001",
         Base => 16
      );
      Check
      (  Left  => "5",
         Right => "2",
         M     => "11"
      );
      Check
      (  Left  => "5",
         Right => "3",
         M     => "11"
      );
      Check
      (  Left  => "5",
         Right => "4",
         M     => "11"
      );
      Check
      (  Left  => "5",
         Right => "2",
         M     => "11"
      );
      Check
      (  Left  => "0000000000000001",
         Right => "0000000000000001",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "51",
         Right => "108",
         M     => "111"
      );
      Check
      (  Left  => "10",
         Right => "9",
         M     => "11"
      );
      Check
      (  Left  => "1000000000000000",
         Right => "2000000000000000",
         M     => "F000000000000000" &
                  "0000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "1000800000000000",
         Right => "2000800000000000",
         M     => "F000000000000001",
         Base  => 16
      );
      Check
      (  Left  => "540019781128412936473322405310",
         Right => "515692107665463680305819378593",
         M     => "750791094644726559640638407699"
      );
   end;
   declare
      procedure Check
                (  Power, M : String;
                   Base : Positive := 10
                )  is
         Domain  : constant Montgomery_Domain :=
                            Create (Value (M, Base));
         P        : constant Unbounded_Unsigned := Value (Power, Base);
         R_Pow    : Unbounded_Unsigned;
         X_Pow    : Unbounded_Unsigned;
      begin
         R_Pow := Pow_Of_Two (Domain, P);
         X_Pow := Mod_Pow_Of_Two (P, Domain.Modulus);
         if X_Pow /= R_Pow then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Montgomery domain exponent 2 ** "
               &  Power
               &  " is "
               &  Image (R_Pow, Base)
               &  " /= "
               &  Image (X_Pow, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Montgomery Mod_Pow test");
      Check
      (  Power => "2",
         M     => "11"
      );
      Check
      (  Power => "128",
         M     => "11"
      );
      Check
      (  Power => "00000000000000002" &
                  "00000000000000000",
         M     => "11",
         Base => 16
      );
      Check
      (  Power => "00000000000000002",
         M     => "10000000000000000" &
                  "00000000000000001",
         Base => 16
      );
      Check
      (  Power => "02198761F378FB88" &
                  "FFDCF862D6FA24CF" &
                  "49EDC8CDF1C3FA44" &
                  "B327820FF437DD9B" &
                  "F8C350C9D70873F2" &
                  "06BFBFDEF7B6EE26" &
                  "1D6D7497BE2E1BB2" &
                  "BE58844C9CF658D8" &
                  "FAF179817D855227" &
                  "D47A86E10C6F9ACA" &
                  "221445D3B91E8C0F" &
                  "C278F186766929B4" &
                  "67D41F39EA2B8892" &
                  "E9A80688DACB55F2" &
                  "911611FE9732A809" &
                  "855F1DA9B5DC2D31",
         M     => "8661D87CDE3EE23F" &
                  "F73E18B5BE8933D2" &
                  "7B72337C70FE912C" &
                  "C9E083FD0DF766FE" &
                  "30D43275C21CFC81" &
                  "AFEFF7BDEDBB8987" &
                  "5B5D25EF8B86ECAF" &
                  "962113273D96363E" &
                  "BC5E605F615489F5" &
                  "1EA1B8431BE6B288" &
                  "851174EE47A303F0" &
                  "9E3C619D9A4A6D19" &
                  "F507CE7A8AE224BA" &
                  "6A01A236B2D57CA4" &
                  "45847FA5CCAA0261" &
                  "57C76A6D770B4C41",
         Base => 16
      );
   end;
   declare
      procedure Check (N : Natural; R : Unbounded_Unsigned) is
         Result : constant Unbounded_Unsigned := Fibonacci (N);
      begin
         if Result /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Fibonacci"
               &  Integer'Image (N)
               &  " is "
               &  Image (Result)
               &  " /= "
               &  Image (R)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Fibonacci test");
      for I in Fibonacci_Numbers'Range loop
         Check (I, Fibonacci_Numbers (I));
      end loop;
   end;
   declare
      procedure Check (N : Natural; M : String)  is
         Modulus : constant Unbounded_Unsigned := Value (M);
         R1      : constant Unbounded_Unsigned :=
                            Fibonacci (N) mod Modulus;
         R2      : constant Unbounded_Unsigned :=
                            Fibonacci
                            (  From_Half_Word (Half_Word (N)),
                               Modulus
                            );
      begin
         if R1 /= R2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Fibonacci"
               &  Integer'Image (N)
               &  " mod "
               &  M
               &  " is "
               &  Image (R2)
               &  " /= "
               &  Image (R1)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Fibonacci under modulo test 1");
      for I in Fibonacci_Numbers'Range loop
         Check (I, "5");
      end loop;
      for I in Fibonacci_Numbers'Range loop
         Check (I, "3");
      end loop;
      for I in Fibonacci_Numbers'Range loop
         Check (I, "15444333556");
      end loop;
      for I in Fibonacci_Numbers'Range loop
         Check (I, "451786544333556");
      end loop;
      for I in Fibonacci_Numbers'Range loop
         Check (I, "1304969544928657");
      end loop;
      for I in Fibonacci_Numbers'Range loop
         Check (I, "165580141");
      end loop;
   end;
   declare
      function Fibonacci_Mod (N, M : Unbounded_Unsigned)
         return Unbounded_Unsigned is
         F0, F1 : Unbounded_Unsigned;
         Count  : Unbounded_Unsigned;
      begin
         Set (F1, 1);
         Set (Count, N);
         Sub (Count, 1);
         while not Is_Zero (Count) loop
            Swap (F1, F0);
            Add (F1, F0);
            Modulo (F1, M);
            Sub (Count, 1);
         end loop;
         return F1;
      end Fibonacci_Mod;

      procedure Check (N, M : String; Base : Positive := 10)  is
         Left    : constant Unbounded_Unsigned := Value (N, Base);
         Modulus : constant Unbounded_Unsigned := Value (M, Base);
         R1      : constant Unbounded_Unsigned :=
                            Fibonacci_Mod (Left, Modulus);
         R2      : constant Unbounded_Unsigned :=
                            Fibonacci (Left, Modulus);
      begin
         if R1 /= R2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Fibonacci"
               &  N
               &  " mod "
               &  M
               &  " is "
               &  Image (R2, Base)
               &  " /= "
               &  Image (R1, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Fibonacci under modulo test 2");
      Check ("10", "11");
      -- Several seconds, comment out if debugging
      -- Check ("24157817", "24157818");
      -- Check ("0000000009DE8D6C", "0000000009DE8D6D", 16);
   end;
   declare
      procedure Check (N, M : String; Base : Positive := 10)  is
         Left    : constant Unbounded_Unsigned := Value (N, Base);
         Modulus : constant Unbounded_Unsigned := Value (M, Base);
         Domain  : constant Montgomery_Domain  := Create (Modulus);
         R1      : constant Unbounded_Unsigned :=
                       From_Domain (Domain, Fibonacci (Domain, Left));
         R2      : constant Unbounded_Unsigned :=
                       Fibonacci (Left, Modulus);
      begin
         if R1 /= R2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Fibonacci (Montgomery) "
               &  N
               &  " mod "
               &  M
               &  " is "
               &  Image (R2, Base)
               &  " /= "
               &  Image (R1, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Fibonacci under modulo in Montgomery domain test 3");
      Check ("10", "11");
      Check ("24157817", "2415781");
      Check ("0000000009DE8D6C", "0000000009DE8D6D", 16);
   end;
   declare
      procedure Check
                (  N   : String;
                   E_1 : Primality_Test_Outcome;
                   E_2 : Primality_Test_Outcome
                )  is
         Result : constant Primality_Test_Outcome :=
                           Fibonacci_Probably_Prime (Value (N));
      begin
         if Result not in E_1..E_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Fibonacci "
               &  N
               &  " is "
               &  Primality_Test_Outcome'Image (Result)
               &  " /= "
               &  Primality_Test_Outcome'Image (E_1)
               &  ".."
               &  Primality_Test_Outcome'Image (E_2)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Fibonacci_Probably_Prime test");
   --
   -- * marks false positives
   --
      Check ("24157817",             Undecided, Undecided); -- *
      Check ("165580141",            Undecided, Undecided); -- *
      Check ("806515533049393",      Undecided, Composite);
      Check ("1597",                 Undecided, Undecided);
      Check ("233",                  Undecided, Undecided);
      Check ("21",                   Composite, Composite);
      Check ("18699199384836356663", Undecided, Undecided);
      Check ("17908251027575790097", Undecided, Undecided);
      Check ("10953742525620032441", Undecided, Undecided);
      Check ("13496181268022124907", Undecided, Undecided);
      Check ("13756265695458089029", Undecided, Undecided);
      Check ("1304969544928657",     Undecided, Composite);
      Check
      (  "9436639673033417338310735304941495952152881531054818703016" &
         "5936229578960209523421808912459795329035203510284576187160" &
         "0763866437004412165477329142505789342618915108271402670435" &
         "9200722516079834891363947256471505544520151246135935948879" &
         "5427875530231001298552452230535485049737222714000227878890" &
         "892901228389026881",
         Undecided, Composite
      );
   end;
   declare
      procedure Check
                (  N   : String;
                   E_1 : Primality_Test_Outcome;
                   E_2 : Primality_Test_Outcome
                )  is
         Domain : constant Montgomery_Domain := Create (Value (N));
         Result : constant Primality_Test_Outcome :=
                           Fibonacci_Probably_Prime (Domain);
      begin
         if Result not in E_1..E_2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Fibonacci "
               &  N
               &  " is "
               &  Primality_Test_Outcome'Image (Result)
               &  " /= "
               &  Primality_Test_Outcome'Image (E_1)
               &  ".."
               &  Primality_Test_Outcome'Image (E_2)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Montgomery Fibonacci_Probably_Prime test");
   --
   -- * marks false positives
   --
      Check ("24157817",             Undecided, Undecided); -- *
      Check ("165580141",            Undecided, Undecided); -- *
      Check ("806515533049393",      Undecided, Composite);
      Check ("1597",                 Undecided, Undecided);
      Check ("233",                  Undecided, Undecided);
      Check ("21",                   Composite, Composite);
      Check ("18699199384836356663", Undecided, Undecided);
      Check ("17908251027575790097", Undecided, Undecided);
      Check ("10953742525620032441", Undecided, Undecided);
      Check ("13496181268022124907", Undecided, Undecided);
      Check ("13756265695458089029", Undecided, Undecided);
      Check ("1304969544928657",     Undecided, Composite);
      Check
      (  "9436639673033417338310735304941495952152881531054818703016" &
         "5936229578960209523421808912459795329035203510284576187160" &
         "0763866437004412165477329142505789342618915108271402670435" &
         "9200722516079834891363947256471505544520151246135935948879" &
         "5427875530231001298552452230535485049737222714000227878890" &
         "892901228389026881",
         Undecided, Composite
      );
   end;
   declare
      procedure Check (N, E : Unbounded_Unsigned) is
         Result : constant Unbounded_Unsigned := Phi (N);
      begin
         if Result /= E then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Phi "
               &  Image (N)
               &  " is "
               &  Image (Result)
               &  " /= "
               &  Image (E)
               &  " (expected)"
            )  );
         end if;
      end Check;
      Phis : constant array (Half_Word range 1..48) of Half_Word :=
         (   1,   1,   2,   2,   4,   2,   6,   4,   6,   4,  10,   4,
            12,   6,   8,   8,  16,   6,  18,   8,  12,  10,  22,   8,
            20,  12,  18,  12,  28,   8,  30,  16,  20,  16,  24,  12,
            36,  18,  24,  16,  40,  12,  42,  20,  24,  22,  46,  16
         );
   begin
      Put_Line ("Euler's totient test");
      for I in Phis'Range loop
         Check (From_Half_Word (I), From_Half_Word (Phis (I)));
      end loop;
   end;
   declare
      procedure Check
                (  N : String;
                   E : Boolean;
                   Base : Positive := 10
                )  is
         Left   : constant Unbounded_Unsigned := Value (N, Base);
         Result : Bit_Count;
      begin
         Result := Is_Proth (Left);
         if (Result /= 0) /= E then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Is_Proth ("
               &  N
               &  ") is "
               &  Boolean'Image (Result /= 0)
               &  " /= "
               &  Boolean'Image (E)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Proth number test");
      Check
      (  "0000000000000001" &
         "0000000000000000" &
         "0000000000000001" &
         "0000000000000000" &
         "0000000000000001",
         False,
         16
      );
      Check
      (  "FFFFFFFFFFFFFFFF" &
         "FFFFFFFFFFFFFFFF" &
         "0000000000000000" &
         "0000000000000001",
         True,
         16
      );
      Check
      (  "7FFFFFFFFFFFFFFF" &
         "FFFFFFFFFFFFFFFF" &
         "0000000000000000" &
         "0000000000000001",
         True,
         16
      );
      Check
      (  "0000000000000101",
         True,
         2
      );
      Check
      (  "0000000000001101",
         True,
         2
      );
      Check
      (  "0000000000010101",
         False,
         2
      );
      Check (Image (From_Half_Word (7) * 4 + 1), False);
      Check (Image (From_Half_Word (3) * 4 + 1), True);
      Check ("1",   False);
      Check ("2",   False);
      Check ("3",   True);
      Check ("4",   False);
      Check ("353", True);
      Check ("352", False);
      Check
      (  "0000400000000001",
         True,
         16
      );
      Check
      (  "0009999999999801",
         False,
         16
      );
      Check
      (  "0010400000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000001",
         True,
         16
      );
      Check
      (  "0010400000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000",
         False,
         16
      );
      Check
      (  "0010000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0010400000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000001",
         False,
         16
      );
   end;
   declare
      procedure Check
                (  N, M : String;
                   Base : Positive := 10
                )  is
         Power   : constant Unbounded_Unsigned := Value (N, Base);
         Modulus : constant Unbounded_Unsigned := Value (M, Base);
         R1, R2  : Unbounded_Unsigned;
      begin
         Mod_Pow_Of_Two (Power, Modulus, R1);
         Mod_Pow (Two, Power, Modulus, R2);
         if R1 /= R2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "2 ** "
               &  N
               &  " mod "
               &  M
               &  " is "
               &  Image (R1, Base)
               &  " /= "
               &  Image (R2, Base)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Mod_Pow_Of_Two test");
      Check
      (  "0000400000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "00000000000004FB",
         "0001000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "00000000000013ED",
         Base => 16
      );
      Check ("10",  "1025");
      Check ("10",  "1024");
      Check ("10",  "4025");
      Check ("0",   "10");
      Check ("10",  "1");
      Check ("10",  "3");
      Check ("10",  "4");
      Check ("10",  "7");
      Check ("10",  "5");
      Check ("10",  "9");
      Check
      (  "2000",
         "0000800000000000" &
         "0000000000000000" &
         "0000000000000000",
         16
      );
      Check
      (  "1029",
         "1000000000000000" &
         "0000000000000000",
         16
      );
      Check
      (  "1029",
         "1000000000000000" &
         "0000000000000001",
         16
      );
      Check
      (  "10029",
         "2000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000001",
         16
      );
      Check
      (  "10029",
         "1000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000001",
         16
      );
      Check
      (  "10029",
         "1FFFFFFFFFFFFFFF" &
         "FFFFFFFFFFFFFFFF" &
         "FFFFFFFFFFFFFFFF" &
         "FFFFFFFFFFFFFFFF",
         16
      );
      Check ("200",   "1000000000003", Base => 16);
      Check ("80087", "10010F",        Base => 16);
      Check ("20", "11");
      Check ("10", "11");
      Check
      (  "0000000000000800" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000429",
         "0000000000001000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000000" &
         "0000000000000853",
         16
      );
   end;
   declare
      procedure Check (N : String; E : Primality_Test_Outcome) is
         Result : constant Primality_Test_Outcome :=
                           Fermat_Probably_Prime (Value (N), 15);
      begin
         if Result /= E then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Fermat probable "
               &  N
               &  " is "
               &  Primality_Test_Outcome'Image (Result)
               &  " /= "
               &  Primality_Test_Outcome'Image (E)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Fermat_Probably_Prime test");
      Check ("2",  Prime);
      Check ("3",  Prime);
      Check ("5",  Prime);
      Check ("7",  Prime);
      Check ("11", Undecided);
      Check ("71", Undecided);

      Check ("18699199384836356663", Undecided);
      Check ("17908251027575790097", Undecided);
      Check ("10953742525620032441", Undecided);
      Check ("13496181268022124907", Undecided);
      Check ("13756265695458089029", Undecided);

      Check
      (  "9892036654808464360172886905559265083557295093226696746179" &
         "0948584315647051443",
         Undecided
      );
      Check
      (  "9456020830884701574749852388406339467160667190494466636006" &
         "8158221458669711639",
         Undecided
      );
      Check
      (  "4494179990554414939947092970931085130153737870495584992054" &
         "9234787172992757311826281150838665599829907456697437371147" &
         "2560655026288668094291699357843464363003144674940345912431" &
         "129144354948751003607115263071543163",
         Undecided
      );
      Check
      (  "2309758599932041506664235389885578395555602439290654154349" &
         "8090425831053075300672385713974233464012253359851759767480" &
         "7096648905501653461687601339782814316124971547968912893214" &
         "002992086353183070342498989426570593",
         Undecided
      );
      Check
      (  "5521712099665906221540423207019333379125265462121169655563" &
         "4954038884494934936299434980646045369617751107653777455503" &
         "7706789360724602069497295978083915145245772885538211355586" &
         "7743022746090187341871655890805971735385789993",
         Undecided
      );
      Check
      (  "3618502788666131106986593281521497120414687020801267626233" &
         "049500247285301239",
         Undecided
      );
      Check
      (  "5789604461865809771178549250434395392663499233282028201972" &
         "8792003956564819949",
         Undecided
      );
      Check
      (  "9850501549098619803069760025035903451269934817616361666987" &
         "073351061430442874302652853566563721228910201656997576599",
         Undecided
      );
      Check
      (  "4230758200257591033292257971409734654901789970971399803421" &
         "7522897561970639123926132812109468141778230245837569601494" &
         "931472367",
         Undecided
      );
      Check
      (  "6864797660130609714981900799081393217269435300143305409394" &
         "4634591855431833976560521225596406614545549772963113914808" &
         "58037121987999716643812574028291115057151",
         Undecided
      );

      Check ("0", Composite);
      Check ("1", Composite);

      Check
      (  "2128417509121468791277119989830729774821167291476384804196" &
         "8395774954376176754",
         Composite
      );
      Check
      (  "6084766654921918907427900243509372380954290099172559290432" &
         "744450051395395951",
         Composite
      );
      Check
      (  "8459435049322191838921335299203232428036771124794067565288" &
         "8030554255915464401",
         Composite
      );
      Check
      (  "82793403787388584738507275144194252681",
         Composite
      );
      Check
      (  "1195068768795265792518361315725116351898245581",
         Composite
      );
      Check ("1579751", Undecided);
      Check ("1884791", Undecided);
      Check ("3818929", Undecided);
      Check ("4080359", Undecided);
      Check ("4145951", Undecided);

      Check ("3673744903",  Composite);
      Check ("3281593591",  Composite);
      Check ("2385076987",  Composite);
      Check ("2738053141",  Composite);
      Check ("2009621503",  Composite);
      Check ("1502682721",  Composite);
      Check ("255866131",   Composite);
      Check ("117987841",   Composite);
      Check ("6368689",     Composite);
      Check ("8725753",     Composite);
      Check ("80579735209", Composite);
      Check ("587861",      Composite);
      Check ("105919633",   Composite);

      Check
      (  "803837457453639491257079614341942108138837688287558145837" &
         "488917522297427376533365218650233616396004545791504202360" &
         "320876656996676098728404396540823292873879185086916685732" &
         "826776177102938969773947016708230428687109997439976544144" &
         "845341155872450633409279022275296229414984230688168540432" &
         "6457534018329786111298960644845216191652872597534901",
         Composite
      );
      Check
      (  "203956878356401977405765866929034577280193993314348263" &
         "094772646453283062722701277632936616063144088173312372" &
         "882677123879538709400158306567338328279154499698366071" &
         "906766440037074217117805690872792848149112022286332144" &
         "876183376326512083574821647933992961249917319836219304" &
         "274280243803104015000563790123",
         Undecided
      );
      Check ("989", Composite);

      Check ("3239", Composite);
      Check ("5777", Composite);

      Check ("10877", Composite);
      Check ("27971", Composite);
      Check ("29681", Composite);
      Check ("30739", Composite);
      Check ("31631", Composite);
      Check ("39059", Composite);
      Check ("72389", Composite);
      Check ("73919", Composite);
      Check ("75077", Composite);

      Check ("100127", Composite);
      Check ("113573", Composite);
      Check ("125249", Composite);
      Check ("137549", Composite);
      Check ("137801", Composite);
      Check ("153931", Composite);
      Check ("155819", Composite);
      Check ("161027", Composite);
      Check ("162133", Composite);
      Check ("189419", Composite);
      Check ("218321", Composite);
      Check ("231703", Composite);
      Check ("249331", Composite);
      Check ("370229", Composite);
      Check ("429479", Composite);
      Check ("430127", Composite);
      Check ("459191", Composite);
      Check ("473891", Composite);
      Check ("480689", Composite);
      Check ("600059", Composite);
      Check ("621781", Composite);
      Check ("632249", Composite);
      Check ("635627", Composite);
   end;
   declare
      procedure Check (N : String; E : Primality_Test_Outcome) is
         Domain : constant Montgomery_Domain := Create (Value (N));
         Result : constant Primality_Test_Outcome :=
                           Fermat_Probably_Prime (Domain, 15);
      begin
         if Result /= E then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Montgomery Fermat probable "
               &  N
               &  " is "
               &  Primality_Test_Outcome'Image (Result)
               &  " /= "
               &  Primality_Test_Outcome'Image (E)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Montgomery Fermat_Probably_Prime test");
      Check ("7",  Prime);
      Check ("11", Undecided);
      Check ("71", Undecided);

      Check ("18699199384836356663", Undecided);
      Check ("17908251027575790097", Undecided);
      Check ("10953742525620032441", Undecided);
      Check ("13496181268022124907", Undecided);
      Check ("13756265695458089029", Undecided);

      Check
      (  "9892036654808464360172886905559265083557295093226696746179" &
         "0948584315647051443",
         Undecided
      );
      Check
      (  "9456020830884701574749852388406339467160667190494466636006" &
         "8158221458669711639",
         Undecided
      );
      Check
      (  "4494179990554414939947092970931085130153737870495584992054" &
         "9234787172992757311826281150838665599829907456697437371147" &
         "2560655026288668094291699357843464363003144674940345912431" &
         "129144354948751003607115263071543163",
         Undecided
      );
      Check
      (  "2309758599932041506664235389885578395555602439290654154349" &
         "8090425831053075300672385713974233464012253359851759767480" &
         "7096648905501653461687601339782814316124971547968912893214" &
         "002992086353183070342498989426570593",
         Undecided
      );
      Check
      (  "5521712099665906221540423207019333379125265462121169655563" &
         "4954038884494934936299434980646045369617751107653777455503" &
         "7706789360724602069497295978083915145245772885538211355586" &
         "7743022746090187341871655890805971735385789993",
         Undecided
      );
      Check
      (  "3618502788666131106986593281521497120414687020801267626233" &
         "049500247285301239",
         Undecided
      );
      Check
      (  "5789604461865809771178549250434395392663499233282028201972" &
         "8792003956564819949",
         Undecided
      );
      Check
      (  "9850501549098619803069760025035903451269934817616361666987" &
         "073351061430442874302652853566563721228910201656997576599",
         Undecided
      );
      Check
      (  "4230758200257591033292257971409734654901789970971399803421" &
         "7522897561970639123926132812109468141778230245837569601494" &
         "931472367",
         Undecided
      );
      Check
      (  "6864797660130609714981900799081393217269435300143305409394" &
         "4634591855431833976560521225596406614545549772963113914808" &
         "58037121987999716643812574028291115057151",
         Undecided
      );
      Check
      (  "6084766654921918907427900243509372380954290099172559290432" &
         "744450051395395951",
         Composite
      );
      Check
      (  "8459435049322191838921335299203232428036771124794067565288" &
         "8030554255915464401",
         Composite
      );
      Check
      (  "82793403787388584738507275144194252681",
         Composite
      );
      Check
      (  "1195068768795265792518361315725116351898245581",
         Composite
      );
      Check ("1579751", Undecided);
      Check ("1884791", Undecided);
      Check ("3818929", Undecided);
      Check ("4080359", Undecided);
      Check ("4145951", Undecided);

      Check ("3673744903",  Composite);
      Check ("3281593591",  Composite);
      Check ("2385076987",  Composite);
      Check ("2738053141",  Composite);
      Check ("2009621503",  Composite);
      Check ("1502682721",  Composite);
      Check ("255866131",   Composite);
      Check ("117987841",   Composite);
      Check ("6368689",     Composite);
      Check ("8725753",     Composite);
      Check ("80579735209", Composite);
      Check ("587861",      Composite);
      Check ("105919633",   Composite);

      Check
      (  "803837457453639491257079614341942108138837688287558145837" &
         "488917522297427376533365218650233616396004545791504202360" &
         "320876656996676098728404396540823292873879185086916685732" &
         "826776177102938969773947016708230428687109997439976544144" &
         "845341155872450633409279022275296229414984230688168540432" &
         "6457534018329786111298960644845216191652872597534901",
         Composite
      );
      Check
      (  "203956878356401977405765866929034577280193993314348263" &
         "094772646453283062722701277632936616063144088173312372" &
         "882677123879538709400158306567338328279154499698366071" &
         "906766440037074217117805690872792848149112022286332144" &
         "876183376326512083574821647933992961249917319836219304" &
         "274280243803104015000563790123",
         Undecided
      );
      Check ("989", Composite);

      Check ("3239", Composite);
      Check ("5777", Composite);

      Check ("10877", Composite);
      Check ("27971", Composite);
      Check ("29681", Composite);
      Check ("30739", Composite);
      Check ("31631", Composite);
      Check ("39059", Composite);
      Check ("72389", Composite);
      Check ("73919", Composite);
      Check ("75077", Composite);

      Check ("100127", Composite);
      Check ("113573", Composite);
      Check ("125249", Composite);
      Check ("137549", Composite);
      Check ("137801", Composite);
      Check ("153931", Composite);
      Check ("155819", Composite);
      Check ("161027", Composite);
      Check ("162133", Composite);
      Check ("189419", Composite);
      Check ("218321", Composite);
      Check ("231703", Composite);
      Check ("249331", Composite);
      Check ("370229", Composite);
      Check ("429479", Composite);
      Check ("430127", Composite);
      Check ("459191", Composite);
      Check ("473891", Composite);
      Check ("480689", Composite);
      Check ("600059", Composite);
      Check ("621781", Composite);
      Check ("632249", Composite);
      Check ("635627", Composite);
   end;
   declare
      procedure Check (N : String; E : Primality_Test_Outcome) is
         Result : constant Primality_Test_Outcome :=
                           Lucas_Probably_Prime (Value (N));
      begin
         if Result /= E then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Lucas probable "
               &  N
               &  " is "
               &  Primality_Test_Outcome'Image (Result)
               &  " /= "
               &  Primality_Test_Outcome'Image (E)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Lucas_Probably_Prime test");
      Check ("2",  Prime);
      Check ("3",  Prime);
      Check ("5",  Prime);
      Check ("7",  Prime);
      Check ("11", Undecided);
      Check ("71", Undecided);

      Check ("18699199384836356663", Undecided);
      Check ("17908251027575790097", Undecided);
      Check ("10953742525620032441", Undecided);
      Check ("13496181268022124907", Undecided);
      Check ("13756265695458089029", Undecided);

      Check
      (  "9892036654808464360172886905559265083557295093226696746179" &
         "0948584315647051443",
         Undecided
      );
      Check
      (  "9456020830884701574749852388406339467160667190494466636006" &
         "8158221458669711639",
         Undecided
      );
      Check
      (  "4494179990554414939947092970931085130153737870495584992054" &
         "9234787172992757311826281150838665599829907456697437371147" &
         "2560655026288668094291699357843464363003144674940345912431" &
         "129144354948751003607115263071543163",
         Undecided
      );
      Check
      (  "2309758599932041506664235389885578395555602439290654154349" &
         "8090425831053075300672385713974233464012253359851759767480" &
         "7096648905501653461687601339782814316124971547968912893214" &
         "002992086353183070342498989426570593",
         Undecided
      );
      Check
      (  "5521712099665906221540423207019333379125265462121169655563" &
         "4954038884494934936299434980646045369617751107653777455503" &
         "7706789360724602069497295978083915145245772885538211355586" &
         "7743022746090187341871655890805971735385789993",
         Undecided
      );
      Check
      (  "3618502788666131106986593281521497120414687020801267626233" &
         "049500247285301239",
         Undecided
      );
      Check
      (  "5789604461865809771178549250434395392663499233282028201972" &
         "8792003956564819949",
         Undecided
      );
      Check
      (  "9850501549098619803069760025035903451269934817616361666987" &
         "073351061430442874302652853566563721228910201656997576599",
         Undecided
      );
      Check
      (  "4230758200257591033292257971409734654901789970971399803421" &
         "7522897561970639123926132812109468141778230245837569601494" &
         "931472367",
         Undecided
      );
      Check
      (  "6864797660130609714981900799081393217269435300143305409394" &
         "4634591855431833976560521225596406614545549772963113914808" &
         "58037121987999716643812574028291115057151",
         Undecided
      );

      Check ("0", Composite);
      Check ("1", Composite);

      Check
      (  "2128417509121468791277119989830729774821167291476384804196" &
         "8395774954376176754",
         Composite
      );
      Check
      (  "6084766654921918907427900243509372380954290099172559290432" &
         "744450051395395951",
         Composite
      );
      Check
      (  "8459435049322191838921335299203232428036771124794067565288" &
         "8030554255915464401",
         Composite
      );
      Check
      (  "82793403787388584738507275144194252681",
         Composite
      );
      Check
      (  "1195068768795265792518361315725116351898245581",
         Composite
      );
      Check ("1579751", Undecided);
      Check ("1884791", Undecided);
      Check ("3818929", Undecided);
      Check ("4080359", Undecided);
      Check ("4145951", Undecided);

      Check ("3673744903",  Composite);
      Check ("3281593591",  Composite);
      Check ("2385076987",  Composite);
      Check ("2738053141",  Composite);
      Check ("2009621503",  Composite);
      Check ("1502682721",  Composite);
      Check ("255866131",   Composite);
      Check ("117987841",   Composite);
      Check ("6368689",     Composite);
      Check ("8725753",     Composite);
      Check ("80579735209", Composite);
      Check ("587861",      Composite);
      Check ("105919633",   Composite);

      Check
      (  "803837457453639491257079614341942108138837688287558145837" &
         "488917522297427376533365218650233616396004545791504202360" &
         "320876656996676098728404396540823292873879185086916685732" &
         "826776177102938969773947016708230428687109997439976544144" &
         "845341155872450633409279022275296229414984230688168540432" &
         "6457534018329786111298960644845216191652872597534901",
         Composite
      );
      Check
      (  "203956878356401977405765866929034577280193993314348263" &
         "094772646453283062722701277632936616063144088173312372" &
         "882677123879538709400158306567338328279154499698366071" &
         "906766440037074217117805690872792848149112022286332144" &
         "876183376326512083574821647933992961249917319836219304" &
         "274280243803104015000563790123",
         Undecided
      );
   --
   -- Lucas psequdoprimes, here the test will fail
   --
      if False then
         Check ("989", Composite);

         Check ("3239", Composite);
         Check ("5777", Composite);

         Check ("10877", Composite);
         Check ("27971", Composite);
         Check ("29681", Composite);
         Check ("30739", Composite);
         Check ("31631", Composite);
         Check ("39059", Composite);
         Check ("72389", Composite);
         Check ("73919", Composite);
         Check ("75077", Composite);

         Check ("100127", Composite);
         Check ("113573", Composite);
         Check ("125249", Composite);
         Check ("137549", Composite);
         Check ("137801", Composite);
         Check ("153931", Composite);
         Check ("155819", Composite);
         Check ("161027", Composite);
         Check ("162133", Composite);
         Check ("189419", Composite);
         Check ("218321", Composite);
         Check ("231703", Composite);
         Check ("249331", Composite);
         Check ("370229", Composite);
         Check ("429479", Composite);
         Check ("430127", Composite);
         Check ("459191", Composite);
         Check ("473891", Composite);
         Check ("480689", Composite);
         Check ("600059", Composite);
         Check ("621781", Composite);
         Check ("632249", Composite);
         Check ("635627", Composite);
      end if;
   end;
   declare
      procedure Check (N : String; E : Primality_Test_Outcome) is
         Domain : constant Montgomery_Domain := Create (Value (N));
         Result : constant Primality_Test_Outcome :=
                           Lucas_Probably_Prime (Domain);
      begin
         if Result /= E then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Montgomery Lucas probable "
               &  N
               &  " is "
               &  Primality_Test_Outcome'Image (Result)
               &  " /= "
               &  Primality_Test_Outcome'Image (E)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Montgomery domain Lucas_Probably_Prime test");
      Check ("7",  Prime);
      Check ("3",  Prime);
      Check ("5",  Prime);
      Check ("11", Undecided);
      Check ("71", Undecided);

      Check ("18699199384836356663", Undecided);
      Check ("17908251027575790097", Undecided);
      Check ("10953742525620032441", Undecided);
      Check ("13496181268022124907", Undecided);
      Check ("13756265695458089029", Undecided);

      Check
      (  "9892036654808464360172886905559265083557295093226696746179" &
         "0948584315647051443",
         Undecided
      );
      Check
      (  "9456020830884701574749852388406339467160667190494466636006" &
         "8158221458669711639",
         Undecided
      );
      Check
      (  "4494179990554414939947092970931085130153737870495584992054" &
         "9234787172992757311826281150838665599829907456697437371147" &
         "2560655026288668094291699357843464363003144674940345912431" &
         "129144354948751003607115263071543163",
         Undecided
      );
      Check
      (  "2309758599932041506664235389885578395555602439290654154349" &
         "8090425831053075300672385713974233464012253359851759767480" &
         "7096648905501653461687601339782814316124971547968912893214" &
         "002992086353183070342498989426570593",
         Undecided
      );
      Check
      (  "5521712099665906221540423207019333379125265462121169655563" &
         "4954038884494934936299434980646045369617751107653777455503" &
         "7706789360724602069497295978083915145245772885538211355586" &
         "7743022746090187341871655890805971735385789993",
         Undecided
      );
      Check
      (  "3618502788666131106986593281521497120414687020801267626233" &
         "049500247285301239",
         Undecided
      );
      Check
      (  "5789604461865809771178549250434395392663499233282028201972" &
         "8792003956564819949",
         Undecided
      );
      Check
      (  "9850501549098619803069760025035903451269934817616361666987" &
         "073351061430442874302652853566563721228910201656997576599",
         Undecided
      );
      Check
      (  "4230758200257591033292257971409734654901789970971399803421" &
         "7522897561970639123926132812109468141778230245837569601494" &
         "931472367",
         Undecided
      );
      Check
      (  "6864797660130609714981900799081393217269435300143305409394" &
         "4634591855431833976560521225596406614545549772963113914808" &
         "58037121987999716643812574028291115057151",
         Undecided
      );
      Check
      (  "6084766654921918907427900243509372380954290099172559290432" &
         "744450051395395951",
         Composite
      );
      Check
      (  "8459435049322191838921335299203232428036771124794067565288" &
         "8030554255915464401",
         Composite
      );
      Check
      (  "82793403787388584738507275144194252681",
         Composite
      );
      Check
      (  "1195068768795265792518361315725116351898245581",
         Composite
      );
      Check ("1579751", Undecided);
      Check ("1884791", Undecided);
      Check ("3818929", Undecided);
      Check ("4080359", Undecided);
      Check ("4145951", Undecided);

      Check ("3673744903",  Composite);
      Check ("3281593591",  Composite);
      Check ("2385076987",  Composite);
      Check ("2738053141",  Composite);
      Check ("2009621503",  Composite);
      Check ("1502682721",  Composite);
      Check ("255866131",   Composite);
      Check ("117987841",   Composite);
      Check ("6368689",     Composite);
      Check ("8725753",     Composite);
      Check ("80579735209", Composite);
      Check ("587861",      Composite);
      Check ("105919633",   Composite);

      Check
      (  "803837457453639491257079614341942108138837688287558145837" &
         "488917522297427376533365218650233616396004545791504202360" &
         "320876656996676098728404396540823292873879185086916685732" &
         "826776177102938969773947016708230428687109997439976544144" &
         "845341155872450633409279022275296229414984230688168540432" &
         "6457534018329786111298960644845216191652872597534901",
         Composite
      );
      Check
      (  "203956878356401977405765866929034577280193993314348263" &
         "094772646453283062722701277632936616063144088173312372" &
         "882677123879538709400158306567338328279154499698366071" &
         "906766440037074217117805690872792848149112022286332144" &
         "876183376326512083574821647933992961249917319836219304" &
         "274280243803104015000563790123",
         Undecided
      );
   --
   -- Lucas psequdoprimes, here the test will fail
   --
      if False then
         Check ("989", Composite);

         Check ("3239", Composite);
         Check ("5777", Composite);

         Check ("10877", Composite);
         Check ("27971", Composite);
         Check ("29681", Composite);
         Check ("30739", Composite);
         Check ("31631", Composite);
         Check ("39059", Composite);
         Check ("72389", Composite);
         Check ("73919", Composite);
         Check ("75077", Composite);

         Check ("100127", Composite);
         Check ("113573", Composite);
         Check ("125249", Composite);
         Check ("137549", Composite);
         Check ("137801", Composite);
         Check ("153931", Composite);
         Check ("155819", Composite);
         Check ("161027", Composite);
         Check ("162133", Composite);
         Check ("189419", Composite);
         Check ("218321", Composite);
         Check ("231703", Composite);
         Check ("249331", Composite);
         Check ("370229", Composite);
         Check ("429479", Composite);
         Check ("430127", Composite);
         Check ("459191", Composite);
         Check ("473891", Composite);
         Check ("480689", Composite);
         Check ("600059", Composite);
         Check ("621781", Composite);
         Check ("632249", Composite);
         Check ("635627", Composite);
      end if;
   end;
   declare
      procedure Check (N : String; E1, E2 : Primality_Test_Outcome) is
         Result : constant Primality_Test_Outcome :=
                           Is_Prime (Value (N), 10, 3);
      begin
         if Result /= E1 and then Result /= E2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Miller-Rabin probable "
               &  N
               &  " is "
               &  Primality_Test_Outcome'Image (Result)
               &  " /= "
               &  Primality_Test_Outcome'Image (E1)
               &  ".."
               &  Primality_Test_Outcome'Image (E2)
               &  " (expected)"
            )  );
         end if;
      end Check;
   begin
      Put_Line ("Miller_Rabin_Probably_Prime test");
      Check ("2",  Prime, Prime);
      Check ("3",  Prime, Prime);
      Check ("5",  Prime, Prime);
      Check ("7",  Prime, Prime);
      Check ("11", Prime, Prime);
      Check ("71", Prime, Prime);

      Check ("18699199384836356663", Prime, Prime);
      Check ("17908251027575790097", Prime, Prime);
      Check ("10953742525620032441", Prime, Prime);
      Check ("13496181268022124907", Prime, Prime);
      Check ("13756265695458089029", Prime, Prime);

      Check
      (  "9892036654808464360172886905559265083557295093226696746179" &
         "0948584315647051443",
         Prime, Prime
      );
      Check
      (  "9456020830884701574749852388406339467160667190494466636006" &
         "8158221458669711639",
         Prime, Prime
      );
      Check
      (  "4494179990554414939947092970931085130153737870495584992054" &
         "9234787172992757311826281150838665599829907456697437371147" &
         "2560655026288668094291699357843464363003144674940345912431" &
         "129144354948751003607115263071543163",
         Prime, Prime
      );
      Check
      (  "2309758599932041506664235389885578395555602439290654154349" &
         "8090425831053075300672385713974233464012253359851759767480" &
         "7096648905501653461687601339782814316124971547968912893214" &
         "002992086353183070342498989426570593",
         Prime, Prime
      );
      Check
      (  "5521712099665906221540423207019333379125265462121169655563" &
         "4954038884494934936299434980646045369617751107653777455503" &
         "7706789360724602069497295978083915145245772885538211355586" &
         "7743022746090187341871655890805971735385789993",
         Prime, Prime
      );
      Check
      (  "3618502788666131106986593281521497120414687020801267626233" &
         "049500247285301239",
         Prime, Prime
      );
      Check
      (  "5789604461865809771178549250434395392663499233282028201972" &
         "8792003956564819949",
         Prime, Prime
      );
      Check
      (  "9850501549098619803069760025035903451269934817616361666987" &
         "073351061430442874302652853566563721228910201656997576599",
         Prime, Prime
      );
      Check
      (  "4230758200257591033292257971409734654901789970971399803421" &
         "7522897561970639123926132812109468141778230245837569601494" &
         "931472367",
         Prime, Prime
      );
      Check
      (  "6864797660130609714981900799081393217269435300143305409394" &
         "4634591855431833976560521225596406614545549772963113914808" &
         "58037121987999716643812574028291115057151",
         Prime, Prime
      );

      Check ("0", Composite, Composite);
      Check ("1", Composite, Composite);

      Check
      (  "2128417509121468791277119989830729774821167291476384804196" &
         "8395774954376176754",
         Composite, Composite
      );
      Check
      (  "6084766654921918907427900243509372380954290099172559290432" &
         "744450051395395951",
         Composite, Undecided
      );
      Check
      (  "8459435049322191838921335299203232428036771124794067565288" &
         "8030554255915464401",
         Composite, Composite
      );
      Check
      (  "82793403787388584738507275144194252681",
         Composite, Composite
      );
      Check
      (  "1195068768795265792518361315725116351898245581",
         Composite, Composite
      );
      Check ("1579751", Prime, Prime);
      Check ("1884791", Prime, Prime);
      Check ("3818929", Prime, Prime);
      Check ("4080359", Prime, Prime);
      Check ("4145951", Prime, Prime);

      Check ("3673744903",  Composite, Composite);
      Check ("3281593591",  Composite, Composite);
      Check ("2385076987",  Composite, Composite);
      Check ("2738053141",  Composite, Composite);
      Check ("2009621503",  Composite, Composite);
      Check ("1502682721",  Composite, Composite);
      Check ("255866131",   Composite, Composite);
      Check ("117987841",   Composite, Composite);
      Check ("6368689",     Composite, Composite);
      Check ("8725753",     Composite, Composite);
      Check ("80579735209", Composite, Composite);
      Check ("587861",      Composite, Composite);
      Check ("105919633",   Composite, Composite);

      Check
      (  "803837457453639491257079614341942108138837688287558145837" &
         "488917522297427376533365218650233616396004545791504202360" &
         "320876656996676098728404396540823292873879185086916685732" &
         "826776177102938969773947016708230428687109997439976544144" &
         "845341155872450633409279022275296229414984230688168540432" &
         "6457534018329786111298960644845216191652872597534901",
         Composite, Composite
      );
      Check
      (  "203956878356401977405765866929034577280193993314348263" &
         "094772646453283062722701277632936616063144088173312372" &
         "882677123879538709400158306567338328279154499698366071" &
         "906766440037074217117805690872792848149112022286332144" &
         "876183376326512083574821647933992961249917319836219304" &
         "274280243803104015000563790123",
         Prime, Undecided
      );
      Check ("989", Composite, Composite);

      Check ("3239", Composite, Composite);
      Check ("5777", Composite, Composite);

      Check ("10877", Composite, Composite);
      Check ("27971", Composite, Composite);
      Check ("29681", Composite, Composite);
      Check ("30739", Composite, Composite);
      Check ("31631", Composite, Composite);
      Check ("39059", Composite, Composite);
      Check ("72389", Composite, Composite);
      Check ("73919", Composite, Composite);
      Check ("75077", Composite, Composite);

      Check ("100127", Composite, Composite);
      Check ("113573", Composite, Composite);
      Check ("125249", Composite, Composite);
      Check ("137549", Composite, Composite);
      Check ("137801", Composite, Composite);
      Check ("153931", Composite, Composite);
      Check ("155819", Composite, Composite);
      Check ("161027", Composite, Composite);
      Check ("162133", Composite, Composite);
      Check ("189419", Composite, Composite);
      Check ("218321", Composite, Composite);
      Check ("231703", Composite, Composite);
      Check ("249331", Composite, Composite);
      Check ("370229", Composite, Composite);
      Check ("429479", Composite, Composite);
      Check ("430127", Composite, Composite);
      Check ("459191", Composite, Composite);
      Check ("473891", Composite, Composite);
      Check ("480689", Composite, Composite);
      Check ("600059", Composite, Composite);
      Check ("621781", Composite, Composite);
      Check ("632249", Composite, Composite);
      Check ("635627", Composite, Composite);

      Check
      (  "9436639673033417338310735304941495952152881531054818703016" &
         "5936229578960209523421808912459795329035203510284576187160" &
         "0763866437004412165477329142505789342618915108271402670435" &
         "9200722516079834891363947256471505544520151246135935948879" &
         "5427875530231001298552452230535485049737222714000227878890" &
         "892901228389026881",
         Prime, Prime
      );
      Check ("12889",           Prime, Prime);
      Check ("12888",           Composite, Composite);
      Check ("211597",          Prime, Prime);
      Check ("100000011479",    Prime, Prime);
      Check ("88160341",        Prime, Prime);
      Check ("428525246550001", Prime, Prime);
      Check ("37778931862957161710341", Composite, Composite);
      Check
      (  "50216813883093446110686315385661331328818843555712276103941",
         Composite, Composite
      );
      for N in List'Range loop
         Check (Strings_Edit.Integers.Image (N), List (N), List (N));
      end loop;
   end;
   declare
      procedure Report (T : Duration; Times: Positive; Text : String) is
         Line    : String (1..80);
         Pointer : Integer := 1;
      begin
         Pointer := 1;
         Put (Line, Pointer, Text);
         Put
         (  Destination => Line,
            Pointer     => Pointer,
            Value       => Duration'Image (T / Times),
            Field       => 15,
            Justify     => Right
         );
         Put
         (  Destination => Line,
            Pointer     => Pointer,
            Value       => Duration'Image (T),
            Field       => 15,
            Justify     => Right
         );
         Put_Line (Line (1..Pointer - 1));
      end Report;

      procedure Report
                (  Title              : String;
                   T1, T2, T3, T4, T5 : Duration;
                   Times              : Positive
                )  is
      begin
         Put_Line (Title);
         Put_Line
         (  "                      Callee"
         &  "     Average, s       Total, s"
         );
         Report (T1, Times, "                    Mod_Pow:");
         Report (T2, Times, "         Montgomery Mod_Pow:");
         Report (T3, Times, "           Parallel Mod_Pow:");
         Report (T4, Times, "Parallel Montgomery Mod_Pow:");
         Report (T5, Times, "            Barrett Mod_Pow:");
      end Report;

      Worker_Count : constant := 16;
      Threshold    : constant := 6;

      Server  : Job_Server (Worker_Count);
      Cache   : Unbounded_Unsigned_Array (1..Worker_Count + 5);
      Base    : Unbounded_Unsigned;
      Power   : Unbounded_Unsigned;
      Modulus : Unbounded_Unsigned;
      Times   : constant := 30;
      Start   : Time;
      T1, T2  : Duration;
      T3, T4  : Duration;
      T5      : Duration;
   begin
      Set
      (  Modulus,
         Value
         (  "0000000000001000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000853",
            16
      )  );
      Set
      (  Base,
         Value
         (  "65799999999999908903764300997777567789043355664512578970" &
            "78800000984666684332333331999999999999997777893214569789"
      )  );
      Set
      (  Power,
         Value
         (  "0000000000000800" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000000" &
            "0000000000000429",
            16
      )  );
      declare
         Result : Unbounded_Unsigned;
         Left   : Unbounded_Unsigned := Base;
         Right  : Unbounded_Unsigned := Power;
         Count  : Natural := 0;
      begin
         Start := Clock;
         for Try in 1..Times loop
            Mod_Pow (Left, Right, Modulus, Result);
            if not Is_Zero (Result) then
               Count := Count + 1; -- Take care of optimization
            end if;
            Add (Left, 2);
            Add (Right, 2);
         end loop;
         T1 := Clock - Start;
      end;
      declare
         Domain : Montgomery_Domain;
         Result : Unbounded_Unsigned;
         Left   : Unbounded_Unsigned := Base;
         Right  : Unbounded_Unsigned := Power;
         Count  : Natural := 0;
      begin
         Start := Clock;
         Domain := Create (Modulus);
         for Try in 1..Times loop
            Result := Pow (Domain, Left, Right);
            if not Is_Zero (Result) then
               Count := Count + 1; -- Take care of optimization
            end if;
            Add (Left,  2);
            Add (Right, 2);
         end loop;
         T2 := Clock - Start;
      end;
      declare
         Result : Unbounded_Unsigned;
         Left   : Unbounded_Unsigned := Base;
         Right  : Unbounded_Unsigned := Power;
         Count  : Natural := 0;
      begin
         Start := Clock;
         for Try in 1..Times loop
            Mod_Pow_Karatsuba
            (  Left,
               Right,
               Modulus,
               Server,
               Threshold,
               Result
            );
            if not Is_Zero (Result) then
               Count := Count + 1; -- Take care of optimization
            end if;
            Add (Left,  2);
            Add (Right, 2);
         end loop;
         T3 := Clock - Start;
      end;
      declare
         Domain : Montgomery_Domain;
         Result : Unbounded_Unsigned;
         Left   : Unbounded_Unsigned := Base;
         Right  : Unbounded_Unsigned := Power;
         Count  : Natural := 0;
      begin
         Start := Clock;
         Domain := Create (Modulus);
         for Try in 1..Times loop
            Pow
            (  Domain,
               Left,
               Right,
               Server,
               Cache,
               Threshold,
               Result
            );
            if not Is_Zero (Result) then
               Count := Count + 1; -- Take care of optimization
            end if;
            Add (Left,  2);
            Add (Right, 2);
         end loop;
         T4 := Clock - Start;
      end;
      declare
         Reducer : Barrett_Reducer;
         Result  : Unbounded_Unsigned;
         Left    : Unbounded_Unsigned := Base;
         Right   : Unbounded_Unsigned := Power;
         Count   : Natural := 0;
      begin
         Start := Clock;
         Reducer := Create (Modulus);
         for Try in 1..Times loop
            Mod_Pow
            (  Reducer,
               Left,
               Right,
               Result
            );
            if not Is_Zero (Result) then
               Count := Count + 1; -- Take care of optimization
            end if;
            Add (Left,  2);
            Add (Right, 2);
         end loop;
         T5 := Clock - Start;
      end;
      Report
      (  "Comparing modular exponentiation implementations",
         T1, T2, T3, T4, T5, Times
      );

      declare
         Result : Unbounded_Unsigned;
         Right  : Unbounded_Unsigned := Power;
         Count  : Natural := 0;
      begin
         Start := Clock;
         for Try in 1..Times loop
            Mod_Pow (Two, Right, Modulus, Result);
            if not Is_Zero (Result) then
               Count := Count + 1; -- Take care of optimization
            end if;
            Add (Right, 2);
         end loop;
         T1 := Clock - Start;
      end;
      declare
         Domain : Montgomery_Domain;
         Result : Unbounded_Unsigned;
         Right  : Unbounded_Unsigned := Power;
         Count  : Natural := 0;
      begin
         Start := Clock;
         Domain := Create (Modulus);
         for Try in 1..Times loop
            Result := Pow (Domain, Two, Right);
            if not Is_Zero (Result) then
               Count := Count + 1; -- Take care of optimization
            end if;
            Add (Right, 2);
         end loop;
         T2 := Clock - Start;
      end;
      declare
         Result : Unbounded_Unsigned;
         Right  : Unbounded_Unsigned := Power;
         Count  : Natural := 0;
      begin
         Start := Clock;
         for Try in 1..Times loop
            Mod_Pow_Karatsuba
            (  Two,
               Right,
               Modulus,
               Server,
               Threshold,
               Result
            );
            if not Is_Zero (Result) then
               Count := Count + 1; -- Take care of optimization
            end if;
            Add (Right, 2);
         end loop;
         T3 := Clock - Start;
      end;
      declare
         Domain : Montgomery_Domain;
         Result : Unbounded_Unsigned;
         Right  : Unbounded_Unsigned := Power;
         Count  : Natural := 0;
      begin
         Start := Clock;
         Domain := Create (Modulus);
         for Try in 1..Times loop
            Pow
            (  Domain,
               Two,
               Right,
               Server,
               Cache,
               Threshold,
               Result
            );
            if not Is_Zero (Result) then
               Count := Count + 1; -- Take care of optimization
            end if;
            Add (Right, 2);
         end loop;
         T4 := Clock - Start;
      end;
      declare
         Reducer : Barrett_Reducer;
         Result  : Unbounded_Unsigned;
         Right   : Unbounded_Unsigned := Power;
         Count   : Natural := 0;
      begin
         Start := Clock;
         Reducer := Create (Modulus);
         for Try in 1..Times loop
            Mod_Pow
            (  Reducer,
               Two,
               Right,
               Result
            );
            if not Is_Zero (Result) then
               Count := Count + 1; -- Take care of optimization
            end if;
            Add (Right, 2);
         end loop;
         T5 := Clock - Start;
      end;
      Report
      (  "Comparing modular 2**K exponentiation implementations",
         T1, T2, T3, T4, T5, Times
      );
      Put_Line ("Worker Jobs");
      declare
         Line    : String (1..80);
         Pointer : Integer := 1;
      begin
         for No in 1..Server.Workers_Count loop
            Pointer := 1;
            Put
            (  Destination => Line,
               Pointer     => Pointer,
               Value       => Integer'Image (No),
               Field       => 6,
               Justify     => Right
            );
            Put
            (  Destination => Line,
               Pointer     => Pointer,
               Value       =>
                  Integer'Image (Get_Jobs_Completed (Server, No))
            );
            Put_Line (Line (1..Pointer - 1));
         end loop;
      end;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Unbounded_Unsigneds;
