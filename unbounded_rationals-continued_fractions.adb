--                                                                    --
--  package Unbounded_Rationals.    Copyright (c)  Dmitry A. Kazakov  --
--             Continued_Fractions                 Luebeck            --
--  Implementation                                 Spring, 2025       --
--                                                                    --
--                                Last revision :  08:56 30 Jun 2025  --
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

package body Unbounded_Rationals.Continued_Fractions is

   procedure Add
             (  Fraction : in out Continued_Fraction;
                Term     : Unbounded_Integer
             )  is
   begin
      Put (Fraction.Terms, Fraction.Length + 1, To_Term (Term));
      Fraction.Length := Fraction.Length + 1;
   end Add;

   procedure Add
             (  Fraction : in out Continued_Fraction;
                Term     : Half_Word;
                Sign     : Boolean
             )  is
   begin
      Put (Fraction.Terms, Fraction.Length + 1, (False, Sign, Term));
      Fraction.Length := Fraction.Length + 1;
   end Add;

   procedure Delete_Term
             (  Fraction : in out Continued_Fraction;
                Count    : Natural := 1
             )  is
   begin
      for Index in 1..Count loop
         exit when Fraction.Length = 0;
         Put (Fraction.Terms, Fraction.Length, (False, False, 0));
         Fraction.Length := Fraction.Length - 1;
      end loop;
   end Delete_Term;

   procedure Erase (Fraction : in out Continued_Fraction) is
   begin
      Delete_Term (Fraction, Fraction.Length);
   end Erase;

   function From_Term (Value : Term) return Unbounded_Integer is
   begin
      if Value.Unbounded then
         return Compose (Value.Long, Value.Sign);
      else
         return Compose (From_Half_Word (Value.Short), Value.Sign);
      end if;
   end From_Term;

   procedure Enumerate
             (  Value   : Unbounded_Rational;
                Visitor : in out Term_Visitor'Class
             )  is
      Remainder : Unbounded_Rational := Value;
      Item      : Unbounded_Integer;
      Done      : Boolean;
   begin
      if Is_Zero (Remainder) then
         return;
      end if;
      for Index in 1..Integer'Last loop
         Item := To_Unbounded_Integer (Remainder);
         On_Term (Visitor, Value, Item, Done);
         exit when Done;
         Remainder := Remainder - Item;
         exit when Is_Zero (Remainder);
         Remainder := One / Remainder;
      end loop;
   end Enumerate;

   procedure Generic_Enumerate (Value : Unbounded_Rational) is
      Remainder : Unbounded_Rational := Value;
      Item      : Unbounded_Integer;
   begin
      if Is_Zero (Remainder) then
         return;
      end if;
      for Index in 1..Integer'Last loop
         Split (Remainder, Item);
         exit when not On_Term (Value, Item)
           or else Is_Zero (Remainder);
         Invert (Remainder);
      end loop;
   end Generic_Enumerate;

   function Get_Length (Fraction : Continued_Fraction) return Natural is
   begin
      return Fraction.Length;
   end Get_Length;

   function Get_Term
            (  Fraction : Continued_Fraction;
               Index    : Positive
            )  return Unbounded_Integer is
   begin
      if Index > Fraction.Length then
         raise Constraint_Error;
      else
         return From_Term (Fraction.Terms.Vector (Index));
      end if;
   end Get_Term;

   function Get_Value (Fraction : Continued_Fraction)
      return Unbounded_Rational is
   begin
      if Fraction.Length = 0 then
         return Zero;
      end if;
      declare
         Vector : Terms_Array renames Fraction.Terms.Vector.all;
         Result : Unbounded_Rational :=
                  To_Unbounded_Rational
                  (  From_Term (Vector (Fraction.Length))
                  );
      begin
         for Index in reverse 1..Fraction.Length - 1 loop
            Result := One / Result + From_Term (Vector (Index));
         end loop;
         return Result;
      end;
   end Get_Value;

   function Is_Zero (Fraction : Continued_Fraction) return Boolean is
   begin
      return Fraction.Length = 0;
   end Is_Zero;

   procedure Set
             (  Fraction : in out Continued_Fraction;
                Value    : Unbounded_Rational
             )  is
      Remainder : Unbounded_Rational := Value;
      Item      : Unbounded_Integer;
   begin
      Fraction.Length := 0;
      if Is_Zero (Remainder) then
         return;
      end if;
      for Index in 1..Integer'Last loop
         Split (Remainder, Item);
         Put (Fraction.Terms, Index, To_Term (Item));
         Fraction.Length := Index;
         exit when Is_Zero (Remainder);
         Invert (Remainder);
      end loop;
   end Set;

   function To_Term (Value : Unbounded_Integer) return Term is
      Mantissa : constant Unbounded_Unsigned := Get_Mantissa (Value);
   begin
      if Is_Zero (Mantissa) then
         return (  Unbounded => False,
                   Sign      => Is_Negative (Value),
                   Short     => 0
                );
      elsif Get_Length (Mantissa) = 1 then
         return (  Unbounded => False,
                   Sign      => Is_Negative (Value),
                   Short     => To_Half_Word (Mantissa)
                );
      else
         return (  Unbounded => True,
                   Sign      => Is_Negative (Value),
                   Long      => Mantissa
                );
      end if;
   end To_Term;

end Unbounded_Rationals.Continued_Fractions;
