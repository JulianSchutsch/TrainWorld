with Ada.Unchecked_Deallocation;

package body ProtectedBasics is

   protected body Counter_Type is

      procedure Decrement(NewValue : out Integer) is
      begin
         Value:=Value-1;
         NewValue:=Value;
      end Decrement;
      ------------------------------------------------------------------------

      procedure Set(NewValue : Integer) is
      begin
         Value:=NewValue;
      end Set;
      ------------------------------------------------------------------------

      function Get
        return Integer is
      begin
         return Value;
      end Get;
      ------------------------------------------------------------------------

   end Counter_Type;
   ---------------------------------------------------------------------------

   package body Queue is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Entry_Type,
         Name   => Entry_Access);

      protected body Queue_Type is

         procedure Put
           (Element : Element_Type) is

            NewEntry : Entry_Access:=new Entry_Type;

         begin

            NewEntry.Element:=Element;

            if Last/=null then
               Last.Next:=NewEntry;
            end if;
            Last:=NewEntry;

         end Put;
         ---------------------------------------------------------------------

         procedure Get
           (Element : out Element_Type) is

            OldFirst : Entry_Access:=First;

         begin

            if First=null then
               Element:=NullElement;
               return;
            end if;

            Element:=First.Element;

            First:=First.Next;
            if First=null then
               Last:=null;
            end if;

            Free(OldFirst);

         end Get;
         ---------------------------------------------------------------------

         function Empty
           return Boolean is
         begin
            return First=null;
         end Empty;
         ---------------------------------------------------------------------

         procedure Clear is
            Current : Entry_Access;
            Next    : Entry_Access;
         begin
            Current:=First;
            while Current/=null loop
               Next:=Current.Next;
               Free(Current);
               Current:=Next;
            end loop;
         end Clear;
         ---------------------------------------------------------------------

      end Queue_Type;
      ------------------------------------------------------------------------

   end Queue;
   ----------------------------------------------------------------------------

end ProtectedBasics;
