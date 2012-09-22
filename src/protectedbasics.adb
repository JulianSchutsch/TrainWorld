with Ada.Unchecked_Deallocation;
--with Ada.Text_IO; use Ada.Text_IO;

package body ProtectedBasics is

   protected body PollingBarrier_Type is

      function GetMemberCount
        return Integer is
      begin
         return MemberCount;
      end GetMemberCount;
      ------------------------------------------------------------------------

      procedure Join
        (State : in out BarrierState_Enum) is
      begin
         State:=BarrierStateNull;
         StateCounts(BarrierStateNull):=StateCounts(BarrierStateNull)+1;
         MemberCount:=MemberCount+1;
         pragma Assert(StateCounts(BarrierStateNull)+StateCounts(BarrierState1)+StateCounts(BarrierState2)=MemberCount);
      end Join;
      ------------------------------------------------------------------------

      procedure Leave
        (State : in out BarrierState_Enum) is
      begin
         StateCounts(State):=StateCounts(State)-1;
         State:=BarrierStateInvalid;
         MemberCount:=MemberCount-1;
         pragma Assert(StateCounts(BarrierStateNull)+StateCounts(BarrierState1)+StateCounts(BarrierState2)=MemberCount);
      end Leave;
      ------------------------------------------------------------------------

      procedure TestBarrier
        (State : in out BarrierState_Enum;
         Pass  : out Boolean) is
      begin

         pragma Assert(State/=BarrierStateInvalid);
         pragma Assert(State=BarrierState1 or State=BarrierState2 or State=BarrierStateNull);
         -- Is it necesary to add this asker to the current barrier?
         if State=BarrierStateNull then
            StateCounts(BarrierStateNull):=StateCounts(BarrierStateNull)-1;
            State:=CurrentBarrier;
            StateCounts(State):=StateCounts(State)+1;
         end if;

         pragma Assert(CurrentBarrier=BarrierState1 or CurrentBarrier=BarrierState2);
         -- Test if the current barrier is complete
         if StateCounts(CurrentBarrier)=MemberCount then
            if CurrentBarrier=BarrierState1 then
               CurrentBarrier:=BarrierState2;
            else
               CurrentBarrier:=BarrierState1;
            end if;
         end if;

         pragma Assert(State=BarrierState1 or State=BarrierState2);
         -- Check if the barrier has been switched to pass/next
         Pass:=State/=CurrentBarrier;
         if Pass then
            StateCounts(State):=StateCounts(State)-1;
            State:=BarrierStateNull;
            StateCounts(BarrierStateNull):=StateCounts(BarrierStateNull)+1;
         end if;
         pragma Assert(StateCounts(BarrierStateNull)+StateCounts(BarrierState1)+StateCounts(BarrierState2)=MemberCount);

      end TestBarrier;
      ------------------------------------------------------------------------

   end PollingBarrier_Type;
   ---------------------------------------------------------------------------

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
            else
               First:=NewEntry;
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
