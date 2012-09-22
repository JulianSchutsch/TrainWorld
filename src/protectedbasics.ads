package ProtectedBasics is

   protected type Counter_Type is
      procedure Decrement
        (NewValue : out Integer);
      procedure Set
        (NewValue : Integer);
      function Get
        return Integer;
   private
      Value : Integer;
   end Counter_Type;
   ---------------------------------------------------------------------------

   generic

      type Element_Type is private;
      NullElement : Element_Type;

   package Queue is

      type Entry_Type is private;
      type Entry_Access is access all Entry_Type;

      protected type Queue_Type is

         procedure Put
           (Element : Element_Type);
         procedure Get
           (Element : out Element_Type);
         function Empty
           return Boolean;
         procedure Clear;

      private

         First : Entry_Access:=null;
         Last  : Entry_Access:=null;

      end Queue_Type;

      type Queue_Access is access all Queue_Type;
      ------------------------------------------------------------------------
   private

      type Entry_Type is
         record
            Element : Element_Type;
            Next    : Entry_Access;
            Last    : Entry_Access;
         end record;

   end Queue;

   type BarrierState_Enum is
     (BarrierStateInvalid,
      BarrierStateNull,
      BarrierState1,
      BarrierState2);

   type BarrierStateCount_Array is array(BarrierState_Enum) of Natural;

   protected type PollingBarrier_Type is

      procedure Join
        (State : in out BarrierState_Enum);

      procedure Leave
        (State : in out BarrierState_Enum);

      procedure TestBarrier
        (State : in out BarrierState_Enum;
         Pass  : out Boolean);

      function GetMemberCount
        return Integer;

   private

      MemberCount      : Natural:=0;
      StateCounts      : BarrierStateCount_Array:=(others => 0);
      CurrentBarrier   : BarrierState_Enum:=(BarrierState1);

   end PollingBarrier_Type;
   ---------------------------------------------------------------------------

end ProtectedBasics;
