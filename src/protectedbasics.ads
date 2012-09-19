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

end ProtectedBasics;
