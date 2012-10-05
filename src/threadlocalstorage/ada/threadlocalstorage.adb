package body ThreadLocalStorage is

   C : Content_Type:=NullValue;
   pragma Thread_Local_Storage(C);

   procedure Set(Content : Content_Type) is
   begin
      C:=Content;
   end Set;
   ---------------------------------------------------------------------------

   procedure Get(Content : out Content_Type) is
   begin
      Content:=C;
   end Get;
   ---------------------------------------------------------------------------

end ThreadLocalStorage;
