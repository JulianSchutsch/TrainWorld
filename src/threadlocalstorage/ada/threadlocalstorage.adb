package body ThreadLocalStorage is

   C : Content_ClassAccess:=null;
   pragma Thread_Local_Storage(C);

   procedure Set(Content : Content_ClassAccess) is
   begin
      C:=Content;
   end Set;
   ---------------------------------------------------------------------------

   procedure Get(Content : out Content_ClassAccess) is
   begin
      Content:=C;
   end Get;
   ---------------------------------------------------------------------------

end ThreadLocalStorage;
