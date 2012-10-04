generic
   type Content_Type is tagged limited private;
   type Content_ClassAccess is access all Content_Type'Class;

package ThreadLocalStorage is

   ThreadLocalStorageOverflow : Exception;

   procedure Set(Content : Content_ClassAccess);
   procedure Get(Content : out Content_ClassAccess);

end ThreadLocalStorage;
