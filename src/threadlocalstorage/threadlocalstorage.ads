generic
   type Content_Type is private;
   NullValue : Content_Type;
package ThreadLocalStorage is

   ThreadLocalStorageOverflow : Exception;

   procedure Set(Content : Content_Type);
   procedure Get(Content : out Content_Type);

end ThreadLocalStorage;
