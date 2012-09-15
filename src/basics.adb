with System.Address_Image;
with Ada.Text_IO; use Ada.Text_IO;

package body Basics is

   procedure PutAddr
     (Address : System.Address) is
   begin
      Put(System.Address_Image(Address));
   end PutAddr;
   ---------------------------------------------------------------------------

end Basics;
