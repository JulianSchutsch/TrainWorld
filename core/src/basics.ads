with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;

package Basics is

   function U(Str:String) return Unbounded_String renames To_Unbounded_String;

   procedure PutAddr
     (Address : System.Address);

end Basics;
