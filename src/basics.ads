with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;
with Ada.Finalization;

package Basics is

   type C_Type is new Ada.Finalization.Controlled with null record;
   type C_ClassAccess is access all C_Type'Class;

   function U(Str:String) return Unbounded_String renames To_Unbounded_String;

   procedure PutAddr
     (Address : System.Address);

end Basics;
