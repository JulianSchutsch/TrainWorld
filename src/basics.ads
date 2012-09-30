with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;
with Ada.Finalization;
with Interfaces.C;

package Basics is

   type PtrInt_Type is new Interfaces.C.size_t;

   type UnboundedString_Array is array(Integer range <>) of Unbounded_String;

   type C_Type is new Ada.Finalization.Controlled with null record;
   type C_ClassAccess is access all C_Type'Class;

   function U(Str:String) return Unbounded_String renames To_Unbounded_String;

   procedure PutAddr
     (Address : System.Address);

end Basics;
