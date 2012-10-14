pragma Ada_2012;

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

   type String_Ref is new Ada.Finalization.Controlled with private;

   overriding
   procedure Finalize
     (StrRef : in out String_Ref);

   overriding
   procedure Adjust
     (StrRef : in out String_Ref);

   function Get
     (StrRef : String_Ref)
      return String;

   function RefConstStr
     (Str : not null access constant String)
      return String_Ref;

   function RefStr
     (Str : String)
      return String_Ref;

   function "="
     (Left  : String_Ref;
      Right : String_Ref)
      return Boolean;

   function "="
     (Left  : String_Ref;
      Right : String)
      return Boolean;

private

   type String_Cont(Len : Natural) is
      record
         Content : String(1..Len);
         Count   : Natural:=1;
      end record;

   type String_ContAccess is access all String_Cont;

   type String_Ref is new Ada.Finalization.Controlled with
      record
         Const    : access constant String:=null;
         NonConst : String_ContAccess;
      end record;

end Basics;
