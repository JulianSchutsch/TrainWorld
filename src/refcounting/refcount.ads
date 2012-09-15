pragma Ada_2012;

with Ada.Finalization;

package RefCount is

   type Ref_Interface is new Ada.Finalization.Limited_Controlled with private;

   generic

      type Interface_Type is abstract new Ref_Interface with private;
      type Interface_ClassAccess is access all Interface_Type'Class;

   package Ref is

      type Ref_Type is new Ada.Finalization.Controlled with
         record
            I : Interface_ClassAccess;
         end record;

      overriding
      procedure Adjust
        (Ref : in out Ref_Type);

      overriding
      procedure Finalize
        (Ref : in out Ref_Type);
      ------------------------------------------------------------------------

      function MakeRef
        (Object : Interface_ClassAccess)
         return Ref.Ref_Type;

   end Ref;

private

   type Ref_Interface is new Ada.Finalization.Limited_Controlled with
      record
         Count : Natural:=1;
      end record;

end RefCount;