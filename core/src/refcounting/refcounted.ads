pragma Ada_2012;

with Ada.Finalization;

generic

   type Refered_Type(<>) is tagged private;

   with procedure Destructor
     (RefType : access Refered_Type'Class);

package RefCounted is
   type Ref_Type(Data:access Refered_Type'Class) is tagged private with Implicit_Dereference => Data;

private

   type Ref_Type(Data:access Refered_Type'Class) is new Ada.Finalization.Controlled with
      record
         Count : Natural:=1;
      end record;

   procedure Finalize
     (Ref : in out Ref_Type);

   procedure Adjust
     (Ref : in out Ref_Type);

end RefCounted;
