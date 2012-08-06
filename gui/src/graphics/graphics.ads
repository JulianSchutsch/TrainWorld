pragma Ada_2012;

with RefCounted;
with Implementations;

package Graphics is

   type Context_Type is tagged null record;

   procedure Destructor
     (Context : access Context_Type'Class);

   package Context_Reference is new RefCounted
     (Refered_Type => Context_Type,
      Destructor   => Destructor);

   subtype Context_Ref is Context_Reference.Ref_Type;

   package Implementations is new Standard.Implementations
     (Implementation_Type => Context_Reference.Ref_Type);

end Graphics;
