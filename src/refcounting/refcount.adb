with Ada.Unchecked_Deallocation;

package body RefCount is

   package body Ref is

      type Ref_Access is access all Ref_Interface;

      procedure SetNull
        (Ref : in out Ref_Type) is
      begin
         Finalize(Ref);
      end SetNull;
      ------------------------------------------------------------------------

      procedure Adjust
        (Ref: in out Ref_Type) is
      begin
         if Ref.I/=null then
            Ref_Access(Ref.I).Count:=Ref_Access(Ref.I).Count+1;
         end if;
      end Adjust;
      ------------------------------------------------------------------------

      procedure Finalize
        (Ref: in out Ref_Type) is

         procedure Free is new Ada.Unchecked_Deallocation
           (Object => Interface_Type'Class,
            Name   => Interface_ClassAccess);

      begin
         if Ref.I/=null then
            Ref_Access(Ref.I).Count:=Ref_Access(Ref.I).Count-1;
            if Ref_Access(Ref.I).Count=0 then
               Free(Ref.I);
            end if;
            Ref.I:=null;
         end if;
      end Finalize;
      ------------------------------------------------------------------------

      function MakeNewRef
        (Object : Interface_ClassAccess)
         return Ref.Ref_Type is
      begin
         return R:Ref_Type do
            R.I:=Object;
         end return;
      end MakeNewRef;
      ------------------------------------------------------------------------

      function MakeConstRef
        (Object : Interface_ClassAccess)
         return Ref.Ref_Type is
      begin
         return R:Ref_Type do
            R.I:=Object;
            Ref_Access(Object).Count:=Ref_Access(Object).Count+1;
         end return;
      end MakeConstRef;
      ------------------------------------------------------------------------

   end Ref;

end RefCount;
