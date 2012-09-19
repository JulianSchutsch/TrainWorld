with Ada.Unchecked_Deallocation;
with Basics; use Basics;
with Ada.Text_IO; use Ada.Text_IO;

package body RefCount is

   package body Ref is

      type Ref_Access is access all Ref_Interface;

      procedure Adjust
        (Ref: in out Ref_Type) is
      begin
         if Ref.I/=null then
            PutAddr(Ref.I.all'Address);
            Put_Line("Inc"&Integer'Image(Ref_Access(Ref.I).Count));
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
            PutAddr(Ref.I.all'Address);
            Put_Line("Dec"&Integer'Image(Ref_Access(Ref.I).Count));
            Ref_Access(Ref.I).Count:=Ref_Access(Ref.I).Count-1;
            if Ref_Access(Ref.I).Count=0 then
               Put_Line("Free");
               Free(Ref.I);
            end if;
         end if;
      end Finalize;
      ------------------------------------------------------------------------

      function MakeRef
        (Object : Interface_ClassAccess)
         return Ref.Ref_Type is
      begin
         return R:Ref_Type do
            PutAddr(Object.all'Address);
            R.I:=Object;
            Ref_Access(Object).Count:=Ref_Access(Object).Count+1;
         end return;
      end MakeRef;
      ------------------------------------------------------------------------

   end Ref;

end RefCount;
