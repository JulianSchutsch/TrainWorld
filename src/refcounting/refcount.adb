with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Basics; use Basics;

package body RefCount is

   package body Ref is

      type Ref_Access is access all Ref_Interface;

      procedure Adjust
        (Ref: in out Ref_Type) is
      begin
         if Ref.I/=null then
            Put_Line("Finalize, Inc");
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
            Put_Line("Finalize, Dec");
            Ref_Access(Ref.I).Count:=Ref_Access(Ref.I) .Count-1;
            if Ref_Access(Ref.I).Count=0 then
               Put_Line("Free Ref");
               Free(Ref.I);
               Put_Line("Freed");
            end if;
         end if;
      end Finalize;
      ------------------------------------------------------------------------

      function MakeRef
        (Object : Interface_ClassAccess)
         return Ref.Ref_Type is
      begin
         return R:Ref_Type do
            Put_Line("Create Ref");
            PutAddr(Object.all'Address);
            Put_Line("Conv");
            R.I:=Object;
            Put_Line("Inc Count");
            Ref_Access(Object).Count:=Ref_Access(Object).Count+1;
         end return;
      end MakeRef;
      ------------------------------------------------------------------------

   end Ref;

end RefCount;
