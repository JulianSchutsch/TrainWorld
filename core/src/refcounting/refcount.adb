with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

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
               Free(Ref.I);
            end if;
         end if;
      end Finalize;
      ------------------------------------------------------------------------

   end Ref;

end RefCount;
