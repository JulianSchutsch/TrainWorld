package body RefCounted is

   procedure Finalize
     (Ref: in out Ref_Type) is
   begin
      Ref.Count:=Ref.Count-1;
      if Ref.Count=0 then
         Destructor(Ref.Data);
      end if;
   end Finalize;
   ---------------------------------------------------------------------------

   procedure Adjust
     (Ref: in out Ref_Type) is
   begin
      Ref.Count:=Ref.Count+1;
   end Adjust;
   ---------------------------------------------------------------------------

end RefCounted;
