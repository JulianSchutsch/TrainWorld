pragma Ada_2012;

with Ada.Unchecked_Deallocation;

package body GlobalLoop.Test is

   MaxProcs : constant := 4;

   Touched : array(1..MaxProcs) of Boolean;
   TouchedCount : Natural:=0;

   type Proc_Type is new Process_Type with
      record
         ID : Integer;
      end record;
   type Proc_Access is access all Proc_Type;

   procedure Process
     (P : in out Proc_Type) is
   begin
      Touched(P.ID):=True;
      TouchedCount:=TouchedCount+1;
   end Process;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Proc_Type,
      Name   => Proc_Access);

   Procs : array(1..MaxProcs) of Proc_Access:=(others => null);

   procedure CheckActive is
      ExpectedCount : Natural:=0;
   begin

      for b of Touched loop
         b:=False;
      end loop;

      TouchedCount:=0;

      Process;

      for i in Touched'Range loop

         if (Procs(i)/=null) and then (Procs(i).Enabled) then
            ExpectedCount:=ExpectedCount+1;
         end if;

         if Touched(i) xor Procs(i)/=null then
            ReportIssue("Process active/inactive which shouldn't : "&Integer'Image(i)&"::"&Boolean'Image(Touched(i))&"<->"&Boolean'Image(Procs(i)/=null));
         end if;

      end loop;

      if ExpectedCount/=TouchedCount then
         ReportIssue("Different number of processes active than should be, process leak?"&Natural'Image(TouchedCount)&" vs "&Natural'Image(ExpectedCount));
      end if;

   end CheckActive;
   ---------------------------------------------------------------------------

   procedure Create(Count : Integer) is
   begin
      for i in 1..Count loop
         Procs(i)    := new Proc_Type;
         Procs(i).Enable;
         Procs(i).ID := i;
      end loop;
   end Create;
   ---------------------------------------------------------------------------

   procedure Destroy(Index : Integer) is
   begin
      pragma Assert(Procs(Index)/=null);
      Free(Procs(Index));
      pragma Assert(Procs(Index)=null);
      for i in Index..Procs'Last-1 loop
         Procs(i):=Procs(i+1);
         if Procs(i)/=null then
            Procs(i).ID:=i;
         end if;
      end loop;
      Procs(Procs'Last):=null;
   end Destroy;
   ---------------------------------------------------------------------------

   procedure DestroySeq(Seq:Natural;Count : Natural) is
      C : Integer:=Seq;
   begin
      if Count>=1 then
         for i in reverse 1..Count loop
            pragma Assert(Procs(C mod i+Procs'First)/=null);
            Destroy(C mod i+Procs'First);
            CheckActive;
            C := C/i;
         end loop;
      end if;
      pragma Assert(Procs(Procs'First)=null);
   end DestroySeq;
   ---------------------------------------------------------------------------

   procedure TestSeq(Seq,Count : Integer) is
   begin
      CheckActive;
      Create(Count);
      CheckActive;
      DestroySeq(Seq,Count);
   end TestSeq;
   ---------------------------------------------------------------------------

   procedure TestCount(Count : Integer) is
      Fak : Integer:=1;
   begin
      for i in 2..Count loop
         Fak:=Fak*i;
      end loop;
      for i in 0..Fak-1 loop
         TestSeq(i,Count);
      end loop;
   end TestCount;
   ---------------------------------------------------------------------------

   procedure Test is
   begin
      for i in 1..4 loop
         TestCount(i);
      end loop;
   end Test;
   ---------------------------------------------------------------------------

end GlobalLoop.Test;
