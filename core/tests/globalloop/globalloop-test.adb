pragma Ada_2012;

with Ada.Unchecked_Deallocation;

package body GlobalLoop.Test is

   MaxProcs : constant := 4;

   Touched : array(1..MaxProcs) of Boolean;

   type Proc_Type is new Process_Type with
      record
         ID : Integer;
      end record;
   type Proc_Access is access all Proc_Type;

   procedure Process
     (P : in out Proc_Type) is
   begin
      Touched(P.ID):=True;
   end Process;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Proc_Type,
      Name   => Proc_Access);

   Procs : array(1..MaxProcs) of Proc_Access:=(others => null);

   procedure CheckActive is
   begin

      for b of Touched loop
         b:=False;
      end loop;

      Process;

      for i in Touched'Range loop
         if Touched(i) xor Procs(i)/=null then
            ReportIssue("Process active/inactive which shouldn't : "&Integer'Image(i));
         end if;
      end loop;

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
      Free(Procs(Index));
      for i in Index..Procs'Last-1 loop
         Procs(i):=Procs(i+1);
      end loop;
      Procs(Procs'Last):=null;
   end Destroy;
   ---------------------------------------------------------------------------

   procedure DestroySeq(Seq,Count : Integer) is
      C : Integer:=Seq;
   begin
      for i in Count..1 loop
         Destroy(C mod i+1);
         CheckActive;
         C:=C/i;
      end loop;
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
