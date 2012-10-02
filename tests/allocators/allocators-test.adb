pragma Ada_2012;

with FirstFitLinearAllocator;
with Ada.Numerics.Float_Random;
--with Ada.Text_IO; use Ada.Text_IO;

package body Allocators.Test is

   MinimumMemory      : constant:=4096;
   TaggedMemorySize   : constant:=16384;
   TaggedBlockCount   : constant:=128;
   TaggedMaxBlockSize : constant:=1024;

   procedure TestInvalidSizeRequest
     (Allocator : Allocator_ClassAccess) is
   begin

      Allocator.Init(1024);

      declare
         Dummy : Block_ClassAccess:=null;
      begin
         Dummy:=Allocator.Allocate(0);
         ReportIssue("Allocation of zero bytes returned without exception");
         if Dummy/=null then
            ReportIssue(" and even worse, returned something non null");
         end if;
      exception
         when InvalidSizeRequest =>
            null;
      end;

      declare
         Dummy : Block_ClassAccess:=null;
      begin
         Dummy:=Allocator.Allocate(1025);
         ReportIssue("Allocation of 1025 bytes >1024 present returned without exception");
         if Dummy/=null then
            ReportIssue(" and even worse, returned something non null");
         end if;
      exception
         when InvalidSizeRequest =>
            null;
      end;

   end TestInvalidSizeRequest;
   ---------------------------------------------------------------------------

   procedure TestValidSizeRequest
     (Allocator : Allocator_ClassAccess) is
   begin

      Allocator.Init(MinimumMemory);
      declare
         Valid : Block_ClassAccess:=null;
      begin
         Valid:=Allocator.Allocate(1);
         if Valid=null then
            ReportIssue("Could not allocate single byte (single)");
         else
            Allocator.Release(Valid);
         end if;
      end;

      declare
         Valid : Block_ClassAccess:=null;
      begin
         Valid:=Allocator.Allocate(MinimumMemory);
         if Valid=null then
            ReportIssue("Could not allocate entire memory (single)");
         else
            Allocator.Release(Valid);
         end if;
      end;

      declare
         Valid : Block_ClassAccess:=null;
      begin
         Valid:=Allocator.Allocate(MinimumMemory/2);
         if Valid=null then
            ReportIssue("Could not allocate 1/2 of the entire memory (single)");
         else
            Allocator.Release(Valid);
         end if;
      end;

      declare
         Valid1 : Block_ClassAccess:=null;
         Valid2 : Block_ClassAccess:=null;
      begin

         Valid1:=Allocator.Allocate(1);
         if Valid1=null then
            ReportIssue("Could not allocate first byte (double)");
         end if;

         Valid2:=Allocator.Allocate(1);
         if Valid2=null then
            ReportIssue("Could not allocate second byte (double)");
         end if;

         if Valid1/=null then
            Allocator.Release(Valid1);
         end if;

         if Valid2/=null then
            Allocator.Release(Valid2);
         end if;

      end;

      declare
         Valid1 : Block_ClassAccess:=null;
         Valid2 : Block_ClassAccess:=null;
      begin

         Valid1:=Allocator.Allocate(1);
         if Valid1=null then
            ReportIssue("Could not allocate first byte (double,2)");
         end if;

         Valid2:=Allocator.Allocate(1);
         if Valid2=null then
            ReportIssue("Could not allocate second byte (double,2)");
         end if;

         if Valid2/=null then
            Allocator.Release(Valid2);
         end if;

         if Valid1/=null then
            Allocator.Release(Valid1);
         end if;

      end;
   end TestValidSizeRequest;
   ---------------------------------------------------------------------------

   procedure TestMonteCarloAndTaggedMemory
     (Allocator : Allocator_ClassAccess) is

      use Ada.Numerics.Float_Random;

      Memory : array(PtrInt_Type range 0..TaggedMemorySize-1) of Natural         := (others=>0);
      Blocks : array(Natural range 1..TaggedBlockCount) of Block_ClassAccess := (others=>null);

      Gen : Ada.Numerics.Float_Random.Generator;
      CurrentBlock : Natural;
      Amount       : PtrInt_Type;

      procedure SetTags is
      begin

         for i in Blocks(CurrentBlock).Start..Blocks(CurrentBlock).Start+Blocks(CurrentBlock).Size-1 loop
            if Memory(i)/=0 then
               ReportIssue("Two memory regions occupy the same region"&PtrInt_Type'Image(i));
               return;
            end if;
            Memory(i):=CurrentBlock;
         end loop;

      end SetTags;
      ------------------------------------------------------------------------

      procedure RemoveTags is
      begin

         for i in Blocks(CurrentBlock).Start..Blocks(CurrentBlock).Start+Blocks(CurrentBlock).Size-1 loop
            if Memory(i)=0 then
               ReportIssue("Release memory block bytes which are not occupied");
               return;
            end if;
            Memory(i):=0;
         end loop;

      end RemoveTags;
      ------------------------------------------------------------------------

   begin

      Allocator.Init(MinimumMemory);

      Reset(Gen);

      for LoopNr in 1..100_000 loop
         -- Select block
         CurrentBlock:=Natural(Float'Rounding(Random(Gen)*Float(TaggedBlockCount-1)))+1;
         if Blocks(CurrentBlock)=null then

            Amount:=PtrInt_Type(Float'Rounding(Random(Gen)*Float(TaggedMaxBlockSize-1)))+1;
            Blocks(CurrentBlock):=Allocator.Allocate(Amount);
            if Blocks(CurrentBlock)/=null then
               SetTags;
            end if;

         else

            if Blocks(CurrentBlock)/=null then
               RemoveTags;
               Allocator.Release(Blocks(CurrentBlock));
               Blocks(CurrentBlock):=null;
            end if;

         end if;

      end loop;

   end TestMonteCarloAndTaggedMemory;

   -- TODO: Test double init, invalid init etc
   procedure TestFirstFitLinear is
   begin

      declare
         Allocator : FirstFitLinearAllocator.Allocator_Type;
      begin
         TestInvalidSizeRequest(Allocator'Unrestricted_Access);
      end;

      declare
         Allocator : FirstFitLinearAllocator.Allocator_Type;
      begin
         TestValidSizeRequest(Allocator'Unrestricted_Access);
      end;

      declare
         Allocator : FirstFitLinearAllocator.Allocator_Type;
      begin
         TestMonteCarloAndTaggedMemory(Allocator'Unrestricted_Access);
      end;

   end TestFirstFitLinear;
   ---------------------------------------------------------------------------

end Allocators.Test;
