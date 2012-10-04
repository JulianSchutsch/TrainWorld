pragma Ada_2012;

with Ada.Unchecked_Deallocation;

package body Allocators.FirstFitLinear is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Block_Type,
      Name   => Block_Access);

   procedure Finalize
     (Allocator : in out Allocator_Type) is
   begin

      pragma Assert(Allocator.Blocks=Allocator.FreeBlocks,"Possible remaining allocated blocks");
      if Allocator.FreeBlocks/=null then
         pragma Assert(Allocator.FreeBlocks.NextFree=null,"Unexpected second free block in Finalize");
         pragma Assert(Allocator.FreeBlocks.PreviousFree=null,"Unexpected 'previous' free");
         pragma Assert(Allocator.FreeBlocks.Next=null,"Unexpected 'next'");
         pragma Assert(Allocator.FreeBlocks.Previous=null,"Unexpected 'previous'");
         Free(Allocator.FreeBlocks);
      end if;

   end Finalize;
   ---------------------------------------------------------------------------

   procedure Init
     (Allocator : in out Allocator_Type;
      Size      : PtrInt_Type) is

      AllBlock : Block_Access renames Allocator.FreeBlocks;
   begin

      pragma Assert(Allocator.FreeBlocks=null,"Allocator initialized twice");
      pragma Assert(Allocator.Blocks=null,"Allocator initialized twice and all memory used");
      AllBlock:=new Block_Type;
      AllBlock.Start   := 0;
      AllBlock.Size    := Size;
      Allocator.Blocks := AllBlock;

      Allocator.ManagedAmount:=Size;
      pragma Assert(not AllBlock.Allocated);
      pragma Assert(Allocator.FreeBlocks.NextFree=null);
      pragma Assert(Allocator.FreeBlocks.PreviousFree=null);
      pragma Assert(Allocator.Blocks=Allocator.FreeBlocks);

   end Init;
   ---------------------------------------------------------------------------

   procedure RemoveFreeBlock
     (Allocator : in out Allocator_Type;
      Block     : Block_Access) is
   begin

      pragma Assert(not Block.Allocated);

      if Block.PreviousFree/=null then
         Block.PreviousFree.NextFree:=Block.NextFree;
      else
         Allocator.FreeBlocks:=Block.NextFree;
      end if;

      if Block.NextFree/=null then
         Block.NextFree.PreviousFree:=Block.PreviousFree;
      end if;

      Block.Allocated    := True;
      Block.PreviousFree := null;
      Block.NextFree     := null;

   end RemoveFreeBlock;
   ---------------------------------------------------------------------------

   function Allocate
     (Allocator : in out Allocator_Type;
      Size      : PtrInt_Type)
      return Allocators.Block_ClassAccess is

      Current : Block_Access:=Allocator.FreeBlocks;

   begin

      if (Size=0) or (Size>Allocator.ManagedAmount) then
         raise Allocators.InvalidSizeRequest;
      end if;

      while Current/=null loop

         if Current.Size>=Size then

            if Current.Size=Size then

               RemoveFreeBlock(Allocator,Current);
               Current.Allocated:=True;
               pragma Assert(Current.PreviousFree=null);
               pragma Assert(Current.NextFree=null);
               return Allocators.Block_ClassAccess(Current);


            else

               -- This new should be replaced by a special new
               declare
                  NewBlock : constant Block_Access:=new Block_Type;
               begin

                  NewBlock.Start     := Current.Start;
                  NewBlock.Size      := Size;
                  NewBlock.Allocated := True;
                  NewBlock.Previous  := Current.Previous;
                  NewBlock.Next      := Current;

                  pragma Assert(NewBlock.NextFree=null);
                  pragma Assert(NewBlock.PreviousFree=null);

                  if NewBlock.Previous/=null then
                     NewBlock.Previous.Next:=NewBlock;
                  else
                     Allocator.Blocks:=NewBlock;
                  end if;

                  Current.Previous := NewBlock;
                  Current.Start    := Current.Start+Size;
                  Current.Size     := Current.Size-Size;

                  return Allocators.Block_ClassAccess(NewBlock);

               end;

            end if;

         end if;

         Current:=Current.NextFree;

      end loop;

      return null;

   end Allocate;
   ---------------------------------------------------------------------------

   procedure Release
     (Allocator : in out Allocator_Type;
      Block     : in out Allocators.Block_ClassAccess) is

      Current  : Block_Access;
      Next     : Block_Access;
      NewStart : PtrInt_Type;
      NewSize  : PtrInt_Type;
      BlockA   : constant Block_Access:=Block_Access(Block);

      SumDelete : PtrInt_Type:=0;

   begin
      pragma Assert(BlockA/=null,"Release of null block");
      pragma Assert(BlockA.Allocated);
      pragma Assert(BlockA.PreviousFree=null);
      pragma Assert(BlockA.NextFree=null);

      -- Merge lower end
      Current:=BlockA.Previous;

      while Current/=null and then not Current.Allocated loop

         Next:=Current.Previous;
         if Next/=null then
            pragma Assert(Next.Start+Next.Size=Current.Start);
         end if;
         SumDelete:=SumDelete+Current.Size;
         -- It is possible to avoid this
         RemoveFreeBlock(Allocator,Current);
         Free(Current);
         Current:=Next;

      end loop;

      if Current/=null then
         Current.Next := BlockA;
         NewStart     := Current.Start+Current.Size;
      else
         Allocator.Blocks := BlockA;
         NewStart         := 0;
      end if;

      BlockA.Previous:=Current;

      -- Merge upper end
      Current:=BlockA.Next;

      while Current/=null and then not Current.Allocated loop
         Next := Current.Next;
         if Next/=null then
            pragma Assert(Next.Start=Current.Start+Current.Size);
         end if;
         SumDelete:=SumDelete+Current.Size;
         RemoveFreeBlock(Allocator,Current);
         Free(Current);
         Current := Next;
      end loop;

      if Current/=null then
         Current.Previous := BlockA;
         NewSize:=Current.Start-NewStart;
      else
         NewSize:=Allocator.ManagedAmount-NewStart;
      end if;

      BlockA.Next:=Current;

      pragma Assert(NewSize=SumDelete+BlockA.Size);
      -- Add new free block containing memory from all merged blocks
      BlockA.Start     := NewStart;
      BlockA.Size      := NewSize;
      BlockA.Allocated := False;

      BlockA.NextFree:=Allocator.FreeBlocks;
      if Allocator.FreeBlocks/=null then
         Allocator.FreeBlocks.PreviousFree:=BlockA;
      end if;
      Allocator.FreeBlocks:=BlockA;

      Block:=null;

   end Release;
   ---------------------------------------------------------------------------

end Allocators.FirstFitLinear;
