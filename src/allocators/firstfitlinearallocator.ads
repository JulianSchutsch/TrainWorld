pragma Ada_2012;

with Basics; use basics;
with Ada.Finalization;
with Allocators;

package FirstFitLinearAllocator is

   type Allocator_Type is new Ada.Finalization.Limited_Controlled and Allocators.Allocator_Interface with private;

   procedure Init
     (Allocator : in out Allocator_Type;
      Size      : PtrInt_Type);

   function Allocate
     (Allocator : in out Allocator_Type;
      Size      : PtrInt_Type)
      return Allocators.Block_ClassAccess;

   procedure Release
     (Allocator : in out Allocator_Type;
      Block     : in out Allocators.Block_ClassAccess);

private

   type Block_Type;
   type Block_Access is access all Block_Type;
   type Block_Type is new Allocators.Block_Type with
      record
         Allocated     : Boolean:=False;
         PreviousFree  : Block_Access:=null;
         NextFree      : Block_Access:=null;
         Next          : Block_Access:=null;
         Previous      : Block_Access:=null;
      end record;

   type Allocator_Type is new Ada.Finalization.Limited_Controlled and Allocators.Allocator_Interface with
      record
         Blocks     : Block_Access:=null;
         FreeBlocks : Block_Access:=null;
         ManagedAmount : PtrInt_Type:=0;
      end record;

end FirstFitLinearAllocator;
