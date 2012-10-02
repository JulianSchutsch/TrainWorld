pragma Ada_2012;

with Basics; use Basics;

package Allocators is

   InvalidSizeRequest : Exception;

   type Block_Type is tagged record
      Start : PtrInt_Type; -- Readonly, used by implementation
      Size  : PtrInt_Type; -- Readonly, used by implementation
   end record;

   type Block_ClassAccess is access all Block_Type'Class;

   type Allocator_Interface is limited interface;
   type Allocator_ClassAccess is access all Allocator_Interface'Class;

   not overriding
   procedure Init
     (Allocator : in out Allocator_Interface;
      Size      : PtrInt_Type) is abstract;

   not overriding
   function Allocate
     (Allocator : in out Allocator_Interface;
      Size      : PtrInt_Type)
      return Block_ClassAccess is abstract;

   not overriding
   procedure Release
     (Allocator : in out Allocator_Interface;
      Block     : in out Block_ClassAccess) is abstract;
   ---------------------------------------------------------------------------

end Allocators;
