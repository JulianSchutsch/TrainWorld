pragma Ada_2012;

with RefCount;
with Basics; use Basics;
with Allocators;
with Allocators.FirstFitLinear;

package OpenGL.LinearBuffer is

   FailedMap           : Exception;
   FailedUnmap         : Exception;
   FailedAllocate      : Exception;

   type LinearRange_Interface is abstract new RefCount.Ref_Interface with
      record
         AssocHeight : Integer;
         AssocWidth  : Integer;
         AssocOffset : PtrInt_Type;
      end record;
   type LinearRange_ClassAccess is access all LinearRange_Interface'Class;

   not overriding
   function Map
     (BufferRange : in out LinearRange_Interface)
      return System.Address is abstract;

   not overriding
   procedure Unmap
     (BufferRange : in out LinearRange_Interface) is abstract;

   not overriding
   function BindCompatible
     (BufferRange1 : in out LinearRange_Interface;
      BufferRange2 : in out LinearRange_Interface'Class)
      return Boolean is abstract;

   not overriding
   procedure Bind
     (BufferRange : in out LinearRange_Interface;
      Unit        : Natural) is abstract;

   package LinearRangeRef is new RefCount.Ref(LinearRange_Interface,LinearRange_ClassAccess);

   subtype LinearRange_Ref is LinearRangeRef.Ref_Type;
   ---------------------------------------------------------------------------

   type LinearRange_Type is new LinearRange_Interface with private;
   ---------------------------------------------------------------------------

   type LinearBuffers_Interface is abstract new RefCount.Ref_Interface with null record;
   type LinearBuffers_ClassAccess is access all LinearBuffers_Interface'Class;

   not overriding
   procedure SetInternalFormat
     (Buffers : in out LinearBuffers_Interface;
      Format  : GLenum_Type) is abstract;

   not overriding
   procedure SetBufferBlockSize
     (Buffers : in out LinearBuffers_Interface;
      Amount  : PtrInt_Type) is abstract;

   not overriding
   procedure Allocate
     (Buffers     : in out LinearBuffers_Interface;
      Amount      : PtrInt_Type;
      BufferRange : in out LinearRange_Ref) is abstract;

   -- Allocates without changing the reference pointer
   not overriding
   procedure AllocateConst
     (Buffers     : in out LinearBuffers_Interface;
      Amount      : PtrInt_Type;
      BufferRange : LinearRange_ClassAccess) is abstract;

   package LinearBuffersRef is new RefCount.Ref(LinearBuffers_Interface,LinearBuffers_ClassAccess);
   ---------------------------------------------------------------------------

   type LinearBuffers_Type is new LinearBuffers_Interface with private;

   overriding
   procedure SetInternalFormat
     (Buffers : in out LinearBuffers_Type;
      Format  : GLenum_Type);

   overriding
   procedure SetBufferBlockSize
     (Buffers : in out LinearBuffers_Type;
      Size    : PtrInt_Type);

   overriding
   procedure Allocate
     (Buffers     : in out LinearBuffers_Type;
      Amount      : PtrInt_Type;
      BufferRange : in out LinearRange_Ref);

   overriding
   procedure AllocateConst
     (Buffers     : in out LinearBuffers_Type;
      Amount      : PtrInt_Type;
      BufferRange : LinearRange_ClassAccess);

   overriding
   procedure Finalize
     (Buffers : in out LinearBuffers_Type);

private

   type LinearRange_Access is access all LinearRange_Type;

   type LinearBuffer_Type;
   type LinearBuffer_Access is access all LinearBuffer_Type;

   type LinearBuffers_Access is access all LinearBuffers_Type;

   type LinearRange_Type is new LinearRange_Interface with
      record
         Buffer    : LinearBuffer_Access  := null;
         Block     : Allocators.Block_ClassAccess := null;
         Buffers   : LinearBuffers_Access        := null;
      end record;

   overriding
   function Map
     (BufferRange : in out LinearRange_Type)
      return System.Address;

   overriding
   procedure Unmap
     (BufferRange : in out LinearRange_Type);

   overriding
   procedure Bind
     (BufferRange : in out LinearRange_Type;
      Unit        : Natural);

   overriding
   function BindCompatible
     (BufferRange1 : in out LinearRange_Type;
      BufferRange2 : in out LinearRange_Interface'Class)
      return Boolean;

   overriding
   procedure Finalize
     (BufferRange : in out LinearRange_Type);
   ---------------------------------------------------------------------------

   type LinearBuffer_Type is
      record
         BufferID     : aliased GLuint_Type:=0;
         TextureID    : aliased GLuint_Type:=0;
         Next         : LinearBuffer_Access:=null;
         Previous     : LinearBuffer_Access:=null;
         Allocator    : Allocators.FirstFitLinear.Allocator_Type;
      end record;

   type LinearBuffers_Type is new LinearBuffers_Interface with
      record
         FirstBuffer : LinearBuffer_Access:=null;
         LastBuffer  : LinearBuffer_Access:=null;
         BufferSize  : PtrInt_Type:=0;
         Format      : GLenum_Type:=GL_RGBA8;
      end record;

end OpenGL.LinearBuffer;
