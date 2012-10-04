pragma Ada_2012;

with RefCount;
with Basics; use Basics;
with Allocators;
with Allocators.FirstFitLinear;

package OpenGL.TextureBuffer is

   FailedMap           : Exception;
   FailedUnmap         : Exception;

   type TextureBuffersRange_Interface is abstract new RefCount.Ref_Interface with null record;
   type TextureBuffersRange_ClassAccess is access all TextureBuffersRange_Interface'Class;

   not overriding
   function Map
     (TextureBufferRange : in out TextureBuffersRange_Interface)
      return System.Address is abstract;

   not overriding
   procedure Unmap
     (TextureBufferRange : in out TextureBuffersRange_Interface) is abstract;

   not overriding
   procedure Bind
     (TextureBufferRange : in out TextureBuffersRange_Interface) is abstract;

   package TextureBuffersRangeRef is new RefCount.Ref(TextureBuffersRange_Interface,TextureBuffersRange_ClassAccess);

   subtype TextureBuffersRange_Ref is TextureBuffersRangeRef.Ref_Type;
   ---------------------------------------------------------------------------

   type TextureBuffers_Interface is abstract new RefCount.Ref_Interface with null record;
   type TextureBuffers_ClassAccess is access all TextureBuffers_Interface'Class;

   not overriding
   procedure SetInternalFormat
     (TextureBuffers : in out TextureBuffers_Interface;
      Format         : GLenum_Type) is abstract;

   not overriding
   procedure SetBufferBlockSize
     (TextureBuffers : in out TextureBuffers_Interface;
      Size           : PtrInt_Type) is abstract;

   not overriding
   procedure Allocate
     (TextureBuffers : in out TextureBuffers_Interface;
      Size           : PtrInt_Type;
      BufferRange    : in out TextureBuffersRange_Ref) is abstract;

   package TextureBuffersRef is new RefCount.Ref(TextureBuffers_Interface,TextureBuffers_ClassAccess);
   ---------------------------------------------------------------------------

   type TextureBuffers_Type is new TextureBuffers_Interface with private;

   overriding
   procedure SetInternalFormat
     (TextureBuffers : in out TextureBuffers_Type;
      Format         : GLenum_Type);

   overriding
   procedure SetBufferBlockSize
     (TextureBuffers : in out TextureBuffers_Type;
      Size           : PtrInt_Type);

   overriding
   procedure Allocate
     (TextureBuffers : in out TextureBuffers_Type;
      Size           : PtrInt_Type;
      BufferRange    : in out TextureBuffersRange_Ref);

   overriding
   procedure Finalize
     (TextureBuffers : in out TextureBuffers_Type);

private

   type TextureBuffersRange_Type;
   type TextureBuffersRange_Access is access all TextureBuffersRange_Type;

   type TextureBuffersBuffer_Type;
   type TextureBuffersBuffer_Access is access all TextureBuffersBuffer_Type;

   type TextureBuffers_Access is access all TextureBuffers_Type;

   type TextureBuffersRange_Type is new TextureBuffersRange_Interface with
      record
         Buffer    : TextureBuffersBuffer_Access  := null;
         Block     : Allocators.Block_ClassAccess := null;
         Buffers   : TextureBuffers_Access        := null;
      end record;

   overriding
   function Map
     (TextureBufferRange : in out TextureBuffersRange_Type)
      return System.Address;

   overriding
   procedure Unmap
     (TextureBufferRange : in out TextureBuffersRange_Type);

   overriding
   procedure Bind
     (TextureBufferRange : in out TextureBuffersRange_Type);

   overriding
   procedure Finalize
     (TextureBufferRange : in out TextureBuffersRange_Type);
   ---------------------------------------------------------------------------

   type TextureBuffersBuffer_Type is
      record
         BufferID     : aliased GLuint_Type:=0;
         TextureID    : aliased GLuint_Type:=0;
         Next         : TextureBuffersBuffer_Access:=null;
         Previous     : TextureBuffersBuffer_Access:=null;
         Allocator    : Allocators.FirstFitLinear.Allocator_Type;
      end record;

   type TextureBuffers_Type is new TextureBuffers_Interface with
      record
         FirstBuffer : TextureBuffersBuffer_Access:=null;
         LastBuffer  : TextureBuffersBuffer_Access:=null;
         BufferSize  : PtrInt_Type:=0;
         Format      : GLenum_Type:=GL_RGBA8;
      end record;

end OpenGL.TextureBuffer;
