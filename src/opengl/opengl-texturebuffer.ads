pragma Ada_2012;

with RefCount;
with Basics; use Basics;

package OpenGL.TextureBuffer is

   BufferRangeTooLarge : Exception;
   FailedMap           : Exception;

   type TextureBufferRange_Interface is abstract new RefCount.Ref_Interface with null record;
   type TextureBufferRange_ClassAccess is access all TextureBufferRange_Interface'Class;

   not overriding
   function Map
     (TextureBufferRange : in out TextureBufferRange_Interface)
      return System.Address is abstract;

   package TextureBufferRangeRef is new RefCount.Ref(TextureBufferRange_Interface,TextureBufferRange_ClassAccess);

   subtype TextureBufferRange_Ref is TextureBufferRangeRef.Ref_Type;
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
   function Allocate
     (TextureBuffers : in out TextureBuffers_Interface;
      Size           : PtrInt_Type)
      return TextureBufferRange_Ref is abstract;

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
   function Allocate
     (TextureBuffers : in out TextureBuffers_Type;
      Size           : PtrInt_Type)
      return TextureBufferRange_Ref;

   overriding
   procedure Finalize
     (TextureBuffers : in out TextureBuffers_Type);

private

   type TextureBuffersRange_Type;
   type TextureBuffersRange_Access is access all TextureBuffersRange_Type;

   type TextureBuffersBuffer_Type;
   type TextureBuffersBuffer_Access is access all TextureBuffersBuffer_Type;

   type TextureBuffersRange_Type is new TextureBufferRange_Interface with
      record
         Buffer    : TextureBuffersBuffer_Access:=null;
         Start     : PtrInt_Type;
         Size      : PtrInt_Type;
         Next      : TextureBuffersRange_Access:=null;
         Previous  : TextureBuffersRange_Access:=null;
      end record;

   overriding
   function Map
     (TextureBufferRange : in out TextureBuffersRange_Type)
      return System.Address;

   type TextureBuffersBuffer_Type is
      record
         BufferID     : aliased GLuint_Type:=0;
         TextureID    : aliased GLuint_Type:=0;
         FirstRange   : TextureBuffersRange_Access:=null;
         LastRange    : TextureBuffersRange_Access:=null;
         UsedBytes    : PtrInt_Type:=0;
         FillBytes    : PtrInt_Type:=0;
         Next         : TextureBuffersBuffer_Access:=null;
         Previous     : TextureBuffersBuffer_Access:=null;
      end record;

   type TextureBuffers_Type is new TextureBuffers_Interface with
      record
         Buffers     : TextureBuffersBuffer_Access:=null;
         BufferSize  : PtrInt_Type:=0;
         Format      : GLenum_Type:=GL_RGBA8I;
      end record;

end OpenGL.TextureBuffer;
