pragma Ada_2012;

with Ada.Tags; use Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;

package body OpenGL.TextureBuffer is

   procedure Finalize
     (TextureBufferRange : in out TextureBuffersRange_Type) is
   begin

--      Put_Line("TextureBufferRange.Finalize");
      if TextureBufferRange.Buffer/=null then
         TextureBufferRange.Buffer.Allocator.Release(TextureBufferRange.Block);
         TextureBufferRange.Buffers.DecrementRefCount;
         TextureBufferRange.Buffer:=null;
         TextureBufferRange.Buffers:=null;
      end if;

   end Finalize;
   ---------------------------------------------------------------------------

   procedure Bind
     (TextureBufferRange : in out TextureBuffersRange_Type) is
   begin
      BindTexture
        (target  => GL_TEXTURE_BUFFER,
         unit    => 0,
         texture => TextureBufferRange.Buffer.TextureID);
   end Bind;
   ---------------------------------------------------------------------------

   procedure Unmap
     (TextureBufferRange : in out TextureBuffersRange_Type) is
      pragma Unreferenced(TextureBufferRange);
   begin
      if glUnmapBuffer(GL_TEXTURE_BUFFER)=0 then
         raise FailedUnmap;
      end if;
      BindTextureBuffer(0);
   end Unmap;
   ---------------------------------------------------------------------------

   function Map
     (TextureBufferRange : in out TextureBuffersRange_Type)
      return System.Address is

      use type System.Address;

      Result : System.Address;

   begin

      -- TODO: Make aaccess an option
      BindTextureBuffer(TextureBufferRange.Buffer.BufferID);
      Result:=glMapBufferRange
        (target  => GL_TEXTURE_BUFFER,
         offset  => GLintptr_Type(TextureBufferRange.Block.Start),
         length  => GLsizeiptr_Type(TextureBufferRange.Block.Size),
         aaccess => GL_MAP_WRITE_BIT);
      if Result=System.Null_Address then
         raise FailedMap with "GLError:"&GLenum_Type'Image(glGetError.all);
      end if;
      return Result;

   end Map;
   ---------------------------------------------------------------------------

   procedure SetInternalFormat
     (TextureBuffers : in out TextureBuffers_Type;
      Format         : GLenum_Type) is
   begin
      -- TODO: This either requires a check or an update on all buffers
      TextureBuffers.Format:=Format;
   end SetInternalFormat;
   ---------------------------------------------------------------------------

   procedure SetBufferBlockSize
     (TextureBuffers : in out TextureBuffers_Type;
      Size           : PtrInt_Type) is
   begin
      pragma Assert(Size>0 and Size<=PtrInt_Type'Last);
      TextureBuffers.BufferSize:=Size;
   end SetBufferBlockSize;
   ---------------------------------------------------------------------------

   procedure Allocate
     (TextureBuffers : in out TextureBuffers_Type;
      Size           : PtrInt_Type;
      BufferRange    : in out TextureBuffersRange_Ref) is

      use type Allocators.Block_ClassAccess;

   begin

      pragma Assert(Size/=0,"Buffer range size too small");
      pragma Assert(Size<=TextureBuffers.BufferSize,"Buffer range size too large");

      declare
         Buffer      : TextureBuffersBuffer_Access:=TextureBuffers.FirstBuffer;
         BufferBlock : Allocators.Block_ClassAccess;
      begin

         while Buffer/=null loop
            BufferBlock:=Buffer.Allocator.Allocate(Size);
            if BufferBlock/=null then
               exit;
            end if;
            Buffer:=Buffer.Next;
         end loop;

         if BufferBlock=null then

            Put_Line("Create new Buffer");
            Buffer:=new TextureBuffersBuffer_Type;
            Buffer.Previous:=TextureBuffers.LastBuffer;
            if Buffer.Previous/=null then
               Buffer.Previous.Next:=Buffer;
            else
               TextureBuffers.FirstBuffer:=Buffer;
            end if;
            TextureBuffers.LastBuffer:=Buffer;

            -- TODO : This part must be updated to handle cases with exceptions!!!
            -- Generate Buffer
            glGenBuffers
              (n        => 1,
               buffers  => Buffer.BufferID'Access);

            Put_Line(GLuint_Type'Image(Buffer.BufferID));
            -- TODO : HANDLE BufferID=0

            -- Assign buffer size
            Put_Line("Bind Buffer");
            BindTextureBuffer(Buffer.BufferID);
            Put_Line("Buffer Bound?");
            glBufferData
              (target => GL_TEXTURE_BUFFER,
               size   => GLsizeiptr_Type(size),
               data   => System.Null_Address,
               usage  => GL_DYNAMIC_DRAW);
            Put_Line("Gen Tex");
            AssertError("Initialize TexBuffer Object 2");

            -- Generate Texture
            glGenTextures
              (n        => 1,
               textures => Buffer.TextureID'Access);
            -- TODO: Handle TextureID=0
            AssertError("Initialize TexBuffer Object 3");

            glBindTexture
              (target  => GL_TEXTURE_BUFFER,
               texture => Buffer.TextureID);
            AssertError("Initialize TexBuffer Object 4");

            glTexBuffer
              (target         => GL_TEXTURE_BUFFER,
               internalformat => TextureBuffers.Format,
               buffer         => Buffer.BufferID);
            AssertError("Initialize TexBuffer Object");

            Buffer.Allocator.Init(TextureBuffers.BufferSize);
            BufferBlock:=Buffer.Allocator.Allocate(Size);
            pragma Assert(BufferBlock/=null);
         end if;

         pragma Assert(Buffer/=null);

         -- Check if the buffer range is allocated and has the right tag
         if not (BufferRange.I/=null and then BufferRange.I'Tag=TextureBuffersRange_Type'Tag) then
            BufferRange:=TextureBuffersRangeRef.MakeInitialRef(new TextureBuffersRange_Type);
         end if;

         declare
            BufferRangeI : constant TextureBuffersRange_Access:=TextureBuffersRange_Access(BufferRange.I);
         begin

            TextureBuffers.IncrementRefCount;
            BufferRangeI.Buffer:=Buffer;
            BufferRangeI.Buffers:=TextureBuffers'Unrestricted_Access;
            BufferRangeI.Block:=Bufferblock;

         end;

      end;

   end Allocate;
   ---------------------------------------------------------------------------

   procedure ReleaseBuffer
     (TextureBuffers : in out TextureBuffers_Type'Class;
      Buffer         : TextureBuffersBuffer_Access) is
   begin
      pragma Assert(Buffer/=null);

      -- Unlink buffer
      if Buffer.Previous/=null then
         Buffer.Previous.Next:=Buffer.Next;
      else
         TextureBuffers.FirstBuffer:=Buffer.Next;
      end if;

      if Buffer.Next/=null then
         Buffer.Next.Previous:=Buffer.Previous;
      else
         TextureBuffers.LastBuffer:=Buffer.Previous;
      end if;

      pragma Assert(Buffer.TextureID/=0);
      pragma Assert(Buffer.BufferID/=0);

      Put_Line("Release Buffer");
      DeleteTexture(Buffer.TextureID);
      DeleteBuffer(Buffer.BufferID);

   end ReleaseBuffer;
   ---------------------------------------------------------------------------

   procedure Finalize
     (TextureBuffers : in out TextureBuffers_Type) is
   begin
--      Put_Line("TextureBuffers.Finalize");

      Put_Line("Finalize TextureBuffers");
      while TextureBuffers.FirstBuffer/=null loop
         ReleaseBuffer(TextureBuffers,TextureBuffers.FirstBuffer);
      end loop;

   end Finalize;
   ---------------------------------------------------------------------------

end OpenGL.TextureBuffer;
