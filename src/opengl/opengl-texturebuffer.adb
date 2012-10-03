pragma Ada_2012;

with Ada.Tags; use Ada.Tags;

package body OpenGL.TextureBuffer is

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

            -- TODO : HANDLE BufferID=0

            -- Temporary:???
            BindTextureBuffer(Buffer.BufferID);

            AssertError("Initialize TexBuffer Object 1");

            glBufferData
              (target => GL_TEXTURE_BUFFER,
               size   => GLsizeiptr_Type(size),
               data   => System.Null_Address,
               usage  => GL_DYNAMIC_DRAW);
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
            BufferRange:=TextureBuffersRangeRef.MakeNewRef(new TextureBuffersRange_Type);
         end if;

         declare
            BufferRangeI : constant TextureBuffersRange_Access:=TextureBuffersRange_Access(BufferRange.I);
         begin

            BufferRangeI.Buffer:=Buffer;
            BufferRangeI.Block:=Bufferblock;

         end;

      end;

   end Allocate;
   ---------------------------------------------------------------------------

   procedure Finalize
     (TextureBuffers : in out TextureBuffers_Type) is
   begin
      -- TODO: Cleanup the mess
      null;
   end Finalize;
   ---------------------------------------------------------------------------

end OpenGL.TextureBuffer;
