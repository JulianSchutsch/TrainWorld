pragma Ada_2012;

with Ada.Tags; use Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;

package body OpenGL.LinearBuffer is

   procedure Finalize
     (BufferRange : in out LinearRange_Type) is
   begin

--      Put_Line("TextureBufferRange.Finalize");
      if BufferRange.Buffer/=null then
         BufferRange.Buffer.Allocator.Release(BufferRange.Block);
         BufferRange.Buffers.DecrementRefCount;
         BufferRange.Buffer:=null;
         BufferRange.Buffers:=null;
      end if;

   end Finalize;
   ---------------------------------------------------------------------------

   procedure Bind
     (BufferRange : in out LinearRange_Type) is
   begin
      BindTexture
        (target  => GL_TEXTURE_BUFFER,
         unit    => 0,
         texture => BufferRange.Buffer.TextureID);
   end Bind;
   ---------------------------------------------------------------------------

   procedure Unmap
     (BufferRange : in out LinearRange_Type) is
      pragma Unreferenced(BufferRange);
   begin
      if glUnmapBuffer(GL_TEXTURE_BUFFER)=0 then
         raise FailedUnmap;
      end if;
      BindTextureBuffer(0);
   end Unmap;
   ---------------------------------------------------------------------------

   function Map
     (BufferRange : in out LinearRange_Type)
      return System.Address is

      use type System.Address;

      Result : System.Address;

   begin

      -- TODO: Make aaccess an option
      BindTextureBuffer(BufferRange.Buffer.BufferID);
      Result:=glMapBufferRange
        (target  => GL_TEXTURE_BUFFER,
         offset  => GLintptr_Type(BufferRange.Block.Start),
         length  => GLsizeiptr_Type(BufferRange.Block.Size),
         aaccess => GL_MAP_WRITE_BIT);
      if Result=System.Null_Address then
         raise FailedMap with "GLError:"&GLenum_Type'Image(glGetError.all);
      end if;
      return Result;

   end Map;
   ---------------------------------------------------------------------------

   procedure SetInternalFormat
     (Buffers : in out LinearBuffers_Type;
      Format  : GLenum_Type) is
   begin
      -- TODO: This either requires a check or an update on all buffers
      Buffers.Format:=Format;
   end SetInternalFormat;
   ---------------------------------------------------------------------------

   procedure SetBufferBlockSize
     (Buffers : in out LinearBuffers_Type;
      Size           : PtrInt_Type) is
   begin
      pragma Assert(Size>0 and Size<=PtrInt_Type'Last);
      Buffers.BufferSize:=Size;
   end SetBufferBlockSize;
   ---------------------------------------------------------------------------

   procedure ReleaseBuffer
     (Buffers : in out LinearBuffers_Type'Class;
      Buffer         : LinearBuffer_Access) is
   begin
      pragma Assert(Buffer/=null);

      -- Unlink buffer
      if Buffer.Previous/=null then
         Buffer.Previous.Next:=Buffer.Next;
      else
         Buffers.FirstBuffer:=Buffer.Next;
      end if;

      if Buffer.Next/=null then
         Buffer.Next.Previous:=Buffer.Previous;
      else
         Buffers.LastBuffer:=Buffer.Previous;
      end if;

--      pragma Assert(Buffer.TextureID/=0);
--      pragma Assert(Buffer.BufferID/=0);

      if Buffer.TextureID/=0 then
         DeleteTexture(Buffer.TextureID);
      end if;
      if Buffer.BufferID/=0 then
         DeleteBuffer(Buffer.BufferID);
      end if;

   end ReleaseBuffer;
   ---------------------------------------------------------------------------

   procedure Allocate
     (Buffers : in out LinearBuffers_Type;
      Size           : PtrInt_Type;
      BufferRange    : in out LinearRange_Ref) is

      use type Allocators.Block_ClassAccess;

   begin

      pragma Assert(Size/=0,"Buffer range size too small");
      pragma Assert(Size<=Buffers.BufferSize,"Buffer range size too large");

      declare
         Buffer      : LinearBuffer_Access:=Buffers.FirstBuffer;
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

            Buffer          := new LinearBuffer_Type;
            Buffer.Previous := Buffers.LastBuffer;
            if Buffer.Previous/=null then
               Buffer.Previous.Next:=Buffer;
            else
               Buffers.FirstBuffer:=Buffer;
            end if;
            Buffers.LastBuffer:=Buffer;

            glGenBuffers
              (n        => 1,
               buffers  => Buffer.BufferID'Access);

            if Buffer.BufferID=0 then
               ReleaseBuffer(Buffers,Buffer);
               raise FailedAllocate with "Couldn't allocate buffer with glGenBuffers";
            end if;

            -- Assign buffer size
            BindTextureBuffer(Buffer.BufferID);
            glBufferData
              (target => GL_TEXTURE_BUFFER,
               size   => GLsizeiptr_Type(size),
               data   => System.Null_Address,
               usage  => GL_DYNAMIC_DRAW);
            -- TODO: Check if OpenGL could allocate enough memory for this at all
            AssertError("Initialize TexBuffer Object 2");

            -- Generate Texture
            glGenTextures
              (n        => 1,
               textures => Buffer.TextureID'Access);
            if Buffer.TextureID=0 then
               ReleaseBuffer(Buffers,Buffer);
               raise FailedAllocate with "Couldn't allocate texture with glGenTextures";
            end if;
            -- TODO: Handle TextureID=0
            AssertError("Initialize TexBuffer Object 3");

            glBindTexture
              (target  => GL_TEXTURE_BUFFER,
               texture => Buffer.TextureID);
            AssertError("Initialize TexBuffer Object 4");

            glTexBuffer
              (target         => GL_TEXTURE_BUFFER,
               internalformat => Buffers.Format,
               buffer         => Buffer.BufferID);
            AssertError("Initialize TexBuffer Object");

            Buffer.Allocator.Init(Buffers.BufferSize);
            BufferBlock:=Buffer.Allocator.Allocate(Size);
            pragma Assert(BufferBlock/=null);
         end if;

         pragma Assert(Buffer/=null);

         -- Check if the buffer range is allocated and has the right tag
         if not (BufferRange.I/=null and then BufferRange.I'Tag=LinearRange_Type'Tag) then
            BufferRange:=LinearRangeRef.MakeInitialRef(new LinearRange_Type);
         end if;

         declare
            BufferRangeI : constant LinearRange_Access:=LinearRange_Access(BufferRange.I);
         begin

            Buffers.IncrementRefCount;
            BufferRangeI.Buffer  := Buffer;
            BufferRangeI.Buffers := Buffers'Unrestricted_Access;
            BufferRangeI.Block   := Bufferblock;

         end;

      end;

   end Allocate;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Buffers : in out LinearBuffers_Type) is
   begin
--      Put_Line("TextureBuffers.Finalize");

      Put_Line("Finalize TextureBuffers");
      while Buffers.FirstBuffer/=null loop
         ReleaseBuffer(Buffers,Buffers.FirstBuffer);
      end loop;

   end Finalize;
   ---------------------------------------------------------------------------

end OpenGL.LinearBuffer;
