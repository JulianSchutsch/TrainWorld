pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body OpenGL.TextureBuffer is

   procedure Bind
     (TextureBufferRange : in out TextureBuffersRange_Type) is
   begin
      glBindBuffer(GL_TEXTURE_BUFFER,TextureBufferRange.Buffer.BufferID);
      glBindTexture(GL_TEXTURE_BUFFER,TextureBufferRange.Buffer.TextureID);
   end Bind;
   ---------------------------------------------------------------------------

   procedure Unmap
     (TextureBufferRange : in out TextureBuffersRange_Type) is
      pragma Unreferenced(TextureBufferRange);
   begin
      if glUnmapBuffer(GL_TEXTURE_BUFFER)=0 then
         raise FailedUnmap;
      end if;
      glBindBuffer(GL_TEXTURE_BUFFER,0);
   end Unmap;
   ---------------------------------------------------------------------------

   function Map
     (TextureBufferRange : in out TextureBuffersRange_Type)
      return System.Address is

      use type System.Address;

      Result : System.Address;

   begin
      -- TODO: CHange BindBuffer to OGL BindBuffer-Cache variant.
      Put_Line(PtrInt_Type'Image(TextureBufferRange.Start)&".."&PtrInt_Type'Image(TextureBufferRange.Size));
      -- TODO: Make aaccess an option
      glBindBuffer(GL_TEXTURE_BUFFER,TextureBufferRange.Buffer.BufferID);
      Result:=glMapBufferRange
        (target  => GL_TEXTURE_BUFFER,
         offset  => GLintptr_Type(TextureBufferRange.Start),
         length  => GLsizeiptr_Type(TextureBufferRange.Size),
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

   function Allocate
     (TextureBuffers : in out TextureBuffers_Type;
      Size           : PtrInt_Type)
      return TextureBufferRange_Ref is

   begin
      -- 1. Does it make sense to compress the buffers now? Check ratios? This shall be omitted for now...
      -- 2. Is there enough space left for the range in any buffer?
      --    if not, does allocation of a new buffer solve this problem?
      --      no => raise exception
      --      yes => add buffer
      -- 3. What is the best place to place the new range?
      --    => In the buffer with the least amount of space left!
      if Size>TextureBuffers.BufferSize then
         raise BufferRangeTooLArge;
      end if;

      declare
         Buffer            : TextureBuffersBuffer_Access:=TextureBuffers.Buffers;
         MinimumFree       : PtrInt_Type:=PtrInt_Type'Last;
         MinimumFreeBuffer : TextureBuffersBuffer_Access:=null;
         BufferFree        : PtrInt_Type;
      begin

         -- Select buffer with smallest portion of remaining free space
         -- enough for the new buffer range.
         while Buffer/=null loop
            BufferFree:=TextureBuffers.BufferSize-Buffer.FillBytes;
            if BufferFree>=Size then
               if BufferFree<=MinimumFree then
                  MinimumFree:=BufferFree;
                  MinimumFreeBuffer:=Buffer;
               end if;
            end if;
            Buffer:=Buffer.Next;
         end loop;

         -- If nothing has been found, create new buffer
         if MinimumFreeBuffer=null then

            MinimumFreeBuffer:=new TextureBuffersBuffer_Type;
            MinimumFreeBuffer.Next:=TextureBuffers.Buffers;

            if TextureBuffers.Buffers/=null then
               TextureBuffers.Buffers.Previous:=MinimumFreeBuffer;
            end if;

            -- Generate Buffer
            glGenBuffers
              (n        => 1,
               buffers  => MinimumFreeBuffer.BufferID'Access);
            -- TODO: Replace Asserts by exceptions 2*
            pragma Assert(MinimumFreeBuffer.BufferID/=0);
            -- Temporary:
            glBindBuffer(GL_TEXTURE_BUFFER,MinimumFreeBuffer.BufferID);
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
               textures => MinimumFreeBuffer.TextureID'Access);
            pragma Assert(MinimumFreeBuffer.TextureID/=0);
            AssertError("Initialize TexBuffer Object 3");
            glBindTexture
              (target  => GL_TEXTURE_BUFFER,
               texture => MinimumFreeBuffer.TextureID);
            AssertError("Initialize TexBuffer Object 4");
            glTexBuffer
              (target         => GL_TEXTURE_BUFFER,
               internalformat => TextureBuffers.Format,
               buffer         => MinimumFreeBuffer.BufferID);
            glBindBuffer
              (target  => GL_TEXTURE_BUFFER,
               buffer => 0);
            AssertError("Initialize TexBuffer Object");

         end if;

         pragma Assert(MinimumFreeBuffer/=null);

         -- Add range to buffer
         declare
            NewRange : constant TextureBuffersRange_Access:=new TextureBuffersRange_Type;
         begin

            NewRange.Previous:=MinimumFreeBuffer.LastRange;
            if NewRange.Previous/=null then
               NewRange.Previous.Next:=NewRange;
            else
               MinimumFreeBuffer.FirstRange:=NewRange;
            end if;

            NewRange.Start  := MinimumFreeBuffer.FillBytes;
            NewRange.Size   := Size;
            NewRange.Buffer := MinimumFreeBuffer;

            MinimumFreeBuffer.FillBytes := MinimumFreeBuffer.FillBytes+Size;
            MinimumFreeBuffer.UsedBytes := MinimumFreeBuffer.UsedBytes+Size;

            return TextureBufferRangeRef.MakeNewRef(TextureBufferRange_ClassAccess(NewRange));

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
