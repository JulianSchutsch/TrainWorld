with OpenGL.Test; use OpenGL.Test;

package body OpenGL.TextureBuffer.Test is

   procedure TestAllocation is
   begin

      BindEvents;
      -- Automatic finalization
      declare
         Buffers     : TextureBuffers_Type;
         BufferRange : TextureBuffersRange_Ref;
      begin
         Buffers.SetBufferBlockSize(1024*1024);
         Buffers.Allocate(1024,BufferRange);
      end;

      -- Manual finalization for BufferRange
      declare
         Buffers     : TextureBuffers_Type;
         BufferRange : TextureBuffersRange_Ref;
      begin
         Buffers.SetBufferBlockSize(1024*1024);
         Buffers.Allocate(1024,BufferRange);
         BufferRange.SetNull;
      end;

      UnbindEvents;

   end TestAllocation;
   ---------------------------------------------------------------------------

end OpenGL.TextureBuffer.Test;
