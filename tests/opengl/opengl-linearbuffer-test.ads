with TestFrameWork; use TestFrameWork;

package OpenGL.LinearBuffer.Test is

   procedure TestAllocation;
   procedure TestMonteCarlo;

   StrOpenGLTextureBufferAllocation : aliased constant String:="OpenGL.TextureBuffer.Allocation";
   StrOpenGLTextureBufferMonteCarlo : aliased constant String:="OpenGL.TextureBuffer.MonteCarlo";

   Tests : Test_Array:=
     ((Name=>RefConstStr(StrOpenGLTextureBufferAllocation'Access),
       Test=>TestAllocation'Access),
      (Name=>RefConstStr(StrOpenGLTextureBufferMonteCarlo'Access),
       Test=>TestMonteCarlo'Access));

end OpenGL.LinearBuffer.Test;
