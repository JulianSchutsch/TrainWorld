with TestFrameWork; use TestFrameWork;

package OpenGL.TextureBuffer.Test is

   procedure TestAllocation;

   Tests : Test_Array:=
     (0=>(Name=>U("OpenGL.TextureBuffer.Allocation"),
          Test=>TestAllocation'Access));

end OpenGL.TextureBuffer.Test;
