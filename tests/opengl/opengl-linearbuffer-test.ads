with TestFrameWork; use TestFrameWork;

package OpenGL.LinearBuffer.Test is

   procedure TestAllocation;
   procedure TestMonteCarlo;

   Tests : Test_Array:=
     ((Name=>U("OpenGL.TextureBuffer.Allocation"),
       Test=>TestAllocation'Access),
      (Name=>U("OpenGL.TextureBuffer.MonteCarlo"),
       Test=>TestMonteCarlo'Access));

end OpenGL.LinearBuffer.Test;
