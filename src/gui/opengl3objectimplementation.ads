pragma Ada_2012;

with Config;
with OpenGL.LinearBuffer;
with GUI;

package OpenGL3ObjectImplementation is

   procedure Register;

private

   type OGL3ImplConfig_Type is new Config.Config_Type with
      record
         null;
      end record;
   type OGL3ImplConfig_Access is access all OGL3ImplConfig_Type;

   type OGL3Impl_Type is abstract new GUI.GUIObjectImplementation_Interface with
      record
         Buffers  : OpenGL.LinearBuffer.LinearBuffers_Type;
      end record;
   type OGL3Impl_Access is access all OGL3Impl_Type;

end OpenGL3ObjectImplementation;
