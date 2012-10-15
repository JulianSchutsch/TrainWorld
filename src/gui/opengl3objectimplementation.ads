pragma Ada_2012;

with Config;
with OpenGL.LinearBuffer;
with OpenGL.Program;
with OpenGL; use OpenGL;
with GUI;
with Types;

package OpenGL3ObjectImplementation is

   procedure Register;

private

   type OGL3ImplConfig_Type is new Config.Config_Type with
      record
         null;
      end record;
   type OGL3ImplConfig_Access is access all OGL3ImplConfig_Type;

   type Rect_Type is
      record
         Offset : Types.Cardinal32;
         Height : Types.Cardinal32;
         Width  : Types.Cardinal32;
         Res1   : Types.Cardinal32;
         --
         Vertx      : Types.Cardinal32;
         Verty      : Types.Cardinal32;
         VertHeight : Types.Cardinal32;
         VertWidth  : Types.Cardinal32;
         --
         Texx      : Types.Cardinal32;
         Texy      : Types.Cardinal32;
         TexHeight : Types.Cardinal32;
         TexWidth  : Types.Cardinal32;
      end record;
   pragma Convention(C,Rect_Type);
   type Rect_Access is access all Rect_Type;
   pragma No_Strict_Aliasing(Rect_Access);

   type OGL3Impl_Type is abstract new GUI.GUIObjectImplementation_Interface with
      record
         TexBuffers         : OpenGL.LinearBuffer.LinearBuffers_Type;
         RectBuffers        : OpenGL.LinearBuffer.LinearBuffers_Type;
         RectBuffer         : aliased OpenGL.LinearBuffer.LinearRange_Type;
         RectMap            : Rect_Access:=null;
         RectPos            : Rect_Access:=null;
         RectMapFill        : Natural:=0;
         CurrentBufferRange : OpenGL.LinearBuffer.LinearRange_ClassAccess:=null;
         FragmentShader     : aliased OpenGL.Program.Shader_Type;
         VertexShader       : aliased OpenGL.Program.Shader_Type;
         Program            : OpenGL.Program.Program_Type;
         VertBuffer         : aliased GLuint_Type;
         VertArray          : aliased GLuint_Type;
         OrthoMatrixUniform : aliased GLint_Type;
         TexBufferUniform   : aliased GLint_Type;
         RectBufferUniform  : aliased GLint_Type;
      end record;
   type OGL3Impl_ClassAccess is access all OGL3Impl_Type'Class;

   not overriding
   procedure PushRect
     (Impl        : in out OGL3Impl_Type;
      BufferRange : OpenGL.LinearBuffer.LinearRange_ClassAccess;
      Vertx       : Integer;
      Verty       : Integer;
      VertHeight  : Integer;
      VertWidth   : Integer;
      Texx        : Integer;
      Texy        : Integer;
      TexHeight   : Integer;
      TexWidth    : Integer);

   not overriding
   procedure FlushPaint
     (Impl : in out OGL3Impl_Type);

end OpenGL3ObjectImplementation;
