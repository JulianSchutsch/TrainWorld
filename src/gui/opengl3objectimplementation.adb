pragma Ada_2012;

with Graphics;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GUIImplInterface_Window;
with OpenGL3ObjectImplementation.Window;
with Ada.Unchecked_Conversion;
with System;
with Basics; use Basics;

package body OpenGL3ObjectImplementation is

   function Conv is new Ada.Unchecked_Conversion
     (Source => System.Address,
      Target => Rect_Access);

   RectBufferElements : constant:=1024*1024;
   RectBufferSize     : constant:=RectBufferElements*3*4*4; -- 2 * GL_RGBA32UI, each holding 4 entries of DWORD

   VertexShaderSource : constant String:=
     "#version 140"&Character'Val(10)&
     "in vec2 in_Position;"&
     "uniform usamplerBuffer data;"&
     "uniform mat4 OrthoMatrix;"&
     "flat out uint ex_Offset;"&
     "flat out uint ex_Width;"&
     "flat out uint ex_TexHeight;"&
     "flat out uint ex_TexWidth;"&
     "out float ex_Texx;"&
     "out float ex_Texy;"&
     "void main(void)"&
     "{"&
     "  uvec4 texdata = texelFetch(data,gl_InstanceID*3);"&
     "  uvec4 visdim  = texelFetch(data,gl_InstanceID*3+1);"&
     "  uvec4 texdim  = texelFetch(data,gl_InstanceID*3+2);"&
     "  uint height   = texdata.y;"&
     "  uint width    = texdata.z;"&
     "  ex_Offset     = texdata.x;"&
     "  ex_Width      = width;"&
     "  ex_Texx       = (in_Position.x*(texdim.w)+texdim.x)/width;"&
     "  ex_Texy       = (in_Position.y*(texdim.z)+texdim.y)/height;"&
     "  gl_Position   = OrthoMatrix*vec4(float(in_Position.x*visdim.w+visdim.x),"&
     "                                   float(in_Position.y*visdim.z+visdim.y),"&
     "                                   0.0,1.0);"&
     "}"&Character'Val(0);

   FragmentShaderSource : constant String:=
     "#version 140"&Character'Val(10)&
     "flat in uint ex_Offset;"&
     "flat in uint ex_Width;"&
     "flat in uint ex_TexHeight;"&
     "flat in uint ex_TexWidth;"&
     "in float ex_Texx;"&
     "in float ex_Texy;"&
     "out vec4 out_Color;"&
     "uniform samplerBuffer tex;"&
     "void main(void)"&
     "{"&
     "  int Texx = int((ex_Texx-floor(ex_Texx))*ex_TexWidth);"&
     "  int Texy = int((ex_Texy-floor(ex_Texy))*ex_TexHeight);"&
     "  out_Color=texelFetch(tex,int(ex_Offset)+Texx+Texy*int(ex_Width));"&
--     "  out_Color=vec4(1.0,1.0,1.0,1.0);"&
     "}"&Character'Val(0);

   VertData : GLfloat_Array(0..7):=
     (0.0,0.0,
      1.0,0.0,
      0.0,1.0,
      1.0,1.0);

   type OpenGL3OI_Type is new OGL3Impl_Type with
      record
         WindowData : aliased OpenGL3ObjectImplementation.Window.WindowImpl_Data;
      end record;
   type OpenGL3OI_Access is access all OpeNGL3OI_Type'Class;

   overriding
   function CreateWindowImpl
     (ObjectImplementation : in out OpenGL3OI_Type)
      return GUIImplInterface_Window.WindowImpl_ClassAccess;

   overriding
   procedure DestroyWindowImpl
     (ObjectImplementation : in out OpenGL3OI_Type;
      WindowImpl           : in out GUIImplInterface_Window.WindowImpl_ClassAccess);

   overriding
   procedure StartPainting
     (ObjectImplementation : in out OpenGL3OI_Type);

   overriding
   procedure StopPainting
     (ObjectImplementation : in out OpenGL3OI_Type);

   overriding
   procedure Resize
     (ObjectImplementation : in out OpenGL3OI_Type;
      Height               : Natural;
      Width                : Natural);

   overriding
   procedure Finalize
     (ObjectImplementation : in out OpenGL3OI_Type);
   ---------------------------------------------------------------------------

   procedure FlushPaint
     (Impl : in out OGL3Impl_Type) is
   begin

      if Impl.RectMapFill=0 then
         return;
      end if;
--      Put_Line("Draw Rect:"&Integer'Image(Impl.RectMapFill));

      Impl.CurrentBufferRange.Bind(1);
      glDrawArraysInstanced(GL_TRIANGLE_STRIP,0,4,GLint_Type(Impl.RectMapFill));

      Impl.RectMapFill := 0;
      Impl.RectPos     := Impl.RectMap;
      Impl.CurrentBufferRange := null;


   end FlushPaint;
   ---------------------------------------------------------------------------

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
      TexWidth    : Integer) is

      use type OpenGL.LinearBuffer.LinearRange_ClassAccess;

      function "+" is new Add
        (Data_Type   => Rect_Type,
         Data_Access => Rect_Access);

   begin

      pragma Assert(Impl.RectMap/=null,"PushRect only permitted between StartPainting and StopPainting");
      if Impl.RectMapFill=RectBufferElements then
         Impl.FlushPaint;
      end if;
      if Impl.CurrentBufferRange/=null then
         if not Impl.CurrentBufferRange.BindCompatible(BufferRange.all) then
            Impl.FlushPaint;
         end if;
      end if;

      Impl.CurrentBufferRange:=BufferRange;

      Impl.RectPos.Offset     := Types.Cardinal32(BufferRange.AssocOffset);
      Impl.RectPos.Height     := Types.Cardinal32(BufferRange.AssocHeight);
      Impl.RectPos.Width      := Types.Cardinal32(BufferRange.AssocWidth);
      Impl.RectPos.Vertx      := Types.Cardinal32(Vertx);
      Impl.RectPos.Verty      := Types.Cardinal32(Verty);
      Impl.RectPos.VertHeight := Types.Cardinal32(VertHeight);
      Impl.RectPos.VertWidth  := Types.Cardinal32(VertWidth);
      Impl.RectPos.Texx       := Types.Cardinal32(Texx);
      Impl.RectPos.Texy       := Types.Cardinal32(Texy);
      Impl.RectPos.TexHeight  := Types.Cardinal32(TexHeight);
      Impl.RectPos.TexWidth   := Types.Cardinal32(TexWidth);
--      Put_Line(Types.Cardinal32'Image(Impl.RectPos.VertHeight));
--      Put_Line(Types.Cardinal32'Image(Impl.RectPos.VertWidth));
      Impl.RectPos            := Impl.RectPos+1;
      Impl.RectMapFill        := Impl.RectMapFill+1;

   end PushRect;
   ---------------------------------------------------------------------------

   procedure Resize
     (ObjectImplementation : in out OpenGL3OI_Type;
      Height               : Natural;
      Width                : Natural) is
   begin

      Put_Line("Impl.Resize"&Natural'Image(Height)&":"&Natural'Image(Width));
      glViewport(0,0,GLsizei_Type(Width),GLsizei_Type(Height));
      ObjectImplementation.Program.UseProgram;
      declare
         Matrix : GLFloat_Matrix4x4;
      begin
         Matrix:=OrthoMatrix
           (Left   => 0.0,
            Right  => GLfloat_Type(Width),
            Bottom => GLfloat_Type(Height),
            Top    => 0.0);
         glUniformMatrix4fv
           (location  => ObjectImplementation.OrthoMatrixUniform,
            count     => 1,
            transpose => 1,
            value     => Matrix(0,0)'Access);
      end;

   end Resize;
   ---------------------------------------------------------------------------

   procedure StartPainting
     (ObjectImplementation : in out OpenGL3OI_Type) is
   begin
      glClearColor(0.0,1.0,0.0,1.0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      ObjectImplementation.RectMap:=Conv(ObjectImplementation.RectBuffer.Map);
      ObjectImplementation.RectPos:=ObjectImplementation.RectMap;
      ObjectImplementation.RectBuffer.Bind(0);
      ObjectImplementation.Program.UseProgram;
      glBindVertexArray(ObjectImplementation.VertArray);
   end StartPainting;
   ---------------------------------------------------------------------------

   procedure StopPainting
     (ObjectImplementation : in out OpenGL3OI_Type) is
   begin
      ObjectImplementation.FlushPaint;
      ObjectImplementation.RectBuffer.Unmap;
   end StopPainting;
   ---------------------------------------------------------------------------

   function CreateWindowImpl
     (ObjectImplementation : in out OpenGL3OI_Type)
      return GUIImplInterface_Window.WindowImpl_ClassAccess is
   begin
      return OpenGL3ObjectImplementation.Window.Create
        (Impl => ObjectImplementation'Unrestricted_Access,
         Data => ObjectImplementation.WindowData'Unrestricted_Access);
   end CreateWindowImpl;
   ---------------------------------------------------------------------------

   procedure DestroyWindowImpl
     (ObjectImplementation : in out OpenGL3OI_Type;
      WindowImpl           : in out GUIImplInterface_Window.WindowImpl_ClassAccess) is
   begin
      null;
   end DestroyWindowImpl;
   ---------------------------------------------------------------------------

   procedure Finalize
     (ObjectImplementation : in out OpenGL3OI_Type) is
      pragma Unreferenced(ObjectImplementation);
   begin
      Put_Line("Finalize OGL3I");
      null;
   end Finalize;
   ---------------------------------------------------------------------------

   function Constructor
     (GenConfig  : Config.Config_ClassAccess;
      ImplConfig : Config.Config_ClassAccess;
      Parameter  : Graphics.Context_Info)
      return GUI.GUIObjectImplementation_Ref is

      pragma Unreferenced(GenConfig,Parameter);

      use type Config.Config_ClassAccess;

      NewImpl : constant OpenGL3OI_Access:=new OpenGL3OI_Type;
      Ref     : constant GUI.GUIObjectImplementation_Ref:=GUI.GUIObjectImplementationRef.MakeInitialRef(GUI.GUIObjectImplementation_ClassAccess(NewImpl));
      Config  : aliased OGL3ImplConfig_Type;

   begin
      Put_Line("Constructor called");
      if ImplConfig/=null then
         Config:=OGL3ImplConfig_Type(ImplConfig.all);
      end if;
      -- TODO: use an amount validated by OGL itself
      NewImpl.TexBuffers.SetBufferBlockSize(1024*1024);
      NewImpl.TexBuffers.SetInternalFormat(GL_RGBA8);
      NewImpl.RectBuffers.SetBufferBlockSize(RectBufferSize);
      NewImpl.RectBuffers.SetInternalFormat(GL_RGBA32UI);
      NewImpl.RectBuffers.AllocateConst
        (Amount      => RectBufferSize,
         BufferRange => NewImpl.RectBuffer'Access);

      NewImpl.FragmentShader.Create
        (ShaderType => OpenGL.Program.ShaderFragment,
         Source     => FragmentShaderSource);
      Put_Line("Compile Fragment Shader:"&To_String(NewImpl.FragmentShader.GetCompileLog));

      NewImpl.VertexShader.Create
        (ShaderType => OpenGL.Program.ShaderVertex,
         Source     => VertexShaderSource);
      Put_Line("Compile Vertex Shader:"&To_String(NewImpl.VertexShader.GetCompileLog));

      NewImpl.Program.Create
        ((OpenGL.Program.ShaderVertex => OpenGL.Program.ShaderRef.MakeAdditionalRef(NewImpl.VertexShader'Access),
          OpenGL.Program.ShaderFragment => OpenGL.Program.ShaderRef.MakeAdditionalRef(NewImpl.FragmentShader'Access)));
      Put_Line("Link:"&To_String(NewImpl.Program.GetLinkLog));
      NewImpl.Program.BindAttribLocation(0,"in_Position");
      NewImpl.Program.UseProgram;

      Put_Line("Create Vert Buffer");
      glGenVertexArrays(1,NewImpl.VertArray'Access);
      glBindVertexArray(NewImpl.VertArray);

      glGenBuffers(1,NewImpl.VertBuffer'Access);
      glBindBuffer(GL_ARRAY_BUFFER,NewImpl.VertBuffer);
      AssertError("PostVertInit.1");
      glBufferData
        (target => GL_ARRAY_BUFFER,
         size   => VertData'Size/8,
         data   => VertData(0)'Address,
         usage  => GL_STATIC_DRAW);
      AssertError("PostVertInit.2");
      glVertexAttribPointer
        (index      => 0,
         size       => 2,
         ttype      => GL_FLOAT,
         normalized => GL_FALSE,
         stride     => 0,
         pointer    => System.Null_Address);
      glEnableVertexAttribArray(0);
      glBindVertexArray(0);
      AssertError("PostVertInit.3");

      NewImpl.Program.UseProgram;

      NewImpl.TexBufferUniform   := NewImpl.Program.GetUniformLocation("tex");
      NewImpl.RectBufferUniform  := NewImpl.Program.GetUniformLocation("data");
      NewImpl.OrthoMatrixUniform := NewImpl.Program.GetUniformLocation("OrthoMatrix");
      Put_Line("OrthoMatrixUniform"&GLint_Type'Image(NewImpl.OrthoMatrixUniform));

      glUniform1i(NewImpl.RectBufferUniform,0);
      glUniform1i(NewImpl.TexBufferUniform,1);
      AssertError("PostVertInit.4");

      NewImpl.WindowData.Setup
        (Impl          => NewImpl,
         Configuration => Config);
      Put_Line("****");

      return Ref;

   end Constructor;
   ---------------------------------------------------------------------------

   function Compatible
     (GenConfig  : Config.Config_ClassAccess;
      ImplConfig : Config.Config_ClassAccess;
      Parameter  : Graphics.Context_Info)
      return Boolean is
      pragma Unreferenced(GenConfig, ImplConfig);
   begin

      Put_Line("Compatible?");
      Put_Line(To_String(Parameter.InterfaceType));
      Put_Line(Natural'Image(Parameter.VersionMajor));
      return To_String(Parameter.InterfaceType)="OpenGL" and (Parameter.VersionMajor>=3);

   end Compatible;
   ---------------------------------------------------------------------------

   procedure Register is
   begin
      Put_Line("Register");
      GUI.ObjectImplementations.Register(RefStr("OpenGL3 Object Implementation"),Compatible'Access,Constructor'access);
   end Register;
   ---------------------------------------------------------------------------

end OpenGL3ObjectImplementation;
