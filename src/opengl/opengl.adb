-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of TrainWorld
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published
--   by the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

pragma Ada_2005;

with Ada.Unchecked_Conversion;
with VersionParser;
with Ada.Text_IO; use Ada.Text_IO;
with Basics; use Basics;

package body OpenGL is

   NullChar : constant Character:=Character'Val(0);

   function Conv is new Ada.Unchecked_Conversion(System.Address,glClearColor_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glGetString_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glGetIntegerv_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glClear_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glViewport_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glDrawArrays_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glFinish_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glGetStringi_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glGetError_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glGenTextures_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glBindTexture_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glTexParameteri_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glDeleteTextures_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glTexImage2D_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glTexSubImage2D_Access);

   -- Buffer Objects
   function Conv is new Ada.Unchecked_Conversion(System.Address,glGenBuffers_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glBindBuffer_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glBufferData_Access);
   -- Vertex Attributes
   function Conv is new Ada.Unchecked_Conversion(System.Address,glVertexAttribPointer_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glEnableVertexAttribArray_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glGenVertexArrays_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glBindVertexArray_Access);
   -- GLSL
   function Conv is new Ada.Unchecked_Conversion(System.Address,glCreateProgram_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glDeleteProgram_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glUseProgram_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glAttachShader_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glDetachShader_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glLinkProgram_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glGetProgramiv_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glGetShaderInfoLog_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glGetUniformLocation_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glCreateShader_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glDeleteShader_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glShaderSource_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glCompileShader_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glGetShaderiv_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glGetProgramInfoLog_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glBindAttribLocation_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glUniform1i_Access);

   function glGetString
     (name    : GLenum_Type;
      GetProc : not null GetProc_Access)
      return String is

      extglGetString : constant glGetString_Access:=Conv(GetProc("glGetString"&NullChar));
      Str            : constant chars_ptr:=extglGetString(name);

   begin

      if Str/=Null_Ptr then
         return Value(Str);
      else
         return "";
      end if;

   end glGetString;
   ---------------------------------------------------------------------------

   procedure PreGetIntegerv
     (pname    : GLenum_Type;
      params  : access GLint_Type;
      GetProc : not null GetProc_Access) is

      extglGetIntegerv : constant glGetIntegerv_Access:=Conv(GetProc("glGetIntegerv"&NullChar));

   begin
      extglGetIntegerv(pname,params);
   end PreGetIntegerv;
   ---------------------------------------------------------------------------

   function GetVersion
     (GetProc : not null GetProc_Access)
      return OpenGLVersion_Type is

      VersionString : constant String:=glGetString(GL_VERSION,GetProc);

   begin
      Put_Line("Version:"&VersionString&":");
      return Result : OpenGLVersion_Type do
         declare
            -- The limit is necessary because some implementations add content to the string
            Version : constant VersionParser.Version_Type:=VersionParser.Parse(VersionString,Limit=>2);
         begin
            if Version'Length/=2 then
               raise InvalidOpenGLVersion;
            end if;
            if Version(Version'First)>3 then
               PreGetIntegerv(GL_MAJOR_VERSION,Result.Major'Access,GetProc);
               PreGetIntegerv(GL_MINOR_VERSION,Result.Minor'Access,GetProc);
            else
               Result.Major:=GLint_Type(Version(Version'First));
               Result.Minor:=GLint_Type(Version(Version'First+1));
            end if;
         end;
      end return;

   end GetVersion;
   ---------------------------------------------------------------------------

   function IsExtensionSupported
     (Name : String)
      return Boolean is
   begin
      for i in Extensions'Range loop
         if Extensions(i)=Name then
            return True;
         end if;
      end loop;
      return False;
   end IsExtensionSupported;
   ---------------------------------------------------------------------------

   procedure ProcessExtensionString
     (ExtStr : String) is
      Start    : Integer := ExtStr'First;
      Count    : Natural := 0;
      Position : Natural := 0;
   begin

      for i in ExtStr'Range loop
         if ExtStr(i)=' ' then
            if i/=Start then
               Count:=Count+1;
            end if;
            Start:=i+1;
         end if;
      end loop;

      if ExtStr'Last/=Start then
         Count:=Count+1;
      end if;

      Extensions:=new Extension_Array(0..Count-1);

      Start:=ExtStr'First;
      for i in ExtStr'Range loop
         if ExtStr(i)=' ' then
            if i/=Start then
               Extensions(Position) := U(ExtStr(Start..i-1));
               Position             := Position+1;
            end if;
            Start:=i+1;
         end if;
      end loop;

      if ExtStr'Last/=Start then
         Extensions(Position) := U(ExtStr(Start..ExtStr'Last));
      end if;

   end ProcessExtensionString;
   ---------------------------------------------------------------------------

   procedure ReadExtensionsByGetString
     (GetProc : GetProc_Access) is

   begin
      ProcessExtensionString(glGetString(GL_EXTENSIONS,GetProc));
   end ReadExtensionsByGetString;
   ---------------------------------------------------------------------------

   procedure ReadExtensionsByGetStringi is
      Count : aliased Glint_Type;
   begin
      glGetIntegerv(GL_NUM_EXTENSIONS,Count'Access);
      Extensions:=new Extension_Array(0..Integer(Count)-1);
      for i in 0..Count-1 loop
         declare
            Str : constant chars_ptr:=glGetStringi(GL_EXTENSIONS,i);
         begin
            if Str=Null_Ptr then
               raise InvalidExtensionString;
            end if;
            Extensions(Integer(i)):=U(Value(Str));
         end;
      end loop;
   end ReadExtensionsByGetStringi;
   ---------------------------------------------------------------------------

   procedure LoadFunctions
     (DefaultGetProc   : not null GetProc_Access;
      ExtensionGetProc : not null GetProc_Access;
      Compatible       : Boolean) is
      pragma Unreferenced(Compatible);

      Version : constant OpenGLVersion_Type:=GetVersion(DefaultGetProc);

   begin

      -- TODO: It is not entirely safe to say what can be loaded with ExtensionProc
      --       This needs testing and research
      glGetError       := Conv(DefaultGetProc("glGetError"));
      glClear          := Conv(DefaultGetProc("glClear"));
      glClearColor     := Conv(DefaultGetProc("glClearColor"));
      glViewport       := Conv(DefaultGetProc("glViewport"));
      glFinish         := Conv(DefaultGetProc("glFinish"));
      glGetIntegerv    := Conv(DefaultgetProc("glGetIntegerv"));
      glTexParameteri  := Conv(DefaultGetProc("glTexParameteri"));
      glDrawArrays     := Conv(ExtensionGetProc("glDrawArrays"));
      glGenTextures    := Conv(ExtensionGetProc("glGenTextures"));
      glBindTexture    := Conv(ExtensionGetProc("glBindTexture"));
      glDeleteTextures := Conv(ExtensionGetProc("glDeleteTextures"));
      glTexImage2D     := Conv(DefaultGetProc("glTexImage2D"));
      glTexSubImage2D  := Conv(ExtensionGetProc("glTexSubImage2D"));
      AssertError;

      if Version.Major>=3 then
         glGetStringi := Conv(ExtensionGetProc("glGetStringi"&NullChar));
      end if;
      -- TODO: Check if this is still a valid method or if you
      --  need to apply something new for OGL 3
      if Version.Major>=3 then
         ReadExtensionsByGetStringi;
      else
         ReadExtensionsByGetString(DefaultGetProc);
      end if;

      AssertError;
      -- Buffer Objects
      if (Version.Major>=2) or ((Version.Major=1) and (Version.Minor>=5)) then
         SupportBufferObjects:=True;
         glGenBuffers := Conv(ExtensionGetProc("glGenBuffers"));
         glBindBuffer := Conv(ExtensionGetProc("glBindBuffer"));
         glBufferData := Conv(ExtensionGetProc("glBufferData"));
      end if;

      Put_line("Buffer Objects");
      AssertError;
      -- VertexAttrib
      if Version.Major>=2 then
         SupportVertexAttributes:=True;
         glVertexAttribPointer     := Conv(ExtensionGetProc("glVertexAttribPointer"));
         glEnableVertexAttribArray := Conv(ExtensionGetProc("glEnableVertexAttribArray"));
         glBindAttribLocation      := Conv(ExtensionGetProc("glBindAttribLocation"));
      end if;

      Put_Line("Vertex Array");
      AssertError;

      if (Version.Major>=3) or
        IsExtensionSupported("GL_ARB_vertex_array_object") then
         glBindVertexArray:=Conv(ExtensionGetProc("glBindVertexArray"));
         glGenVertexArrays:=Conv(ExtensionGetProc("glGenVertexArrays"));
      end if;

      Put_Line("Check GLSL");
      AssertError;

      -- GLSL
      if (Version.Major>=2)then
         SupportProgram := True;
         -- TODO: Check if Get
         declare
            Version : constant VersionParser.Version_Type:=VersionParser.Parse(glGetString(GL_SHADING_LANGUAGE_VERSION,DefaultGetProc),Limit=>2);
         begin
            if Version'Length/=2 then
               raise InvalidGLSLVersion;
            end if;
            GLSLVersion.Major:=GLint_Type(Version(Version'First));
            GLSLVersion.Minor:=GLint_Type(Version(Version'First+1));
         end;
         glCreateProgram      := Conv(ExtensionGetProc("glCreateProgram"));
         glDeleteProgram      := Conv(ExtensionGetProc("glDeleteProgram"));
         glUseProgram         := Conv(ExtensionGetProc("glUseProgram"));
         glAttachShader       := Conv(ExtensionGetProc("glAttachShader"));
         glDetachShader       := Conv(ExtensionGetProc("glDetachShader"));
         glLinkProgram        := Conv(ExtensionGetProc("glLinkProgram"));
         glGetProgramiv       := Conv(ExtensionGetProc("glGetProgramiv"));
         glGetShaderInfoLog   := Conv(ExtensionGetProc("glGetShaderInfoLog"));
         glGetUniformLocation := Conv(ExtensionGetProc("glGetUniformLocation"));
         glGetProgramInfoLog  := Conv(ExtensionGetProc("glGetProgramInfoLog"));
         glCreateShader       := Conv(ExtensionGetProc("glCreateShader"));
         glDeleteShader       := Conv(ExtensionGetProc("glDeleteShader"));
         glShaderSource       := Conv(ExtensionGetProc("glShaderSource"));
         glCompileShader      := Conv(ExtensionGetProc("glCompileShader"));
         glGetShaderiv        := Conv(ExtensionGetProc("glGetShaderiv"));
         glUniform1i          := Conv(ExtensionGetProc("glUniform1i"));
      end if;
      AssertError;

   end LoadFunctions;
   ---------------------------------------------------------------------------

   procedure AssertError is
      Error : GLenum_Type;
   begin

      Error:=glGetError.all;

      if Error/=0 then
         raise OpenGLError
           with "OpenGL Error detected with error number "
             &GLenum_Type'Image(Error);
      end if;

   end AssertError;
   ---------------------------------------------------------------------------

end OpenGL;
