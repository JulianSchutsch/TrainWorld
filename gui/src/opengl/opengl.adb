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

package body OpenGL is

   NullChar : constant Character:=Character'Val(0);

   function Conv is new Ada.Unchecked_Conversion(System.Address,glClearColor_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glGetString_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glGetIntegerv_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glClear_Access);

   -- Buffer Objects
   function Conv is new Ada.Unchecked_Conversion(System.Address,glGenBuffers_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glBindBuffer_Access);
   function Conv is new Ada.Unchecked_Conversion(System.Address,glBufferData_Access);
   -- Vertex Attributes
   function Conv is new Ada.Unchecked_Conversion(System.Address,glVertexAttribPointer_Access);
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

   procedure glGetIntegerv
     (pname    : GLenum_Type;
      params  : access GLint_Type;
      GetProc : not null GetProc_Access) is

      extglGetIntegerv : constant glGetIntegerv_Access:=Conv(GetProc("glGetIntegerv"&NullChar));

   begin
      extglGetIntegerv(pname,params);
   end glGetIntegerv;
   ---------------------------------------------------------------------------

   function GetVersion
     (GetProc : not null GetProc_Access)
      return OpenGLVersion_Type is

      VersionString : constant String:=glGetString(GL_VERSION,GetProc);

   begin
      Put_Line("Version"&VersionString&":");
      return Result : OpenGLVersion_Type do
         declare
            Version : constant VersionParser.Version_Type:=VersionParser.Parse(VersionString);
         begin
            if Version'Length/=3 then
               raise InvalidOpenGLVersion;
            end if;
            if Version(Version'First)>3 then
               glGetIntegerv(GL_MAJOR_VERSION,Result.Major'Access,GetProc);
               glGetIntegerv(GL_MINOR_VERSION,Result.Minor'Access,GetProc);
            else
               Result.Major:=GLint_Type(Version(Version'First));
               Result.Minor:=GLint_Type(Version(Version'First+1));
            end if;
         end;
      end return;

   end GetVersion;
   ---------------------------------------------------------------------------

   procedure LoadFunctions
     (DefaultGetProc   : not null GetProc_Access;
      ExtensionGetProc : not null GetProc_Access;
      Compatible       : Boolean) is
      pragma Unreferenced(Compatible);

      Version : constant OpenGLVersion_Type:=GetVersion(DefaultGetProc);

   begin
      glClear:=Conv(DefaultGetProc("glClear"&NullChar));
      glClearColor:=Conv(DefaultGetProc("glClearColor"&NullChar));
      -- Buffer Objects
      if (Version.Minor>=2) or ((Version.Major=1) and (Version.Minor>=5)) then
         SupportBufferObjects:=True;
         glGenBuffers:=Conv(ExtensionGetProc("glGenBuffers"&NullChar));
         glBindBuffer:=Conv(ExtensionGetProc("glBindBuffer"&NullChar));
         glBufferData:=Conv(ExtensionGetProc("glBufferData"&NullChar));
      end if;
      -- VertexAttrib
      if Version.Major>2 then
         SupportVertexAttributes:=True;
         glVertexAttribPointer:=Conv(ExtensionGetProc("glVertexAttribPointer"&NullChar));
      end if;
      -- GLSL
      if Version.Major>2 then
         SupportProgram := True;
         glCreateProgram      := Conv(ExtensionGetProc("glCreateProgram"&NullChar));
         glDeleteProgram      := Conv(ExtensionGetProc("glDeleteProgram"&NullChar));
         glUseProgram         := Conv(ExtensionGetProc("glUseProgram"&NullChar));
         glAttachShader       := Conv(ExtensionGetProc("glAttachShader"&NullChar));
         glDetachShader       := Conv(ExtensionGetProc("glDetachShader"&NullChar));
         glLinkProgram        := Conv(ExtensionGetProc("glLinkProgram"&NullChar));
         glGetProgramiv       := Conv(ExtensionGetProc("glGetProgramiv"&NullChar));
         glGetShaderInfoLog   := Conv(ExtensionGetProc("glGetShaderInfoLog"&NullChar));
         glGetUniformLocation := Conv(ExtensionGetProc("glGetUniformLocation"&NullChar));
         glCreateShader       := Conv(ExtensionGetProc("glCreateShader"&NullChar));
         glDeleteShader       := Conv(ExtensionGetProc("glDeleteShader"&NullChar));
         glShaderSource       := Conv(ExtensionGetProc("glShaderSource"&NullChar));
         glCompileShader      := Conv(ExtensionGetProc("glCompileShader"&NullChar));
         glGetShaderiv        := Conv(ExtensionGetProc("glGetShaderiv"&NullChar));
      end if;

   end LoadFunctions;
   ---------------------------------------------------------------------------

   procedure AssertError is
      Error : GLenum_Type;
   begin

      Error:=glGetError;

      if Error/=0 then
         raise OpenGLError
           with "OpenGL Error detected with error number "
             &GLenum_Type'Image(Error);
      end if;

   end AssertError;
   ---------------------------------------------------------------------------

end OpenGL;
