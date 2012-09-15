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

with Plattform; use Plattform;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Build is

   procedure WriteCopyright
     (File : File_Type) is
   begin
      Put_Line(File,"-------------------------------------------------------------------------------");
      Put_Line(File,"--   Copyright 2012 Julian Schutsch");
      Put_Line(File,"--");
      Put_Line(File,"--   This file is part of TrainWorld");
      Put_Line(File,"--");
      Put_Line(File,"--   ParallelSim is free software: you can redistribute it and/or modify");
      Put_Line(File,"--   it under the terms of the GNU Affero General Public License as published");
      Put_Line(File,"--   by the Free Software Foundation, either version 3 of the License, or");
      Put_Line(File,"--   (at your option) any later version.");
      Put_Line(File,"--");
      Put_Line(File,"--   ParallelSim is distributed in the hope that it will be useful,");
      Put_Line(File,"--   but WITHOUT ANY WARRANTY; without even the implied warranty of");
      Put_Line(File,"--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the");
      Put_Line(File,"--   GNU Affero General Public License for more details.");
      Put_Line(File,"--");
      Put_Line(File,"--   You should have received a copy of the GNU Affero General Public License");
      Put_Line(File,"--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.");
      Put_Line(File,"-------------------------------------------------------------------------------");
      Put_Line(File,"");
   end WriteCopyRight;
   ---------------------------------------------------------------------------

   function StrOn
     (On:Boolean)
      return String is
   begin
      if On then
         return "On";
      else
         return "Off";
      end if;
   end StrOn;
   ---------------------------------------------------------------------------

   ConfigGPRPath       : constant String:="../gpr/config.gpr";
   GraphicsImplPathAds : constant String:="../src/graphics/graphics-impl.ads";
   GraphicsImplPathAdb : constant String:="../src/graphics/graphics-impl.adb";
   File                : File_Type;

   function Win32OpenGL
     return Boolean is
   begin
      return Detected=PlattformWindowsNT;
   end Win32OpenGL;
   ---------------------------------------------------------------------------

   function GLXOpenGL
     return Boolean is
   begin
      return Detected/=PlattformWindowsNT;
   end GLXOpenGL;
   ---------------------------------------------------------------------------

begin

   Plattform.Initialize;
   begin
      Delete_File(To_String(BasePath)&ConfigGPRPath);
   exception
      when others =>
         null;
   end;

   Create
     (File => File,
      Mode => Out_File,
      Name => To_String(BasePath)&ConfigGPRPath);
   WriteCopyright(File);
   Put_Line(File,"-- This file was automatically created using /build/build");
   Put_Line(File,"");
   Put_Line(File,"abstract project Config is");
   Put_Line(File,"");
   Put_Line(File,"   type Plattform_Type is");
   Put_Line(File,"      (""Unknown"",");
   Put_Line(File,"       ""Linux"",");
   Put_Line(File,"       ""BSD"",");
   Put_Line(File,"       ""WindowsNT"");");
   Put_Line(File,"");
   Put_Line(File,"   type Switch_Type is");
   Put_Line(File,"      (""On"",");
   Put_Line(File,"       ""Off"");");
   Put_Line(File,"");
   Put_Line(File,"   Plattform   : Plattform_Type:="""&To_String(PlattformID(Detected))&""";");
   Put_Line(File,"");

   -- Win32 OpenGL
   Put_Line(File,"   Win32OpenGL : Switch_Type:="""&StrOn(Win32OpenGL)&""";");

   -- GLX OpenGL
   Put_Line(File,"   XOpenGL     : Switch_Type:="""&StrOn(GLXOpenGL)&""";");

   Put_Line(File,"");
   Put_Line(File,"end Config;");

   Close(File);

   begin
      Delete_File(To_String(BasePath)&GraphicsImplPathAds);
   exception
      when others =>
         null;
   end;

   Create
     (File => File,
      Mode => Out_File,
      Name => To_String(BasePath)&GraphicsImplPathAds);

   WriteCopyright(File);
   Put_Line(File,"package Graphics.Impl is");
   Put_Line(File,"   procedure Register;");
   Put_Line(File,"end Graphics.Impl;");

   Close(File);

   begin
      Delete_File(To_String(BasePath)&GraphicsImplPathAdb);
   exception
      when others =>
         null;
   end;

   Create
     (File => File,
      Mode => Out_File,
      Name => To_String(BasePath)&GraphicsImplPathAdb);

   WriteCopyright(File);
   if Win32OpenGL then
      Put_Line(File,"with OpenGL.Win32Context;");
   end if;
   if GLXOpenGL then
      Put_Line(File,"with OpenGL.GLXContext;");
   end if;
   Put_Line(File,"package body Graphics.Impl is");
   Put_Line(File,"   procedure Register is");
   Put_Line(File,"   begin");
   if Win32OpenGL then
      Put_Line(File,"      OpenGL.Win32Context.Register;");
   end if;
   if GLXOpenGL then
      Put_Line(File,"      OpenGL.GLXContext.Register;");
   end if;
   Put_Line(File,"   end Register;");
   Put_Line(File,"end Graphics.Impl;");

   Close(File);

end Build;
