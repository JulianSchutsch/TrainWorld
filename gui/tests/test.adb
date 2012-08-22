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

-- Revision History
--   2.Aug 2012 Julian Schutsch
--     - Original version

pragma Ada_2012;

with Graphics;
with OpenGL.Win32Context;
with Ada.Text_IO; use Ada.Text_IO;
with Config;
with Basics; use Basics;
with OpenGL; use OpenGL;
with GlobalLoop;

procedure Test is

   C : Graphics.Context_Ref;
   pragma Warnings(Off,C);
   Configuration : Config.ConfigNode_Type;
   Terminated : Boolean:=False;
   pragma Warnings(Off,Terminated);

   procedure OnContextClose
     (Data : C_ClassAccess) is
      pragma Unreferenced(Data);
   begin
      Terminated:=True;
   end OnContextClose;
   ---------------------------------------------------------------------------

   procedure OnContextPaint
     (Data : C_ClassAccess) is
      pragma Unreferenced(Data);
   begin
      glClearColor(1.0,1.0,0.0,1.0);
      glClear(GL_COLOR_BUFFER_BIT);
   end OnContextPaint;
   ---------------------------------------------------------------------------

begin

   OpenGL.Win32Context.Register;

   Configuration.SetImplementation( U("OpenGL"));
   Graphics.CreateConfig
     (Configuration =>  Configuration,
      WindowType => Graphics.WindowTypeWindow);

   Put_Line("Find");
   C:=Graphics.Implementations.Utilize(Configuration);
   C.I.OnClose:=OnContextClose'Unrestricted_Access;
   C.I.OnPaint:=OnContextPaint'Unrestricted_Access;
   if OpenGL.SupportProgram then
      Put_Line("GLSL supported");
   end if;
   while not Terminated loop
      GlobalLoop.Process;
   end loop;

end Test;
