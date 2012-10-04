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
with Graphics.Impl;
with Ada.Text_IO; use Ada.Text_IO;
with Config;
with Basics; use Basics;
with OpenGL; use OpenGL;
with GlobalLoop;
with Ada.Finalization;
with Interfaces.C;

procedure XTest is

   use type Interfaces.C.double;
   Configuration : Config.ConfigNode_Type;
   Terminated    : Boolean:=False;
   pragma Warnings(Off,Terminated);


   type ContextCallBack_Type is new Ada.Finalization.Limited_Controlled and Graphics.ContextCallBack_Interface with
      record
         null;
      end record;

   overriding
   procedure ContextClose
     (Data : in out ContextCallBack_Type);

   overriding
   procedure ContextPaint
     (Data : in out ContextCallBack_Type);

   overriding
   procedure ContextCreate
     (Data : in out ContextCallBack_Type);

   overriding
   procedure Finalize
     (Data : in out ContextCallBack_Type);
   ---------------------------------------------------------------------------

   procedure Finalize
     (Data : in out ContextCallBack_Type) is
      pragma Unreferenced(Data);
   begin
      Put_Line("Data.Finalize");
   end Finalize;
   ---------------------------------------------------------------------------

   procedure ContextCreate
     (Data : in out ContextCallBack_Type) is

   begin

      null;

   end ContextCreate;
   ---------------------------------------------------------------------------

   procedure ContextClose
     (Data : in out ContextCallBack_Type) is
      pragma Unreferenced(Data);

   begin

      Terminated:=True;

   end ContextClose;
   ---------------------------------------------------------------------------

   procedure ContextPaint
     (Data : in out ContextCallBack_Type) is
      pragma Unreferenced(Data);

   begin

      AssertError("ContextPaint.enter");
      glViewport(0,0,400,400);
      AssertError("ClearColor");
      glClearColor(0.0,1.0,0.0,1.0);
      glClear(GL_COLOR_BUFFER_BIT);
      AssertError("ContextPaint");

   end ContextPaint;
   ---------------------------------------------------------------------------

begin

   Graphics.Impl.Register;

   Configuration.SetImplementation( U("OpenGL"));
   Graphics.CreateConfig
     (Configuration => Configuration,
      WindowType    => Graphics.WindowTypeWindow,
      BufferKind    => Graphics.BufferKindDefault);

   declare
      Context:constant Graphics.Context_Ref:=Graphics.Implementations.Utilize(Configuration);
   begin

      declare
         ContextCallBack : ContextCallBack_Type;
      begin

         Context.I.CallBack:=ContextCallBack'Unrestricted_Access;

         while not Terminated loop
            GlobalLoop.Process;
         end loop;

      end;
      Put_Line("Leaving context area");

   end;

end XTest;
