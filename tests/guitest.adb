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
with Config;
with Basics; use Basics;
with GlobalLoop;
with Interfaces.C;
with GUI;
with GUI.Window;
with OpenGL3ObjectImplementation;
with Ada.Text_IO; use Ada.Text_IO;
pragma Warnings(Off,Ada.Text_IO);

procedure GUITest is

   use type Interfaces.C.double;
   Configuration : Config.ConfigNode_Type;
   Terminated    : Boolean:=False;
   pragma Warnings(Off,Terminated);

   type GUICallBack_Type is limited new GUI.GUICallBack_Interface with
      record
         TGUI   : GUI.GUI_Ref;
         Window : GUI.Window.Window_Type;
      end record;

   overriding
   procedure GUIClose
     (T : in out GUICallBack_Type);

   overriding
   procedure GUICreate
     (T : in out GUICallBack_Type);
   ---------------------------------------------------------------------------

   procedure GUICreate
     (T : in out GUICallBack_Type) is
   begin
      Put_Line("GUICreate");
      T.Window.SetParent(T.TGUI.I.GetBaseLayer);
   end GUICreate;
   ---------------------------------------------------------------------------

   procedure GUIClose
     (T : in out GUICallBack_Type) is
      pragma Unreferenced(T);
   begin
      Terminated:=True;
   end GUIClose;
   ---------------------------------------------------------------------------

begin

   Graphics.Impl.Register;
   OpenGL3ObjectImplementation.Register;

   Configuration.SetImplementation(RefStr("OpenGL"));
   Graphics.CreateConfig
     (Configuration => Configuration,
      WindowType    => Graphics.WindowTypeWindow,
      BufferKind    => Graphics.BufferKindDefault);

   declare
      Context     : constant Graphics.Context_Ref:=Graphics.Implementations.Utilize(Configuration,(others => <>));
      TGUI        : aliased GUI.GUI_Type;
      ThemeConfig : Config.ConfigNode_Type;
      GUICB       : GUICallBack_Type;
   begin

      TGUI.Setup
        (Context => Context,
         Theme   => ThemeConfig);
      GUICB.TGUI:=GUI.GUIRef.MakeAdditionalRef(TGUI'Unrestricted_Access);
      TGUI.CallBack := GUICB'Unrestricted_Access;

      while not Terminated loop
         GlobalLoop.Process;
      end loop;

   end;

end GUITest;
