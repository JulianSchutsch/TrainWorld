-------------------------------------------------------------------------------
-- Copyright 2012 Julian Schutsch
--
-- This file is part of TrainWorld
--
-- ParallelSim is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published
-- by the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- ParallelSim is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with ParallelSim. If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

package body Graphics is

   procedure CreateConfig
     (Configuration : in out Config.ConfigNode_Type;
      WindowType : WindowType_Enum:=WindowTypeWindow;
      BufferKind : BufferKind_Enum:=BufferKindDefault;
      Height : Natural:=768;
      Width : Natural:=1024;
      RedBits : Natural:=8;
      GreenBits : Natural:=8;
      BlueBits : Natural:=8;
      AlphaBits : Natural:=8;
      DepthBits : Natural:=24;
      StencilBits : Natural:=0) is

   begin
      Configuration.SetConfig(new Context_Config'(
        WindowType  => WindowType,
        BufferKind  => BufferKind,
        Height      => Height,
        Width       => Width,
        RedBits     => RedBits,
        GreenBits   => GreenBits,
        BlueBits    => BlueBits,
        AlphaBits   => AlphaBits,
        DepthBits   => DepthBits,
        StencilBits => StencilBits));
   end CreateConfig;

end Graphics;
