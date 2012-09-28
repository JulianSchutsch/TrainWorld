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
--   27.Sep 2012 Julian Schutsch
--     - Original version

-- Usage
--   This package provides a common interface to be used for all graphics
--   context implementations written for it.
--   All implementations must register in the Implementations child package.
--   An context is created given a specific configuration and immediately
--   initialized after the Implementations.Utilize call which returns
--   a Context_Ref.
--
--   The context communicates through a ContextCallBack_Interface which
--   must be assigned to Context_Interface.CallBack.
--   If the initialization was successfull, Context_Interface.OnCreate is
--   called as soon as the CallBack is assigned.
--
--   If the context is closed, a Context_Interface.OnClose is called and
--   no further calls to the CallBack are made. The CallBack access may
--   be set to null by the implementation.

pragma Ada_2012;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with RefCount;
with Implementations;
with Config;
with Basics; use Basics;

package Graphics is

   FailedContextCreation    : Exception;
   InvalidContext           : Exception;
   FailedContextDestruction : Exception;

   type MouseButton_Enum is
     (MouseButtonLeft,
      MouseButtonRight);

   type MouseButton_Array is array(MouseButton_Enum) of Boolean;

   NoMouseButtons : constant Mousebutton_Array:=(others => False);

   type WindowType_Enum is
     (WindowTypeWindow,
      WindowTypeFullDesktopWindow,
      WindowTypeFullScreen);

   type BufferKind_Enum is
     (BufferKindDefault,
      BufferKindSingle,
      BufferKindDouble,
      BufferKindTriple);

   type Context_OnClose is
     access procedure
       (Data : C_ClassAccess);

   type Context_OnResize is
     access procedure
       (Data   : C_ClassAccess;
        Height : Integer;
        Width  : Integer);

   type Context_OnPaint is
     access procedure
       (Data : C_ClassAccess);

   type ContextCallBack_Interface is limited interface;
   type ContextCallBack_ClassAccess is access all ContextCallBack_Interface'Class;

   procedure ContextClose
     (T : in out ContextCallBack_Interface) is null;

   procedure ContextResize
     (T : in out ContextCallBack_Interface) is null;

   procedure ContextPaint
     (T : in out ContextCallBack_Interface) is null;

   procedure ContextCreate
     (T : in out ContextCallBack_Interface) is null;
   ---------------------------------------------------------------------------

   type Context_Interface is new RefCount.Ref_Interface with
      record
         CallBack : ContextCallBack_ClassAccess;
      end record;
   type Context_ClassAccess is access all Context_Interface'Class;

   type Context_Info is
      record
         InterfaceType : Unbounded_String;
         VersionMajor : Natural;
         VersionMinor : Natural;
         VersionPatch : Natural;
      end record;

   package Ref is new RefCount.Ref(Context_Interface,Context_ClassAccess);

   subtype Context_Ref is Ref.Ref_Type;

   type Context_Config is new Config.Config_Type with
      record
         WindowType             : WindowType_Enum:=WindowTypeWindow;
         BufferKind             : BufferKind_Enum:=BufferKindDefault;
         Height                 : Natural:=768;
         Width                  : Natural:=1024;
         RedBits                : Natural:=8;
         GreenBits              : Natural:=8;
         BlueBits               : Natural:=8;
         AlphaBits              : Natural:=8;
         DepthBits              : Natural:=24;
         StencilBits            : Natural:=0;
         WindowTitle            : Unbounded_String:=U("Win");
         ApplicationTitle       : Unbounded_String:=U("App");
      end record;

   package Implementations is new Standard.Implementations(Context_Ref);

   procedure CreateConfig
     (Configuration    : in out Config.ConfigNode_Type;
      WindowType       : WindowType_Enum:=WindowTypeWindow;
      BufferKind       : BufferKind_Enum:=BufferKindDefault;
      Height           : Natural:=768;
      Width            : Natural:=1024;
      RedBits          : Natural:=8;
      GreenBits        : Natural:=8;
      BlueBits         : Natural:=8;
      AlphaBits        : Natural:=8;
      DepthBits        : Natural:=24;
      StencilBits      : Natural:=0;
      WindowTitle      : Unbounded_String:=U("Win");
      ApplicationTitle : Unbounded_String:=U("App"));

end Graphics;
