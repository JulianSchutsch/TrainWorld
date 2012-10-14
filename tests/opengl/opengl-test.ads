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
--   2.Mar 2012 Julian Schutsch
--     - Original version

-- Reason for implementation
--   Other than testing the very simple and obvious functionality of a few
--   wrappers this package provides a test framework for opengl by which
--   all functions can be watched and a certain sequence of events can be
--   verified.

-- Usage
--   All testing must be inbetween BindEvents and UnbindEvents calls where
--   BindEvents loads a fake OGL 3.3 context using the ordinary
--   Load_Functions in OpenGL.
--   All previously loaded functions can be overridden.
--   Unsupported functions are set to null.
--
--   To verify an OpenGL method it is necessary to setup a series of events
--   using PushEvent. Events are not necessarily in order, but can be happen
--   in order unless a CatchEventBarrier is pushed.
--
--                  Event1, Event2, ...
--                   CatchEventBarrier
--                  Event3, Event4, ...
--                   CatchEventBarrier
--                  ...
--
--  After the OpenGL method has been invoked, CheckEventsDone should be called
--  to ensure no events are left in the queue.

with TestFrameWork; use TestFrameWork;
with Basics; use Basics;

package OpenGL.Test is

   type CatchEvent_Enum is
     (CatchEventNone,
      CatchEventBarrier,
      CatchEventBindBuffer,
      CatchEventGenBuffers,
      CatchEventDeleteBuffers,
      CatchEventGenTextures,
      CatchEventDeleteTextures,
      CatchEventBindTexture);

   procedure PushEvent
     (Event  : CatchEvent_Enum;
      Target : GLenum_Type:=0;
      ID     : GLuint_Type:=0);

   procedure CheckEventsBegin;
   procedure CheckEventsEnd;

   procedure BindEvents;

   procedure UnbindEvents;
   ---------------------------------------------------------------------------

   procedure TestTextureBuffer;

   procedure TestTexture;

   StrOpenGLBindTextureBuffer : aliased constant String:="OpenGL.BindTextureBuffer";
   StrOpenGLTextures : aliased constant String:="OpenGL.Texture";

   Tests : Test_Array:=
     ((Name => RefConstStr(StrOpenGLBindTextureBuffer'Access),
       Test => TestTextureBuffer'Access),
      (Name => RefConstStr(StrOpenGLTextures'Access),
       Test => TestTexture'Access));

end OpenGL.Test;
