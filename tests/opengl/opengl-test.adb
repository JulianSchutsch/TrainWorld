-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of TrainWorld
--
--   TrainWorld is free software: you can redistribute it and/or modify
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

pragma Ada_2012;

with System;
with Ada.Unchecked_Conversion;

package body OpenGL.Test is

   EventCheckingEnabled : Boolean:=False;

   -- Returned by CatchGetString
   CVersionString   : constant chars_ptr:=New_String("3.3");
   CExtensionString : constant chars_ptr:=New_String("");
   CGLSLVersion     : constant chars_ptr:=New_String("3.30");

   -- Used by glGenBuffers and glDeleteBuffer
   BufferCount        : constant GLuint_Type:=1024;
   Buffers            : array(1..BufferCount) of Boolean:=(others => False);
   BufferType         : array(1..BufferCount) of GLenum_Type:=(others => 0);
   BoundTextureBuffer : GLuint_Type:=0;

   TextureCount   : constant GLuint_Type:=1024;
   Textures       : array(1..TextureCount) of Boolean:=(others => False);
   TextureType    : array(1..TextureCount) of GLenum_Type:=(others => 0);
   BoundTexture2D : GLuint_Type;
   BoundTextureBufferTexture : GLuint_Type;

   procedure ResetBuffers is
   begin

      for i in Buffers'Range loop

         if Buffers(i)/=False then
            ReportIssue("Buffer Object "&GLuint_Type'Image(i)&" not freed");
            Buffers(i):=False;
         end if;

         BufferType(i):=0;

      end loop;

      BoundTextureBuffer:=0;

   end ResetBuffers;
   ---------------------------------------------------------------------------

   procedure ResetTextures is
   begin

      for i in Textures'Range loop

         if Textures(i)/=False then
            ReportIssue("Texture "&GLuint_Type'Image(i)&" not freed");
            Textures(i):=False;
         end if;

         TextureType(i):=0;

      end loop;

      BoundTexture2D:=0;
      BoundTextureBufferTexture:=0;

   end ResetTextures;
   ---------------------------------------------------------------------------

   function AllocateBuffer
     return GLuint_Type is
   begin
      for i in Buffers'Range loop
         if Buffers(i)=False then
            Buffers(i):=True;
            return i;
         end if;
      end loop;
      return 0;
   end AllocateBuffer;
   ---------------------------------------------------------------------------

   function AllocateTexture
     return GLuint_Type is
   begin
      for i in Textures'Range loop
         if Textures(i)=False then
            Textures(i):=True;
            return i;
         end if;
      end loop;
      return 0;
   end AllocateTexture;
   ---------------------------------------------------------------------------

   function "+"
     (Left  : GLuint_Access;
      Right : Integer)
      return GLuint_Access is

      use type Interfaces.C.size_t;

      function GLuintAccessToSizeT is new Ada.Unchecked_Conversion
        (Source => GLuint_Access,
         Target => Interfaces.C.size_t);

      function SizeTToGLuintAccess is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.size_t,
         Target => GLuint_Access);

   begin
      return SizeTToGLuintAccess
        (GLuintAccessToSizeT(Left)+GLuint_Type'Size/8*Interfaces.C.size_t(Right));
   end "+";
   ---------------------------------------------------------------------------

   type CatchEvent_Type is
      record
         Event  : CatchEvent_Enum:=CatchEventNone;
         Target : GLenum_Type;
         ID     : GLuint_Type;
      end record;

   type CatchEvent_Array is array (Natural range <>) of CatchEvent_Type;

   Events        : CatchEvent_Array(0..9999);
   EventCount    : Natural:=0;

   procedure PushEvent
     (Event  : CatchEvent_Enum;
      Target : GLenum_Type:=0;
      ID     : GLuint_Type:=0) is
   begin
      declare
         This : CatchEvent_Type renames Events(EventCount);
      begin
         This.Event  := Event;
         This.Target := Target;
         This.ID     := ID;
      end;
      EventCount:=EventCount+1;
   end PushEvent;
   ---------------------------------------------------------------------------

   procedure RemoveEvent
     (Index : Integer) is
   begin

      for i in Index..EventCount-2 loop
         Events(i):=Events(i+1);
      end loop;

      EventCount:=EventCount-1;

   end RemoveEvent;
   ---------------------------------------------------------------------------

   procedure CheckEvent
     (Event       : CatchEvent_Enum;
      CheckTarget : Boolean:=False;
      Target      : GLenum_Type:=0;
      CheckID     : Boolean:=False;
      ID          : GLuint_Type) is

      MatchIndex : Integer:=-1;

   begin

      if not EventCheckingEnabled then
         return;
      end if;

      -- Check if an event is present before the next barrier
      EventLoop:
      for i in reverse 0..EventCount-1 loop

         if Events(i).Event=CatchEventBarrier then
            exit EventLoop;
         end if;

         if Events(i).Event=Event then
            declare
               Match : Boolean:=True;
            begin
               if CheckTarget then
                  Match:=Events(i).Target=Target;
               end if;
               if CheckID then
                  Match:=Match and Events(i).ID=ID;
               end if;
               if Match then
                  MatchIndex:=i;
                  exit EventLoop;
               end if;
            end;

         end if;

      end loop EventLoop;

      if MatchIndex/=-1 then
         RemoveEvent(MatchIndex);
      end if;

      while (EventCount>0) and then (Events(0).Event=CatchEventBarrier) loop
         RemoveEvent(0);
      end loop;

      if MatchIndex=-1 then
         ReportIssue("Failed CheckEvent for "&CatchEvent_Enum'Image(Event)&
                       " Target="&GLenum_Type'Image(Target)&
                       " ID="&GLuint_Type'Image(ID));
      end if;

   end CheckEvent;
   ---------------------------------------------------------------------------

   procedure CheckEventsMiddle is
   begin
      if EventCount/=0 then
         ReportIssue("Were expecting more events");
      end if;
   end CheckEventsMiddle;
   ---------------------------------------------------------------------------

   procedure CheckEventsBegin is
   begin
      EventCheckingEnabled:=True;
   end CheckEventsBegin;
   ---------------------------------------------------------------------------

   procedure CheckEventsEnd is
   begin
      CheckEventsMiddle;
      EventCheckingEnabled:=False;
   end CheckEventsEnd;
   ---------------------------------------------------------------------------

   procedure CatchBindBuffer
     (target : GLenum_Type;
      buffer : GLuint_Type);
   pragma Convention(StdCall,CatchBindBuffer);

   procedure CatchBindBuffer
     (target : GLenum_Type;
      buffer : GLuint_Type) is
   begin

      CheckEvent
        (Event       => CatchEventBindBuffer,
         CheckTarget => True,
         Target      => Target,
         CheckID     => True,
         ID          => Buffer);

      if buffer not in Buffers'Range then
         ReportIssue("CatchBindBuffer : Either 0 or impossible buffer");
         return;
      end if;

      if not Buffers(buffer) then
         ReportIssue("CatchBindBuffer : Buffer not allocated.");
         return;
      end if;

      if (BufferType(buffer)=0) or (BufferType(buffer)=target) then
         BufferType(buffer):=target;
         case target is
            when GL_TEXTURE_BUFFER =>
               if BoundTextureBuffer=Buffer then
                  ReportIssue("CatchBindBuffer : Buffer allready set");
               end if;
               BoundTextureBuffer:=Buffer;
            when others =>
               ReportIssue("Unknown target");
         end case;
      else
         ReportIssue("CatchBindBuffer : Buffer does not fit target");
      end if;

   end CatchBindBuffer;
   ---------------------------------------------------------------------------

   procedure CatchBufferData
     (target : GLenum_Type;
      size   : GLsizeiptr_Type;
      data   : System.Address;
      usage  : GLenum_Type);
   pragma Convention(StdCall,CatchBufferData);

   procedure CatchBufferData
     (target : GLenum_Type;
      size   : GLsizeiptr_Type;
      data   : System.Address;
      usage  : GLenum_Type) is
      pragma Unreferenced(size,data,usage);
   begin
      case target is
         when GL_TEXTURE_BUFFER =>
            if BoundTextureBuffer=0 then
               ReportIssue("CatchBufferData : No texture buffer bound.");
               return;
            end if;
         when others =>
            ReportIssue("CatchBufferData : Target not supported");
            return;
      end case;
      -- Possible : Store the size etc for later reference
   end CatchBufferData;
   ---------------------------------------------------------------------------

   function CatchGetString
     (name : GLenum_Type)
      return chars_ptr;
   pragma Convention(StdCall,CatchGetString);

   function CatchGetString
     (name : GLenum_Type)
      return chars_ptr is
   begin
      case name is
         when GL_VERSION =>
            return CVersionString;
         when GL_EXTENSIONS =>
            return CExtensionString;
         when GL_SHADING_LANGUAGE_VERSION =>
            return CGLSLVersion;
         when others =>
            ReportIssue("CatchGetString unprepared for request:"&GLenum_Type'Image(name));
            return Null_Ptr;
      end case;
   end CatchGetString;
   ---------------------------------------------------------------------------

   function CatchError
     return GLenum_Type;
   pragma Convention(StdCall,CatchError);

   function CatchError
     return GLenum_Type is
   begin
      return 0;
   end CatchError;
   ---------------------------------------------------------------------------

   procedure CatchGetIntegerv
     (pname  : GLenum_Type;
      params : access GLuint_Type);
   pragma Convention(StdCall,CatchGetIntegerv);

   procedure CatchGetIntegerv
     (pname  : GLenum_Type;
      params : access GLuint_Type) is
   begin
      case pname is
         when GL_NUM_EXTENSIONS =>
            params.all:=0;
         when GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS =>
            params.all:=1;
         when others =>
            ReportIssue("CatchGetIntegerv unprepared for request:"&GLenum_Type'Image(pname));
      end case;
   end CatchGetIntegerv;
   ---------------------------------------------------------------------------

   procedure CatchGenBuffers
     (n       : GLsizei_Type;
      buffers : access GLuint_Type);
   pragma Convention(StdCall,CatchGenBuffers);

   procedure CatchGenBuffers
     (n       : GLsizei_Type;
      buffers : access GLuint_Type) is

      Pointer : GLuint_Access:=GLuint_Access(buffers);

   begin

      for i in 1..n loop
         Pointer.all := AllocateBuffer;
         Pointer     := Pointer+1;
      end loop;

   end CatchGenBuffers;
   ---------------------------------------------------------------------------

   procedure CatchGenTextures
     (n        : GLsizei_Type;
      textures : access GLuint_Type) is

      Pointer : GLuint_Access:=GLuint_Access(textures);

   begin

      for i in 1..n loop
         Pointer.all:=AllocateTexture;
         Pointer:=Pointer+1;
      end loop;

   end CatchgenTextures;
   ---------------------------------------------------------------------------

   procedure CatchBindTexture
     (target  : GLenum_Type;
      texture : GLuint_Type) is
   begin
      -- TODO: Insert CheckEvent
      if texture not in Textures'Range then
         ReportIssue("CatchBindBuffer: either 0 or impossible texture");
         return;
      end if;

      if Textures(texture)=False then
         ReportIssue("CatchBindBuffer: Texture not allocated");
         return;
      end if;

      if (TextureType(texture)=0) or (TextureType(texture)=target) then
         case target is
            when GL_TEXTURE_BUFFER =>
               if BoundTextureBufferTexture=texture then
                  ReportIssue("CatchBindTexture: TextureBuffer allready bound");
               end if;
               BoundTextureBufferTexture:=texture;
            when GL_TEXTURE_2D =>
               if BoundTexture2D=texture then
                  ReportIssue("CatchBindTexture: Texture2D allready bound");
               end if;
               BoundTexture2D:=texture;
            when others =>
               ReportIssue("CatchBindTexture: Target not supported");
         end case;

         TextureType(texture):=target;

      else
         ReportIssue("CatchBindTexture: Texture does not fit target");
      end if;

   end CatchBindTexture;
   ---------------------------------------------------------------------------

   procedure CatchTexBuffer
     (target         : GLenum_Type;
      internalFormat : GLenum_Type;
      buffer         : GLuint_Type) is
      pragma Unreferenced(internalFormat);
      pragma Unreferenced(buffer);
   begin

      case target is
         when GL_TEXTURE_BUFFER =>
            if BoundTextureBufferTexture=0 then
               ReportIssue("CatchTexBuffer: No Buffer Texture bound");
            end if;
--            if (Buffers(buffer)=GL_TEXTURE_BUFFER) then
--               null;
--            end if;
-- TODO: Lots of checking
         when others =>
            ReportIssue("CatchTexBuffer: Targget not supported");
            return;
      end case;

   end CatchTexBuffer;
   ---------------------------------------------------------------------------

   function GetProc
     (Str : String)
      return System.Address is
   begin

      if Str="glGetString" then
         return CatchGetString'Address;
      end if;
      if Str="glBindBuffer" then
         return CatchBindBuffer'Address;
      end if;
      if Str="glGetError" then
         return CatchError'Address;
      end if;
      if Str="glGetIntegerv" then
         return CatchGetIntegerv'Address;
      end if;
      if Str="glGenBuffers" then
         return CatchGenBuffers'Address;
      end if;
      if Str="glBufferData" then
         return CatchBufferData'Address;
      end if;
      if Str="glGenTextures" then
         return CatchGenTextures'Address;
      end if;
      if Str="glBindTexture" then
         return CatchBindTexture'Address;
      end if;
      if Str="glTexBuffer" then
         return CatchTexBuffer'Address;
      end if;

      return System.Null_Address;

   end GetProc;
   ---------------------------------------------------------------------------

   procedure BindEvents is
   begin
      LoadFunctions(GetProc'Access,Compatible=>False);
   end BindEvents;
   ---------------------------------------------------------------------------

   procedure UnbindEvents is
   begin
      ResetBuffers;
      ResetTextures;
      null; -- Not yet implemented, should later remove all assigned function
            -- calls
   end UnbindEvents;
   ---------------------------------------------------------------------------

   procedure TestBindTextureBuffer is

   begin
      BindEvents;
      CheckEventsBegin;

      BindTextureBuffer(0);
      CheckEventsMiddle;

      PushEvent
        (Event  => CatchEventBindBuffer,
         Target => GL_TEXTURE_BUFFER,
         ID     => 1);
      BindTextureBuffer(1);
      CheckEventsMiddle;

      BindTextureBuffer(1);
      CheckEventsMiddle;

      PushEvent
        (Event  => CatchEventBindBuffer,
         Target => GL_TEXTURE_BUFFEr,
         ID     => 0);
      BindTextureBuffer(0);

      CheckEventsEnd;
      UnbindEvents;

   end TestBindTextureBuffer;
   ---------------------------------------------------------------------------

   procedure TestBindTexture is
   begin
      BindEvents;
      CheckEventsBegin;
      CheckEventsEnd;
      UnbindEvents;
   end TestBindTexture;
   ---------------------------------------------------------------------------

end OpenGL.Test;
