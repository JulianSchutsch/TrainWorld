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

pragma Ada_2012;

with System;

package body OpenGL.Test is

   type CatchEvent_Type is
      record
         Event  : CatchEvent_Enum:=CatchEventNone;
         Target : GLenum_Type;
         ID     : GLuint_Type;
      end record;

   type CatchEvent_Array is array (Natural range <>) of CatchEvent_Type;

   Events        : CatchEvent_Array(0..99);
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

   procedure CheckEventsDone is
   begin
      if EventCount/=0 then
         ReportIssue("Were expecting more events");
      end if;
   end CheckEventsDone;
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
   end CatchBindBuffer;

   CVersionString   : constant chars_ptr:=New_String("3.3");
   CExtensionString : constant chars_ptr:=New_String("");
   CGLSLVersion     : constant chars_ptr:=New_String("3.30");

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
      null; -- Not yet implemented, should later remove all assigned function
            -- calls
   end UnbindEvents;
   ---------------------------------------------------------------------------

   procedure TestBindTextureBuffer is

   begin
      BindEvents;

      BindTextureBuffer(0);
      CheckEventsDone;

      PushEvent
        (Event  => CatchEventBindBuffer,
         Target => GL_TEXTURE_BUFFER,
         ID     => 1);
      BindTextureBuffer(1);
      CheckEventsDone;

      BindTextureBuffer(1);
      CheckEventsDone;

      PushEvent
        (Event  => CatchEventBindBuffer,
         Target => GL_TEXTURE_BUFFEr,
         ID     => 0);
      BindTextureBuffer(0);
      CheckEventsDone;

      UnbindEvents;

   end TestBindTextureBuffer;
   ---------------------------------------------------------------------------

   procedure TestBindTexture is
   begin
      BindEvents;
      UnbindEvents;
   end TestBindTexture;
   ---------------------------------------------------------------------------

end OpenGL.Test;
