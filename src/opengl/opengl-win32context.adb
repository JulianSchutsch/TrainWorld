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

with Graphics; use Graphics;
with Basics; use Basics;
with Ada.Text_IO; use Ada.Text_IO;
with Opengl; use Opengl;
with System;
with Config;
with Interfaces.C.Strings;
with Win32; use Win32;
with Win32.Ole32; use Win32.Ole32;
with Win32.User32; use Win32.User32;
with Win32.Kernel32; use Win32.Kernel32;
with Win32.GDI32; use Win32.GDI32;
with Win32.OpenGL32; use Win32.OpenGL32;
with Ada.Unchecked_Conversion;
with Bytes;
with GlobalLoop;

package body OpenGL.Win32Context is

   LibraryHandle : HMODULE_Type := NULLHANDLE;
   LibraryCount  : Natural:=0;
   LibraryName   : constant String:="opengl32.dll"&Character'Val(0);

   type Context_Type;
   type Context_Access is access all Context_Type;

   type Context_Process is new GlobalLoop.Process_Type with
      record
         Context : Context_Access:=null;
      end record;

   overriding
   procedure Process
     (P : in out Context_Process);

   type Context_Type is new Graphics.Context_Interface with
      record

         WindowHandle        : HWND_Type         := NULLHANDLE;
         DeviceContext       : HDC_Type          := NULLHANDLE;
         RenderContext       : HGLRC_Type        := NULLHANDLE;
         DestroySignalSend   : Boolean           := False;
         DoubleBuffered      : Boolean           := True;
         MouseButtonsPressed : MouseButton_Array := NoMouseButtons;
         HasCapture          : Boolean           := False;
         LibraryLoaded       : Boolean           := False;
         LoopProcess         : Context_Process;

         CSTR_ClassName : Interfaces.C.Strings.chars_ptr
           := Interfaces.C.Strings.Null_Ptr;
         CSTR_Title     : Interfaces.C.Strings.chars_ptr
           := Interfaces.C.Strings.Null_Ptr;

      end record;

   overriding
   procedure Finalize
     (Context : in out Context_Type);
   ---------------------------------------------------------------------------

   procedure Process
     (P : in out Context_Process) is

      BoolResult : BOOL_Type;
      LResult    : LRESULT_Type;
      lMsg       : aliased MSG_Type;

      pragma Unreferenced(BoolResult,LResult);

   begin

      if P.Context=null then
         raise InvalidContext with "Process not properly initialized";
      end if;

      while PeekMessage
        (lpMsg => lMsg'Access,
         hwnd  => P.Context.WindowHandle,
         wMsgFilterMin => 0,
         wMsgFilterMax => 0,
         wRemoveMsg    => PM_REMOVE)/=0 loop

         BoolResult:=TranslateMessage(lMsg'Access);
         LResult   :=DispatchMessage(lMsg'Access);
      end loop;

      if P.Context.DestroySignalSend and P.Context.OnClose/=null then
         P.Context.OnClose(P.Context.Data);
         P.Disable;
      end if;

   end Process;
   ---------------------------------------------------------------------------

   procedure Paint
     (Context : in out Context_Type) is

      Result : BOOL_Type;
      pragma Unreferenced(Result);

   begin

      if Context.RenderContext=NULLHANDLE then
         return;
      end if;

      Result:=wglMakeCurrent
        (hdc   => Context.DeviceContext,
         hglrc => Context.RenderContext);

      if Context.OnPaint/=null then
         Context.OnPaint(Context.Data);
      end if;

      if Context.DoubleBuffered then
         Result:=SwapBuffers(Context.DeviceContext);
      else
         glFinish;
      end if;

   end Paint;
   ---------------------------------------------------------------------------

   function WndProc
     (hWnd   : HWND_Type;
      uMsg   : UINT_Type;
      wParam : WPARAM_Type;
      lParam : LPARAM_Type)
      return LRESULT_Type;
   pragma Convention(C,WndProc);

   function WndProc
     (hWnd   : HWND_Type;
      uMsg   : UINT_Type;
      wParam : WPARAM_Type;
      lParam : LPARAM_Type)
      return LRESULT_Type is

      -- Context belonging to this window
      Context : Context_Access;

   begin
      -- Extract the Context_Access from the windows LongPtr storage
      -- associated with this window.
      -- Context will be null if WM_CREATE has not been called before
      -- PORTABILITY : Expects Context_Access to be a simple pointer
      --   Shown to work with GNAT GPL 2010 (20100603)
      declare
         function ConvertLongPtrToContextAccess is new Ada.Unchecked_Conversion
           (Source => LONG_PTR_Type,
            Target => Context_Access);
      begin
         Context:=ConvertLongPtrToContextAccess
           (GetWindowLongPtr(hWnd,0));
      end;

      case uMsg is
         when WM_MOUSELEAVE =>
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_PAINT =>
            Paint(Context.all);
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);
         when WM_CREATE =>
            declare
               Result : UINT_PTR_Type;
               pragma Unreferenced(Result);
               -- TODO: Maybe you should check this result, but a way
               --       to propagate the error to NewContext is required first.
            begin
               Result:=SetTimer
                 (hwnd        => hwnd,
                  nIDEvent    => 1,
                  uElapse     => 10,
                  lpTimerFunc => System.Null_Address);
            end;

            declare
               function ConvertLParamToCREATESTRUCTPtr is
                 new Ada.Unchecked_Conversion
                   (Source => LPARAM_Type,
                    Target => CREATESTRUCT_Access);

               CreateStruct : constant CREATESTRUCT_Access
                 :=ConvertLParamToCREATESTRUCTPtr(lParam);
               Result : LONG_Type;
               pragma Unreferenced(Result);
            begin
               Result:=User32.SetWindowLongPtr
                 (hWnd      => hWnd,
                  nIndex    => 0,
                  dwNewLong => CreateStruct.lpCreateParams);
            end;
            return 0;
         when WM_SIZE =>
--            GUI.PropagateContextResize
--              (Context => Context_ClassAccess(Context),
--               Width   => Integer(LOWORD(lParam)),
--               Height  => Integer(HIWORD(lParam)));
            return 0;
         when WM_SIZING =>
            return 0;
         when WM_LBUTTONDBLCLK =>
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);
         when WM_RBUTTONDBLCLK =>
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_LBUTTONDOWN =>
--            MouseDown
--              (Context     => Context,
--               MouseButton => LeftButton,
--               AbsX        => GET_X_LPARAM(lParam),
--               AbsY        => GET_Y_LPARAM(lParam));
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_LBUTTONUP =>
--            MouseUp
--              (Context     => Context,
--               MouseButton => LeftButton,
--               AbsX        => GET_X_LPARAM(lParam),
--               AbsY        => GET_Y_LPARAM(lParam));
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_RBUTTONDOWN =>
--            MouseDown
--              (Context     => Context,
--               MouseButton => RightButton,
--               AbsX        => GET_X_LPARAM(lParam),
--               AbsY        => GET_Y_LPARAM(lParam));
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_RBUTTONUP =>
--            MouseUp
--              (Context     => Context,
--               MouseButton => RightButton,
--               AbsX        => GET_X_LPARAM(lParam),
--               AbsY        => GET_Y_LPARAM(lParam));
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_MOUSEMOVE =>
--            ContextMouseMove
--              (Context     => Context_ClassAccess(Context),
--               AbsX        => GET_X_LPARAM(lParam),
--               AbsY        => GET_Y_LPARAM(lParam));
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_DESTROY =>
            declare
               BoolResult : BOOL_Type;
               pragma Unreferenced(BoolResult);
            begin
               BoolResult:=PostMessage
                 (hWnd   => hWnd,
                  Msg    => WM_QUIT,
                  wParam => 0,
                  lParam => System.Null_Address);
            end;
            Context.DestroySignalSend:=True;
            return 0;

         when WM_TIMER =>
            Paint(Context.all);
            return 0;

         when WM_ERASEBKGND =>
            return 1;

         when WM_KEYDOWN =>
--            if wParam<=255 then
--               GUI.ContextKeyDown
--                 (Context => Context_ClassAccess(Context),
--                  Key     => KeyTable(Integer(wparam)));
--            end if;
            return User32.DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_KEYUP =>
--            if wParam<=255 then
--               GUI.ContextKeyUp
--                 (Context => Context_ClassAccess(Context),
--                  Key     => KeyTable(Integer(wParam)));
--            end if;
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);
         when WM_CHAR =>
            declare
               -- Warnings are unnecessary, this is a dirty 32 to 16 bit
               -- convert.
               pragma Warnings(Off);
               function Convert is new Ada.Unchecked_Conversion
                 (Source => WPARAM_Type,
                  Target => Wide_Character);
               pragma Warnings(On);
            begin
--               if wParam>=32 then
--                  GUI.ContextCharacterInput
--                    (Context => Context_ClassAccess(Context),
--                     Chars   => UCS2ToUTF8(Convert(wParam)));
--               end if;
null;
            end;
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);
         when others =>
            return DefWindowProc
              (hWnd   => hWnd,
               uMsg   => uMsg,
               wParam => wParam,
               lParam => lParam);
      end case;
   end WndProc;
   ---------------------------------------------------------------------------

   function OpenGL32GetProc
     (Str : String)
      return System.Address is

      use type System.Address;

      Result : System.Address;

   begin

      Result := GetProcAddress(LibraryHandle,Str(Str'First)'Address);
      if result=System.Null_Address then
         raise FailedContextCreation with "GetProcAddress return null for """&Str(Str'First..Str'Last-1)&"""";
      end if;
      return Result;

   end OpenGL32GetProc;
   ---------------------------------------------------------------------------

   function WGLGetProc
     (Str : String)
      return System.Address is

      use type System.Address;

      Result : System.Address;

   begin

      pragma Assert(Str/="" and Str(Str'Last)=Character'Val(0));
      Put_Line("GetProc:"&Str&":");
      Result := wglGetProcAddress(Str(Str'First)'Address);
      if Result=System.Null_Address then
         raise FailedContextCreation with "wglGetProcAddress returned null for """&Str(Str'First..Str'Last-1)&"""";
      end if;
      return Result;

   end WGLGetProc;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Context : in out Context_Type) is
      IntResult : Interfaces.C.int;
      BoolResult : BOOL_Type;
      pragma Unreferenced(IntResult,BoolResult);
   begin

      Put_Line("Finalize Win32 Context");
      if Context.DeviceContext/=NULLHANDLE then
         Intresult:=ReleaseDC
           (hWnd => Context.WindowHandle,
            hDC  => Context.DeviceContext);
      end if;
      if Context.WindowHandle/=NULLHANDLE then
         BoolResult:=DestroyWindow
           (hWnd => Context.WindowHandle);
      end if;
      -- TODO :Remove Window Class
      Interfaces.C.Strings.Free(Context.CSTR_ClassName);
      Interfaces.C.Strings.Free(Context.CSTR_Title);
      Put_Line("Done Win32 Context");
   end Finalize;
   ---------------------------------------------------------------------------

   function ContextConstructor
     (GenConfig  : Config.Config_ClassAccess;
      ImplConfig : Config.Config_ClassAccess)
      return Graphics.Context_Ref is
      pragma Unreferenced(ImplConfig);

      use type Interfaces.C.int;
      use type Config.Config_ClassAccess;

      Context    : constant Context_Access:=new Context_Type;
      ContextRef : Context_Ref;
      GUID       : aliased GUID_Type;
      WndClass   : aliased WNDCLASS_Type;
      dwStyle    : DWORD_Type;
      dwExStyle  : DWORD_Type;
      HInstance  : HINSTANCE_Type;

      BoolResult : BOOL_Type;
      HWNDResult : HWND_Type;
      pragma Unreferenced(BoolResult,HWNDResult);

      GConfig : Context_Config;
      Version : OpenGLVersion_Type;

   begin

      if GenConfig/=null then
         GConfig:=Graphics.Context_Config(GenConfig.all);
      end if;

      ContextRef.I:=Context_ClassAccess(Context);

      Context.LoopProcess.Context:=Context;
      Context.LoopProcess.Enable;

      HInstance:=GetModuleHandle(lpModuleName => Interfaces.C.Strings.Null_Ptr);

      if Win32.Ole32.CoCreateGuid(GUID'Access)/=S_OK then
         raise Graphics.FailedContextCreation with "Call to CoCreateGuid failed with "
           &DWORD_Type'Image(Win32.GetLastError);
      end if;

      Context.CSTR_ClassName:=Interfaces.C.Strings.New_String(GUIDToString(GUID));

      WndClass.Style:=CS_HREDRAW or CS_VREDRAW or CS_OWNDC;
      WndClass.lpfnWndProc:=WndProc'Access;
      WndClass.lpszClassName := Context.CSTR_ClassName;
      WndClass.cbWndExtra:=System.Address'Size/8;
      WndClass.hIcon:=LoadIcon
        (hInstance  => NULLHANDLE,
         lpIconName => MAKEINTRESOURCE(IDI_APPLICATION));
      WndClass.hCursor:=LoadCursor
        (hInstance    => NULLHANDLE,
         lpCursorName => MAKEINTRESOURCE(IDC_ARROW));

      if RegisterClass(WndClass'Access)=0 then
         raise Graphics.FailedContextCreation with "Failed to register window class with "
           &DWORD_Type'Image(Win32.GetLastError);
      end if;

      dwStyle := WS_OVERLAPPEDWINDOW
        or WS_CLIPCHILDREN
        or WS_CLIPSIBLINGS;
      dwExStyle:= WS_EX_APPWINDOW
        or WS_EX_CLIENTEDGE;

      Context.WindowHandle:=CreateWindowEx
        (dwExStyle => dwExStyle,
         lpClassName => Context.CSTR_ClassName,
         lpWindowName => Context.CSTR_Title,
         dwStyle     => dwStyle,
         x => 100,
         y => 100,
         nwidth => 1024,
         nheight => 768,
         hwndparent => NULLHANDLE,
         hMenu      => NULLHANDLE,
         hInstance  => HInstance,
         lpParam    => Context.all'Address);
      if Context.WindowHandle=NULLHANDLE then
         raise Graphics.FailedContextCreation with "Failed call to CreateWindowEx with "
           &DWORD_Type'Image(GetLastError);
      end if;

      Context.DeviceContext:=GetDC(Context.WindowHandle);
      if Context.DeviceContext=NULLHANDLE then
         raise Graphics.FailedContextCreation with "Failed call to GetDC with "
           &DWORD_TYPE'Image(GetLastError);
      end if;
      BoolResult:=ShowWindow
        (hWnd     => Context.WindowHandle,
         nCmdShow => SW_SHOW);
      BoolResult:=SetForegroundWindow(Context.WindowHandle);
      HWNDResult:=SetFocus(Context.WindowHandle);

      declare
         pdf         : aliased PIXELFORMATDESCRIPTOR_Type;
         PixelFormat : Interfaces.C.int;
      begin
         pdf.nSize:=PIXELFORMATDESCRIPTOR_Type'Size/8;
         pdf.nVersion   := 1;
         pdf.dwFlags    := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL;
         pdf.iPixelType := PFD_TYPE_RGBA;
         pdf.cColorBits := BYTE_Type(GConfig.RedBits+GConfig.GreenBits+GConfig.BlueBits+GConfig.AlphaBits);
         pdf.cDepthBits := BYTE_Type(GConfig.DepthBits);
         pdf.iLayerType := PFD_MAIN_PLANE;
         PixelFormat := ChoosePixelFormat
           (hdc => Context.DeviceContext,
            ppfd => pdf'Access);
         if PixelFormat=0 then
            raise Graphics.FailedContextCreation with "Failed call to ChoosePixelFormat with "
              &DWORD_TYPE'Image(GetLastError);
         end if;
         if SetPixelFormat
           (hdc          => Context.DeviceContext,
            iPixelFormat => PixelFormat,
            ppfd         => pdf'Access)/=Standard.Win32.TRUE then
            raise Graphics.FailedContextCreation with "Failed call to SetPixelForamt with "
              &DWORD_TYPE'Image(GetLastError);
         end if;
      end;

      Context.RenderContext := wglCreateContext(Context.DeviceContext);
      if Context.RenderContext=NULLHANDLE then
         raise Graphics.FailedContextCreation with "Could not create first opengl context using wglCreateContext, error :"
           &DWORD_Type'Image(GetLastError);
      end if;

      if wglMakeCurrent
        (hdc => Context.DeviceContext,
         hglrc => Context.RenderContext)/=Standard.Win32.TRUE then
         raise Graphics.FailedContextCreation with "Could not make first opengl context current with wglMakeCurrent, error :"
           &DWORD_Type'Image(GetLastError);
      end if;

      if LibraryCount=0 then
         LibraryHandle:=LoadLibrary(LibraryName(LibraryName'First)'Address);
         if LibraryHandle=NULLHANDLE then
            raise Graphics.FailedContextCreation with "Could not load library handle";
         end if;
      end if;

      Context.LibraryLoaded := True;
      LibraryCount  := LibraryCount+1;

      Version:=GetVersion(OpenGL32GetProc'Access);
      Put_Line("Version:"&GLint_Type'Image(Version.Major)&"."&GLint_Type'Image(Version.Minor));

      -- OpenGL <3.2 are treated as "old"
      if (Version.Major<3) or ((Version.Major=3) and (Version.Minor<2)) then
         Put_Line("Created compatible context");
         OpenGL.LoadFunctions(OpenGL32GetProc'Access,WGLGetProc'Access,True);
         return ContextRef;
      end if;

      declare
         NewContext : HGLRC_Type;
         Attribs : Bytes.Int_Array:=
           (WGL_CONTEXT_MAJOR_VERSION_ARB,Bytes.Int_Type(Version.Major),
            WGL_CONTEXT_MINOR_VERSION_ARB,Bytes.Int_Type(Version.Minor),
            WGL_CONTEXT_FLAGS_ARB,WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB,
            WGL_CONTEXT_PROFILE_MASK_ARB,WGL_CONTEXT_CORE_PROFILE_BIT_ARB,
            0);
      begin

         NewContext:=wglCreateContextAttribsARB
           (hdc           => Context.DeviceContext,
            hShareContext => NULLHANDLE,
            attriblist    => Attribs(Attribs'First)'Unchecked_Access);
         if NewContext=NULLHANDLE then
            -- TODO: Add Error note, but use allready present context
            OpenGL.LoadFunctions(OpenGL32GetProc'Access,WGLGetProc'Access,True);
            return ContextRef;
         end if;

         if wglMakeCurrent(NULLHANDLE,NULLHANDLE)/=Standard.Win32.TRUE then
            if wglDeleteContext(NewContext)/=Standard.Win32.TRUE then
               null;
            end if;
            raise Graphics.FailedContextCreation with "Failed call to wglMakeCurrent with NULLHANDLE";
         end if;

         if wglDeleteContext(Context.RenderContext)/=Standard.Win32.TRUE then
            if wglDeleteContext(NewContext)/=Standard.Win32.TRUE then
               null;
            end if;
            raise Graphics.FailedContextCreation with "Failed call to wglDeleteContext for help context";
         end if;

         Context.RenderContext:=NewContext;

         if wglMakeCurrent(Context.DeviceContext,NewContext)/=Standard.Win32.TRUE then
            raise Graphics.FailedContextCreation with "Failed to call wglMakeCurrent with OpenGL >3.2 context";
         end if;

      end;

      OpenGL.LoadFunctions(OpenGL32GetProc'Access,WGLGetProc'Access,False);

      return ContextRef;

   end ContextConstructor;
   ---------------------------------------------------------------------------

   procedure Register is
   begin
      Graphics.Implementations.Register
        (U("OpenGL"),ContextConstructor'Access);
   end Register;

end OpenGL.Win32Context;
