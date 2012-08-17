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
--with Ada.Text_IO; use Ada.Text_IO;
with Opengl; use Opengl;
pragma Unreferenced(OpenGL);
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

package body OpenGL.Win32Context is

   type Context_Type is new Graphics.Context_Interface with
      record
         WindowHandle        : HWND_Type         := NULLHANDLE;
         DeviceContext       : HDC_Type          := NULLHANDLE;
         RenderContext       : HGLRC_Type        := NULLHANDLE;
         DestroySignalSend   : Boolean           := False;
         DoubleBuffered      : Boolean;
         MouseButtonsPressed : MouseButton_Array := NoMouseButtons;
         HasCapture          : Boolean           := False;

         CSTR_ClassName : Interfaces.C.Strings.chars_ptr
           := Interfaces.C.Strings.Null_Ptr;
         CSTR_Title     : Interfaces.C.Strings.chars_ptr
           := Interfaces.C.Strings.Null_Ptr;
      end record;

   type Context_Access is access all Context_Type;

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
--            Paint(Context.all);
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
            -- TODO : Extract Resize
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
--            Paint(Context.all);
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

   begin

      if GenConfig/=null then
         GConfig:=Graphics.Context_Config(GenConfig.all);
      end if;

      ContextRef.I:=Graphics.Ref.Interface_ClassAccess(Context);

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

      Context.DeviceContext := wglCreateContext(Context.DeviceContext);
      if Context.DeviceContext=NULLHANDLE then
         raise Graphics.FailedContextCreation with "Could not create first opengl context using wglCreateContext, error :"
           &DWORD_Type'Image(GetLastError);
      end if;

      if wglMakeCurrent
        (hdc => Context.DeviceContext,
         hglrc => Context.RenderContext)/=Standard.Win32.TRUE then
         raise Graphics.FailedContextCreation with "Could not make first opengl context current with wglMakeCurrent, error :"
           &DWORD_Type'Image(GetLastError);
      end if;

      return ContextRef;

   end ContextConstructor;
   ---------------------------------------------------------------------------

   procedure Register is
   begin
      Graphics.Implementations.Register
        (U("OpenGL"),ContextConstructor'Access);
   end Register;

end OpenGL.Win32Context;
