pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__guitest.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__guitest.adb");
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exception_table_E");
   E123 : Short_Integer; pragma Import (Ada, E123, "ada__containers_E");
   E091 : Short_Integer; pragma Import (Ada, E091, "ada__io_exceptions_E");
   E045 : Short_Integer; pragma Import (Ada, E045, "ada__strings_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "ada__strings__maps_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "ada__tags_E");
   E082 : Short_Integer; pragma Import (Ada, E082, "ada__streams_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "interfaces__c_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "interfaces__c__strings_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "system__exceptions_E");
   E084 : Short_Integer; pragma Import (Ada, E084, "system__finalization_root_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "ada__finalization_E");
   E086 : Short_Integer; pragma Import (Ada, E086, "system__storage_pools_E");
   E075 : Short_Integer; pragma Import (Ada, E075, "system__finalization_masters_E");
   E071 : Short_Integer; pragma Import (Ada, E071, "system__storage_pools__subpools_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "system__pool_global_E");
   E108 : Short_Integer; pragma Import (Ada, E108, "system__file_control_block_E");
   E131 : Short_Integer; pragma Import (Ada, E131, "ada__streams__stream_io_E");
   E098 : Short_Integer; pragma Import (Ada, E098, "system__file_io_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__secondary_stack_E");
   E047 : Short_Integer; pragma Import (Ada, E047, "ada__strings__unbounded_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "system__os_lib_E");
   E129 : Short_Integer; pragma Import (Ada, E129, "system__strings__stream_ops_E");
   E093 : Short_Integer; pragma Import (Ada, E093, "ada__text_io_E");
   E114 : Short_Integer; pragma Import (Ada, E114, "basics_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "config_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "globalloop_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "implementations_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "opengl_E");
   E147 : Short_Integer; pragma Import (Ada, E147, "opengl__win32context_E");
   E127 : Short_Integer; pragma Import (Ada, E127, "refcount_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "graphics_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "opengl__program_E");
   E151 : Short_Integer; pragma Import (Ada, E151, "endianess_E");
   E149 : Short_Integer; pragma Import (Ada, E149, "bytes_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "versionparser_E");
   E156 : Short_Integer; pragma Import (Ada, E156, "win32_E");
   E161 : Short_Integer; pragma Import (Ada, E161, "win32__opengl32_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "opengl__win32context__finalize_body");
      begin
         E147 := E147 - 1;
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "opengl__program__finalize_body");
      begin
         E143 := E143 - 1;
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "opengl__program__finalize_spec");
      begin
         F3;
      end;
      E122 := E122 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "graphics__finalize_spec");
      begin
         F4;
      end;
      E127 := E127 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "refcount__finalize_spec");
      begin
         F5;
      end;
      E120 := E120 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "globalloop__finalize_spec");
      begin
         F6;
      end;
      E116 := E116 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "config__finalize_spec");
      begin
         F7;
      end;
      E114 := E114 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "basics__finalize_spec");
      begin
         F8;
      end;
      E093 := E093 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "ada__text_io__finalize_spec");
      begin
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__file_io__finalize_body");
      begin
         E098 := E098 - 1;
         F10;
      end;
      E047 := E047 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "ada__strings__unbounded__finalize_spec");
      begin
         F11;
      end;
      E075 := E075 - 1;
      E071 := E071 - 1;
      E131 := E131 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "ada__streams__stream_io__finalize_spec");
      begin
         F12;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "system__file_control_block__finalize_spec");
      begin
         E108 := E108 - 1;
         F13;
      end;
      E110 := E110 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "system__pool_global__finalize_spec");
      begin
         F14;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "system__storage_pools__subpools__finalize_spec");
      begin
         F15;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "system__finalization_masters__finalize_spec");
      begin
         F16;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");
   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Zero_Cost_Exceptions : Integer;
      pragma Import (C, Zero_Cost_Exceptions, "__gl_zero_cost_exceptions");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");

      procedure Install_Handler;
      pragma Import (C, Install_Handler, "__gnat_install_handler");

      Handler_Installed : Integer;
      pragma Import (C, Handler_Installed, "__gnat_handler_installed");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Zero_Cost_Exceptions := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      if Handler_Installed = 0 then
         Install_Handler;
      end if;

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E021 := E021 + 1;
      Ada.Containers'Elab_Spec;
      E123 := E123 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E091 := E091 + 1;
      Ada.Strings'Elab_Spec;
      E045 := E045 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E082 := E082 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E027 := E027 + 1;
      System.Finalization_Root'Elab_Spec;
      E084 := E084 + 1;
      Ada.Finalization'Elab_Spec;
      E081 := E081 + 1;
      System.Storage_Pools'Elab_Spec;
      E086 := E086 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      System.Pool_Global'Elab_Spec;
      E110 := E110 + 1;
      System.File_Control_Block'Elab_Spec;
      E108 := E108 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E131 := E131 + 1;
      E071 := E071 + 1;
      System.Finalization_Masters'Elab_Body;
      E075 := E075 + 1;
      E102 := E102 + 1;
      E100 := E100 + 1;
      Ada.Tags'Elab_Body;
      E058 := E058 + 1;
      E051 := E051 + 1;
      System.Soft_Links'Elab_Body;
      E011 := E011 + 1;
      System.Secondary_Stack'Elab_Body;
      E015 := E015 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E047 := E047 + 1;
      System.Os_Lib'Elab_Body;
      E105 := E105 + 1;
      System.File_Io'Elab_Body;
      E098 := E098 + 1;
      System.Strings.Stream_Ops'Elab_Body;
      E129 := E129 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E093 := E093 + 1;
      Basics'Elab_Spec;
      E114 := E114 + 1;
      Config'Elab_Spec;
      E116 := E116 + 1;
      Globalloop'Elab_Spec;
      E120 := E120 + 1;
      E125 := E125 + 1;
      Opengl'Elab_Spec;
      Refcount'Elab_Spec;
      E127 := E127 + 1;
      Graphics'Elab_Spec;
      E122 := E122 + 1;
      Opengl.Program'Elab_Spec;
      Opengl.Program'Elab_Body;
      E143 := E143 + 1;
      E151 := E151 + 1;
      E149 := E149 + 1;
      Versionparser'Elab_Spec;
      E141 := E141 + 1;
      E135 := E135 + 1;
      E156 := E156 + 1;
      E161 := E161 + 1;
      Opengl.Win32context'Elab_Body;
      E147 := E147 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_guitest");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   C:\Users\alexandrus\tw\gpr\basics.o
   --   C:\Users\alexandrus\tw\gpr\config.o
   --   C:\Users\alexandrus\tw\gpr\globalloop.o
   --   C:\Users\alexandrus\tw\gpr\implementations.o
   --   C:\Users\alexandrus\tw\gpr\refcount.o
   --   C:\Users\alexandrus\tw\gpr\graphics.o
   --   C:\Users\alexandrus\tw\gpr\opengl-program.o
   --   C:\Users\alexandrus\tw\tests\guitest.o
   --   C:\Users\alexandrus\tw\gpr\types.o
   --   C:\Users\alexandrus\tw\gpr\endianess.o
   --   C:\Users\alexandrus\tw\gpr\bytes.o
   --   C:\Users\alexandrus\tw\gpr\versionparser.o
   --   C:\Users\alexandrus\tw\gpr\opengl.o
   --   C:\Users\alexandrus\tw\gpr\win32.o
   --   C:\Users\alexandrus\tw\gpr\win32-gdi32.o
   --   C:\Users\alexandrus\tw\gpr\win32-kernel32.o
   --   C:\Users\alexandrus\tw\gpr\win32-ole32.o
   --   C:\Users\alexandrus\tw\gpr\win32-opengl32.o
   --   C:\Users\alexandrus\tw\gpr\win32-user32.o
   --   C:\Users\alexandrus\tw\gpr\opengl-win32context.o
   --   -LC:\Users\alexandrus\tw\tests\
   --   -LC:\Users\alexandrus\tw\gpr\
   --   -LC:/gnat/2012/lib/gcc/i686-pc-mingw32/4.5.4/adalib/
   --   -static
   --   -lgnat
   --   -Wl,--stack=0x2000000
--  END Object file/option list   

end ada_main;
