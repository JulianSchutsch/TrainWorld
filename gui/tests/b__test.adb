pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__test.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__test.adb");
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E015 : Short_Integer; pragma Import (Ada, E015, "system__soft_links_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exception_table_E");
   E131 : Short_Integer; pragma Import (Ada, E131, "ada__containers_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "ada__io_exceptions_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "ada__strings_E");
   E111 : Short_Integer; pragma Import (Ada, E111, "ada__strings__maps_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "ada__tags_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__streams_E");
   E050 : Short_Integer; pragma Import (Ada, E050, "interfaces__c_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "interfaces__c__strings_E");
   E031 : Short_Integer; pragma Import (Ada, E031, "system__exceptions_E");
   E077 : Short_Integer; pragma Import (Ada, E077, "system__finalization_root_E");
   E075 : Short_Integer; pragma Import (Ada, E075, "ada__finalization_E");
   E096 : Short_Integer; pragma Import (Ada, E096, "system__storage_pools_E");
   E088 : Short_Integer; pragma Import (Ada, E088, "system__finalization_masters_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "system__storage_pools__subpools_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "ada__calendar_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__calendar__delays_E");
   E098 : Short_Integer; pragma Import (Ada, E098, "system__pool_global_E");
   E086 : Short_Integer; pragma Import (Ada, E086, "system__file_control_block_E");
   E139 : Short_Integer; pragma Import (Ada, E139, "ada__streams__stream_io_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "system__file_io_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__secondary_stack_E");
   E107 : Short_Integer; pragma Import (Ada, E107, "ada__strings__unbounded_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "system__os_lib_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "system__strings__stream_ops_E");
   E067 : Short_Integer; pragma Import (Ada, E067, "ada__text_io_E");
   E104 : Short_Integer; pragma Import (Ada, E104, "basics_E");
   E126 : Short_Integer; pragma Import (Ada, E126, "config_E");
   E133 : Short_Integer; pragma Import (Ada, E133, "implementations_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "opengl_E");
   E147 : Short_Integer; pragma Import (Ada, E147, "opengl__win32context_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "refcount_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "graphics_E");
   E155 : Short_Integer; pragma Import (Ada, E155, "endianess_E");
   E153 : Short_Integer; pragma Import (Ada, E153, "bytes_E");
   E149 : Short_Integer; pragma Import (Ada, E149, "win32_E");

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
      E130 := E130 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "graphics__finalize_spec");
      begin
         F2;
      end;
      E135 := E135 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "refcount__finalize_spec");
      begin
         F3;
      end;
      E126 := E126 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "config__finalize_spec");
      begin
         F4;
      end;
      E067 := E067 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "ada__text_io__finalize_spec");
      begin
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "system__file_io__finalize_body");
      begin
         E073 := E073 - 1;
         F6;
      end;
      E107 := E107 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "ada__strings__unbounded__finalize_spec");
      begin
         F7;
      end;
      E088 := E088 - 1;
      E102 := E102 - 1;
      E139 := E139 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "ada__streams__stream_io__finalize_spec");
      begin
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__file_control_block__finalize_spec");
      begin
         E086 := E086 - 1;
         F9;
      end;
      E098 := E098 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__pool_global__finalize_spec");
      begin
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "system__storage_pools__subpools__finalize_spec");
      begin
         F11;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "system__finalization_masters__finalize_spec");
      begin
         F12;
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
      E025 := E025 + 1;
      Ada.Containers'Elab_Spec;
      E131 := E131 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E078 := E078 + 1;
      Ada.Strings'Elab_Spec;
      E105 := E105 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E068 := E068 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E031 := E031 + 1;
      System.Finalization_Root'Elab_Spec;
      E077 := E077 + 1;
      Ada.Finalization'Elab_Spec;
      E075 := E075 + 1;
      System.Storage_Pools'Elab_Spec;
      E096 := E096 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E008 := E008 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E006 := E006 + 1;
      System.Pool_Global'Elab_Spec;
      E098 := E098 + 1;
      System.File_Control_Block'Elab_Spec;
      E086 := E086 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E139 := E139 + 1;
      E102 := E102 + 1;
      System.Finalization_Masters'Elab_Body;
      E088 := E088 + 1;
      E080 := E080 + 1;
      E050 := E050 + 1;
      Ada.Tags'Elab_Body;
      E058 := E058 + 1;
      E111 := E111 + 1;
      System.Soft_Links'Elab_Body;
      E015 := E015 + 1;
      System.Secondary_Stack'Elab_Body;
      E019 := E019 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E107 := E107 + 1;
      System.Os_Lib'Elab_Body;
      E083 := E083 + 1;
      System.File_Io'Elab_Body;
      E073 := E073 + 1;
      System.Strings.Stream_Ops'Elab_Body;
      E137 := E137 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E067 := E067 + 1;
      E104 := E104 + 1;
      Config'Elab_Spec;
      E126 := E126 + 1;
      E133 := E133 + 1;
      Opengl'Elab_Spec;
      E143 := E143 + 1;
      Refcount'Elab_Spec;
      E135 := E135 + 1;
      Graphics'Elab_Spec;
      E130 := E130 + 1;
      E155 := E155 + 1;
      E153 := E153 + 1;
      E149 := E149 + 1;
      Opengl.Win32context'Elab_Body;
      E147 := E147 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_test");

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
   --   C:\Users\Alexandrus\tw\core\gpr\basics.o
   --   C:\Users\Alexandrus\tw\core\gpr\config.o
   --   C:\Users\Alexandrus\tw\core\gpr\implementations.o
   --   C:\Users\Alexandrus\tw\gui\gpr\opengl.o
   --   C:\Users\Alexandrus\tw\core\gpr\refcount.o
   --   C:\Users\Alexandrus\tw\gui\gpr\graphics.o
   --   C:\Users\Alexandrus\tw\gui\tests\test.o
   --   C:\Users\Alexandrus\tw\core\gpr\types.o
   --   C:\Users\Alexandrus\tw\core\gpr\endianess.o
   --   C:\Users\Alexandrus\tw\core\gpr\bytes.o
   --   C:\Users\Alexandrus\tw\core\gpr\win32.o
   --   C:\Users\Alexandrus\tw\core\gpr\win32-gdi32.o
   --   C:\Users\Alexandrus\tw\core\gpr\win32-kernel32.o
   --   C:\Users\Alexandrus\tw\core\gpr\win32-ole32.o
   --   C:\Users\Alexandrus\tw\core\gpr\win32-opengl32.o
   --   C:\Users\Alexandrus\tw\core\gpr\win32-user32.o
   --   C:\Users\Alexandrus\tw\gui\gpr\opengl-win32context.o
   --   -LC:\Users\Alexandrus\tw\gui\tests\
   --   -LC:\Users\Alexandrus\tw\core\gpr\
   --   -LC:\Users\Alexandrus\tw\gui\gpr\
   --   -LC:/gnat/2012/lib/gcc/i686-pc-mingw32/4.5.4/adalib/
   --   -static
   --   -lgnat
   --   -Wl,--stack=0x2000000
--  END Object file/option list   

end ada_main;
