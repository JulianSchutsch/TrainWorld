pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__coretest.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__coretest.adb");
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exception_table_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "ada__containers_E");
   E079 : Short_Integer; pragma Import (Ada, E079, "ada__io_exceptions_E");
   E104 : Short_Integer; pragma Import (Ada, E104, "ada__strings_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "ada__strings__maps_E");
   E046 : Short_Integer; pragma Import (Ada, E046, "ada__tags_E");
   E069 : Short_Integer; pragma Import (Ada, E069, "ada__streams_E");
   E060 : Short_Integer; pragma Import (Ada, E060, "interfaces__c_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "interfaces__c__strings_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "system__exceptions_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "system__finalization_root_E");
   E076 : Short_Integer; pragma Import (Ada, E076, "ada__finalization_E");
   E097 : Short_Integer; pragma Import (Ada, E097, "system__storage_pools_E");
   E089 : Short_Integer; pragma Import (Ada, E089, "system__finalization_masters_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "system__storage_pools__subpools_E");
   E099 : Short_Integer; pragma Import (Ada, E099, "system__pool_global_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "system__file_control_block_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "ada__streams__stream_io_E");
   E074 : Short_Integer; pragma Import (Ada, E074, "system__file_io_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__secondary_stack_E");
   E106 : Short_Integer; pragma Import (Ada, E106, "ada__strings__unbounded_E");
   E084 : Short_Integer; pragma Import (Ada, E084, "system__os_lib_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "system__strings__stream_ops_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__text_io_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "basics_E");
   E127 : Short_Integer; pragma Import (Ada, E127, "config_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "globalloop_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "implementations_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "testframework_E");
   E129 : Short_Integer; pragma Import (Ada, E129, "config__test_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "gimplementationstest_E");
   E148 : Short_Integer; pragma Import (Ada, E148, "globalloop__test_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "endianess_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "bytes_E");
   E062 : Short_Integer; pragma Import (Ada, E062, "bytes__test_E");
   E142 : Short_Integer; pragma Import (Ada, E142, "endianess__test_E");
   E150 : Short_Integer; pragma Import (Ada, E150, "versionparser_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "versionparser__test_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "globalloop__test__finalize_body");
      begin
         E148 := E148 - 1;
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "config__test__finalize_body");
      begin
         E129 := E129 - 1;
         F2;
      end;
      E146 := E146 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "globalloop__finalize_spec");
      begin
         F3;
      end;
      E127 := E127 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "config__finalize_spec");
      begin
         F4;
      end;
      E066 := E066 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "basics__finalize_spec");
      begin
         F5;
      end;
      E068 := E068 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "ada__text_io__finalize_spec");
      begin
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__file_io__finalize_body");
      begin
         E074 := E074 - 1;
         F7;
      end;
      E106 := E106 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "ada__strings__unbounded__finalize_spec");
      begin
         F8;
      end;
      E089 := E089 - 1;
      E103 := E103 - 1;
      E138 := E138 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "ada__streams__stream_io__finalize_spec");
      begin
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__file_control_block__finalize_spec");
      begin
         E087 := E087 - 1;
         F10;
      end;
      E099 := E099 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "system__pool_global__finalize_spec");
      begin
         F11;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "system__storage_pools__subpools__finalize_spec");
      begin
         F12;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "system__finalization_masters__finalize_spec");
      begin
         F13;
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
      E130 := E130 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E079 := E079 + 1;
      Ada.Strings'Elab_Spec;
      E104 := E104 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E069 := E069 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E027 := E027 + 1;
      System.Finalization_Root'Elab_Spec;
      E078 := E078 + 1;
      Ada.Finalization'Elab_Spec;
      E076 := E076 + 1;
      System.Storage_Pools'Elab_Spec;
      E097 := E097 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      System.Pool_Global'Elab_Spec;
      E099 := E099 + 1;
      System.File_Control_Block'Elab_Spec;
      E087 := E087 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E138 := E138 + 1;
      E103 := E103 + 1;
      System.Finalization_Masters'Elab_Body;
      E089 := E089 + 1;
      E081 := E081 + 1;
      E060 := E060 + 1;
      Ada.Tags'Elab_Body;
      E046 := E046 + 1;
      E110 := E110 + 1;
      System.Soft_Links'Elab_Body;
      E011 := E011 + 1;
      System.Secondary_Stack'Elab_Body;
      E015 := E015 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E106 := E106 + 1;
      System.Os_Lib'Elab_Body;
      E084 := E084 + 1;
      System.File_Io'Elab_Body;
      E074 := E074 + 1;
      System.Strings.Stream_Ops'Elab_Body;
      E136 := E136 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E068 := E068 + 1;
      Basics'Elab_Spec;
      E066 := E066 + 1;
      Config'Elab_Spec;
      E127 := E127 + 1;
      Globalloop'Elab_Spec;
      E146 := E146 + 1;
      E132 := E132 + 1;
      E125 := E125 + 1;
      Config.Test'Elab_Spec;
      Config.Test'Elab_Body;
      E129 := E129 + 1;
      Gimplementationstest'Elab_Spec;
      Gimplementationstest'Elab_Body;
      E144 := E144 + 1;
      Globalloop.Test'Elab_Spec;
      Globalloop.Test'Elab_Body;
      E148 := E148 + 1;
      E057 := E057 + 1;
      E055 := E055 + 1;
      Bytes.Test'Elab_Spec;
      E062 := E062 + 1;
      Endianess.Test'Elab_Spec;
      E142 := E142 + 1;
      Versionparser'Elab_Spec;
      E150 := E150 + 1;
      Versionparser.Test'Elab_Spec;
      E152 := E152 + 1;
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
   --   C:\Users\alexandrus\tw\gpr\basics.o
   --   C:\Users\alexandrus\tw\gpr\config.o
   --   C:\Users\alexandrus\tw\gpr\globalloop.o
   --   C:\Users\alexandrus\tw\gpr\implementations.o
   --   C:\Users\alexandrus\tw\gpr\testframework.o
   --   C:\Users\alexandrus\tw\tests\config-test.o
   --   C:\Users\alexandrus\tw\tests\gimplementationstest.o
   --   C:\Users\alexandrus\tw\tests\globalloop-test.o
   --   C:\Users\alexandrus\tw\gpr\types.o
   --   C:\Users\alexandrus\tw\gpr\endianess.o
   --   C:\Users\alexandrus\tw\gpr\bytes.o
   --   C:\Users\alexandrus\tw\tests\bytes-test.o
   --   C:\Users\alexandrus\tw\tests\endianess-test.o
   --   C:\Users\alexandrus\tw\gpr\versionparser.o
   --   C:\Users\alexandrus\tw\tests\versionparser-test.o
   --   C:\Users\alexandrus\tw\tests\coretest.o
   --   -LC:\Users\alexandrus\tw\tests\
   --   -LC:\Users\alexandrus\tw\gpr\
   --   -LC:/gnat/2012/lib/gcc/i686-pc-mingw32/4.5.4/adalib/
   --   -static
   --   -lgnat
   --   -Wl,--stack=0x2000000
--  END Object file/option list   

end ada_main;
