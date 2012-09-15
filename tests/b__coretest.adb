pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__coretest.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__coretest.adb");
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exception_table_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "ada__containers_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "ada__io_exceptions_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "ada__strings_E");
   E106 : Short_Integer; pragma Import (Ada, E106, "ada__strings__maps_E");
   E045 : Short_Integer; pragma Import (Ada, E045, "ada__tags_E");
   E071 : Short_Integer; pragma Import (Ada, E071, "ada__streams_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "interfaces__c_E");
   E089 : Short_Integer; pragma Import (Ada, E089, "interfaces__c__strings_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__secondary_stack_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "system__finalization_root_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "ada__finalization_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "ada__strings__unbounded_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "system__storage_pools_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__finalization__heap_management_E");
   E092 : Short_Integer; pragma Import (Ada, E092, "system__os_lib_E");
   E097 : Short_Integer; pragma Import (Ada, E097, "system__pool_global_E");
   E095 : Short_Integer; pragma Import (Ada, E095, "system__file_control_block_E");
   E133 : Short_Integer; pragma Import (Ada, E133, "ada__streams__stream_io_E");
   E086 : Short_Integer; pragma Import (Ada, E086, "system__file_io_E");
   E131 : Short_Integer; pragma Import (Ada, E131, "system__strings__stream_ops_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "ada__text_io_E");
   E064 : Short_Integer; pragma Import (Ada, E064, "basics_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "config_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "globalloop_E");
   E127 : Short_Integer; pragma Import (Ada, E127, "implementations_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "testframework_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "config__test_E");
   E139 : Short_Integer; pragma Import (Ada, E139, "gimplementationstest_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "globalloop__test_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "endianess_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "bytes_E");
   E060 : Short_Integer; pragma Import (Ada, E060, "bytes__test_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "endianess__test_E");
   E145 : Short_Integer; pragma Import (Ada, E145, "versionparser_E");
   E147 : Short_Integer; pragma Import (Ada, E147, "versionparser__test_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
      LE_Set : Boolean;
      pragma Import (Ada, LE_Set, "__gnat_library_exception_set");
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "globalloop__test__finalize_body");
      begin
         E143 := E143 - 1;
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "config__test__finalize_body");
      begin
         E124 := E124 - 1;
         F2;
      end;
      E141 := E141 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "globalloop__finalize_spec");
      begin
         F3;
      end;
      E122 := E122 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "config__finalize_spec");
      begin
         F4;
      end;
      E064 := E064 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "basics__finalize_spec");
      begin
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "ada__text_io__finalize_body");
      begin
         E066 := E066 - 1;
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "ada__text_io__finalize_spec");
      begin
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "ada__streams__stream_io__finalize_body");
      begin
         E133 := E133 - 1;
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__file_io__finalize_body");
      begin
         E086 := E086 - 1;
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "ada__streams__stream_io__finalize_spec");
      begin
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "system__file_control_block__finalize_spec");
      begin
         E095 := E095 - 1;
         F11;
      end;
      E097 := E097 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "system__pool_global__finalize_spec");
      begin
         F12;
      end;
      E068 := E068 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "ada__finalization__heap_management__finalize_spec");
      begin
         F13;
      end;
      E102 := E102 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "ada__strings__unbounded__finalize_spec");
      begin
         F14;
      end;
      E073 := E073 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "system__finalization_root__finalize_spec");
      begin
         F15;
      end;
      if LE_Set then
         declare
            LE : Ada.Exceptions.Exception_Occurrence;
            pragma Import (Ada, LE, "__gnat_library_exception");
            procedure Raise_From_Controlled_Operation (X : Ada.Exceptions.Exception_Occurrence;  From_Abort : Boolean);
            pragma Import (Ada, Raise_From_Controlled_Operation, "__gnat_raise_from_controlled_operation");
         begin
            Raise_From_Controlled_Operation (LE, False);
         end;
      end if;
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
      E125 := E125 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E087 := E087 + 1;
      Ada.Strings'Elab_Spec;
      E100 := E100 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E071 := E071 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      E089 := E089 + 1;
      E058 := E058 + 1;
      Ada.Tags'Elab_Body;
      E045 := E045 + 1;
      E106 := E106 + 1;
      System.Soft_Links'Elab_Body;
      E011 := E011 + 1;
      System.Secondary_Stack'Elab_Body;
      E015 := E015 + 1;
      System.Finalization_Root'Elab_Spec;
      E073 := E073 + 1;
      Ada.Finalization'Elab_Spec;
      E070 := E070 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E102 := E102 + 1;
      System.Storage_Pools'Elab_Spec;
      E081 := E081 + 1;
      Ada.Finalization.Heap_Management'Elab_Spec;
      E068 := E068 + 1;
      System.Os_Lib'Elab_Body;
      E092 := E092 + 1;
      System.Pool_Global'Elab_Spec;
      E097 := E097 + 1;
      System.File_Control_Block'Elab_Spec;
      E095 := E095 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      System.File_Io'Elab_Body;
      E086 := E086 + 1;
      Ada.Streams.Stream_Io'Elab_Body;
      E133 := E133 + 1;
      System.Strings.Stream_Ops'Elab_Body;
      E131 := E131 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E066 := E066 + 1;
      Basics'Elab_Spec;
      E064 := E064 + 1;
      Config'Elab_Spec;
      E122 := E122 + 1;
      Globalloop'Elab_Spec;
      E141 := E141 + 1;
      E127 := E127 + 1;
      E120 := E120 + 1;
      Config.Test'Elab_Spec;
      Config.Test'Elab_Body;
      E124 := E124 + 1;
      Gimplementationstest'Elab_Spec;
      Gimplementationstest'Elab_Body;
      E139 := E139 + 1;
      Globalloop.Test'Elab_Spec;
      Globalloop.Test'Elab_Body;
      E143 := E143 + 1;
      E055 := E055 + 1;
      E053 := E053 + 1;
      Bytes.Test'Elab_Spec;
      E060 := E060 + 1;
      Endianess.Test'Elab_Spec;
      E137 := E137 + 1;
      Versionparser'Elab_Spec;
      E145 := E145 + 1;
      Versionparser.Test'Elab_Spec;
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
   --   /home/alexander/tw/gpr/basics.o
   --   /home/alexander/tw/gpr/config.o
   --   /home/alexander/tw/gpr/globalloop.o
   --   /home/alexander/tw/gpr/implementations.o
   --   /home/alexander/tw/gpr/testframework.o
   --   /home/alexander/tw/tests/config-test.o
   --   /home/alexander/tw/tests/gimplementationstest.o
   --   /home/alexander/tw/tests/globalloop-test.o
   --   /home/alexander/tw/gpr/types.o
   --   /home/alexander/tw/gpr/endianess.o
   --   /home/alexander/tw/gpr/bytes.o
   --   /home/alexander/tw/tests/bytes-test.o
   --   /home/alexander/tw/tests/endianess-test.o
   --   /home/alexander/tw/gpr/versionparser.o
   --   /home/alexander/tw/tests/versionparser-test.o
   --   /home/alexander/tw/tests/coretest.o
   --   -L/home/alexander/tw/tests/
   --   -L/home/alexander/tw/gpr/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/4.5.3/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;
