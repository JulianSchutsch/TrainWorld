pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__nettest.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__nettest.adb");
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E013 : Short_Integer; pragma Import (Ada, E013, "system__soft_links_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exception_table_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "ada__containers_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__io_exceptions_E");
   E097 : Short_Integer; pragma Import (Ada, E097, "ada__strings_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "ada__strings__maps_E");
   E049 : Short_Integer; pragma Import (Ada, E049, "ada__tags_E");
   E047 : Short_Integer; pragma Import (Ada, E047, "ada__streams_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "interfaces__c_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "interfaces__c__strings_E");
   E029 : Short_Integer; pragma Import (Ada, E029, "system__exceptions_E");
   E067 : Short_Integer; pragma Import (Ada, E067, "system__finalization_root_E");
   E065 : Short_Integer; pragma Import (Ada, E065, "ada__finalization_E");
   E088 : Short_Integer; pragma Import (Ada, E088, "system__storage_pools_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "system__finalization_masters_E");
   E094 : Short_Integer; pragma Import (Ada, E094, "system__storage_pools__subpools_E");
   E163 : Short_Integer; pragma Import (Ada, E163, "system__task_info_E");
   E090 : Short_Integer; pragma Import (Ada, E090, "system__pool_global_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "system__file_control_block_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "ada__streams__stream_io_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "system__file_io_E");
   E017 : Short_Integer; pragma Import (Ada, E017, "system__secondary_stack_E");
   E099 : Short_Integer; pragma Import (Ada, E099, "ada__strings__unbounded_E");
   E075 : Short_Integer; pragma Import (Ada, E075, "system__os_lib_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "system__strings__stream_ops_E");
   E181 : Short_Integer; pragma Import (Ada, E181, "system__tasking__protected_objects_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "ada__text_io_E");
   E096 : Short_Integer; pragma Import (Ada, E096, "basics_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "config_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "globalloop_E");
   E127 : Short_Integer; pragma Import (Ada, E127, "implementations_E");
   E005 : Short_Integer; pragma Import (Ada, E005, "netdemo_E");
   E129 : Short_Integer; pragma Import (Ada, E129, "refcount_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "streams_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "network_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "pipenetwork_E");
   E142 : Short_Integer; pragma Import (Ada, E142, "endianess_E");
   E140 : Short_Integer; pragma Import (Ada, E140, "bytes_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "pipenetwork__finalize_body");
      begin
         E138 := E138 - 1;
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "netdemo__finalize_body");
      begin
         E005 := E005 - 1;
         F2;
      end;
      E124 := E124 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "network__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "streams__finalize_spec");
      begin
         E130 := E130 - 1;
         F4;
      end;
      E129 := E129 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "refcount__finalize_spec");
      begin
         F5;
      end;
      E122 := E122 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "globalloop__finalize_spec");
      begin
         F6;
      end;
      E118 := E118 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "config__finalize_spec");
      begin
         F7;
      end;
      E096 := E096 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "basics__finalize_spec");
      begin
         F8;
      end;
      E058 := E058 - 1;
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
         E063 := E063 - 1;
         F10;
      end;
      E099 := E099 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "ada__strings__unbounded__finalize_spec");
      begin
         F11;
      end;
      E080 := E080 - 1;
      E094 := E094 - 1;
      E134 := E134 - 1;
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
         E078 := E078 - 1;
         F13;
      end;
      E090 := E090 - 1;
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
      E023 := E023 + 1;
      Ada.Containers'Elab_Spec;
      E125 := E125 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E068 := E068 + 1;
      Ada.Strings'Elab_Spec;
      E097 := E097 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E047 := E047 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E029 := E029 + 1;
      System.Finalization_Root'Elab_Spec;
      E067 := E067 + 1;
      Ada.Finalization'Elab_Spec;
      E065 := E065 + 1;
      System.Storage_Pools'Elab_Spec;
      E088 := E088 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      System.Task_Info'Elab_Spec;
      E163 := E163 + 1;
      System.Pool_Global'Elab_Spec;
      E090 := E090 + 1;
      System.File_Control_Block'Elab_Spec;
      E078 := E078 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E134 := E134 + 1;
      E094 := E094 + 1;
      System.Finalization_Masters'Elab_Body;
      E080 := E080 + 1;
      E072 := E072 + 1;
      E070 := E070 + 1;
      Ada.Tags'Elab_Body;
      E049 := E049 + 1;
      E103 := E103 + 1;
      System.Soft_Links'Elab_Body;
      E013 := E013 + 1;
      System.Secondary_Stack'Elab_Body;
      E017 := E017 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E099 := E099 + 1;
      System.Os_Lib'Elab_Body;
      E075 := E075 + 1;
      System.File_Io'Elab_Body;
      E063 := E063 + 1;
      System.Strings.Stream_Ops'Elab_Body;
      E132 := E132 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E181 := E181 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E058 := E058 + 1;
      Basics'Elab_Spec;
      E096 := E096 + 1;
      Config'Elab_Spec;
      E118 := E118 + 1;
      Globalloop'Elab_Spec;
      E122 := E122 + 1;
      E127 := E127 + 1;
      Refcount'Elab_Spec;
      E129 := E129 + 1;
      Streams'Elab_Spec;
      E130 := E130 + 1;
      Network'Elab_Spec;
      E124 := E124 + 1;
      Netdemo'Elab_Body;
      E005 := E005 + 1;
      E142 := E142 + 1;
      E140 := E140 + 1;
      Pipenetwork'Elab_Body;
      E138 := E138 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_nettest");

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
   --   C:\Users\alexandrus\tw\tests\nettest.o
   --   C:\Users\alexandrus\tw\gpr\refcount.o
   --   C:\Users\alexandrus\tw\gpr\streams.o
   --   C:\Users\alexandrus\tw\gpr\network.o
   --   C:\Users\alexandrus\tw\tests\netdemo.o
   --   C:\Users\alexandrus\tw\gpr\types.o
   --   C:\Users\alexandrus\tw\gpr\endianess.o
   --   C:\Users\alexandrus\tw\gpr\bytes.o
   --   C:\Users\alexandrus\tw\gpr\pipenetwork.o
   --   -LC:\Users\alexandrus\tw\tests\
   --   -LC:\Users\alexandrus\tw\gpr\
   --   -LC:/gnat/2012/lib/gcc/i686-pc-mingw32/4.5.4/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -Xlinker
   --   --stack=0x200000,0x1000
   --   -mthreads
   --   -Wl,--stack=0x2000000
--  END Object file/option list   

end ada_main;
