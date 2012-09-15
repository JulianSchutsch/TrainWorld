pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__rfstest.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__rfstest.adb");
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exception_table_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "ada__io_exceptions_E");
   E095 : Short_Integer; pragma Import (Ada, E095, "ada__strings_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "ada__strings__maps_E");
   E045 : Short_Integer; pragma Import (Ada, E045, "ada__tags_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "ada__streams_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "interfaces__c_E");
   E082 : Short_Integer; pragma Import (Ada, E082, "interfaces__c__strings_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__secondary_stack_E");
   E060 : Short_Integer; pragma Import (Ada, E060, "system__finalization_root_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "ada__finalization_E");
   E097 : Short_Integer; pragma Import (Ada, E097, "ada__strings__unbounded_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "system__storage_pools_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "ada__finalization__heap_management_E");
   E085 : Short_Integer; pragma Import (Ada, E085, "system__os_lib_E");
   E090 : Short_Integer; pragma Import (Ada, E090, "system__pool_global_E");
   E088 : Short_Integer; pragma Import (Ada, E088, "system__file_control_block_E");
   E077 : Short_Integer; pragma Import (Ada, E077, "system__file_io_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "ada__text_io_E");
   E094 : Short_Integer; pragma Import (Ada, E094, "basics_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "refcount_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "resourcefilesystem_E");
   E114 : Short_Integer; pragma Import (Ada, E114, "streams_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
      LE_Set : Boolean;
      pragma Import (Ada, LE_Set, "__gnat_library_exception_set");
   begin
      E053 := E053 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "resourcefilesystem__finalize_spec");
      begin
         F1;
      end;
      E070 := E070 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "refcount__finalize_spec");
      begin
         F2;
      end;
      E094 := E094 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "basics__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "ada__text_io__finalize_body");
      begin
         E072 := E072 - 1;
         F4;
      end;
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
         E077 := E077 - 1;
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__file_control_block__finalize_spec");
      begin
         E088 := E088 - 1;
         F7;
      end;
      E090 := E090 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "system__pool_global__finalize_spec");
      begin
         F8;
      end;
      E055 := E055 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "ada__finalization__heap_management__finalize_spec");
      begin
         F9;
      end;
      E097 := E097 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "ada__strings__unbounded__finalize_spec");
      begin
         F10;
      end;
      E060 := E060 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "system__finalization_root__finalize_spec");
      begin
         F11;
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
      Ada.Io_Exceptions'Elab_Spec;
      E078 := E078 + 1;
      Ada.Strings'Elab_Spec;
      E095 := E095 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E058 := E058 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      E082 := E082 + 1;
      E080 := E080 + 1;
      Ada.Tags'Elab_Body;
      E045 := E045 + 1;
      E101 := E101 + 1;
      System.Soft_Links'Elab_Body;
      E011 := E011 + 1;
      System.Secondary_Stack'Elab_Body;
      E015 := E015 + 1;
      System.Finalization_Root'Elab_Spec;
      E060 := E060 + 1;
      Ada.Finalization'Elab_Spec;
      E057 := E057 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E097 := E097 + 1;
      System.Storage_Pools'Elab_Spec;
      E068 := E068 + 1;
      Ada.Finalization.Heap_Management'Elab_Spec;
      E055 := E055 + 1;
      System.Os_Lib'Elab_Body;
      E085 := E085 + 1;
      System.Pool_Global'Elab_Spec;
      E090 := E090 + 1;
      System.File_Control_Block'Elab_Spec;
      E088 := E088 + 1;
      System.File_Io'Elab_Body;
      E077 := E077 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E072 := E072 + 1;
      Basics'Elab_Spec;
      E094 := E094 + 1;
      Refcount'Elab_Spec;
      E070 := E070 + 1;
      Resourcefilesystem'Elab_Spec;
      E053 := E053 + 1;
      Streams'Elab_Spec;
      E114 := E114 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_rfstest");

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
   --   /home/alexander/tw/gpr/refcount.o
   --   /home/alexander/tw/gpr/resourcefilesystem.o
   --   /home/alexander/tw/gpr/types.o
   --   /home/alexander/tw/gpr/streams.o
   --   /home/alexander/tw/tests/rfstest.o
   --   -L/home/alexander/tw/tests/
   --   -L/home/alexander/tw/gpr/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/4.5.3/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;
