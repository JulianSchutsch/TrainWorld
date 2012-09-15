pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__build.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__build.adb");
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E015 : Short_Integer; pragma Import (Ada, E015, "system__soft_links_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exception_table_E");
   E155 : Short_Integer; pragma Import (Ada, E155, "ada__containers_E");
   E114 : Short_Integer; pragma Import (Ada, E114, "ada__io_exceptions_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "ada__strings_E");
   E075 : Short_Integer; pragma Import (Ada, E075, "ada__strings__maps_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "ada__strings__maps__constants_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "ada__tags_E");
   E085 : Short_Integer; pragma Import (Ada, E085, "ada__streams_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "interfaces__c_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "interfaces__c__strings_E");
   E149 : Short_Integer; pragma Import (Ada, E149, "system__regpat_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "ada__calendar_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "ada__calendar__time_zones_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__secondary_stack_E");
   E091 : Short_Integer; pragma Import (Ada, E091, "system__finalization_root_E");
   E084 : Short_Integer; pragma Import (Ada, E084, "ada__finalization_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "ada__strings__unbounded_E");
   E099 : Short_Integer; pragma Import (Ada, E099, "system__storage_pools_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__directories_E");
   E082 : Short_Integer; pragma Import (Ada, E082, "ada__finalization__heap_management_E");
   E126 : Short_Integer; pragma Import (Ada, E126, "system__os_lib_E");
   E131 : Short_Integer; pragma Import (Ada, E131, "system__pool_global_E");
   E129 : Short_Integer; pragma Import (Ada, E129, "system__file_control_block_E");
   E159 : Short_Integer; pragma Import (Ada, E159, "ada__streams__stream_io_E");
   E117 : Short_Integer; pragma Import (Ada, E117, "system__file_io_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "system__regexp_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "system__strings__stream_ops_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "ada__text_io_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "basics_E");
   E140 : Short_Integer; pragma Import (Ada, E140, "plattform_E");
   E142 : Short_Integer; pragma Import (Ada, E142, "portableexec_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
      LE_Set : Boolean;
      pragma Import (Ada, LE_Set, "__gnat_library_exception_set");
   begin
      E142 := E142 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "portableexec__finalize_spec");
      begin
         F1;
      end;
      E144 := E144 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "basics__finalize_spec");
      begin
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "ada__text_io__finalize_body");
      begin
         E138 := E138 - 1;
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "ada__text_io__finalize_spec");
      begin
         F4;
      end;
      E006 := E006 - 1;
      E136 := E136 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "system__regexp__finalize_spec");
      begin
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "ada__streams__stream_io__finalize_body");
      begin
         E159 := E159 - 1;
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__file_io__finalize_body");
      begin
         E117 := E117 - 1;
         F7;
      end;
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
         E129 := E129 - 1;
         F9;
      end;
      E131 := E131 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__pool_global__finalize_spec");
      begin
         F10;
      end;
      E082 := E082 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "ada__finalization__heap_management__finalize_spec");
      begin
         F11;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "ada__directories__finalize_spec");
      begin
         F12;
      end;
      E105 := E105 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "ada__strings__unbounded__finalize_spec");
      begin
         F13;
      end;
      E091 := E091 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "system__finalization_root__finalize_spec");
      begin
         F14;
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
      E025 := E025 + 1;
      Ada.Containers'Elab_Spec;
      E155 := E155 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E114 := E114 + 1;
      Ada.Strings'Elab_Spec;
      E073 := E073 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E078 := E078 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E085 := E085 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Regpat'Elab_Spec;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E008 := E008 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E053 := E053 + 1;
      E149 := E149 + 1;
      E121 := E121 + 1;
      E119 := E119 + 1;
      Ada.Tags'Elab_Body;
      E087 := E087 + 1;
      E075 := E075 + 1;
      System.Soft_Links'Elab_Body;
      E015 := E015 + 1;
      System.Secondary_Stack'Elab_Body;
      E019 := E019 + 1;
      System.Finalization_Root'Elab_Spec;
      E091 := E091 + 1;
      Ada.Finalization'Elab_Spec;
      E084 := E084 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E105 := E105 + 1;
      System.Storage_Pools'Elab_Spec;
      E099 := E099 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Finalization.Heap_Management'Elab_Spec;
      E082 := E082 + 1;
      System.Os_Lib'Elab_Body;
      E126 := E126 + 1;
      System.Pool_Global'Elab_Spec;
      E131 := E131 + 1;
      System.File_Control_Block'Elab_Spec;
      E129 := E129 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      System.File_Io'Elab_Body;
      E117 := E117 + 1;
      Ada.Streams.Stream_Io'Elab_Body;
      E159 := E159 + 1;
      System.Regexp'Elab_Spec;
      E136 := E136 + 1;
      Ada.Directories'Elab_Body;
      E006 := E006 + 1;
      System.Strings.Stream_Ops'Elab_Body;
      E157 := E157 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E138 := E138 + 1;
      Basics'Elab_Spec;
      E144 := E144 + 1;
      Plattform'Elab_Spec;
      Portableexec'Elab_Spec;
      E142 := E142 + 1;
      E140 := E140 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_build");

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
   --   /home/alexander/tw/build/build.o
   --   /home/alexander/tw/gpr/portableexec.o
   --   /home/alexander/tw/gpr/plattform.o
   --   -L/home/alexander/tw/build/
   --   -L/home/alexander/tw/gpr/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/4.5.3/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;
