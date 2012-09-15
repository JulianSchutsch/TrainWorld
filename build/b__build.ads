pragma Ada_95;
with System;
package ada_main is
   pragma Warnings (Off);

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2011 (20110419)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_build" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#e708f855#;
   pragma Export (C, u00001, "buildB");
   u00002 : constant Version_32 := 16#7d892fe9#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#2d81b798#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#23e1f70b#;
   pragma Export (C, u00004, "systemS");
   u00005 : constant Version_32 := 16#2989cad8#;
   pragma Export (C, u00005, "system__memoryB");
   u00006 : constant Version_32 := 16#e96a4b1e#;
   pragma Export (C, u00006, "system__memoryS");
   u00007 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00007, "adaS");
   u00008 : constant Version_32 := 16#e4c5cfb2#;
   pragma Export (C, u00008, "ada__exceptionsB");
   u00009 : constant Version_32 := 16#04af002e#;
   pragma Export (C, u00009, "ada__exceptionsS");
   u00010 : constant Version_32 := 16#52aba3be#;
   pragma Export (C, u00010, "ada__exceptions__last_chance_handlerB");
   u00011 : constant Version_32 := 16#48e7b9e5#;
   pragma Export (C, u00011, "ada__exceptions__last_chance_handlerS");
   u00012 : constant Version_32 := 16#360d120c#;
   pragma Export (C, u00012, "system__soft_linksB");
   u00013 : constant Version_32 := 16#5da35d94#;
   pragma Export (C, u00013, "system__soft_linksS");
   u00014 : constant Version_32 := 16#92dc3a55#;
   pragma Export (C, u00014, "system__parametersB");
   u00015 : constant Version_32 := 16#204bcc0a#;
   pragma Export (C, u00015, "system__parametersS");
   u00016 : constant Version_32 := 16#1907a5d3#;
   pragma Export (C, u00016, "system__secondary_stackB");
   u00017 : constant Version_32 := 16#378fd0a5#;
   pragma Export (C, u00017, "system__secondary_stackS");
   u00018 : constant Version_32 := 16#ace32e1e#;
   pragma Export (C, u00018, "system__storage_elementsB");
   u00019 : constant Version_32 := 16#d92c8a93#;
   pragma Export (C, u00019, "system__storage_elementsS");
   u00020 : constant Version_32 := 16#4f750b3b#;
   pragma Export (C, u00020, "system__stack_checkingB");
   u00021 : constant Version_32 := 16#80434b27#;
   pragma Export (C, u00021, "system__stack_checkingS");
   u00022 : constant Version_32 := 16#53547b86#;
   pragma Export (C, u00022, "system__exception_tableB");
   u00023 : constant Version_32 := 16#b28f2bae#;
   pragma Export (C, u00023, "system__exception_tableS");
   u00024 : constant Version_32 := 16#ff3fa16b#;
   pragma Export (C, u00024, "system__htableB");
   u00025 : constant Version_32 := 16#cc3e5bd4#;
   pragma Export (C, u00025, "system__htableS");
   u00026 : constant Version_32 := 16#8b7dad61#;
   pragma Export (C, u00026, "system__string_hashB");
   u00027 : constant Version_32 := 16#057d2f9f#;
   pragma Export (C, u00027, "system__string_hashS");
   u00028 : constant Version_32 := 16#6a8a6a74#;
   pragma Export (C, u00028, "system__exceptionsB");
   u00029 : constant Version_32 := 16#b55fce9f#;
   pragma Export (C, u00029, "system__exceptionsS");
   u00030 : constant Version_32 := 16#b012ff50#;
   pragma Export (C, u00030, "system__img_intB");
   u00031 : constant Version_32 := 16#213a17c9#;
   pragma Export (C, u00031, "system__img_intS");
   u00032 : constant Version_32 := 16#dc8e33ed#;
   pragma Export (C, u00032, "system__tracebackB");
   u00033 : constant Version_32 := 16#4266237e#;
   pragma Export (C, u00033, "system__tracebackS");
   u00034 : constant Version_32 := 16#4900ab7d#;
   pragma Export (C, u00034, "system__unsigned_typesS");
   u00035 : constant Version_32 := 16#907d882f#;
   pragma Export (C, u00035, "system__wch_conB");
   u00036 : constant Version_32 := 16#9c0ad936#;
   pragma Export (C, u00036, "system__wch_conS");
   u00037 : constant Version_32 := 16#22fed88a#;
   pragma Export (C, u00037, "system__wch_stwB");
   u00038 : constant Version_32 := 16#b11bf537#;
   pragma Export (C, u00038, "system__wch_stwS");
   u00039 : constant Version_32 := 16#5d4d477e#;
   pragma Export (C, u00039, "system__wch_cnvB");
   u00040 : constant Version_32 := 16#82f45fe0#;
   pragma Export (C, u00040, "system__wch_cnvS");
   u00041 : constant Version_32 := 16#f77d8799#;
   pragma Export (C, u00041, "interfacesS");
   u00042 : constant Version_32 := 16#75729fba#;
   pragma Export (C, u00042, "system__wch_jisB");
   u00043 : constant Version_32 := 16#d686c4f4#;
   pragma Export (C, u00043, "system__wch_jisS");
   u00044 : constant Version_32 := 16#ada34a87#;
   pragma Export (C, u00044, "system__traceback_entriesB");
   u00045 : constant Version_32 := 16#71c0194a#;
   pragma Export (C, u00045, "system__traceback_entriesS");
   u00046 : constant Version_32 := 16#8c3c7d53#;
   pragma Export (C, u00046, "system__crtlS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  system%s
   --  system.htable%s
   --  system.img_int%s
   --  system.img_int%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  system.standard_library%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.soft_links%s
   --  system.unsigned_types%s
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.secondary_stack%s
   --  system.soft_links%b
   --  system.secondary_stack%b
   --  system.traceback%s
   --  ada.exceptions%b
   --  system.traceback%b
   --  build%b
   --  END ELABORATION ORDER


end ada_main;
