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
                    "GNAT Version: GPL 2012 (20120509)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_nettest" & ASCII.NUL;
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
   u00001 : constant Version_32 := 16#01ce048e#;
   pragma Export (C, u00001, "nettestB");
   u00002 : constant Version_32 := 16#3935bd10#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#63cfd057#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#a9c04d50#;
   pragma Export (C, u00004, "netdemoB");
   u00005 : constant Version_32 := 16#0e50c465#;
   pragma Export (C, u00005, "netdemoS");
   u00006 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00006, "adaS");
   u00007 : constant Version_32 := 16#1ee4165a#;
   pragma Export (C, u00007, "ada__exceptionsB");
   u00008 : constant Version_32 := 16#ad007709#;
   pragma Export (C, u00008, "ada__exceptionsS");
   u00009 : constant Version_32 := 16#16173147#;
   pragma Export (C, u00009, "ada__exceptions__last_chance_handlerB");
   u00010 : constant Version_32 := 16#e3a511ca#;
   pragma Export (C, u00010, "ada__exceptions__last_chance_handlerS");
   u00011 : constant Version_32 := 16#6daf90c4#;
   pragma Export (C, u00011, "systemS");
   u00012 : constant Version_32 := 16#0071025c#;
   pragma Export (C, u00012, "system__soft_linksB");
   u00013 : constant Version_32 := 16#fc13008d#;
   pragma Export (C, u00013, "system__soft_linksS");
   u00014 : constant Version_32 := 16#27940d94#;
   pragma Export (C, u00014, "system__parametersB");
   u00015 : constant Version_32 := 16#db4d9c04#;
   pragma Export (C, u00015, "system__parametersS");
   u00016 : constant Version_32 := 16#17775d6d#;
   pragma Export (C, u00016, "system__secondary_stackB");
   u00017 : constant Version_32 := 16#79c1b76a#;
   pragma Export (C, u00017, "system__secondary_stackS");
   u00018 : constant Version_32 := 16#ace32e1e#;
   pragma Export (C, u00018, "system__storage_elementsB");
   u00019 : constant Version_32 := 16#9762ed5c#;
   pragma Export (C, u00019, "system__storage_elementsS");
   u00020 : constant Version_32 := 16#4f750b3b#;
   pragma Export (C, u00020, "system__stack_checkingB");
   u00021 : constant Version_32 := 16#ce0d2ce8#;
   pragma Export (C, u00021, "system__stack_checkingS");
   u00022 : constant Version_32 := 16#7b9f0bae#;
   pragma Export (C, u00022, "system__exception_tableB");
   u00023 : constant Version_32 := 16#fcc14c61#;
   pragma Export (C, u00023, "system__exception_tableS");
   u00024 : constant Version_32 := 16#84debe5c#;
   pragma Export (C, u00024, "system__htableB");
   u00025 : constant Version_32 := 16#ee07deca#;
   pragma Export (C, u00025, "system__htableS");
   u00026 : constant Version_32 := 16#8b7dad61#;
   pragma Export (C, u00026, "system__string_hashB");
   u00027 : constant Version_32 := 16#4b334850#;
   pragma Export (C, u00027, "system__string_hashS");
   u00028 : constant Version_32 := 16#aad75561#;
   pragma Export (C, u00028, "system__exceptionsB");
   u00029 : constant Version_32 := 16#61515873#;
   pragma Export (C, u00029, "system__exceptionsS");
   u00030 : constant Version_32 := 16#010db1dc#;
   pragma Export (C, u00030, "system__exceptions_debugB");
   u00031 : constant Version_32 := 16#55dfb510#;
   pragma Export (C, u00031, "system__exceptions_debugS");
   u00032 : constant Version_32 := 16#b012ff50#;
   pragma Export (C, u00032, "system__img_intB");
   u00033 : constant Version_32 := 16#6f747006#;
   pragma Export (C, u00033, "system__img_intS");
   u00034 : constant Version_32 := 16#dc8e33ed#;
   pragma Export (C, u00034, "system__tracebackB");
   u00035 : constant Version_32 := 16#0c2844b1#;
   pragma Export (C, u00035, "system__tracebackS");
   u00036 : constant Version_32 := 16#907d882f#;
   pragma Export (C, u00036, "system__wch_conB");
   u00037 : constant Version_32 := 16#d244bef9#;
   pragma Export (C, u00037, "system__wch_conS");
   u00038 : constant Version_32 := 16#22fed88a#;
   pragma Export (C, u00038, "system__wch_stwB");
   u00039 : constant Version_32 := 16#ff5592f8#;
   pragma Export (C, u00039, "system__wch_stwS");
   u00040 : constant Version_32 := 16#b8a9e30d#;
   pragma Export (C, u00040, "system__wch_cnvB");
   u00041 : constant Version_32 := 16#ccba382f#;
   pragma Export (C, u00041, "system__wch_cnvS");
   u00042 : constant Version_32 := 16#129923ea#;
   pragma Export (C, u00042, "interfacesS");
   u00043 : constant Version_32 := 16#75729fba#;
   pragma Export (C, u00043, "system__wch_jisB");
   u00044 : constant Version_32 := 16#98c8a33b#;
   pragma Export (C, u00044, "system__wch_jisS");
   u00045 : constant Version_32 := 16#ada34a87#;
   pragma Export (C, u00045, "system__traceback_entriesB");
   u00046 : constant Version_32 := 16#3f8e7e85#;
   pragma Export (C, u00046, "system__traceback_entriesS");
   u00047 : constant Version_32 := 16#1358602f#;
   pragma Export (C, u00047, "ada__streamsS");
   u00048 : constant Version_32 := 16#5331c1d4#;
   pragma Export (C, u00048, "ada__tagsB");
   u00049 : constant Version_32 := 16#c49b6a94#;
   pragma Export (C, u00049, "ada__tagsS");
   u00050 : constant Version_32 := 16#074eccb2#;
   pragma Export (C, u00050, "system__unsigned_typesS");
   u00051 : constant Version_32 := 16#e6965fe6#;
   pragma Export (C, u00051, "system__val_unsB");
   u00052 : constant Version_32 := 16#17e62189#;
   pragma Export (C, u00052, "system__val_unsS");
   u00053 : constant Version_32 := 16#46a1f7a9#;
   pragma Export (C, u00053, "system__val_utilB");
   u00054 : constant Version_32 := 16#660205db#;
   pragma Export (C, u00054, "system__val_utilS");
   u00055 : constant Version_32 := 16#b7fa72e7#;
   pragma Export (C, u00055, "system__case_utilB");
   u00056 : constant Version_32 := 16#c0b3f04c#;
   pragma Export (C, u00056, "system__case_utilS");
   u00057 : constant Version_32 := 16#bc0fac87#;
   pragma Export (C, u00057, "ada__text_ioB");
   u00058 : constant Version_32 := 16#36d750a9#;
   pragma Export (C, u00058, "ada__text_ioS");
   u00059 : constant Version_32 := 16#7a48d8b1#;
   pragma Export (C, u00059, "interfaces__c_streamsB");
   u00060 : constant Version_32 := 16#a539be81#;
   pragma Export (C, u00060, "interfaces__c_streamsS");
   u00061 : constant Version_32 := 16#773a2d5d#;
   pragma Export (C, u00061, "system__crtlS");
   u00062 : constant Version_32 := 16#4a803ccf#;
   pragma Export (C, u00062, "system__file_ioB");
   u00063 : constant Version_32 := 16#60d89729#;
   pragma Export (C, u00063, "system__file_ioS");
   u00064 : constant Version_32 := 16#8cbe6205#;
   pragma Export (C, u00064, "ada__finalizationB");
   u00065 : constant Version_32 := 16#22e22193#;
   pragma Export (C, u00065, "ada__finalizationS");
   u00066 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00066, "system__finalization_rootB");
   u00067 : constant Version_32 := 16#225de354#;
   pragma Export (C, u00067, "system__finalization_rootS");
   u00068 : constant Version_32 := 16#b46168d5#;
   pragma Export (C, u00068, "ada__io_exceptionsS");
   u00069 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00069, "interfaces__cB");
   u00070 : constant Version_32 := 16#f05a3eb1#;
   pragma Export (C, u00070, "interfaces__cS");
   u00071 : constant Version_32 := 16#62120d5e#;
   pragma Export (C, u00071, "interfaces__c__stringsB");
   u00072 : constant Version_32 := 16#603c1c44#;
   pragma Export (C, u00072, "interfaces__c__stringsS");
   u00073 : constant Version_32 := 16#a50435f4#;
   pragma Export (C, u00073, "system__crtl__runtimeS");
   u00074 : constant Version_32 := 16#721198aa#;
   pragma Export (C, u00074, "system__os_libB");
   u00075 : constant Version_32 := 16#a6d80a38#;
   pragma Export (C, u00075, "system__os_libS");
   u00076 : constant Version_32 := 16#4cd8aca0#;
   pragma Export (C, u00076, "system__stringsB");
   u00077 : constant Version_32 := 16#da45da00#;
   pragma Export (C, u00077, "system__stringsS");
   u00078 : constant Version_32 := 16#b2907efe#;
   pragma Export (C, u00078, "system__file_control_blockS");
   u00079 : constant Version_32 := 16#6d35da9a#;
   pragma Export (C, u00079, "system__finalization_mastersB");
   u00080 : constant Version_32 := 16#075a3ce8#;
   pragma Export (C, u00080, "system__finalization_mastersS");
   u00081 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00081, "system__address_imageB");
   u00082 : constant Version_32 := 16#cc430dfe#;
   pragma Export (C, u00082, "system__address_imageS");
   u00083 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00083, "system__img_boolB");
   u00084 : constant Version_32 := 16#9876e12f#;
   pragma Export (C, u00084, "system__img_boolS");
   u00085 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00085, "system__ioB");
   u00086 : constant Version_32 := 16#f3ed678b#;
   pragma Export (C, u00086, "system__ioS");
   u00087 : constant Version_32 := 16#a7a37cb6#;
   pragma Export (C, u00087, "system__storage_poolsB");
   u00088 : constant Version_32 := 16#be018fa9#;
   pragma Export (C, u00088, "system__storage_poolsS");
   u00089 : constant Version_32 := 16#ba5d60c7#;
   pragma Export (C, u00089, "system__pool_globalB");
   u00090 : constant Version_32 := 16#d56df0a6#;
   pragma Export (C, u00090, "system__pool_globalS");
   u00091 : constant Version_32 := 16#88cd69c1#;
   pragma Export (C, u00091, "system__memoryB");
   u00092 : constant Version_32 := 16#a7242cd1#;
   pragma Export (C, u00092, "system__memoryS");
   u00093 : constant Version_32 := 16#17551a52#;
   pragma Export (C, u00093, "system__storage_pools__subpoolsB");
   u00094 : constant Version_32 := 16#738e4bc9#;
   pragma Export (C, u00094, "system__storage_pools__subpoolsS");
   u00095 : constant Version_32 := 16#855a687e#;
   pragma Export (C, u00095, "basicsB");
   u00096 : constant Version_32 := 16#b67cbffd#;
   pragma Export (C, u00096, "basicsS");
   u00097 : constant Version_32 := 16#af50e98f#;
   pragma Export (C, u00097, "ada__stringsS");
   u00098 : constant Version_32 := 16#261c554b#;
   pragma Export (C, u00098, "ada__strings__unboundedB");
   u00099 : constant Version_32 := 16#2bf53506#;
   pragma Export (C, u00099, "ada__strings__unboundedS");
   u00100 : constant Version_32 := 16#86f7ec7f#;
   pragma Export (C, u00100, "ada__strings__searchB");
   u00101 : constant Version_32 := 16#b5a8c1d6#;
   pragma Export (C, u00101, "ada__strings__searchS");
   u00102 : constant Version_32 := 16#96e9c1e7#;
   pragma Export (C, u00102, "ada__strings__mapsB");
   u00103 : constant Version_32 := 16#24318e4c#;
   pragma Export (C, u00103, "ada__strings__mapsS");
   u00104 : constant Version_32 := 16#193a50a3#;
   pragma Export (C, u00104, "system__bit_opsB");
   u00105 : constant Version_32 := 16#c30e4013#;
   pragma Export (C, u00105, "system__bit_opsS");
   u00106 : constant Version_32 := 16#12c24a43#;
   pragma Export (C, u00106, "ada__charactersS");
   u00107 : constant Version_32 := 16#051b1b7b#;
   pragma Export (C, u00107, "ada__characters__latin_1S");
   u00108 : constant Version_32 := 16#c4857ee1#;
   pragma Export (C, u00108, "system__compare_array_unsigned_8B");
   u00109 : constant Version_32 := 16#b7946609#;
   pragma Export (C, u00109, "system__compare_array_unsigned_8S");
   u00110 : constant Version_32 := 16#9d3d925a#;
   pragma Export (C, u00110, "system__address_operationsB");
   u00111 : constant Version_32 := 16#add17953#;
   pragma Export (C, u00111, "system__address_operationsS");
   u00112 : constant Version_32 := 16#6214eb0a#;
   pragma Export (C, u00112, "system__machine_codeS");
   u00113 : constant Version_32 := 16#8d43fb6a#;
   pragma Export (C, u00113, "system__atomic_countersB");
   u00114 : constant Version_32 := 16#2733fc70#;
   pragma Export (C, u00114, "system__atomic_countersS");
   u00115 : constant Version_32 := 16#a6e358bc#;
   pragma Export (C, u00115, "system__stream_attributesB");
   u00116 : constant Version_32 := 16#e89b4b3f#;
   pragma Export (C, u00116, "system__stream_attributesS");
   u00117 : constant Version_32 := 16#e88e6bc1#;
   pragma Export (C, u00117, "configB");
   u00118 : constant Version_32 := 16#d91f1b8b#;
   pragma Export (C, u00118, "configS");
   u00119 : constant Version_32 := 16#39591e91#;
   pragma Export (C, u00119, "system__concat_2B");
   u00120 : constant Version_32 := 16#967f6238#;
   pragma Export (C, u00120, "system__concat_2S");
   u00121 : constant Version_32 := 16#4b56132f#;
   pragma Export (C, u00121, "globalloopB");
   u00122 : constant Version_32 := 16#9b45c1a6#;
   pragma Export (C, u00122, "globalloopS");
   u00123 : constant Version_32 := 16#5e96b894#;
   pragma Export (C, u00123, "networkB");
   u00124 : constant Version_32 := 16#afd3fbf5#;
   pragma Export (C, u00124, "networkS");
   u00125 : constant Version_32 := 16#5e196e91#;
   pragma Export (C, u00125, "ada__containersS");
   u00126 : constant Version_32 := 16#2cba58c8#;
   pragma Export (C, u00126, "implementationsB");
   u00127 : constant Version_32 := 16#8e863d85#;
   pragma Export (C, u00127, "implementationsS");
   u00128 : constant Version_32 := 16#f9d38c24#;
   pragma Export (C, u00128, "refcountB");
   u00129 : constant Version_32 := 16#a2af9fb3#;
   pragma Export (C, u00129, "refcountS");
   u00130 : constant Version_32 := 16#41e011e1#;
   pragma Export (C, u00130, "streamsS");
   u00131 : constant Version_32 := 16#1eadf3c6#;
   pragma Export (C, u00131, "system__strings__stream_opsB");
   u00132 : constant Version_32 := 16#8453d1c6#;
   pragma Export (C, u00132, "system__strings__stream_opsS");
   u00133 : constant Version_32 := 16#a1920867#;
   pragma Export (C, u00133, "ada__streams__stream_ioB");
   u00134 : constant Version_32 := 16#f0e417a0#;
   pragma Export (C, u00134, "ada__streams__stream_ioS");
   u00135 : constant Version_32 := 16#595ba38f#;
   pragma Export (C, u00135, "system__communicationB");
   u00136 : constant Version_32 := 16#ef813eee#;
   pragma Export (C, u00136, "system__communicationS");
   u00137 : constant Version_32 := 16#18ec2770#;
   pragma Export (C, u00137, "pipenetworkB");
   u00138 : constant Version_32 := 16#19d04692#;
   pragma Export (C, u00138, "pipenetworkS");
   u00139 : constant Version_32 := 16#bbe99073#;
   pragma Export (C, u00139, "bytesB");
   u00140 : constant Version_32 := 16#5e546a8e#;
   pragma Export (C, u00140, "bytesS");
   u00141 : constant Version_32 := 16#e96884af#;
   pragma Export (C, u00141, "endianessB");
   u00142 : constant Version_32 := 16#fc384cb5#;
   pragma Export (C, u00142, "endianessS");
   u00143 : constant Version_32 := 16#60f82714#;
   pragma Export (C, u00143, "typesS");
   u00144 : constant Version_32 := 16#8f3bd8ab#;
   pragma Export (C, u00144, "system__taskingB");
   u00145 : constant Version_32 := 16#117023e3#;
   pragma Export (C, u00145, "system__taskingS");
   u00146 : constant Version_32 := 16#9f1b736c#;
   pragma Export (C, u00146, "system__task_primitivesS");
   u00147 : constant Version_32 := 16#1faa77d9#;
   pragma Export (C, u00147, "system__os_interfaceS");
   u00148 : constant Version_32 := 16#3ead0efd#;
   pragma Export (C, u00148, "system__win32S");
   u00149 : constant Version_32 := 16#527a2bd4#;
   pragma Export (C, u00149, "system__task_primitives__operationsB");
   u00150 : constant Version_32 := 16#00837a4c#;
   pragma Export (C, u00150, "system__task_primitives__operationsS");
   u00151 : constant Version_32 := 16#6f001a54#;
   pragma Export (C, u00151, "system__exp_unsB");
   u00152 : constant Version_32 := 16#3a826f18#;
   pragma Export (C, u00152, "system__exp_unsS");
   u00153 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00153, "system__float_controlB");
   u00154 : constant Version_32 := 16#8d53d3f8#;
   pragma Export (C, u00154, "system__float_controlS");
   u00155 : constant Version_32 := 16#1826115c#;
   pragma Export (C, u00155, "system__interrupt_managementB");
   u00156 : constant Version_32 := 16#92c564a4#;
   pragma Export (C, u00156, "system__interrupt_managementS");
   u00157 : constant Version_32 := 16#c313b593#;
   pragma Export (C, u00157, "system__multiprocessorsB");
   u00158 : constant Version_32 := 16#55030fb7#;
   pragma Export (C, u00158, "system__multiprocessorsS");
   u00159 : constant Version_32 := 16#3fcdd715#;
   pragma Export (C, u00159, "system__os_primitivesB");
   u00160 : constant Version_32 := 16#dd7e1ced#;
   pragma Export (C, u00160, "system__os_primitivesS");
   u00161 : constant Version_32 := 16#aa4baafd#;
   pragma Export (C, u00161, "system__win32__extS");
   u00162 : constant Version_32 := 16#5052be8c#;
   pragma Export (C, u00162, "system__task_infoB");
   u00163 : constant Version_32 := 16#ef1d87cb#;
   pragma Export (C, u00163, "system__task_infoS");
   u00164 : constant Version_32 := 16#652aa403#;
   pragma Export (C, u00164, "system__tasking__debugB");
   u00165 : constant Version_32 := 16#f32cb5c6#;
   pragma Export (C, u00165, "system__tasking__debugS");
   u00166 : constant Version_32 := 16#ae97ef6c#;
   pragma Export (C, u00166, "system__concat_3B");
   u00167 : constant Version_32 := 16#1b8592ae#;
   pragma Export (C, u00167, "system__concat_3S");
   u00168 : constant Version_32 := 16#c9fdc962#;
   pragma Export (C, u00168, "system__concat_6B");
   u00169 : constant Version_32 := 16#aa6565d0#;
   pragma Export (C, u00169, "system__concat_6S");
   u00170 : constant Version_32 := 16#def1dd00#;
   pragma Export (C, u00170, "system__concat_5B");
   u00171 : constant Version_32 := 16#7d965e65#;
   pragma Export (C, u00171, "system__concat_5S");
   u00172 : constant Version_32 := 16#3493e6c0#;
   pragma Export (C, u00172, "system__concat_4B");
   u00173 : constant Version_32 := 16#6ff0737a#;
   pragma Export (C, u00173, "system__concat_4S");
   u00174 : constant Version_32 := 16#1eab0e09#;
   pragma Export (C, u00174, "system__img_enum_newB");
   u00175 : constant Version_32 := 16#eaa85b34#;
   pragma Export (C, u00175, "system__img_enum_newS");
   u00176 : constant Version_32 := 16#194ccd7b#;
   pragma Export (C, u00176, "system__img_unsB");
   u00177 : constant Version_32 := 16#98baf045#;
   pragma Export (C, u00177, "system__img_unsS");
   u00178 : constant Version_32 := 16#7b8aedca#;
   pragma Export (C, u00178, "system__stack_usageB");
   u00179 : constant Version_32 := 16#a5188558#;
   pragma Export (C, u00179, "system__stack_usageS");
   u00180 : constant Version_32 := 16#bb8952df#;
   pragma Export (C, u00180, "system__tasking__protected_objectsB");
   u00181 : constant Version_32 := 16#0e06b2d3#;
   pragma Export (C, u00181, "system__tasking__protected_objectsS");
   u00182 : constant Version_32 := 16#2a89d93b#;
   pragma Export (C, u00182, "system__soft_links__taskingB");
   u00183 : constant Version_32 := 16#6ac0d6d0#;
   pragma Export (C, u00183, "system__soft_links__taskingS");
   u00184 : constant Version_32 := 16#17d21067#;
   pragma Export (C, u00184, "ada__exceptions__is_null_occurrenceB");
   u00185 : constant Version_32 := 16#24d5007b#;
   pragma Export (C, u00185, "ada__exceptions__is_null_occurrenceS");
   u00186 : constant Version_32 := 16#ee80728a#;
   pragma Export (C, u00186, "system__tracesB");
   u00187 : constant Version_32 := 16#9fb2f86e#;
   pragma Export (C, u00187, "system__tracesS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.atomic_counters%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.htable%s
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.io%s
   --  system.io%b
   --  system.machine_code%s
   --  system.atomic_counters%b
   --  system.multiprocessors%s
   --  system.os_primitives%s
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.stack_usage%s
   --  system.stack_usage%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  ada.exceptions.is_null_occurrence%s
   --  ada.exceptions.is_null_occurrence%b
   --  system.soft_links%s
   --  system.traces%s
   --  system.traces%b
   --  system.unsigned_types%s
   --  system.exp_uns%s
   --  system.exp_uns%b
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
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
   --  system.address_image%s
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.concat_4%s
   --  system.concat_4%b
   --  system.concat_5%s
   --  system.concat_5%b
   --  system.concat_6%s
   --  system.concat_6%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.tags%s
   --  ada.streams%s
   --  interfaces.c%s
   --  system.multiprocessors%b
   --  interfaces.c.strings%s
   --  system.crtl.runtime%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.finalization%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.storage_pools.subpools%s
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.win32%s
   --  system.os_interface%s
   --  system.interrupt_management%s
   --  system.interrupt_management%b
   --  system.task_info%s
   --  system.task_info%b
   --  system.task_primitives%s
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking%b
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.win32.ext%s
   --  system.task_primitives.operations%b
   --  system.os_primitives%b
   --  system.communication%s
   --  system.communication%b
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.file_control_block%s
   --  ada.streams.stream_io%s
   --  system.file_io%s
   --  ada.streams.stream_io%b
   --  system.secondary_stack%s
   --  system.storage_pools.subpools%b
   --  system.finalization_masters%b
   --  interfaces.c.strings%b
   --  interfaces.c%b
   --  ada.tags%b
   --  ada.strings.maps%b
   --  system.soft_links%b
   --  system.secondary_stack%b
   --  system.address_image%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  system.os_lib%s
   --  system.os_lib%b
   --  system.file_io%b
   --  system.soft_links.tasking%s
   --  system.soft_links.tasking%b
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  system.traceback%s
   --  ada.exceptions%b
   --  system.traceback%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  basics%s
   --  basics%b
   --  config%s
   --  config%b
   --  globalloop%s
   --  globalloop%b
   --  implementations%s
   --  implementations%b
   --  netdemo%s
   --  nettest%b
   --  refcount%s
   --  refcount%b
   --  streams%s
   --  network%s
   --  network%b
   --  pipenetwork%s
   --  netdemo%b
   --  types%s
   --  endianess%s
   --  endianess%b
   --  bytes%s
   --  bytes%b
   --  pipenetwork%b
   --  END ELABORATION ORDER


end ada_main;
