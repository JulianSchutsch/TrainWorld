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

   Ada_Main_Program_Name : constant String := "_ada_guitest" & ASCII.NUL;
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
   u00001 : constant Version_32 := 16#d21b97d3#;
   pragma Export (C, u00001, "guitestB");
   u00002 : constant Version_32 := 16#3935bd10#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#63cfd057#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#1ee4165a#;
   pragma Export (C, u00005, "ada__exceptionsB");
   u00006 : constant Version_32 := 16#ad007709#;
   pragma Export (C, u00006, "ada__exceptionsS");
   u00007 : constant Version_32 := 16#16173147#;
   pragma Export (C, u00007, "ada__exceptions__last_chance_handlerB");
   u00008 : constant Version_32 := 16#e3a511ca#;
   pragma Export (C, u00008, "ada__exceptions__last_chance_handlerS");
   u00009 : constant Version_32 := 16#6daf90c4#;
   pragma Export (C, u00009, "systemS");
   u00010 : constant Version_32 := 16#0071025c#;
   pragma Export (C, u00010, "system__soft_linksB");
   u00011 : constant Version_32 := 16#fc13008d#;
   pragma Export (C, u00011, "system__soft_linksS");
   u00012 : constant Version_32 := 16#27940d94#;
   pragma Export (C, u00012, "system__parametersB");
   u00013 : constant Version_32 := 16#db4d9c04#;
   pragma Export (C, u00013, "system__parametersS");
   u00014 : constant Version_32 := 16#17775d6d#;
   pragma Export (C, u00014, "system__secondary_stackB");
   u00015 : constant Version_32 := 16#79c1b76a#;
   pragma Export (C, u00015, "system__secondary_stackS");
   u00016 : constant Version_32 := 16#ace32e1e#;
   pragma Export (C, u00016, "system__storage_elementsB");
   u00017 : constant Version_32 := 16#9762ed5c#;
   pragma Export (C, u00017, "system__storage_elementsS");
   u00018 : constant Version_32 := 16#4f750b3b#;
   pragma Export (C, u00018, "system__stack_checkingB");
   u00019 : constant Version_32 := 16#ce0d2ce8#;
   pragma Export (C, u00019, "system__stack_checkingS");
   u00020 : constant Version_32 := 16#7b9f0bae#;
   pragma Export (C, u00020, "system__exception_tableB");
   u00021 : constant Version_32 := 16#fcc14c61#;
   pragma Export (C, u00021, "system__exception_tableS");
   u00022 : constant Version_32 := 16#84debe5c#;
   pragma Export (C, u00022, "system__htableB");
   u00023 : constant Version_32 := 16#ee07deca#;
   pragma Export (C, u00023, "system__htableS");
   u00024 : constant Version_32 := 16#8b7dad61#;
   pragma Export (C, u00024, "system__string_hashB");
   u00025 : constant Version_32 := 16#4b334850#;
   pragma Export (C, u00025, "system__string_hashS");
   u00026 : constant Version_32 := 16#aad75561#;
   pragma Export (C, u00026, "system__exceptionsB");
   u00027 : constant Version_32 := 16#61515873#;
   pragma Export (C, u00027, "system__exceptionsS");
   u00028 : constant Version_32 := 16#010db1dc#;
   pragma Export (C, u00028, "system__exceptions_debugB");
   u00029 : constant Version_32 := 16#55dfb510#;
   pragma Export (C, u00029, "system__exceptions_debugS");
   u00030 : constant Version_32 := 16#b012ff50#;
   pragma Export (C, u00030, "system__img_intB");
   u00031 : constant Version_32 := 16#6f747006#;
   pragma Export (C, u00031, "system__img_intS");
   u00032 : constant Version_32 := 16#dc8e33ed#;
   pragma Export (C, u00032, "system__tracebackB");
   u00033 : constant Version_32 := 16#0c2844b1#;
   pragma Export (C, u00033, "system__tracebackS");
   u00034 : constant Version_32 := 16#907d882f#;
   pragma Export (C, u00034, "system__wch_conB");
   u00035 : constant Version_32 := 16#d244bef9#;
   pragma Export (C, u00035, "system__wch_conS");
   u00036 : constant Version_32 := 16#22fed88a#;
   pragma Export (C, u00036, "system__wch_stwB");
   u00037 : constant Version_32 := 16#ff5592f8#;
   pragma Export (C, u00037, "system__wch_stwS");
   u00038 : constant Version_32 := 16#b8a9e30d#;
   pragma Export (C, u00038, "system__wch_cnvB");
   u00039 : constant Version_32 := 16#ccba382f#;
   pragma Export (C, u00039, "system__wch_cnvS");
   u00040 : constant Version_32 := 16#129923ea#;
   pragma Export (C, u00040, "interfacesS");
   u00041 : constant Version_32 := 16#75729fba#;
   pragma Export (C, u00041, "system__wch_jisB");
   u00042 : constant Version_32 := 16#98c8a33b#;
   pragma Export (C, u00042, "system__wch_jisS");
   u00043 : constant Version_32 := 16#ada34a87#;
   pragma Export (C, u00043, "system__traceback_entriesB");
   u00044 : constant Version_32 := 16#3f8e7e85#;
   pragma Export (C, u00044, "system__traceback_entriesS");
   u00045 : constant Version_32 := 16#af50e98f#;
   pragma Export (C, u00045, "ada__stringsS");
   u00046 : constant Version_32 := 16#261c554b#;
   pragma Export (C, u00046, "ada__strings__unboundedB");
   u00047 : constant Version_32 := 16#2bf53506#;
   pragma Export (C, u00047, "ada__strings__unboundedS");
   u00048 : constant Version_32 := 16#86f7ec7f#;
   pragma Export (C, u00048, "ada__strings__searchB");
   u00049 : constant Version_32 := 16#b5a8c1d6#;
   pragma Export (C, u00049, "ada__strings__searchS");
   u00050 : constant Version_32 := 16#96e9c1e7#;
   pragma Export (C, u00050, "ada__strings__mapsB");
   u00051 : constant Version_32 := 16#24318e4c#;
   pragma Export (C, u00051, "ada__strings__mapsS");
   u00052 : constant Version_32 := 16#193a50a3#;
   pragma Export (C, u00052, "system__bit_opsB");
   u00053 : constant Version_32 := 16#c30e4013#;
   pragma Export (C, u00053, "system__bit_opsS");
   u00054 : constant Version_32 := 16#074eccb2#;
   pragma Export (C, u00054, "system__unsigned_typesS");
   u00055 : constant Version_32 := 16#12c24a43#;
   pragma Export (C, u00055, "ada__charactersS");
   u00056 : constant Version_32 := 16#051b1b7b#;
   pragma Export (C, u00056, "ada__characters__latin_1S");
   u00057 : constant Version_32 := 16#5331c1d4#;
   pragma Export (C, u00057, "ada__tagsB");
   u00058 : constant Version_32 := 16#c49b6a94#;
   pragma Export (C, u00058, "ada__tagsS");
   u00059 : constant Version_32 := 16#e6965fe6#;
   pragma Export (C, u00059, "system__val_unsB");
   u00060 : constant Version_32 := 16#17e62189#;
   pragma Export (C, u00060, "system__val_unsS");
   u00061 : constant Version_32 := 16#46a1f7a9#;
   pragma Export (C, u00061, "system__val_utilB");
   u00062 : constant Version_32 := 16#660205db#;
   pragma Export (C, u00062, "system__val_utilS");
   u00063 : constant Version_32 := 16#b7fa72e7#;
   pragma Export (C, u00063, "system__case_utilB");
   u00064 : constant Version_32 := 16#c0b3f04c#;
   pragma Export (C, u00064, "system__case_utilS");
   u00065 : constant Version_32 := 16#c4857ee1#;
   pragma Export (C, u00065, "system__compare_array_unsigned_8B");
   u00066 : constant Version_32 := 16#b7946609#;
   pragma Export (C, u00066, "system__compare_array_unsigned_8S");
   u00067 : constant Version_32 := 16#9d3d925a#;
   pragma Export (C, u00067, "system__address_operationsB");
   u00068 : constant Version_32 := 16#add17953#;
   pragma Export (C, u00068, "system__address_operationsS");
   u00069 : constant Version_32 := 16#6214eb0a#;
   pragma Export (C, u00069, "system__machine_codeS");
   u00070 : constant Version_32 := 16#17551a52#;
   pragma Export (C, u00070, "system__storage_pools__subpoolsB");
   u00071 : constant Version_32 := 16#738e4bc9#;
   pragma Export (C, u00071, "system__storage_pools__subpoolsS");
   u00072 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00072, "system__address_imageB");
   u00073 : constant Version_32 := 16#cc430dfe#;
   pragma Export (C, u00073, "system__address_imageS");
   u00074 : constant Version_32 := 16#6d35da9a#;
   pragma Export (C, u00074, "system__finalization_mastersB");
   u00075 : constant Version_32 := 16#075a3ce8#;
   pragma Export (C, u00075, "system__finalization_mastersS");
   u00076 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00076, "system__img_boolB");
   u00077 : constant Version_32 := 16#9876e12f#;
   pragma Export (C, u00077, "system__img_boolS");
   u00078 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00078, "system__ioB");
   u00079 : constant Version_32 := 16#f3ed678b#;
   pragma Export (C, u00079, "system__ioS");
   u00080 : constant Version_32 := 16#8cbe6205#;
   pragma Export (C, u00080, "ada__finalizationB");
   u00081 : constant Version_32 := 16#22e22193#;
   pragma Export (C, u00081, "ada__finalizationS");
   u00082 : constant Version_32 := 16#1358602f#;
   pragma Export (C, u00082, "ada__streamsS");
   u00083 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00083, "system__finalization_rootB");
   u00084 : constant Version_32 := 16#225de354#;
   pragma Export (C, u00084, "system__finalization_rootS");
   u00085 : constant Version_32 := 16#a7a37cb6#;
   pragma Export (C, u00085, "system__storage_poolsB");
   u00086 : constant Version_32 := 16#be018fa9#;
   pragma Export (C, u00086, "system__storage_poolsS");
   u00087 : constant Version_32 := 16#8d43fb6a#;
   pragma Export (C, u00087, "system__atomic_countersB");
   u00088 : constant Version_32 := 16#2733fc70#;
   pragma Export (C, u00088, "system__atomic_countersS");
   u00089 : constant Version_32 := 16#a6e358bc#;
   pragma Export (C, u00089, "system__stream_attributesB");
   u00090 : constant Version_32 := 16#e89b4b3f#;
   pragma Export (C, u00090, "system__stream_attributesS");
   u00091 : constant Version_32 := 16#b46168d5#;
   pragma Export (C, u00091, "ada__io_exceptionsS");
   u00092 : constant Version_32 := 16#bc0fac87#;
   pragma Export (C, u00092, "ada__text_ioB");
   u00093 : constant Version_32 := 16#36d750a9#;
   pragma Export (C, u00093, "ada__text_ioS");
   u00094 : constant Version_32 := 16#7a48d8b1#;
   pragma Export (C, u00094, "interfaces__c_streamsB");
   u00095 : constant Version_32 := 16#a539be81#;
   pragma Export (C, u00095, "interfaces__c_streamsS");
   u00096 : constant Version_32 := 16#773a2d5d#;
   pragma Export (C, u00096, "system__crtlS");
   u00097 : constant Version_32 := 16#4a803ccf#;
   pragma Export (C, u00097, "system__file_ioB");
   u00098 : constant Version_32 := 16#60d89729#;
   pragma Export (C, u00098, "system__file_ioS");
   u00099 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00099, "interfaces__cB");
   u00100 : constant Version_32 := 16#f05a3eb1#;
   pragma Export (C, u00100, "interfaces__cS");
   u00101 : constant Version_32 := 16#62120d5e#;
   pragma Export (C, u00101, "interfaces__c__stringsB");
   u00102 : constant Version_32 := 16#603c1c44#;
   pragma Export (C, u00102, "interfaces__c__stringsS");
   u00103 : constant Version_32 := 16#a50435f4#;
   pragma Export (C, u00103, "system__crtl__runtimeS");
   u00104 : constant Version_32 := 16#721198aa#;
   pragma Export (C, u00104, "system__os_libB");
   u00105 : constant Version_32 := 16#a6d80a38#;
   pragma Export (C, u00105, "system__os_libS");
   u00106 : constant Version_32 := 16#4cd8aca0#;
   pragma Export (C, u00106, "system__stringsB");
   u00107 : constant Version_32 := 16#da45da00#;
   pragma Export (C, u00107, "system__stringsS");
   u00108 : constant Version_32 := 16#b2907efe#;
   pragma Export (C, u00108, "system__file_control_blockS");
   u00109 : constant Version_32 := 16#ba5d60c7#;
   pragma Export (C, u00109, "system__pool_globalB");
   u00110 : constant Version_32 := 16#d56df0a6#;
   pragma Export (C, u00110, "system__pool_globalS");
   u00111 : constant Version_32 := 16#88cd69c1#;
   pragma Export (C, u00111, "system__memoryB");
   u00112 : constant Version_32 := 16#a7242cd1#;
   pragma Export (C, u00112, "system__memoryS");
   u00113 : constant Version_32 := 16#3351da79#;
   pragma Export (C, u00113, "basicsB");
   u00114 : constant Version_32 := 16#00770dfa#;
   pragma Export (C, u00114, "basicsS");
   u00115 : constant Version_32 := 16#85afda84#;
   pragma Export (C, u00115, "configB");
   u00116 : constant Version_32 := 16#40432fea#;
   pragma Export (C, u00116, "configS");
   u00117 : constant Version_32 := 16#39591e91#;
   pragma Export (C, u00117, "system__concat_2B");
   u00118 : constant Version_32 := 16#967f6238#;
   pragma Export (C, u00118, "system__concat_2S");
   u00119 : constant Version_32 := 16#4b56132f#;
   pragma Export (C, u00119, "globalloopB");
   u00120 : constant Version_32 := 16#9b45c1a6#;
   pragma Export (C, u00120, "globalloopS");
   u00121 : constant Version_32 := 16#0a5e12e3#;
   pragma Export (C, u00121, "graphicsB");
   u00122 : constant Version_32 := 16#2ceeaa07#;
   pragma Export (C, u00122, "graphicsS");
   u00123 : constant Version_32 := 16#5e196e91#;
   pragma Export (C, u00123, "ada__containersS");
   u00124 : constant Version_32 := 16#4c95dd5d#;
   pragma Export (C, u00124, "implementationsB");
   u00125 : constant Version_32 := 16#17da09e4#;
   pragma Export (C, u00125, "implementationsS");
   u00126 : constant Version_32 := 16#cba370d0#;
   pragma Export (C, u00126, "refcountB");
   u00127 : constant Version_32 := 16#a2af9fb3#;
   pragma Export (C, u00127, "refcountS");
   u00128 : constant Version_32 := 16#1eadf3c6#;
   pragma Export (C, u00128, "system__strings__stream_opsB");
   u00129 : constant Version_32 := 16#8453d1c6#;
   pragma Export (C, u00129, "system__strings__stream_opsS");
   u00130 : constant Version_32 := 16#a1920867#;
   pragma Export (C, u00130, "ada__streams__stream_ioB");
   u00131 : constant Version_32 := 16#f0e417a0#;
   pragma Export (C, u00131, "ada__streams__stream_ioS");
   u00132 : constant Version_32 := 16#595ba38f#;
   pragma Export (C, u00132, "system__communicationB");
   u00133 : constant Version_32 := 16#ef813eee#;
   pragma Export (C, u00133, "system__communicationS");
   u00134 : constant Version_32 := 16#38741b62#;
   pragma Export (C, u00134, "graphics__implB");
   u00135 : constant Version_32 := 16#22a425ea#;
   pragma Export (C, u00135, "graphics__implS");
   u00136 : constant Version_32 := 16#43df19e4#;
   pragma Export (C, u00136, "openglB");
   u00137 : constant Version_32 := 16#8e50b4d1#;
   pragma Export (C, u00137, "openglS");
   u00138 : constant Version_32 := 16#ae97ef6c#;
   pragma Export (C, u00138, "system__concat_3B");
   u00139 : constant Version_32 := 16#1b8592ae#;
   pragma Export (C, u00139, "system__concat_3S");
   u00140 : constant Version_32 := 16#194ccd7b#;
   pragma Export (C, u00140, "system__img_unsB");
   u00141 : constant Version_32 := 16#98baf045#;
   pragma Export (C, u00141, "system__img_unsS");
   u00142 : constant Version_32 := 16#391f56b1#;
   pragma Export (C, u00142, "versionparserB");
   u00143 : constant Version_32 := 16#7def6d35#;
   pragma Export (C, u00143, "versionparserS");
   u00144 : constant Version_32 := 16#f35d8418#;
   pragma Export (C, u00144, "opengl__win32contextB");
   u00145 : constant Version_32 := 16#04845a52#;
   pragma Export (C, u00145, "opengl__win32contextS");
   u00146 : constant Version_32 := 16#7c9a26fc#;
   pragma Export (C, u00146, "bytesB");
   u00147 : constant Version_32 := 16#9927dc01#;
   pragma Export (C, u00147, "bytesS");
   u00148 : constant Version_32 := 16#e96884af#;
   pragma Export (C, u00148, "endianessB");
   u00149 : constant Version_32 := 16#fc384cb5#;
   pragma Export (C, u00149, "endianessS");
   u00150 : constant Version_32 := 16#60f82714#;
   pragma Export (C, u00150, "typesS");
   u00151 : constant Version_32 := 16#3493e6c0#;
   pragma Export (C, u00151, "system__concat_4B");
   u00152 : constant Version_32 := 16#6ff0737a#;
   pragma Export (C, u00152, "system__concat_4S");
   u00153 : constant Version_32 := 16#758f2455#;
   pragma Export (C, u00153, "win32B");
   u00154 : constant Version_32 := 16#b8289f30#;
   pragma Export (C, u00154, "win32S");
   u00155 : constant Version_32 := 16#f2401129#;
   pragma Export (C, u00155, "win32__gdi32S");
   u00156 : constant Version_32 := 16#137f3555#;
   pragma Export (C, u00156, "win32__kernel32S");
   u00157 : constant Version_32 := 16#a62182c7#;
   pragma Export (C, u00157, "win32__ole32S");
   u00158 : constant Version_32 := 16#0a495d82#;
   pragma Export (C, u00158, "win32__opengl32B");
   u00159 : constant Version_32 := 16#55b44946#;
   pragma Export (C, u00159, "win32__opengl32S");
   u00160 : constant Version_32 := 16#2491aa8c#;
   pragma Export (C, u00160, "win32__user32S");
   u00161 : constant Version_32 := 16#1ff59b66#;
   pragma Export (C, u00161, "opengl__programB");
   u00162 : constant Version_32 := 16#057b9d51#;
   pragma Export (C, u00162, "opengl__programS");
   u00163 : constant Version_32 := 16#1eab0e09#;
   pragma Export (C, u00163, "system__img_enum_newB");
   u00164 : constant Version_32 := 16#eaa85b34#;
   pragma Export (C, u00164, "system__img_enum_newS");
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
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.soft_links%s
   --  system.unsigned_types%s
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
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
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
   --  opengl%s
   --  opengl.win32context%s
   --  refcount%s
   --  refcount%b
   --  graphics%s
   --  graphics%b
   --  graphics.impl%s
   --  graphics.impl%b
   --  opengl.program%s
   --  opengl.program%b
   --  guitest%b
   --  types%s
   --  endianess%s
   --  endianess%b
   --  bytes%s
   --  bytes%b
   --  versionparser%s
   --  versionparser%b
   --  opengl%b
   --  win32%s
   --  win32%b
   --  win32.gdi32%s
   --  win32.kernel32%s
   --  win32.ole32%s
   --  win32.opengl32%s
   --  win32.opengl32%b
   --  win32.user32%s
   --  opengl.win32context%b
   --  END ELABORATION ORDER


end ada_main;
