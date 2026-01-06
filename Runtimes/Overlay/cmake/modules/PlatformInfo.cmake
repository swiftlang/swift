# TODO: This logic should migrate to CMake once CMake supports installing swiftmodules
set(module_triple_command "${CMAKE_Swift_COMPILER}" -print-target-info)
if(CMAKE_Swift_COMPILER_TARGET)
  list(APPEND module_triple_command -target ${CMAKE_Swift_COMPILER_TARGET})
endif()
execute_process(COMMAND ${module_triple_command} OUTPUT_VARIABLE target_info_json)
message(CONFIGURE_LOG "Swift target info: ${module_triple_command}\n"
"${target_info_json}")

if(NOT SwiftOverlay_MODULE_TRIPLE)
  string(JSON module_triple GET "${target_info_json}" "target" "moduleTriple")
  set(SwiftOverlay_MODULE_TRIPLE "${module_triple}" CACHE STRING "Triple used for installed swift{doc,module,interface} files")
  mark_as_advanced(SwiftOverlay_MODULE_TRIPLE)

  message(CONFIGURE_LOG "Swift module triple: ${module_triple}")
endif()

if(NOT SwiftOverlay_PLATFORM_SUBDIR)
  string(JSON platform GET "${target_info_json}" "target" "platform")
  set(SwiftOverlay_PLATFORM_SUBDIR "${platform}" CACHE STRING "Platform name used for installed swift{doc,module,interface} files")
  mark_as_advanced(SwiftOverlay_PLATFORM_SUBDIR)

  message(CONFIGURE_LOG "Swift platform: ${platform}")
endif()

if(NOT SwiftOverlay_ARCH_SUBDIR)
  string(JSON arch GET "${target_info_json}" "target" "arch")
  set(SwiftOverlay_ARCH_SUBDIR "${arch}" CACHE STRING "Architecture used for setting the architecture subdirectory")
  mark_as_advanced(SwiftOverlay_ARCH_SUBDIR)

  message(CONFIGURE_LOG "Swift Arch: ${arch}")
endif()
