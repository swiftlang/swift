if(NOT SwiftCore_SIZEOF_POINTER)
  set(SwiftCore_SIZEOF_POINTER "${CMAKE_SIZEOF_VOID_P}" CACHE STRING "Size of a pointer in bytes")
  message(CONFIGURE_LOG "Stdlib Pointer size: ${CMAKE_SIZEOF_VOID_P}")
  mark_as_advanced(SwiftCore_SIZEOF_POINTER)
endif()

# TODO: This logic should migrate to CMake once CMake supports installing swiftmodules
set(module_triple_command "${CMAKE_Swift_COMPILER}" -print-target-info)
if(CMAKE_Swift_COMPILER_TARGET)
  list(APPEND module_triple_command -target ${CMAKE_Swift_COMPILER_TARGET})
endif()
execute_process(COMMAND ${module_triple_command} OUTPUT_VARIABLE target_info_json)
message(CONFIGURE_LOG "Swift target info: ${module_triple_command}\n"
"${target_info_json}")

if(NOT SwiftCore_MODULE_TRIPLE)
  string(JSON module_triple GET "${target_info_json}" "target" "moduleTriple")
  set(SwiftCore_MODULE_TRIPLE "${module_triple}" CACHE STRING "Triple used for installed swift{doc,module,interface} files")
  mark_as_advanced(SwiftCore_MODULE_TRIPLE)

  message(CONFIGURE_LOG "Swift module triple: ${module_triple}")
endif()

if(NOT SwiftCore_PLATFORM_SUBDIR)
  string(JSON platform GET "${target_info_json}" "target" "platform")
  set(SwiftCore_PLATFORM_SUBDIR "${platform}" CACHE STRING "Platform name used for installed swift{doc,module,interface} files")
  mark_as_advanced(SwiftCore_PLATFORM_SUBDIR)

  message(CONFIGURE_LOG "Swift platform: ${platform}")
endif()

if(NOT SwiftCore_ARCH_SUBDIR)
  string(JSON arch GET "${target_info_json}" "target" "arch")
  set(SwiftCore_ARCH_SUBDIR "${arch}" CACHE STRING "Architecture used for setting the architecture subdirectory")
  mark_as_advanced(SwiftCore_ARCH_SUBDIR)

  message(CONFIGURE_LOG "Swift Arch: ${arch}")
endif()
