if(NOT SwiftCore_SIZEOF_POINTER)
  set(SwiftCore_SIZEOF_POINTER "${CMAKE_SIZEOF_VOID_P}" CACHE STRING "Size of a pointer in bytes")
  message(CONFIGURE_LOG "Stdlib Pointer size: ${CMAKE_SIZEOF_VOID_P}")
  mark_as_advanced(SwiftCore_SIZEOF_POINTER)
endif()

if(NOT SwiftCore_MODULE_TRIPLE)
  # TODO: This logic should migrate to CMake once CMake supports installing swiftmodules
  set(module_triple_command "${CMAKE_Swift_COMPILER}" -print-target-info)
  if(CMAKE_Swift_COMPILER_TARGET)
    list(APPEND module_triple_command -target ${CMAKE_Swift_COMPILER_TARGET})
  endif()
  execute_process(COMMAND ${module_triple_command} OUTPUT_VARIABLE target_info_json)
  message(CONFIGURE_LOG "Swift target info: ${module_triple_command}\n"
  "${target_info_json}")
  string(JSON module_triple GET "${target_info_json}" "target" "moduleTriple")
  set(SwiftCore_MODULE_TRIPLE "${module_triple}" CACHE STRING "swift module triple used for installed swiftmodule and swiftinterface files")
  mark_as_advanced(SwiftCore_MODULE_TRIPLE)
endif()

if(NOT SwiftCore_SWIFTC_CLANGIMPORTER_RESOURCE_DIR)
  # TODO: We need to separate the concept of compiler resources and the stdlib.
  #       Compiler-resources in the compiler-resource directory are specific to
  #       a given compiler. The headers in `lib/clang/<version>/include` and
  #       `lib/swift/clang/include` correspond with that specific copy clang and
  #       should not be mixed. This won't cause modularization issues because
  #       the one copy of clang should never be looking in the other's resource
  #       directory. If there are issues here, something has gone horribly wrong
  #       and you're looking in the wrong place.
  set(module_triple_command "${CMAKE_Swift_COMPILER}" -print-target-info)
  if(CMAKE_Swift_COMPILER_TARGET)
    list(APPEND module_triple_command -target ${CMAKE_Swift_COMPILER_TARGET})
  endif()
  execute_process(COMMAND ${module_triple_command} OUTPUT_VARIABLE target_info_json)
  string(JSON resource_dir GET "${target_info_json}" "paths" "runtimeResourcePath")
  cmake_path(APPEND resource_dir "clang")
  message(CONFIGURE_LOG "Using Swift clang-importer headers from toolchain Swift\n"
    "clang-importer header directory: ${resource_dir}")
  set(SwiftCore_SWIFTC_CLANGIMPORTER_RESOURCE_DIR "${resource_dir}"
    CACHE FILEPATH "Swift clang-importer resource directory")
  mark_as_advanced(SwiftCore_SWIFTC_CLANGIMPORTER_RESOURCE_DIR)
endif()
