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

# Note: *moduleTriple* doesn't have an "x" on the end of "macos"; just to be
# safe, we support both cases here.
set(availability_platform_macos "macOS")
set(availaiblity_platform_macosx "macOS")
set(availability_platform_ios "iOS")
set(availability_platform_watchos "watchOS")
set(availability_platform_tvos "tvOS")
set(availability_platform_xros "visionOS")
set(availability_platform_bridgeos "bridgeOS")

if(NOT SwiftCore_SWIFT_AVAILABILITY_PLATFORM)
  if(SwiftCore_MODULE_TRIPLE MATCHES ".*-([^-]+)-simulator$")
    set(platform "${CMAKE_MATCH_1}")
  elseif(SwiftCore_MODULE_TRIPLE MATCHES ".*-([^-]+)-msvc$")
    set(platform "${CMAKE_MATCH_1}")
  elseif(SwiftCore_MODULE_TRIPLE MATCHES ".*-([^-]+)$")
    set(platform "${CMAKE_MATCH_1}")
  else()
    message(WARNING "Unable to extract platform name from triple ${SwiftCore_MODULE_TRIPLE}")
  endif()

  if(availability_platform_${platform})
    set(SwiftCore_SWIFT_AVAILABILITY_PLATFORM "${availability_platform_${platform}}")
  else()
    set(SwiftCore_SWIFT_AVAILABILITY_PLATFORM "unknown")
    message(WARNING "Unknown platform ${platform} for availability")
  endif()
endif()

set(SwiftCore_VARIANT_AVAILABILITY_PLATFORM "none")
if(SwiftCore_VARIANT_MODULE_TRIPLE)
  if(SwiftCore_VARIANT_MODULE_TRIPLE MATCHES ".*-([^-]+)$")
    set(platform "${CMAKE_MATCH_1}")
  else()
    message(FATAL_ERROR "Unable to extract platform name from triple ${SwiftCore_VARIANT_MODULE_TRIPLE}")
  endif()

  if(availability_platform_${platform})
    set(SwiftCore_VARIANT_AVAILABILITY_PLATFORM "${availability_platform_${platform}}")
  else()
    message(WARNING "Unknown platform ${platform} for variant availability")
  endif()
endif()
