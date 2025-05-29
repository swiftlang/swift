#===--- CMakeLists.txt - Macro support libraries ------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2023 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

# The location for host plugins, i.e., macros.
set(SWIFT_HOST_PLUGINS_DEST_DIR ${SWIFT_HOST_LIBRARIES_DEST_DIR}/plugins)

# Add a new Swift macro library, which is built for the host and installed
# into lib/swift/host/plugins.
#
# Usage:
#   add_swift_macro_library(name
#     source1 [source2 source3 ...])
#
# name
#   Name of the library (e.g., SwiftMacros).
#
# source1 ...
#   Sources to add into this library.
function(add_swift_macro_library name)
  set(ASML_SOURCES ${ARGN})

  # Add the library.
  add_pure_swift_host_library(${name} SHARED
    PACKAGE_NAME Toolchain
    ${ASML_SOURCES})

  # If we don't have the Swift swift parser, bail out, because the above
  # add_pure_swift_host_library did nothing.
  if (NOT SWIFT_BUILD_SWIFT_SYNTAX)
    return()
  endif()

  # Add rpath to 'lib/{platform}'
  file(RELATIVE_PATH relpath_to_lib
    "${SWIFT_HOST_PLUGINS_DEST_DIR}"
    "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
  )
  _set_pure_swift_link_flags(${name} "${relpath_to_lib}")

  # Add rpath to 'lib/host'
  if(SWIFT_HOST_VARIANT_SDK MATCHES "LINUX|ANDROID|OPENBSD|FREEBSD")
    set_property(TARGET ${name}
      APPEND PROPERTY INSTALL_RPATH "$ORIGIN/..")
  endif()

  # Install into the plugin directory.
  set_target_properties(${name}
    PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY "${SWIFT_HOST_PLUGINS_DEST_DIR}"
      LIBRARY_OUTPUT_DIRECTORY "${SWIFT_HOST_PLUGINS_DEST_DIR}"
  )

  set(destination_dir "lib${LLVM_LIBDIR_SUFFIX}/swift/host/plugins")

  swift_install_in_component(TARGETS ${name}
    RUNTIME
      DESTINATION bin
      COMPONENT compiler
    LIBRARY
      DESTINATION "${destination_dir}"
      COMPONENT compiler
    ARCHIVE
      DESTINATION "${destination_dir}"
      COMPONENT compiler)

  # Export this macro plugin target.
  set_property(GLOBAL APPEND PROPERTY SWIFT_MACRO_PLUGINS ${name})
endfunction()

add_subdirectory(Sources/SwiftMacros)
add_subdirectory(Sources/ObservationMacros)
