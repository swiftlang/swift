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
  add_pure_swift_host_library(${name} SHARED ${ASML_SOURCES})

  # If we don't have the Swift swift parser, bail out, because the above
  # add_pure_swift_host_library did nothing.
  if (NOT SWIFT_SWIFT_PARSER)
    return()
  endif()

  # Install into the plugin directory.
  set_target_properties(${name}
    PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY "${SWIFT_HOST_PLUGINS_DEST_DIR}"
      LIBRARY_OUTPUT_DIRECTORY "${SWIFT_HOST_PLUGINS_DEST_DIR}"
  )

  swift_install_in_component(TARGETS ${name}
    LIBRARY
      DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/host/plugins"
      COMPONENT compiler
    ARCHIVE
      DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/host/plugins"
      COMPONENT compiler)

  # Export this macro plugin target.
  set_property(GLOBAL APPEND PROPERTY SWIFT_MACRO_PLUGINS ${name})
endfunction()

add_subdirectory(Sources/SwiftMacros)
add_subdirectory(Sources/ObservationMacros)
