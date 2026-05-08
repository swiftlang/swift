#===--- SwiftAddWindowsVersionResource.cmake ---------------------------===#
#
# This source file is part of the Swift open source project
#
# Copyright (c) 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===---------------------------------------------------------------------===#
#
# Embeds an `RT_VERSION` (`VS_VERSIONINFO`) resource into a Windows
# shared library or executable target so the resulting PE carries
# readable version metadata in its resource section.  Also satisfies
# WiX's "versioned keypath" criterion for multi-file `<Component>`
# definitions.
#
# Usage:
#
#   swift_add_windows_version_resource(<target>
#     [FILE_DESCRIPTION  <string>]    # default: <INTERNAL_NAME>
#     [INTERNAL_NAME     <string>]    # default: <target>
#     [ORIGINAL_FILENAME <string>]    # default: <target>{.dll,.exe}
#     [PRODUCT_NAME      <string>]    # default: "Swift"
#   )
#
# Version numbers are taken from `${SWIFT_RUNTIME_VERSION}` if set,
# falling back to `${PROJECT_VERSION}`.  No-op on non-Windows targets.

include_guard(GLOBAL)

function(swift_add_windows_version_resource target)
  if(NOT WIN32)
    return()
  endif()
  if(NOT TARGET ${target})
    message(FATAL_ERROR
      "swift_add_windows_version_resource: '${target}' is not a target")
  endif()

  cmake_parse_arguments(PARSE_ARGV 1 ARG
    ""
    "FILE_DESCRIPTION;INTERNAL_NAME;ORIGINAL_FILENAME;PRODUCT_NAME"
    "")

  # Resolve defaults.
  if(NOT ARG_INTERNAL_NAME)
    set(ARG_INTERNAL_NAME "${target}")
  endif()
  if(NOT ARG_FILE_DESCRIPTION)
    set(ARG_FILE_DESCRIPTION "${ARG_INTERNAL_NAME}")
  endif()
  if(NOT ARG_PRODUCT_NAME)
    set(ARG_PRODUCT_NAME "Swift")
  endif()
  if(NOT ARG_ORIGINAL_FILENAME)
    get_target_property(target_type ${target} TYPE)
    if(target_type STREQUAL "EXECUTABLE")
      set(ARG_ORIGINAL_FILENAME "${target}.exe")
    else()
      set(ARG_ORIGINAL_FILENAME "${target}.dll")
    endif()
  endif()

  # Pad the project version to a four-part `MAJOR.MINOR.PATCH.TWEAK`
  # value here rather than at the source: `SwiftProjectVersion` emits
  # `SWIFT_RUNTIME_VERSION` as a 1-3 part value (e.g. "6.4.0") because
  # other consumers across the Swift runtime projects depend on that
  # shape, and Windows `VS_VERSIONINFO` /
  # `<assemblyIdentity version="...">` is the only consumer that
  # requires a quad.  Containing the normalisation in this helper keeps
  # the change to `SwiftProjectVersion` zero.
  if(SWIFT_RUNTIME_VERSION)
    set(version "${SWIFT_RUNTIME_VERSION}")
  elseif(PROJECT_VERSION)
    set(version "${PROJECT_VERSION}")
  else()
    set(version "0.0.0.0")
  endif()
  string(REGEX MATCHALL "[0-9]+" version_parts "${version}")
  list(LENGTH version_parts version_part_count)
  while(version_part_count LESS 4)
    list(APPEND version_parts "0")
    math(EXPR version_part_count "${version_part_count} + 1")
  endwhile()
  list(GET version_parts 0 SWIFT_RC_VERSION_MAJOR)
  list(GET version_parts 1 SWIFT_RC_VERSION_MINOR)
  list(GET version_parts 2 SWIFT_RC_VERSION_PATCH)
  list(GET version_parts 3 SWIFT_RC_VERSION_TWEAK)
  set(SWIFT_RC_VERSION_STRING
    "${SWIFT_RC_VERSION_MAJOR}.${SWIFT_RC_VERSION_MINOR}.${SWIFT_RC_VERSION_PATCH}.${SWIFT_RC_VERSION_TWEAK}")

  # Map argument values to template variables for `configure_file` to
  # substitute.  These names match the `@VAR@` placeholders in the
  # `.rc.in` template.
  set(SWIFT_RC_FILE_DESCRIPTION  "${ARG_FILE_DESCRIPTION}")
  set(SWIFT_RC_INTERNAL_NAME     "${ARG_INTERNAL_NAME}")
  set(SWIFT_RC_ORIGINAL_FILENAME "${ARG_ORIGINAL_FILENAME}")
  set(SWIFT_RC_PRODUCT_NAME      "${ARG_PRODUCT_NAME}")

  # Stage a per-target .rc with all values baked in.  Using
  # `CMAKE_CURRENT_FUNCTION_LIST_DIR` so the template is found relative
  # to *this* module file regardless of where the function is invoked.
  configure_file(
    "${CMAKE_CURRENT_FUNCTION_LIST_DIR}/../resources/swift_runtime_version_resource.rc.in"
    "${CMAKE_CURRENT_BINARY_DIR}/${target}_version_resource.rc"
    @ONLY)

  target_sources(${target}
    PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/${target}_version_resource.rc")
endfunction()
