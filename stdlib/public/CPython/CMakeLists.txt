#===--- CMakeLists.txt - Build the CPython support library ---------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#
#
# SWIFT_ENABLE_TENSORFLOW
#
#===----------------------------------------------------------------------===#

find_package(PythonLibs 2.7 EXACT REQUIRED)
message(STATUS "Building CPython overlay.")

set(SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES)
if(SWIFT_BUILD_DYNAMIC_SDK_OVERLAY)
  list(APPEND SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES SHARED)
endif()
if(SWIFT_BUILD_STATIC_SDK_OVERLAY)
  list(APPEND SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES STATIC)
endif()

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
  set(cmake_symlink_option "copy_directory")
else()
  set(cmake_symlink_option "create_symlink")
endif()

set(sources
  CPython.swift
  module.modulemap)

set(output_dir "${SWIFTLIB_DIR}/CPython")
set(outputs)

add_custom_command(
  OUTPUT "${output_dir}"
  COMMAND ${CMAKE_COMMAND} "-E" "make_directory" "${output_dir}")

# Symlink Python header files.
add_custom_command(
  OUTPUT "${output_dir}/python"
  COMMAND
    "${CMAKE_COMMAND}" "-E" "${cmake_symlink_option}"
    "${PYTHON_INCLUDE_DIRS}"
    "${output_dir}/python"
  COMMENT "Symlinking Python 2.7 headers into ${output_dir}/python")
list(APPEND outputs "${output_dir}/python")

# Copy sources.
foreach(input ${sources})
  add_custom_command(
    OUTPUT "${output_dir}/${input}"
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${input}"
    COMMAND
      "${CMAKE_COMMAND}" "-E" "copy_if_different"
      "${CMAKE_CURRENT_SOURCE_DIR}/${input}"
      "${output_dir}/${input}")
  list(APPEND outputs "${output_dir}/${input}")
endforeach()

# Put the output dir itself last so that it isn't considered the primary output.
list(APPEND outputs "${output_dir}")

add_custom_target(cpython_modulemap
  DEPENDS ${outputs}
  COMMENT "Copying CPython module to ${output_dir}.")

swift_install_in_component(sdk-overlay
  DIRECTORY "${output_dir}/"
  DESTINATION "lib/swift/CPython")

add_swift_library(swiftCPython ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  CPython.swift
  DEPENDS cpython_modulemap
  SWIFT_MODULE_DEPENDS_IOS Darwin
  SWIFT_MODULE_DEPENDS_OSX Darwin
  SWIFT_MODULE_DEPENDS_TVOS Darwin
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin
  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
  SWIFT_MODULE_DEPENDS_HAIKU Glibc
  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  INTERFACE_LINK_LIBRARIES "${PYTHON_LIBRARIES}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}")
