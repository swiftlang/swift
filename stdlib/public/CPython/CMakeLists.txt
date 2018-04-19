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

include("../../../cmake/modules/SwiftList.cmake")

find_package(PythonLibs 2.7 EXACT REQUIRED)
message(STATUS "Building CPython.")

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
  set(cmake_symlink_option "copy_directory")
else()
  set(cmake_symlink_option "create_symlink")
endif()

# Get target SDKs.
set(TARGET_SDKS "OSX" "CYGWIN" "FREEBSD" "LINUX" "HAIKU")
list_intersect("${TARGET_SDKS}" "${SWIFT_SDKS}" TARGET_SDKS)

set(cpython_modulemap_target_list)
foreach(sdk ${TARGET_SDKS})
  foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
    set(arch_modulemaps_subdir "${SWIFT_SDK_${sdk}_LIB_SUBDIR}/${arch}/modulemaps")
    set(module_dir "${SWIFTLIB_DIR}/${arch_modulemaps_subdir}")
    set(output_dir "${module_dir}/CPython")
    set(outputs)

    add_custom_command(
      OUTPUT "${output_dir}"
      COMMAND ${CMAKE_COMMAND} "-E" "make_directory" "${output_dir}")

    # Copy Python header files.
    add_custom_command(
      OUTPUT "${output_dir}/include"
      COMMAND
      "${CMAKE_COMMAND}" "-E" "copy_directory"
      "${PYTHON_INCLUDE_DIRS}"
      "${output_dir}/include"
      COMMENT "Copying Python 2.7 headers into ${output_dir}/include")
    list(APPEND outputs "${output_dir}/include")

    # Copy modulemap.
    add_custom_command(
      OUTPUT "${output_dir}/module.modulemap"
      DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/module.modulemap"
      COMMAND
      "${CMAKE_COMMAND}" "-E" "copy_if_different"
      "${CMAKE_CURRENT_SOURCE_DIR}/module.modulemap"
      "${output_dir}/module.modulemap")
    list(APPEND outputs "${output_dir}/module.modulemap")

    # Put the output dir itself last so that it isn't considered the primary output.
    list(APPEND outputs "${output_dir}")
    list(APPEND cpython_modulemap_target_list ${outputs})

    swift_install_in_component(sdk-overlay
      DIRECTORY "${output_dir}/"
      DESTINATION "lib/swift/${arch_modulemaps_subdir}/CPython")
  endforeach()
endforeach()
add_custom_target(cpython_modulemap DEPENDS ${cpython_modulemap_target_list})
