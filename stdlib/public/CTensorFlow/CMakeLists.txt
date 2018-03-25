#===--- CMakeLists.txt - Build the CTensorFlow support library ------------===#
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

if(SWIFT_ENABLE_TENSORFLOW)
  include_directories(BEFORE
    "${SWIFT_TENSORFLOW_TARGET_INCLUDE_DIR}")
  set(output_dir "${SWIFTLIB_DIR}/CTensorFlow")
  set(outputs)

  add_custom_command(
    OUTPUT "${output_dir}"
    COMMAND ${CMAKE_COMMAND} "-E" "make_directory" "${output_dir}")

  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/module.modulemap_template"
    "${CMAKE_CURRENT_SOURCE_DIR}/module.modulemap_configured")

  # Edit the modulemap to refer to the right directory.
  add_custom_command(
    OUTPUT "${output_dir}/module.modulemap"
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/module.modulemap_configured"
    COMMAND
      "${CMAKE_COMMAND}" "-E" "copy_if_different"
      "${CMAKE_CURRENT_SOURCE_DIR}/module.modulemap_configured"
      "${output_dir}/module.modulemap")
  list(APPEND outputs "${output_dir}/module.modulemap")

  macro(copy_headers src dst)
    add_custom_command(
      OUTPUT "${output_dir}/${dst}"
      DEPENDS "${src}"
      COMMAND
        "${CMAKE_CURRENT_SOURCE_DIR}/fix-tf-includes.sh"
        "${src}"
        "${output_dir}/${dst}"
      )
    list(APPEND outputs "${output_dir}/${dst}")
    set(outputs "${outputs}")
  endmacro(copy_headers)

  copy_headers("${SWIFT_TENSORFLOW_TARGET_INCLUDE_DIR}/tensorflow/c/c_api.h" c_api.h)
  copy_headers("${SWIFT_TENSORFLOW_TARGET_INCLUDE_DIR}/tensorflow/c/c_api_experimental.h" c_api_experimental.h)
  copy_headers("${SWIFT_TENSORFLOW_TARGET_INCLUDE_DIR}/tensorflow/c/eager/c_api.h" c_api_eager.h)
  copy_headers("${CMAKE_CURRENT_SOURCE_DIR}/ctensorflow_init.h" ctensorflow_init.h)

  # Put the output dir itself last so that it isn't considered the primary output.
  list(APPEND outputs "${output_dir}")

  add_custom_target("ctensorflow_modulemap"
    DEPENDS "${outputs}"
    COMMENT "Copying CTensorFlow module to ${output_dir}.")

  swift_install_in_component(stdlib
    DIRECTORY "${output_dir}/"
    DESTINATION "lib/swift/CTensorFlow")

  add_swift_library(swiftCTensorFlow OBJECT_LIBRARY TARGET_LIBRARY
    ctensorflow_init.cpp
    DEPENDS ctensorflow_modulemap
    C_COMPILE_FLAGS "${SWIFT_RUNTIME_CXX_FLAGS}"
    LINK_FLAGS "${SWIFT_RUNTIME_LINK_FLAGS}"
    INTERFACE_LINK_LIBRARIES "${SWIFT_TENSORFLOW_TARGET_LIB_DIR}/libtensorflow.so" "${SWIFT_TENSORFLOW_TARGET_LIB_DIR}/libtensorflow_framework.so"
    INSTALL_IN_COMPONENT never_install)

endif()
