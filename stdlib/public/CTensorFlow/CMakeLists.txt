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

if(SWIFT_ENABLE_TENSORFLOW)
  # Edit the module map to refer to the right directory.
  configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/module.modulemap_template"
        "${CMAKE_CURRENT_SOURCE_DIR}/module.modulemap_configured")

  # Cause the underlying build system to copy module.modulemap_configured
  # in the current directory to module.modulemap in  ${SWIFTLIB_DIR}/CTensorFlow.
  set(input "module.modulemap_configured")
  set(modulemap_output "module.modulemap")
  set(output_dir "${SWIFTLIB_DIR}/CTensorFlow")

  add_custom_command(
      OUTPUT "${output_dir}"
      COMMAND ${CMAKE_COMMAND} "-E" "make_directory" "${output_dir}")
  set(outputs)

  add_custom_command(
      OUTPUT "${output_dir}/${modulemap_output}"
      DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${input}"
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
        "${CMAKE_CURRENT_SOURCE_DIR}/${input}"
        "${output_dir}/${modulemap_output}")
  list(APPEND outputs "${output_dir}/${modulemap_output}")

  # Put the output dir itself last so that it isn't considered the primary output.
  list(APPEND outputs "${output_dir}")

  add_custom_target("copy_ctensorflow_modulemap"
      DEPENDS "${outputs}"
      COMMENT "Copying CTensorFlow modulemap to ${output_dir}")

  set(swift_stdlib_compile_flags "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}")

  add_swift_library(swiftTensorFlowInit OBJECT_LIBRARY TARGET_LIBRARY
      ctensorflow_init.cpp
      "${output_dir}/${modulemap_output}"
      C_COMPILE_FLAGS ${SWIFT_RUNTIME_CXX_FLAGS}
      LINK_FLAGS "${SWIFT_RUNTIME_LINK_FLAGS}"
      INSTALL_IN_COMPONENT never_install)

endif()
