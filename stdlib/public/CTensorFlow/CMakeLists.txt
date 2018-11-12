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

if(NOT SWIFT_ENABLE_TENSORFLOW)
  return()
endif()

include("../../../cmake/modules/SwiftList.cmake")

find_package(TensorFlow REQUIRED)
message(STATUS "Building CTensorFlow.")

include_directories(BEFORE "${TF_INCLUDE_DIR}")

# Get target SDKs.
set(TARGET_SDKS "OSX" "LINUX")
list_intersect("${TARGET_SDKS}" "${SWIFT_SDKS}" TARGET_SDKS)

set(ctensorflow_modulemap_target_list)
foreach(sdk ${TARGET_SDKS})
  foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
    set(arch_modulemaps_subdir "${SWIFT_SDK_${sdk}_LIB_SUBDIR}/${arch}/modulemaps")
    set(module_dir "${SWIFTLIB_DIR}/${arch_modulemaps_subdir}")
    set(output_dir "${module_dir}/CTensorFlow")
    set(outputs)

    add_custom_command(
      OUTPUT "${output_dir}"
      COMMAND ${CMAKE_COMMAND} "-E" "make_directory" "${output_dir}")

    # Copy modulemap.
    add_custom_command(
      OUTPUT "${output_dir}/module.modulemap"
      DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/module.modulemap"
      COMMAND
      "${CMAKE_COMMAND}" "-E" "copy_if_different"
      "${CMAKE_CURRENT_SOURCE_DIR}/module.modulemap"
      "${output_dir}/module.modulemap")
    list(APPEND outputs "${output_dir}/module.modulemap")

    # Copy headers.
    macro(copy_tf_headers src dst)
      add_custom_command(
        OUTPUT "${output_dir}/${dst}"
        DEPENDS "${src}"
        COMMAND
          "${SWIFT_SOURCE_DIR}/utils/fix-tf-headers.sh"
          "${src}"
          "${output_dir}/${dst}")

      list(APPEND outputs "${output_dir}/${dst}")
    endmacro(copy_tf_headers)

    copy_tf_headers("${TF_INCLUDE_DIR}/tensorflow/c/c_api.h" c_api.h)
    copy_tf_headers("${TF_INCLUDE_DIR}/tensorflow/c/c_api_experimental.h" c_api_experimental.h)
    copy_tf_headers("${TF_INCLUDE_DIR}/tensorflow/c/eager/c_api.h" c_api_eager.h)
    copy_tf_headers("${CMAKE_CURRENT_SOURCE_DIR}/ctensorflow_init.h" ctensorflow_init.h)

    # Put the output dir itself last so that it isn't considered the primary output.
    list(APPEND outputs "${output_dir}")
    list(APPEND ctensorflow_modulemap_target_list ${outputs})

    # Install include directory.
    swift_install_in_component(stdlib
      DIRECTORY "${output_dir}/"
      DESTINATION "lib/swift/${arch_modulemaps_subdir}/CTensorFlow")
  endforeach()

  # Install TensorFlow libraries in each SDK subdirectory.
  # FIXME: Currently, this installs the same version of TensorFlow libraries
  # everywhere.
  # To properly support multiple targets, TF library/include variables should be
  # SDK/architecture-specific, just like the ones for ICU.
  swift_install_in_component(stdlib
    FILES "${TF_LIBRARIES}"
    DESTINATION "lib/swift/${SWIFT_SDK_${sdk}_LIB_SUBDIR}")
endforeach()
add_custom_target(ctensorflow_modulemap DEPENDS ${ctensorflow_modulemap_target_list})

add_swift_target_library(swiftCTensorFlow OBJECT_LIBRARY TARGET_LIBRARY
  ctensorflow_init.cpp
  DEPENDS ctensorflow_modulemap
  TARGET_SDKS OSX LINUX
  C_COMPILE_FLAGS "${SWIFT_RUNTIME_CXX_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_LINK_FLAGS}"
  INTERFACE_LINK_LIBRARIES "${TF_LIBRARIES}"
  INSTALL_IN_COMPONENT never_install)
