#===--- CMakeLists.txt - Observation macros library ----------------------===#
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

if (SWIFT_SWIFT_PARSER)
  add_library(ObservationMacros SHARED
    ObservableMacro.swift)

  set_target_properties(ObservationMacros
    PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib/swift/host/plugins"
      LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib/swift/host/plugins"
  )

  target_compile_options(ObservationMacros PRIVATE
    $<$<COMPILE_LANGUAGE:Swift>:-runtime-compatibility-version>
    $<$<COMPILE_LANGUAGE:Swift>:none>)

  # Set the appropriate target triple.
  if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
    set(DEPLOYMENT_VERSION "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_DEPLOYMENT_VERSION}")
  endif()

  if(SWIFT_HOST_VARIANT_SDK STREQUAL ANDROID)
    set(DEPLOYMENT_VERSION ${SWIFT_ANDROID_API_LEVEL})
  endif()

  get_target_triple(target target_variant "${SWIFT_HOST_VARIANT_SDK}" "${SWIFT_HOST_VARIANT_ARCH}"
    MACCATALYST_BUILD_FLAVOR ""
    DEPLOYMENT_VERSION "${DEPLOYMENT_VERSION}")

    target_compile_options(ObservationMacros PRIVATE $<$<COMPILE_LANGUAGE:Swift>:-target;${target}>)
 
  # Workaround a cmake bug, see the corresponding function in swift-syntax
  function(force_target_macros_link_libraries TARGET)
    cmake_parse_arguments(ARGS "" "" "PUBLIC" ${ARGN})

    foreach(DEPENDENCY ${ARGS_PUBLIC})
      target_link_libraries(${TARGET} PRIVATE
        ${DEPENDENCY}
      )
      add_dependencies(${TARGET} ${DEPENDENCY})

      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/forced-${DEPENDENCY}-dep.swift
        COMMAND ${CMAKE_COMMAND} -E touch ${CMAKE_CURRENT_BINARY_DIR}/forced-${DEPENDENCY}-dep.swift
        DEPENDS ${DEPENDENCY}
        )
      target_sources(${TARGET} PRIVATE
        ${CMAKE_CURRENT_BINARY_DIR}/forced-${DEPENDENCY}-dep.swift
      )
    endforeach()
  endfunction()

  set(SWIFT_SYNTAX_MODULES
    SwiftSyntax
    SwiftSyntaxMacros
  )

  # Compute the list of SwiftSyntax targets
  list(TRANSFORM SWIFT_SYNTAX_MODULES PREPEND "SwiftSyntax::"
       OUTPUT_VARIABLE SWIFT_SYNTAX_TARGETS)

  # TODO: Change to target_link_libraries when cmake is fixed
  force_target_link_libraries(ObservationMacros PUBLIC
    ${SWIFT_SYNTAX_TARGETS}
  )

  set(SWIFT_SYNTAX_LIBRARIES_SOURCE_DIR
      "${SWIFT_PATH_TO_EARLYSWIFTSYNTAX_BUILD_DIR}/lib/swift/host")
  set(SWIFT_SYNTAX_LIBRARIES_DEST_DIR
      "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/swift/host")

  # Determine the SwiftSyntax shared library files that were built as
  # part of earlyswiftsyntax.
  list(TRANSFORM SWIFT_SYNTAX_MODULES PREPEND ${CMAKE_SHARED_LIBRARY_PREFIX}
       OUTPUT_VARIABLE SWIFT_SYNTAX_SHARED_LIBRARIES)
  list(TRANSFORM SWIFT_SYNTAX_SHARED_LIBRARIES APPEND
       ${CMAKE_SHARED_LIBRARY_SUFFIX}
       OUTPUT_VARIABLE SWIFT_SYNTAX_SHARED_LIBRARIES)

  # Copy over all of the shared libraries from earlyswiftsyntax so they can
  # be found via RPATH.
  foreach (sharedlib ${SWIFT_SYNTAX_SHARED_LIBRARIES})
    add_custom_command(
      TARGET ObservationMacros PRE_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SWIFT_SYNTAX_LIBRARIES_SOURCE_DIR}/${sharedlib} ${SWIFT_SYNTAX_LIBRARIES_DEST_DIR}/${sharedlib}
      COMMENT "Copying ${sharedlib}"
    )
  endforeach()

  # Copy all of the Swift modules from earlyswiftsyntax so they can be found
  # in the same relative place within the build directory as in the final
  # toolchain.
  list(TRANSFORM SWIFT_SYNTAX_MODULES APPEND ".swiftmodule"
       OUTPUT_VARIABLE SWIFT_SYNTAX_MODULE_DIRS)
  foreach(module_dir ${SWIFT_SYNTAX_MODULE_DIRS})
    file(GLOB module_files
         "${SWIFT_SYNTAX_LIBRARIES_SOURCE_DIR}/${module_dir}/*.swiftinterface")
    add_custom_command(
      TARGET ObservationMacros PRE_BUILD
      COMMAND ${CMAKE_COMMAND} -E make_directory ${SWIFT_SYNTAX_LIBRARIES_DEST_DIR}/${module_dir}
      COMMAND ${CMAKE_COMMAND} -E copy_if_different ${module_files} ${SWIFT_SYNTAX_LIBRARIES_DEST_DIR}/${module_dir}/
      COMMENT "Copying ${module_dir}"
    )
  endforeach()

  target_include_directories(ObservationMacros PUBLIC
    ${SWIFT_SYNTAX_LIBRARIES_DEST_DIR})

  # Ensure the install directory exists before everything gets started
  add_custom_command(
      TARGET ObservationMacros PRE_BUILD
      COMMAND ${CMAKE_COMMAND} -E make_directory "lib${LLVM_LIBDIR_SUFFIX}/swift/host/plugins"
  )

  swift_install_in_component(TARGETS ObservationMacros
    LIBRARY
      DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/host/plugins"
      COMPONENT compiler
    ARCHIVE
      DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/host/plugins"
      COMPONENT compiler)

  set_property(GLOBAL APPEND PROPERTY SWIFT_EXPORTS ObservationMacros)

endif()
