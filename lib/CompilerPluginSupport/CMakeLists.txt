#===--- CMakeLists.txt - Compiler plugin support library -------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===------------------------------------------------------------------------===#

if(SWIFT_SWIFT_PARSER)

  set(module_name "_CompilerPluginSupport")
  set(library_name "swift${module_name}")

  add_library("${library_name}" SHARED
    CompilerPluginSupport.swift)

  if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
    set(DEPLOYMENT_VERSION "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_DEPLOYMENT_VERSION}")
  endif()

  if(SWIFT_HOST_VARIANT_SDK STREQUAL ANDROID)
    set(DEPLOYMENT_VERSION ${SWIFT_ANDROID_API_LEVEL})
  endif()

  # Determine the Swift module path
  set(module_triple ${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_ARCH_${SWIFT_HOST_VARIANT_ARCH}_MODULE})
  set(module_dir "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")
  set(module_base "${module_dir}/${module_name}.swiftmodule")
  set(module_file "${module_base}/${module_triple}.swiftmodule")
  set(module_interface_file "${module_base}/${module_triple}.swiftinterface")
  set(module_doc_file "${module_base}/${module_triple}.swiftdoc")

  get_target_triple(target target_variant "${SWIFT_HOST_VARIANT_SDK}" "${SWIFT_HOST_VARIANT_ARCH}"
    MACCATALYST_BUILD_FLAVOR ""
    DEPLOYMENT_VERSION "${DEPLOYMENT_VERSION}")

  # Add a custom target to create the module directory and remove any old
  # compiled module file.
  add_custom_command_target(
      create_module_dirs_dependency_target
      COMMAND "${CMAKE_COMMAND}" -E make_directory ${module_base}
      COMMAND "${CMAKE_COMMAND}" -E rm -f "${module_file}"
      OUTPUT ${module_base}
      COMMENT "Generating module directory for ${module_name}")
  add_dependencies(${library_name} ${create_module_dirs_dependency_target})

  # Build the module with library evolution enabled and install into the
  # appropriate locations.
  target_compile_options("${library_name}" PRIVATE
    $<$<COMPILE_LANGUAGE:Swift>:
      -module-name;${module_name};
      -enable-library-evolution;
      -emit-module-interface-path;
      "${module_interface_file}" ;
      -target;${target}>)

  target_link_libraries("${library_name}"
    PRIVATE
    swiftCore
  )

  swift_install_in_component(TARGETS "${library_name}"
    RUNTIME
      DESTINATION "bin"
      COMPONENT compiler
    FRAMEWORK
      DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
      COMPONENT compiler
    LIBRARY
      DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
      COMPONENT compiler
    ARCHIVE
      DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
      COMPONENT compiler)

    swift_install_in_component(DIRECTORY "${module_base}"
                               DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift"
                               COMPONENT compiler)

    set_property(GLOBAL APPEND PROPERTY SWIFT_EXPORTS ${library_name})
endif()
