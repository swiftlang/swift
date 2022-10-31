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

  get_target_triple(target target_variant "${SWIFT_HOST_VARIANT_SDK}" "${SWIFT_HOST_VARIANT_ARCH}"
    MACCATALYST_BUILD_FLAVOR ""
    DEPLOYMENT_VERSION "${DEPLOYMENT_VERSION}")

  target_compile_options("${library_name}" PRIVATE
    $<$<COMPILE_LANGUAGE:Swift>:
      -module-name;${module_name};
      -enable-library-evolution;
      -emit-module-interface;
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

endif()
