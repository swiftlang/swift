#===--- CMakeLists.txt - Concurrency support library ---------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2019 - 2022 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

set(swift_distributed_darwin_dependencies)

if((SWIFT_BUILD_CLANG_OVERLAYS
    OR SWIFT_BUILD_TEST_SUPPORT_MODULES)
   AND (NOT DEFINED SWIFT_BUILD_CLANG_OVERLAYS_SKIP_BUILTIN_FLOAT
        OR NOT SWIFT_BUILD_CLANG_OVERLAYS_SKIP_BUILTIN_FLOAT))
  # This library imports Darwin from the SDK, which re-exports
  # _Builtin_float which is part of this build.
  list(APPEND swift_distributed_darwin_dependencies _Builtin_float)
endif()

set(swift_distributed_link_libraries
  swiftCore)


add_swift_target_library(swiftDistributed ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  DistributedActor.cpp
  DistributedActor.swift
  DistributedActorSystem.swift
  DistributedAssertions.swift
  DistributedDefaultExecutor.swift
  DistributedMacros.swift
  DistributedMetadata.swift
  LocalTestingDistributedActorSystem.swift

  SWIFT_MODULE_DEPENDS_IOS ${swift_distributed_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_OSX ${swift_distributed_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_TVOS ${swift_distributed_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_WATCHOS ${swift_distributed_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_ANDROID Android
  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_LINUX_STATIC Musl
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_OPENBSD Glibc
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
  SWIFT_MODULE_DEPENDS_HAIKU Glibc
  SWIFT_MODULE_DEPENDS_WINDOWS CRT WinSDK

  LINK_LIBRARIES ${swift_distributed_link_libraries}

  C_COMPILE_FLAGS
    -DswiftDistributed_EXPORTS
    -I${SWIFT_SOURCE_DIR}/stdlib/include

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -parse-stdlib
    -enable-experimental-feature AllowUnsafeAttribute
    -strict-memory-safety

  LINK_FLAGS
    "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

  SWIFT_MODULE_DEPENDS _Concurrency
  INSTALL_IN_COMPONENT stdlib
)

# ==== ----------------------------------------------------------------------
# MARK: Embedded Swift Distributed library
#
# Phase 1: get a minimal Distributed module building for embedded targets.
# Follows the pattern used by stdlib/public/Concurrency for its embedded
# variant. The user is expected to supply a concrete DistributedActorSystem;
# this module provides only the protocols and supporting types.
set(SWIFT_SHOULD_BUILD_EMBEDDED_DISTRIBUTED TRUE)
is_build_type_optimized("${SWIFT_STDLIB_BUILD_TYPE}" swift_optimized)
if(NOT swift_optimized)
  set(SWIFT_SHOULD_BUILD_EMBEDDED_DISTRIBUTED FALSE)
elseif(BOOTSTRAPPING_MODE STREQUAL "OFF")
  set(SWIFT_SHOULD_BUILD_EMBEDDED_DISTRIBUTED FALSE)
elseif(SWIFT_HOST_VARIANT STREQUAL "windows")
  set(SWIFT_SHOULD_BUILD_EMBEDDED_DISTRIBUTED FALSE)
endif()
if(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB AND SWIFT_SHOULD_BUILD_EMBEDDED_DISTRIBUTED)
  add_custom_target(embedded-distributed)
  add_dependencies(embedded-libraries embedded-distributed)

  set(SWIFT_ENABLE_REFLECTION OFF)
  set(SWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT OFF)
  set(SWIFT_STDLIB_STABLE_ABI OFF)
  set(SWIFT_STDLIB_ENABLE_OBJC_INTEROP OFF)

  # In Embedded Swift, all stdlib APIs should be available always. Replace
  # all availability macros with an empty expansion (`*`).
  set(SWIFT_STDLIB_AVAILABILITY_DEFINITIONS_EMBEDDED)
  foreach(def ${SWIFT_STDLIB_AVAILABILITY_DEFINITIONS})
    string(REGEX REPLACE ":.*" ":*" replaced "${def}")
    list(APPEND SWIFT_STDLIB_AVAILABILITY_DEFINITIONS_EMBEDDED "${replaced}")
  endforeach()
  set(SWIFT_STDLIB_AVAILABILITY_DEFINITIONS "${SWIFT_STDLIB_AVAILABILITY_DEFINITIONS_EMBEDDED}")

  foreach(entry ${EMBEDDED_STDLIB_TARGET_TRIPLES})
    string(REGEX REPLACE "[ \t]+" ";" list "${entry}")
    list(GET list 0 arch)
    list(GET list 1 mod)
    list(GET list 2 triple)

    # Skip Distributed for freestanding Wasm (matches Concurrency policy).
    if("${triple}" MATCHES "-none-wasm$")
      continue()
    endif()

    set(extra_c_compile_flags)

    if(SWIFT_HOST_VARIANT STREQUAL "linux")
      if(NOT "${mod}" MATCHES "-linux-gnu$")
        continue()
      endif()
    elseif(SWIFT_HOST_VARIANT STREQUAL "macosx")
      if(NOT "${mod}" MATCHES "x86_64|arm64|arm64e|armv7|armv7m|armv7em|armv8m.main|armv8.1m.main")
        continue()
      endif()

      if(NOT "${mod}" MATCHES "-apple-" OR "${mod}" MATCHES "-none-macho" OR "${arch}" STREQUAL "armv7m" OR "${arch}" STREQUAL "armv7em" OR "${arch}" STREQUAL "armv8m.main" OR "${arch}" STREQUAL "armv8.1m.main")
        # Host is macOS with a macOS SDK. To be able to build the C++ Distributed runtime for non-Darwin targets using the macOS SDK,
        # we need to pass some extra flags and search paths (matches Concurrency policy).
        set(extra_c_compile_flags -stdlib=libc++ -isystem${SWIFT_SDK_OSX_PATH}/usr/include/c++/v1 -isystem${SWIFT_SDK_OSX_PATH}/usr/include -D__APPLE__)
        if("${arch}" MATCHES "armv7")
          list(APPEND extra_c_compile_flags -DTARGET_CPU_ARM=1)
        elseif("${arch}" MATCHES "armv8")
          list(APPEND extra_c_compile_flags -DTARGET_CPU_ARM=1)
        elseif("${arch}" MATCHES "arm64")
          list(APPEND extra_c_compile_flags -DTARGET_CPU_ARM64=1)
        elseif("${arch}" MATCHES "x86_64")
          list(APPEND extra_c_compile_flags -DTARGET_CPU_X86_64=1)
        endif()
      endif()
    endif()

    set(SWIFT_SDK_embedded_THREADING_PACKAGE none)
    set(SWIFT_SDK_embedded_ARCH_${arch}_MODULE "${mod}")
    set(SWIFT_SDK_embedded_ARCH_${mod}_MODULE "${mod}")
    set(SWIFT_SDK_embedded_LIB_SUBDIR "embedded")
    set(SWIFT_SDK_embedded_ARCH_${arch}_TRIPLE "${triple}")
    set(SWIFT_SDK_embedded_ARCH_${mod}_TRIPLE "${triple}")
    if(SWIFT_EMBEDDED_STDLIB_SDKS_FOR_TARGET_TRIPLES)
      set(SWIFT_SDK_embedded_ARCH_${arch}_PATH "${EMBEDDED_STDLIB_SDK_FOR_${triple}}")
      set(SWIFT_SDK_embedded_ARCH_${mod}_PATH "${EMBEDDED_STDLIB_SDK_FOR_${triple}}")
    endif()

    # lib/swift/embedded/Distributed.swiftmodule
    # lib/swift/embedded/<triple>/libswiftDistributed.a
    add_swift_target_library_single(
      embedded-distributed-${mod}
      swiftDistributed
      STATIC
      IS_STDLIB IS_FRAGILE

      DistributedActor.cpp
      DistributedActor.swift
      DistributedActorSystem.swift
      DistributedAssertions.swift
      DistributedDefaultExecutor.swift
      DistributedMacros.swift
      DistributedMetadata.swift
      # LocalTestingDistributedActorSystem.swift is not built for embedded;
      # it depends on Codable/locks and the user is expected to supply their
      # own concrete actor system.

      SWIFT_COMPILE_FLAGS
        -enable-experimental-feature Embedded
        -parse-stdlib
        -DSWIFT_DISTRIBUTED_EMBEDDED
        -Xfrontend -emit-empty-object-file
        -enable-experimental-feature AllowUnsafeAttribute
        -strict-memory-safety
      C_COMPILE_FLAGS
        ${extra_c_compile_flags}
        -DswiftDistributed_EXPORTS
        -DSWIFT_DISTRIBUTED_EMBEDDED=1
        -DSWIFT_RUNTIME_EMBEDDED=1
        -I${SWIFT_SOURCE_DIR}/stdlib/include
        -ffunction-sections -fdata-sections -fno-exceptions -fno-cxx-exceptions -fno-unwind-tables
      MODULE_DIR "${CMAKE_BINARY_DIR}/lib/swift/embedded"
      SDK "embedded"
      ARCHITECTURE "${arch}"
      ARCHITECTURE_SUBDIR_NAME "${mod}"
      DEPENDS embedded-stdlib-${mod} embedded-concurrency-${mod}
      INSTALL_IN_COMPONENT stdlib
      )
    swift_install_in_component(
      TARGETS embedded-distributed-${mod}
      DESTINATION "lib/swift/embedded/${mod}"
      COMPONENT "stdlib"
      )
    swift_install_in_component(
      FILES "${SWIFTLIB_DIR}/embedded/${mod}/libswiftDistributed.a"
      DESTINATION "lib/swift/embedded/${mod}/"
      COMPONENT "stdlib"
      PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
      )

    set_property(TARGET embedded-distributed-${mod} PROPERTY OSX_ARCHITECTURES "${arch}")

    embedded_amend_archive_commands_on_darwin_host(embedded-distributed-${mod} ${triple})

    add_dependencies(embedded-distributed embedded-distributed-${mod})
  endforeach()
endif()
