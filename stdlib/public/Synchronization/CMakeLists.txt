#===--- CMakeLists.txt - Synchronization library ---------------------------===#
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

set(swift_synchronization_sources
  Atomic.swift
  AtomicBool.swift
  AtomicFloats.swift
  AtomicLazyReference.swift
  AtomicMemoryOrderings.swift
  AtomicOptional.swift
  AtomicPointers.swift
  AtomicRepresentable.swift
  WordPair.swift
)
set(swift_synchronization_gyb_sources
  AtomicIntegers.swift.gyb
  AtomicStorage.swift.gyb
)
set(swift_synchronization_swift_compile_flags
  ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  "-enable-builtin-module"
  "-enable-experimental-feature" "RawLayout"
  "-enable-experimental-feature" "StaticExclusiveOnly"
)

add_swift_target_library(swiftSynchronization ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  ${swift_synchronization_sources}
  GYB_SOURCES ${swift_synchronization_gyb_sources}
  SWIFT_COMPILE_FLAGS ${swift_synchronization_swift_compile_flags}
  LINK_FLAGS
    "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  INSTALL_IN_COMPONENT
    stdlib
  MACCATALYST_BUILD_FLAVOR
    "zippered"
)

# Embedded Synchronization - embedded libraries are built as .swiftmodule only,
# i.e. there is no .o or .a file produced (no binary code is actually produced)
# and only users of a library are going to actually compile any needed code.
if(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB)
  add_custom_target(embedded-synchronization ALL)

  foreach(entry ${EMBEDDED_STDLIB_TARGET_TRIPLES})
    string(REGEX REPLACE "[ \t]+" ";" list "${entry}")
    list(GET list 0 arch)
    list(GET list 1 mod)
    list(GET list 2 triple)
    
    set(SWIFT_SDK_embedded_ARCH_${arch}_MODULE "${mod}")
    set(SWIFT_SDK_embedded_LIB_SUBDIR "embedded")
    set(SWIFT_SDK_embedded_ARCH_${arch}_TRIPLE "${triple}")
    set(SWIFT_SDK_embedded_PATH ${SWIFT_SDK_OSX_PATH})
    set(SWIFT_SDK_embedded_ARCH_${arch}_PATH ${SWIFT_SDK_OSX_PATH})
    set(SWIFT_SDK_embedded_USE_ISYSROOT TRUE)
    add_swift_target_library_single(
      embedded-synchronization-${mod}
      swiftSynchronization
      ONLY_SWIFTMODULE
      IS_FRAGILE
      
      ${swift_synchronization_sources}
      GYB_SOURCES ${swift_synchronization_gyb_sources}

      SWIFT_COMPILE_FLAGS
        ${swift_synchronization_swift_compile_flags}
        -Xcc -D__MACH__ -Xcc -D__APPLE__ -Xcc -ffreestanding -enable-experimental-feature Embedded

      MODULE_DIR "${CMAKE_BINARY_DIR}/lib/swift/embedded"
      SDK "embedded"
      ARCHITECTURE "${arch}"
      DEPENDS embedded-stdlib-${mod}
      INSTALL_IN_COMPONENT stdlib
      )
    add_dependencies(embedded-synchronization embedded-synchronization-${mod})
  endforeach()
endif()
