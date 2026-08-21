#===--- CMakeLists.txt - Observation support library ---------------------===#
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

list(APPEND swift_runtime_library_compile_flags -I${SWIFT_SOURCE_DIR}/stdlib/include -I${SWIFT_SOURCE_DIR}/include)

add_swift_target_library(swiftObservation ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  ContinuousObservation.swift
  Locking.swift
  Observable.swift
  ObservationRegistrar.swift
  ObservationTracking.swift
  Observations.swift
  ThreadLocal.cpp
  ThreadLocal.swift

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    "-enable-experimental-feature" "Macros"
    "-enable-experimental-feature" "ExtensionMacros"
    -Xfrontend -disable-implicit-string-processing-module-import

  C_COMPILE_FLAGS
    ${swift_runtime_library_compile_flags}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

  SWIFT_MODULE_DEPENDS _Concurrency
  SWIFT_MODULE_DEPENDS_ANDROID Android
  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_LINUX_STATIC Musl
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_OPENBSD Glibc
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
  SWIFT_MODULE_DEPENDS_HAIKU Glibc
  SWIFT_MODULE_DEPENDS_WINDOWS WinSDK

  INSTALL_IN_COMPONENT stdlib

  MACCATALYST_BUILD_FLAVOR "zippered"
)

# Embedded Observation. Observation imports _Concurrency, so it can only be
# built for the triples that embedded Concurrency was actually built for;
# stdlib/public/Concurrency/CMakeLists.txt publishes that list in a global
# property as it walks the triples.
get_property(_embedded_observation_triples GLOBAL PROPERTY
  SWIFT_EMBEDDED_CONCURRENCY_TARGET_TRIPLES)

if(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB
   AND TARGET embedded-concurrency
   AND _embedded_observation_triples)
  add_custom_target(embedded-observation)
  add_dependencies(embedded-libraries embedded-observation)
  # The .swiftmodule for embedded _Concurrency has to exist before we can
  # import it.
  add_dependencies(embedded-observation embedded-concurrency)

  # `Locking.swift` reads EMBEDDED_SWIFT_MUTEX_NUM_WORDS from the
  # EmbeddedPlatform bridging header. `LiteralExpressions` is what lets the
  # importer bring that macro across as a usable constant; without it the macro
  # is invisible to Swift. See Synchronization's embedded build for the
  # -pch-output-dir and -plugin-path rationale.
  set(_embedded_observation_swift_flags
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    "-enable-experimental-feature" "Macros"
    "-enable-experimental-feature" "ExtensionMacros"
    "-enable-experimental-feature" "Extern"
    "-enable-experimental-feature" "ValueGenerics"
    "-enable-experimental-feature" "LiteralExpressions"
    -Xfrontend -disable-implicit-string-processing-module-import
    "-plugin-path" "${SWIFT_HOST_PLUGINS_DIR}"
    -internal-import-bridging-header
      ${CMAKE_CURRENT_SOURCE_DIR}/../../../EmbeddedPlatform/swift/EmbeddedPlatform.h
    -pch-output-dir ${CMAKE_CURRENT_BINARY_DIR}/EmbeddedObservationPCH)

  set(_embedded_observation_all_triples "${EMBEDDED_STDLIB_TARGET_TRIPLES}")

  list(APPEND LLVM_OPTIONAL_SOURCES ThreadLocal.cpp)

  foreach(entry ${_embedded_observation_triples})
    string(REGEX REPLACE "[ \t]+" ";" list "${entry}")
    list(GET list 1 mod)

    # Restrict the call below to the current triple by temporarily narrowing the
    # triple list visible to add_embedded_swift_target_library.
    set(EMBEDDED_STDLIB_TARGET_TRIPLES "${entry}")

    # lib/swift/embedded/Observation.swiftmodule
    # lib/swift/embedded/<triple>/libswiftObservation.a
    #
    # The per-triple dependency on embedded Concurrency is what makes the
    # _Concurrency .swiftmodule for *this* triple exist before we try to import
    # it; a dependency on the aggregate embedded-concurrency target is not
    # enough, because these per-triple library targets do not inherit it.
    add_embedded_swift_target_library(
      embedded-observation
      swiftObservation
      IS_STDLIB
      INSTALL_BINARY

      ContinuousObservation.swift
      Locking.swift
      Observable.swift
      ObservationRegistrar.swift
      ObservationTracking.swift
      Observations.swift
      ThreadLocal.swift

      SWIFT_COMPILE_FLAGS ${_embedded_observation_swift_flags}
      DEPENDS embedded-concurrency-${mod}
      INSTALL_IN_COMPONENT stdlib
    )
  endforeach()

  # Restore the full triple list for any later consumers.
  set(EMBEDDED_STDLIB_TARGET_TRIPLES "${_embedded_observation_all_triples}")
endif()
