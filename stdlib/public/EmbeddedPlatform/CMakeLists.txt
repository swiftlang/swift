# Install the EmbeddedPlatform header into "swift".

if(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB)
  add_custom_command(
      OUTPUT "${SWIFT_INCLUDE_DIR}/swift/EmbeddedPlatform.h"
      DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/swift/EmbeddedPlatform.h"
      COMMAND "${CMAKE_COMMAND}" "-E" "copy" "${CMAKE_CURRENT_SOURCE_DIR}/swift/EmbeddedPlatform.h" "${SWIFT_INCLUDE_DIR}/swift")

  add_custom_target("copy_embedded_swift_platform_header"
      DEPENDS "${SWIFT_INCLUDE_DIR}/swift/EmbeddedPlatform.h"
      COMMENT "Copying Embedded Swift platform header to ${SWIFT_INCLUDE_DIR}/swift")

  swift_install_in_component(FILES
      "${CMAKE_CURRENT_SOURCE_DIR}/swift/EmbeddedPlatform.h"
      DESTINATION "include/swift"
      COMPONENT stdlib)

  add_dependencies(embedded-libraries
    "copy_embedded_swift_platform_header")

  add_custom_target(embedded-posix-shim)
  add_dependencies(embedded-libraries embedded-posix-shim)
  add_custom_target(embedded-single-threaded-shim)
  add_dependencies(embedded-libraries embedded-single-threaded-shim)
  add_custom_target(embedded-multi-threaded-posix-shim)
  add_dependencies(embedded-libraries embedded-multi-threaded-posix-shim)
  add_custom_target(embedded-multi-threaded-darwin-shim)
  add_dependencies(embedded-libraries embedded-multi-threaded-darwin-shim)

  # The bridging header annotates its C declarations with Clang bounds-safety
  # attributes (EMBEDDED_SWIFT_COUNTED_BY / EMBEDDED_SWIFT_SIZED_BY, i.e.
  # __counted_by / __sized_by). The ClangImporter expands those into the
  # `_SwiftifyImport` macro, which is provided by the SwiftMacros compiler
  # plugin. Point the compiler at the freshly-built plugin directory
  # (`SWIFT_HOST_PLUGINS_DIR`, computed in stdlib/public/CMakeLists.txt) so it
  # doesn't fall back to a stale plugin from the host toolchain (which may
  # predate `SwiftifyImportMacro`). Unlike `core`, this sibling directory does
  # not inherit `core`'s `-plugin-path`, so add it here.
  set(_embedded_platform_swift_flags
    ${swift_stdlib_compile_flags}
    "-plugin-path" "${SWIFT_HOST_PLUGINS_DIR}"
    "-import-bridging-header" ${CMAKE_CURRENT_SOURCE_DIR}/swift/EmbeddedPlatform.h
    -enable-experimental-feature Extern
    -enable-experimental-feature AllowRuntimeSymbolDeclarations
    "-disable-bridging-pch")

  add_embedded_swift_target_library(
    embedded-posix-shim
    swiftEmbeddedPlatformPOSIX
    PARTIAL_SOURCES_INTENDED
    INSTALL_BINARY
    NON_EMPTY_OBJECT_FILE

    EmbeddedPlatformPOSIX.swift

    SWIFT_COMPILE_FLAGS ${_embedded_platform_swift_flags}
    SKIP_MOD_REGEX "-windows-msvc$"
    SKIP_ARCH_REGEX "avr"
    ARCHITECTURE_KEY mod
    INSTALL_IN_COMPONENT stdlib
  )

  add_embedded_swift_target_library(
    embedded-single-threaded-shim
    swiftEmbeddedPlatformSingleThreaded
    PARTIAL_SOURCES_INTENDED
    INSTALL_BINARY
    NON_EMPTY_OBJECT_FILE

    EmbeddedPlatformSingleThreaded.swift

    SWIFT_COMPILE_FLAGS ${_embedded_platform_swift_flags}
    SKIP_MOD_REGEX "-windows-msvc$"
    SKIP_ARCH_REGEX "avr"
    ARCHITECTURE_KEY mod
    INSTALL_IN_COMPONENT stdlib
  )

  # The POSIX pthread-based shim only makes sense on hosted POSIX targets;
  # skip bare-metal and Windows triples that lack a pthread implementation.
  # Also skip Apple targets when the host is not macOS: cross-compiling to
  # Apple from Linux/Windows without an Apple SDK picks up the host libc's
  # `<pthread.h>`, which fails.
  set(_mt_posix_skip_triple "-none-" "-windows-")
  if(NOT SWIFT_HOST_VARIANT STREQUAL "macosx")
    list(APPEND _mt_posix_skip_triple "-apple-")
  endif()

  add_embedded_swift_target_library(
    embedded-multi-threaded-posix-shim
    swiftEmbeddedPlatformMultiThreadedPOSIX
    PARTIAL_SOURCES_INTENDED
    INSTALL_BINARY
    NON_EMPTY_OBJECT_FILE

    EmbeddedPlatformMultiThreadedPOSIX.c

    C_COMPILE_FLAGS -nostdinc++ -I${CMAKE_CURRENT_SOURCE_DIR}/swift
    SKIP_MOD_REGEX "-windows-msvc$"
    SKIP_ARCH_REGEX "avr"
    SKIP_TRIPLE_REGEX ${_mt_posix_skip_triple}
    ARCHITECTURE_KEY mod
    INSTALL_IN_COMPONENT stdlib
  )

  # The Darwin-specific `os_unfair_lock` shim is only meaningful on hosted
  # Apple target triples, and can only be compiled when the host has an
  # Apple SDK (i.e. macOS). Skip bare-metal `-apple-none-*` triples and
  # non-macOS hosts.
  if(SWIFT_HOST_VARIANT STREQUAL "macosx")
    add_embedded_swift_target_library(
      embedded-multi-threaded-darwin-shim
      swiftEmbeddedPlatformMultiThreadedDarwin
      PARTIAL_SOURCES_INTENDED
      INSTALL_BINARY
      NON_EMPTY_OBJECT_FILE

      EmbeddedPlatformMultiThreadedDarwin.c

      C_COMPILE_FLAGS -nostdinc++ -I${CMAKE_CURRENT_SOURCE_DIR}/swift
      ONLY_TRIPLE_REGEX "-apple-"
      SKIP_TRIPLE_REGEX "-apple-none-"
      SKIP_ARCH_REGEX "avr"
      ARCHITECTURE_KEY mod
      INSTALL_IN_COMPONENT stdlib
    )
  endif()
endif()
