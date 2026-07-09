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

  foreach(entry ${EMBEDDED_STDLIB_TARGET_TRIPLES})
    string(REGEX REPLACE "[ \t]+" ";" list "${entry}")
    list(GET list 0 arch)
    list(GET list 1 mod)
    list(GET list 2 triple)

    if("${mod}" MATCHES "-windows-msvc$")
      continue()
    endif()

    if("${arch}" MATCHES "avr")
      continue()
    endif()

    if (SWIFT_HOST_VARIANT STREQUAL "linux")
      set(extra_c_compile_flags -ffreestanding)
    elseif (SWIFT_HOST_VARIANT STREQUAL "macosx")
      set(extra_c_compile_flags -ffreestanding)
    endif()
    list(APPEND extra_c_compile_flags -nostdinc++)

    set(SWIFT_SDK_embedded_ARCH_${mod}_MODULE "${mod}")
    set(SWIFT_SDK_embedded_LIB_SUBDIR "embedded")
    set(SWIFT_SDK_embedded_ARCH_${mod}_TRIPLE "${triple}")
    if(SWIFT_EMBEDDED_STDLIB_SDKS_FOR_TARGET_TRIPLES)
      set(SWIFT_SDK_embedded_ARCH_${mod}_PATH "${EMBEDDED_STDLIB_SDK_FOR_${triple}}")
    endif()

    add_swift_target_library_single(
      embedded-posix-shim-${mod}
      swiftEmbeddedPlatformPOSIX
      STATIC
      IS_FRAGILE
      PARTIAL_SOURCES_INTENDED

      EmbeddedPlatformPOSIX.swift

      C_COMPILE_FLAGS ${extra_c_compile_flags}
      SWIFT_COMPILE_FLAGS ${swift_stdlib_compile_flags} -Xcc -ffreestanding -enable-experimental-feature Embedded "-import-bridging-header" ${CMAKE_CURRENT_SOURCE_DIR}/swift/EmbeddedPlatform.h -enable-experimental-feature Extern -enable-experimental-feature AllowRuntimeSymbolDeclarations "-disable-bridging-pch"
      MODULE_DIR "${CMAKE_BINARY_DIR}/lib/swift/embedded"
      SDK "embedded"
      ARCHITECTURE "${mod}"
      DEPENDS embedded-stdlib-${mod}
      INSTALL_IN_COMPONENT stdlib
      )
    swift_install_in_component(
      TARGETS embedded-posix-shim-${mod}
      DESTINATION "lib/swift/embedded/${mod}"
      COMPONENT "stdlib"
      )
    swift_install_in_component(
      FILES "${SWIFTLIB_DIR}/embedded/${mod}/libswiftEmbeddedPlatformPOSIX.a"
      DESTINATION "lib/swift/embedded/${mod}/"
      COMPONENT "stdlib"
      PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
      )
    set_property(TARGET embedded-posix-shim-${mod} PROPERTY OSX_ARCHITECTURES "${arch}")

    add_dependencies(embedded-posix-shim embedded-posix-shim-${mod})

    add_swift_target_library_single(
      embedded-single-threaded-shim-${mod}
      swiftEmbeddedPlatformSingleThreaded
      STATIC
      IS_FRAGILE
      PARTIAL_SOURCES_INTENDED

      EmbeddedPlatformSingleThreaded.swift

      C_COMPILE_FLAGS ${extra_c_compile_flags}
      SWIFT_COMPILE_FLAGS ${swift_stdlib_compile_flags} -Xcc -ffreestanding -enable-experimental-feature Embedded "-import-bridging-header" ${CMAKE_CURRENT_SOURCE_DIR}/swift/EmbeddedPlatform.h -enable-experimental-feature Extern -enable-experimental-feature AllowRuntimeSymbolDeclarations "-disable-bridging-pch"
      MODULE_DIR "${CMAKE_BINARY_DIR}/lib/swift/embedded"
      SDK "embedded"
      ARCHITECTURE "${mod}"
      DEPENDS embedded-stdlib-${mod}
      INSTALL_IN_COMPONENT stdlib
      )
    swift_install_in_component(
      TARGETS embedded-single-threaded-shim-${mod}
      DESTINATION "lib/swift/embedded/${mod}"
      COMPONENT "stdlib"
      )
    swift_install_in_component(
      FILES "${SWIFTLIB_DIR}/embedded/${mod}/libswiftEmbeddedPlatformSingleThreaded.a"
      DESTINATION "lib/swift/embedded/${mod}/"
      COMPONENT "stdlib"
      PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
      )
    set_property(TARGET embedded-single-threaded-shim-${mod} PROPERTY OSX_ARCHITECTURES "${arch}")

    add_dependencies(embedded-single-threaded-shim embedded-single-threaded-shim-${mod})

    # The POSIX pthread-based shim only makes sense on hosted POSIX targets;
    # skip bare-metal and Windows triples that lack a pthread implementation.
    # Also skip Apple targets when the host is not macOS: cross-compiling to
    # Apple from Linux/Windows without an Apple SDK picks up the host libc's
    # `<pthread.h>`, which fails.
    if(NOT "${triple}" MATCHES "-none-|-windows-" AND
       (NOT "${triple}" MATCHES "-apple-" OR SWIFT_HOST_VARIANT STREQUAL "macosx"))
      add_swift_target_library_single(
        embedded-multi-threaded-posix-shim-${mod}
        swiftEmbeddedPlatformMultiThreadedPOSIX
        STATIC
        IS_FRAGILE
        PARTIAL_SOURCES_INTENDED

        EmbeddedPlatformMultiThreadedPOSIX.c

        C_COMPILE_FLAGS ${extra_c_compile_flags} -I${CMAKE_CURRENT_SOURCE_DIR}/swift
        MODULE_DIR "${CMAKE_BINARY_DIR}/lib/swift/embedded"
        SDK "embedded"
        ARCHITECTURE "${mod}"
        DEPENDS embedded-stdlib-${mod}
        INSTALL_IN_COMPONENT stdlib
        )
      swift_install_in_component(
        TARGETS embedded-multi-threaded-posix-shim-${mod}
        DESTINATION "lib/swift/embedded/${mod}"
        COMPONENT "stdlib"
        )
      swift_install_in_component(
        FILES "${SWIFTLIB_DIR}/embedded/${mod}/libswiftEmbeddedPlatformMultiThreadedPOSIX.a"
        DESTINATION "lib/swift/embedded/${mod}/"
        COMPONENT "stdlib"
        PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
        )
      set_property(TARGET embedded-multi-threaded-posix-shim-${mod} PROPERTY OSX_ARCHITECTURES "${arch}")

      add_dependencies(embedded-multi-threaded-posix-shim embedded-multi-threaded-posix-shim-${mod})
    endif()

    # The Darwin-specific `os_unfair_lock` shim is only meaningful on hosted
    # Apple target triples, and can only be compiled when the host has an
    # Apple SDK (i.e. macOS). Skip bare-metal `-apple-none-*` triples and
    # non-macOS hosts.
    if("${triple}" MATCHES "-apple-" AND
       NOT "${triple}" MATCHES "-apple-none-" AND
       SWIFT_HOST_VARIANT STREQUAL "macosx")
      add_swift_target_library_single(
        embedded-multi-threaded-darwin-shim-${mod}
        swiftEmbeddedPlatformMultiThreadedDarwin
        STATIC
        IS_FRAGILE
        PARTIAL_SOURCES_INTENDED

        EmbeddedPlatformMultiThreadedDarwin.c

        C_COMPILE_FLAGS ${extra_c_compile_flags} -I${CMAKE_CURRENT_SOURCE_DIR}/swift
        MODULE_DIR "${CMAKE_BINARY_DIR}/lib/swift/embedded"
        SDK "embedded"
        ARCHITECTURE "${mod}"
        DEPENDS embedded-stdlib-${mod}
        INSTALL_IN_COMPONENT stdlib
        )
      swift_install_in_component(
        TARGETS embedded-multi-threaded-darwin-shim-${mod}
        DESTINATION "lib/swift/embedded/${mod}"
        COMPONENT "stdlib"
        )
      swift_install_in_component(
        FILES "${SWIFTLIB_DIR}/embedded/${mod}/libswiftEmbeddedPlatformMultiThreadedDarwin.a"
        DESTINATION "lib/swift/embedded/${mod}/"
        COMPONENT "stdlib"
        PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
        )
      set_property(TARGET embedded-multi-threaded-darwin-shim-${mod} PROPERTY OSX_ARCHITECTURES "${arch}")

      add_dependencies(embedded-multi-threaded-darwin-shim embedded-multi-threaded-darwin-shim-${mod})
    endif()
  endforeach()
endif()
