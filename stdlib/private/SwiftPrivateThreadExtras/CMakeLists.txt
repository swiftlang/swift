foreach(sdk ${SWIFT_SDKS})

set(module_depends)
set(glibc_sdks LINUX FREEBSD CYGWIN HAIKU)

if(${sdk} IN_LIST SWIFT_APPLE_PLATFORMS)
  list(APPEND module_depends Darwin Foundation)
elseif(${sdk} IN_LIST glibc_sdks)
  list(APPEND module_depends Glibc)
elseif(${sdk} STREQUAL WINDOWS)
  list(APPEND module_depends MSVCRT WinSDK)
endif()

add_swift_target_library_single_sdk(swiftSwiftPrivateThreadExtras ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftPrivateThreadExtras.swift
  ThreadBarriers.swift

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  SDK ${sdk}
  SWIFT_MODULE_DEPENDS ${module_depends}
  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  TARGET_SDKS ALL_APPLE_PLATFORMS CYGWIN FREEBSD HAIKU LINUX WINDOWS ANDROID
  INSTALL_IN_COMPONENT stdlib-experimental
  DARWIN_INSTALL_NAME_DIR "${SWIFT_DARWIN_STDLIB_PRIVATE_INSTALL_NAME_DIR}")

endforeach()