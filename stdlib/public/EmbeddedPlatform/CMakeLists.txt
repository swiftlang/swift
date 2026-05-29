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

  add_embedded_swift_target_library(
    embedded-posix-shim
    swiftEmbeddedPlatformPOSIX
    INSTALL_BINARY

    EmbeddedPlatformPOSIX.swift

    SWIFT_COMPILE_FLAGS
      ${swift_stdlib_compile_flags}
      "-import-bridging-header" ${CMAKE_CURRENT_SOURCE_DIR}/swift/EmbeddedPlatform.h
      -enable-experimental-feature Extern
      -enable-experimental-feature AllowRuntimeSymbolDeclarations
      "-disable-bridging-pch"
    SKIP_MOD_REGEX "-windows-msvc$"
    SKIP_ARCH_REGEX "avr"
    ARCHITECTURE_KEY mod
    INSTALL_IN_COMPONENT stdlib
  )
endif()
