if (SWIFT_BUILD_SWIFT_SYNTAX)
  add_pure_swift_host_tool(swift-function-caller-generator
    Sources/swift-function-caller-generator/swift-function-caller-generator.swift
    SWIFT_COMPONENT
      testsuite-tools
    SWIFT_DEPENDENCIES
      SwiftSyntax
      SwiftSyntaxBuilder
      SwiftSyntaxMacros
    PACKAGE_NAME Toolchain
  )

  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${SWIFT_HOST_LIBRARIES_DEST_DIR}")
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${SWIFT_HOST_LIBRARIES_DEST_DIR}")

  if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
    set_property(TARGET swift-function-caller-generator
      APPEND PROPERTY INSTALL_RPATH
      "@loader_path")
  elseif(SWIFT_HOST_VARIANT_SDK MATCHES "LINUX|ANDROID|OPENBSD|FREEBSD")
    set_property(TARGET swift-function-caller-generator
      APPEND PROPERTY INSTALL_RPATH
        "$ORIGIN")
    _set_pure_swift_link_flags(swift-function-caller-generator "../../")
  endif()

  set_property(TARGET ${name}
    PROPERTY BUILD_WITH_INSTALL_RPATH YES)
endif()
