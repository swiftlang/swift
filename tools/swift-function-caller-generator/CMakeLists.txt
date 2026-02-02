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
endif()
