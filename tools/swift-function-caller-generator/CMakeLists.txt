if (SWIFT_BUILD_SWIFT_SYNTAX)
  add_pure_swift_host_tool(swift-function-caller-generator
    Sources/swift-function-caller-generator/swift-function-caller-generator.swift
    SWIFT_COMPONENT
      testsuite-tools
    SWIFT_DEPENDENCIES
      SwiftSyntax
      SwiftSyntaxBuilder
      SwiftSyntaxMacros
      SwiftDiagnostics
      SwiftOperators
    PACKAGE_NAME Toolchain
  )

  set_target_properties(swift-function-caller-generator PROPERTIES
    Swift_MODULE_NAME SwiftFunctionCallerGenerator
  )
endif()
