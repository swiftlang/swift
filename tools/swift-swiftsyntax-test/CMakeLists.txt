add_swift_host_tool(swift-swiftsyntax-test
  main.swift
  CommandLineArguments.swift 
  COMPILE_FLAGS "-module-name" "main"
  DEPENDS 
    swiftSwiftSyntax-macosx
    swiftSwiftSyntax-macosx-x86_64
    swiftSwiftSyntax-swiftmodule-macosx-x86_64
)
