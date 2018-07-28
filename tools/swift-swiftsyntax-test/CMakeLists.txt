add_swift_host_tool(swift-swiftsyntax-test
  main.swift
  CommandLineArguments.swift 
  empty.c # FIXME: If there is no C file in the target Xcode skips the linking phase and doesn't create the executable
  COMPILE_FLAGS "-module-name" "main"
  DEPENDS 
    swiftSwiftSyntax-macosx
    swiftSwiftSyntax-macosx-x86_64
    swiftSwiftSyntax-swiftmodule-macosx-x86_64
)
