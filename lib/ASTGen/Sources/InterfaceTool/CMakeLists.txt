add_pure_swift_host_library(swiftInterfaceTool STATIC CXX_INTEROP
  InterfaceToolBridging.swift
  MinimizeForDependencyScan.swift

  DEPENDENCIES
    swiftAST
  SWIFT_DEPENDENCIES
    _CompilerSwiftParser
    _CompilerSwiftSyntax
    swiftASTGen
)
