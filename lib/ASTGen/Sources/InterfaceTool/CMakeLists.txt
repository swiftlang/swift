add_pure_swift_host_library(swiftInterfaceTool STATIC CXX_INTEROP
  AccessLevelClassification.swift
  CommentRemover.swift
  ImportsOnlyMinimizer.swift
  InterfaceMinimizer.swift
  InterfaceToolBridging.swift

  DEPENDENCIES
    swiftAST
  SWIFT_DEPENDENCIES
    _CompilerSwiftBasicFormat
    _CompilerSwiftParser
    _CompilerSwiftSyntax
    swiftASTGen
)
