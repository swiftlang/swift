add_pure_swift_host_library(swiftIDEUtilsBridging CXX_INTEROP
  NameMatcherBridging.swift

  DEPENDENCIES
    swiftAST
  SWIFT_DEPENDENCIES
    _CompilerSwiftIDEUtils
    _CompilerSwiftSyntax
    _CompilerSwiftIDEUtils
    swiftASTGen
)
