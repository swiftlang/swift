add_pure_swift_host_library(swiftBasicSwift STATIC CXX_INTEROP
  StaticBuildConfiguration+LangOptions.swift
  String+BridgedString.swift

  DEPENDENCIES
    swiftBasic
  SWIFT_DEPENDENCIES
    _CompilerSwiftCompilerPluginMessageHandling
    _CompilerSwiftSyntax
    _CompilerSwiftIfConfig
)
