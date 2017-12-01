add_swift_library(swiftFrontend STATIC
  CompilerInvocation.cpp
  DiagnosticVerifier.cpp
  Frontend.cpp
  FrontendOptions.cpp
  PrintingDiagnosticConsumer.cpp
  SerializedDiagnosticConsumer.cpp
  DEPENDS
    SwiftOptions
    swift-syntax-generated-headers
  LINK_LIBRARIES
    swiftSIL
    swiftMigrator
    swiftOption
    swiftParseSIL
    swiftSema
    swiftSerialization)

