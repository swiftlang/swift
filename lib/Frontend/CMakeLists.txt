add_swift_library(swiftFrontend STATIC
  CompilerInvocation.cpp
  DiagnosticVerifier.cpp
  Frontend.cpp
  FrontendOptions.cpp
  PrintingDiagnosticConsumer.cpp
  SerializedDiagnosticConsumer.cpp
  DEPENDS SwiftOptions
  LINK_LIBRARIES
    swiftSIL
    swiftOption
    swiftParse
    swiftSema
    swiftSerialization)

