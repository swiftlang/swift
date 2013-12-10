add_swift_library(swiftFrontend
  CompilerInvocation.cpp
  DiagnosticVerifier.cpp
  Frontend.cpp
  PrintingDiagnosticConsumer.cpp
  SerializedDiagnosticConsumer.cpp
  DEPENDS swiftAST swiftDriver swiftParse swiftSema swiftSerialization swiftSIL)
