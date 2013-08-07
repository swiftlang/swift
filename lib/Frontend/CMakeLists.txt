add_swift_library(swiftFrontend
  DiagnosticVerifier.cpp
  Frontend.cpp
  PrintingDiagnosticConsumer.cpp
  DEPENDS swiftAST swiftParse swiftSema swiftSerialization swiftSIL)

