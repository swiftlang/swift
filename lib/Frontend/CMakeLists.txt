add_swift_library(swiftFrontend
  CompilerInvocation.cpp
  DiagnosticVerifier.cpp
  Frontend.cpp
  PrintingDiagnosticConsumer.cpp
  DEPENDS swiftAST swiftParse swiftSema swiftSerialization swiftSIL)

