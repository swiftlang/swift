add_swift_library(swiftFrontend
  Frontend.cpp
  PrintingDiagnosticConsumer.cpp
  DEPENDS swiftAST swiftParse swiftSema swiftSerialization swiftSIL)

