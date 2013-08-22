set(LLVM_TARGET_DEFINITIONS FrontendOptions.td)
tablegen(LLVM FrontendOptions.inc -gen-opt-parser-defs)
add_public_tablegen_target(SwiftFrontendOptionsTableGen)

add_swift_library(swiftFrontend
  CompilerInvocation.cpp
  DiagnosticVerifier.cpp
  Frontend.cpp
  PrintingDiagnosticConsumer.cpp
  DEPENDS swiftAST swiftParse swiftSema swiftSerialization swiftSIL)

add_dependencies(swiftFrontend SwiftFrontendOptionsTableGen)
