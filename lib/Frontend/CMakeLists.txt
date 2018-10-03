add_swift_library(swiftFrontend STATIC
  ArgsToFrontendInputsConverter.cpp
  ArgsToFrontendOptionsConverter.cpp
  ArgsToFrontendOutputsConverter.cpp
  CompilerInvocation.cpp
  DiagnosticVerifier.cpp
  Frontend.cpp
  FrontendInputsAndOutputs.cpp
  FrontendOptions.cpp
  ParseableInterfaceSupport.cpp
  PrintingDiagnosticConsumer.cpp
  SerializedDiagnosticConsumer.cpp
  DEPENDS
    SwiftOptions
  LINK_LIBRARIES
    swiftSIL
    swiftMigrator
    swiftOption
    swiftParseSIL
    swiftSema
    swiftSerialization)

