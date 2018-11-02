add_swift_library(swiftFrontend STATIC
  ArgsToFrontendInputsConverter.cpp
  ArgsToFrontendOptionsConverter.cpp
  ArgsToFrontendOutputsConverter.cpp
  CompilerInvocation.cpp
  DiagnosticVerifier.cpp
  Frontend.cpp
  FrontendInputsAndOutputs.cpp
  FrontendOptions.cpp
  OutputFileMap.cpp
  PrintingDiagnosticConsumer.cpp
  SerializedDiagnosticConsumer.cpp
  Types.cpp
  DEPENDS
    SwiftOptions
  LINK_LIBRARIES
    swiftSIL
    swiftMigrator
    swiftOption
    swiftParseSIL
    swiftSema
    swiftSerialization)

