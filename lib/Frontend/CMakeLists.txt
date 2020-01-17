add_swift_host_library(swiftFrontend STATIC
  ArgsToFrontendInputsConverter.cpp
  ArgsToFrontendOptionsConverter.cpp
  ArgsToFrontendOutputsConverter.cpp
  CompilerInvocation.cpp
  DiagnosticVerifier.cpp
  Frontend.cpp
  FrontendInputsAndOutputs.cpp
  FrontendOptions.cpp
  ModuleInterfaceBuilder.cpp
  ModuleInterfaceLoader.cpp
  ModuleInterfaceSupport.cpp
  PrintingDiagnosticConsumer.cpp
  SerializedDiagnosticConsumer.cpp)
add_dependencies(swiftFrontend
  SwiftOptions)
target_link_libraries(swiftFrontend PRIVATE
  swiftSIL
  swiftMigrator
  swiftOption
  swiftParseSIL
  swiftSILGen
  swiftSILOptimizer
  swiftSema
  swiftSerialization)

