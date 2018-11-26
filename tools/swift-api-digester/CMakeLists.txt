add_swift_host_tool(swift-api-digester
  swift-api-digester.cpp
  ModuleAnalyzerNodes.cpp
  ModuleDiagsConsumer.cpp
  LINK_LIBRARIES swiftFrontend swiftIDE
  SWIFT_COMPONENT tools
)
