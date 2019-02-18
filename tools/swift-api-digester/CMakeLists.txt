add_swift_host_tool(swift-api-digester
  swift-api-digester.cpp
  ModuleAnalyzerNodes.cpp
  ModuleDiagsConsumer.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(swift-api-digester
                      PRIVATE
                        swiftFrontend
                        swiftIDE)
