add_swift_host_tool(swift-api-digester
  swift-api-digester.cpp
  ModuleAnalyzerNodes.cpp
  ModuleDiagsConsumer.cpp
  SWIFT_COMPONENTS tools toolchain-tools
)
target_link_libraries(swift-api-digester
                      PRIVATE
                        swiftFrontend
                        swiftSIL
                        swiftIDE)
