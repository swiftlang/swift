add_swift_host_tool(swift-refactor
  swift-refactor.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(swift-refactor
                      PRIVATE
                        swiftDriver
                        swiftFrontend
                        swiftIDE)
