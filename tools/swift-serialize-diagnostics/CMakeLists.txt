add_swift_host_tool(swift-serialize-diagnostics
  swift-serialize-diagnostics.cpp
  SWIFT_COMPONENT tools
  REQUIRED_FOR_MINIMAL_COMPILER
)
target_link_libraries(swift-serialize-diagnostics PRIVATE
  swiftLocalization)
