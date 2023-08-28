add_swift_host_tool(swift-serialize-diagnostics
  swift-serialize-diagnostics.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(swift-serialize-diagnostics PRIVATE
  swiftLocalization)
