add_swift_host_tool(swift-serialize-diagnostics
  swift-serialize-diagnostics.cpp
  SWIFT_COMPONENT tools
  DOES_NOT_USE_SWIFT
)
target_link_libraries(swift-serialize-diagnostics PRIVATE
  swiftLocalization)
