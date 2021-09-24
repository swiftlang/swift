add_swift_host_tool(swift-stdlib-tool
  swift-stdlib-tool.cpp
  SWIFT_COMPONENT compiler)

set_target_properties(swift-stdlib-tool PROPERTIES
  CXX_STANDARD 17
  CXX_STANDARD_REQUIRED ON)

