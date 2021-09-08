add_swift_host_tool(swift-stdlib-tool
  swift-stdlib-tool.cpp
  SWIFT_COMPONENT compiler)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

target_link_libraries(swift-stdlib-tool PRIVATE)
