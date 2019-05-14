add_swift_host_tool(swift-stdlib-tool
  swift-stdlib-tool.mm
  SWIFT_COMPONENTS compiler)

find_library(FOUNDATION NAMES Foundation)
target_link_libraries(swift-stdlib-tool PRIVATE ${FOUNDATION})
