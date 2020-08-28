add_swift_host_tool(swift-def-to-yaml-converter
  swift-def-to-yaml-converter.cpp
  SWIFT_COMPONENT tools
)

target_link_libraries(swift-def-to-yaml-converter PRIVATE
  swiftLocalization)
