add_swift_host_tool(swift-def-to-strings-converter
  swift-def-to-strings-converter.cpp
  SWIFT_COMPONENT tools
  DOES_NOT_USE_SWIFT
)

target_link_libraries(swift-def-to-strings-converter PRIVATE
  swiftLocalization)
