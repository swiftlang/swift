add_swift_host_tool(swift-stdlib-tool
  swift-stdlib-tool.mm)

find_library(FOUNDATION NAMES Foundation)
target_link_libraries(swift-stdlib-tool PRIVATE ${FOUNDATION})

swift_install_in_component(compiler swift-stdlib-tool
    TARGETS swift-stdlib-tool
    RUNTIME DESTINATION "bin")

