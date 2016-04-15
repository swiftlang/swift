
add_swift_executable(swift-format
  swift-format.cpp
  LINK_LIBRARIES swiftIDE
)

swift_install_in_component(compiler
    TARGETS swift-format
    RUNTIME DESTINATION "bin")

