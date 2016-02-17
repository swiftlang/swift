add_swift_executable(swift-reflection-test
  swift-reflection-test.cpp
  LINK_LIBRARIES
    swiftBasic

  COMPONENT_DEPENDS object support
)

swift_install_in_component(tools
    TARGETS swift-reflection-test
    RUNTIME DESTINATION bin)

