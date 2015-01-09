add_swift_executable(swift-update
  swift-update.cpp
  LINK_LIBRARIES
    swiftDriver
    swiftFrontend
  COMPONENT_DEPENDS
    bitreader option support)

swift_install_in_component(tools
    TARGETS swift-update
    RUNTIME DESTINATION bin)
