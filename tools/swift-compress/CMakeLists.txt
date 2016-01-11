add_swift_executable(swift-compress
  swift-compress.cpp
  LINK_LIBRARIES swiftBasic swiftABI
  COMPONENT_DEPENDS support)

swift_install_in_component(compiler
    TARGETS swift-compress
    RUNTIME DESTINATION "bin")

