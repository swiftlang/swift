add_swift_executable(swift-demangle
  swift-demangle.cpp
  LINK_LIBRARIES swiftBasic
  LLVM_COMPONENT_DEPENDS support)

swift_install_in_component(compiler
    TARGETS swift-demangle
    RUNTIME DESTINATION "bin")

