add_swift_executable(swift-demangle
  swift-demangle.cpp
  LINK_LIBRARIES swiftBasic
  COMPONENT_DEPENDS support ${LLVM_TARGETS_TO_BUILD})

swift_install_in_component(compiler
    TARGETS swift-demangle
    RUNTIME DESTINATION "bin")

