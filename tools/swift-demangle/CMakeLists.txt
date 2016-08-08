add_swift_executable(swift-demangle
  swift-demangle.cpp
  LINK_LIBRARIES swiftBasic
  SWIFT_COMPONENT compiler
  LLVM_COMPONENT_DEPENDS support)
