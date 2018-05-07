add_swift_fuzz_tool(swift-demangle-fuzzer
  swift-demangle-fuzzer.cpp
  LINK_LIBRARIES swiftDemangling
  LLVM_COMPONENT_DEPENDS support
  SWIFT_COMPONENT compiler
  )
