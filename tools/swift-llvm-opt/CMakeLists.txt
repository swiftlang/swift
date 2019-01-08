add_swift_host_tool(swift-llvm-opt
  LLVMOpt.cpp
  LINK_LIBRARIES
  swiftIRGen

  # Swift libraries included to appease the linker on linux.
  swiftSema
  swiftAST

  # Clang libraries included to appease the linker on linux.
  clangBasic
  clangCodeGen

  SWIFT_COMPONENT tools
)
