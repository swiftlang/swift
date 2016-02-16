add_swift_executable(swift-llvm-opt
  LLVMOpt.cpp
  LINK_LIBRARIES
  swiftIRGen

  # Swift libraries included to appease the linker on linux.
  swiftSema
  swiftAST

  # Clang libraries included to appease the linker on linux.
  clangBasic
  clangCodeGen

  LLVMDebugInfoCodeView
)

swift_install_in_component(tools
    TARGETS swift-llvm-opt
    RUNTIME DESTINATION bin)

