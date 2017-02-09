add_swift_host_tool(sil-llvm-gen
  SILLLVMGen.cpp
  LINK_LIBRARIES
    swiftFrontend
    swiftIRGen
    swiftSILGen
    swiftSILOptimizer
    # Clang libraries included to appease the linker on linux.
    clangBasic
    clangCodeGen
  LLVM_COMPONENT_DEPENDS
    DebugInfoCodeView
  SWIFT_COMPONENT tools
)
