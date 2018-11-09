add_swift_library(swiftLLVMPasses STATIC
  LLVMSwiftAA.cpp
  LLVMSwiftRCIdentity.cpp
  LLVMARCOpts.cpp
  LLVMARCContract.cpp
  LLVMInlineTree.cpp

  LLVM_COMPONENT_DEPENDS
  analysis ipo
  )

