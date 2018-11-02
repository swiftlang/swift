add_swift_host_library(swiftLLVMPasses STATIC
  LLVMSwiftAA.cpp
  LLVMSwiftRCIdentity.cpp
  LLVMARCOpts.cpp
  LLVMARCContract.cpp
  LLVMInlineTree.cpp
  LLVMMergeFunctions.cpp

  LINK_COMPONENTS
    analysis
  )

