add_swift_library(swiftLLVMPasses STATIC
  LLVMSwiftAA.cpp
  LLVMSwiftRCIdentity.cpp
  LLVMARCOpts.cpp
  LLVMARCContract.cpp
  LLVMInlineTree.cpp
  LLVMStackPromotion.cpp
  LLVMMergeFunctions.cpp

  COMPONENT_DEPENDS
  analysis
  )

