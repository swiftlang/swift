add_swift_library(swiftLLVMPasses
  LLVMSwiftAA.cpp
  LLVMSwiftRCIdentity.cpp
  LLVMARCOpts.cpp
  LLVMARCContract.cpp
  LLVMInlineTree.cpp
  LLVMStackPromotion.cpp
  LLVMMergeFunctions.cpp
  )

add_dependencies(swiftLLVMPasses LLVMAnalysis)
