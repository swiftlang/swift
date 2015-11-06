add_swift_library(swiftLLVMPasses
  LLVMSwiftAA.cpp
  LLVMSwiftRCIdentity.cpp
  LLVMARCOpts.cpp
  LLVMARCContract.cpp
  LLVMStackPromotion.cpp
  )

add_dependencies(swiftLLVMPasses LLVMAnalysis)
