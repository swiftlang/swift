add_swift_library(swiftLLVMPasses
  LLVMSwiftAA.cpp
  LLVMARCOpts.cpp
  LLVMARCContract.cpp
  )

add_dependencies(swiftLLVMPasses LLVMAnalysis)
