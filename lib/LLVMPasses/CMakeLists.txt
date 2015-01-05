add_swift_library(swiftLLVMPasses
  LLVMSwiftAA.cpp
  LLVMARCOpts.cpp
  )

add_dependencies(swiftLLVMPasses LLVMAnalysis)
