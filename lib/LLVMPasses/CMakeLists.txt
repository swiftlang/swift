add_swift_library(swiftLLVMPasses
  LLVMSwiftAA.cpp
  LLVMARCOpts.cpp
  NO_LLVM_COMMON_LIBS
  )

add_dependencies(swiftLLVMPasses LLVMAnalysis)
