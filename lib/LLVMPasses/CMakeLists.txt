add_swift_library(swiftLLVMPasses
  Passes.cpp
  NO_LLVM_COMMON_LIBS
  )

add_dependencies(swiftLLVMPasses LLVMAnalysis)
