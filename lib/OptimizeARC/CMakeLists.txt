add_swift_library(swiftOptimizeARC
  Passes.cpp
  NO_LLVM_COMMON_LIBS
  )

add_dependencies(swiftOptimizeARC LLVMAnalysis)
