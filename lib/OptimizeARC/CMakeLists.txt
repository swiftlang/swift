add_swift_library(swiftOptimizeARC
  Passes.cpp
  COMPONENT_DEPENDS analysis
  NO_LLVM_COMMON_LIBS
  )

