set(LLVM_LINK_COMPONENTS
  Analysis
  IR
  Transforms)

add_swift_library(swiftOptimizeARC
  Passes.cpp)

