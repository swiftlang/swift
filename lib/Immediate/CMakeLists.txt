add_swift_host_library(swiftImmediate STATIC
  Immediate.cpp
  LLVM_LINK_COMPONENTS
    executionengine
    linker
    mcjit
    orcjit
    jitlink
    transformutils)
target_link_libraries(swiftImmediate PRIVATE
  swiftFrontend
  swiftIDE
  swiftIRGen
  swiftSILGen
  swiftSILOptimizer)
