add_swift_host_library(swiftImmediate STATIC
  Immediate.cpp
  REPL.cpp
  LINK_LIBS
    swiftIDE
    swiftFrontend
    swiftSILGen
    swiftSILOptimizer
    swiftIRGen
  LLVM_COMPONENT_DEPENDS
    linker mcjit)

