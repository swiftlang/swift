add_swift_host_library(swiftImmediate STATIC
  Immediate.cpp
  REPL.cpp
  LINK_LIBS
    swiftIDE
    swiftFrontend
    swiftSILGen
    swiftSILOptimizer
    swiftIRGen
  LINK_COMPONENTS
    linker mcjit)

