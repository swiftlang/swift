add_swift_library(swiftImmediate STATIC
  Immediate.cpp
  REPL.cpp
  LINK_LIBRARIES
    swiftIDE
    swiftFrontend
    swiftSILGen
    swiftSILOptimizer
    swiftIRGen
  COMPONENT_DEPENDS
    linker mcjit)

