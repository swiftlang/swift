add_swift_library(swiftImmediate
  Immediate.cpp
  REPL.cpp
  LINK_LIBRARIES
    swiftIDE
    swiftFrontend
    swiftSILGen
    swiftSILPasses
    swiftIRGen
  COMPONENT_DEPENDS
    linker mcjit)

