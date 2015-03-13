add_swift_library(swiftImmediate
  Helpers.cpp
  Immediate.cpp
  LINK_LIBRARIES
    swiftIDE
    swiftFrontend
    swiftSILGen
    swiftSILPasses
    swiftIRGen
  COMPONENT_DEPENDS
    linker mcjit)

