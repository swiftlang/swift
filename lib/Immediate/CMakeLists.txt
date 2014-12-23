add_swift_library(swiftImmediate
  Helpers.cpp
  Immediate.cpp
  LINK_LIBRARIES
    swiftFrontend swiftParse swiftIDE swiftSILGen
    swiftSILPasses swiftIRGen)

