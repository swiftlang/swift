add_swift_library(swiftImmediate
  Helpers.cpp
  Immediate.cpp
  DEPENDS swiftBasic swiftAST swiftFrontend swiftParse swiftIDE swiftSIL
          swiftSILPasses swiftIRGen)
