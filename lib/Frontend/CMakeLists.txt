add_swift_library(swiftFrontend
  Frontend.cpp
  DEPENDS swiftAST swiftParse swiftSema)

