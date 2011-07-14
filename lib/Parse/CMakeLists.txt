add_swift_library(swiftParse
  Lexer.cpp
  Parser.cpp
  DEPENDS swiftSema swiftAST)
