add_swift_library(swiftSema
  NameBinding.cpp
  TypeChecking.cpp
  DEPENDS swiftAST swiftParse)
