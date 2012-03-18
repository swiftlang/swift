add_swift_library(swiftSema
  NameBinding.cpp
  NameLookup.cpp
  TypeChecker.cpp
  TypeChecker.h
  TypeCheckCoercion.cpp
  TypeCheckExpr.cpp
  TypeCheckDecl.cpp
  TypeCheckPattern.cpp
  TypeCheckStmt.cpp
  TypeCheckType.cpp
  DEPENDS swiftAST swiftParse)
