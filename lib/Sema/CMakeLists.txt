add_swift_library(swiftSema
  NameBinding.cpp
  TypeChecking.cpp
  TypeCheckCoercion.cpp
  TypeCheckExpr.cpp
  TypeCheckDecl.cpp
  TypeCheckStmt.cpp
  TypeCheckType.cpp
  DEPENDS swiftAST swiftParse)
