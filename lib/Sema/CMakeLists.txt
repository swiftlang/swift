add_swift_library(swiftSema
  CaptureAnalysis.cpp
  NameBinding.cpp
  NameLookup.cpp
  NameLookup.h
  TypeChecker.cpp
  TypeChecker.h
  TypeCheckCoercion.cpp
  TypeCheckExpr.cpp
  TypeCheckDecl.cpp
  TypeCheckOverload.cpp
  TypeCheckPattern.cpp
  TypeCheckStmt.cpp
  TypeCheckType.cpp
  DEPENDS swiftAST swiftParse)
