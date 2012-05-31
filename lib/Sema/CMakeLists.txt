add_swift_library(swiftSema
  CaptureAnalysis.cpp
  NameBinding.cpp
 TypeChecker.cpp
  TypeChecker.h
  TypeCheckCoercion.cpp
  TypeCheckExpr.cpp
  TypeCheckDecl.cpp
  TypeCheckOverload.cpp
  TypeCheckPattern.cpp
  TypeCheckProtocol.cpp
  TypeCheckREPL.cpp
  TypeCheckStmt.cpp
  TypeCheckType.cpp
  DEPENDS swiftAST swiftParse)
