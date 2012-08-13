add_swift_library(swiftSema
  ArchetypeBuilder.cpp
  ArchetypeBuilder.h
  CaptureAnalysis.cpp
  NameBinding.cpp
  TypeChecker.cpp
  TypeChecker.h
  TypeCheckCoercion.cpp
  TypeCheckConstraints.cpp
  TypeCheckExpr.cpp
  TypeCheckDecl.cpp
  TypeCheckOverload.cpp
  TypeCheckPattern.cpp
  TypeCheckProtocol.cpp
  TypeCheckREPL.cpp
  TypeCheckStmt.cpp
  TypeCheckType.cpp
  DEPENDS swiftAST swiftParse)
