add_swift_library(swiftSema
  ArchetypeBuilder.cpp
  CaptureAnalysis.cpp
  NameBinding.cpp
  TypeChecker.cpp
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
