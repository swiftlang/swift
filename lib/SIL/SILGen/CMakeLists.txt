add_swift_library(swiftSILGen
  Cleanup.cpp
  Condition.cpp
  SILGen.cpp
  SILGenApply.cpp
  SILGenDecl.cpp
  SILGenExpr.cpp
  SILGenLValue.cpp
  SILGenStmt.cpp
  DEPENDS swiftSIL)

