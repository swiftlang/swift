add_swift_library(swiftSILGen
  Cleanup.cpp
  Condition.cpp
  Mangle.cpp
  OwnershipConventions.cpp
  RValue.cpp
  SILGen.cpp
  SILGenApply.cpp
  SILGenDecl.cpp
  SILGenExpr.cpp
  SILGenLValue.cpp
  SILGenPattern.cpp
  SILGenStmt.cpp
  DEPENDS swiftSIL)

