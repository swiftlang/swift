add_swift_library(swiftSILGen
  ArgumentSource.cpp
  Cleanup.cpp
  Condition.cpp
  ManagedValue.cpp
  RValue.cpp
  SILGen.cpp
  SILGenApply.cpp
  SILGenBuiltin.cpp
  SILGenDecl.cpp
  SILGenExpr.cpp
  SILGenLValue.cpp
  SILGenPattern.cpp
  SILGenPoly.cpp
  SILGenStmt.cpp
  LINK_LIBRARIES swiftSIL)

