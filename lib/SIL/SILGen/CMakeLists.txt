add_swift_library(swiftSILGen
  Cleanup.cpp
  Condition.cpp
  SILGen.cpp
  SILGenDecl.cpp
  SILGenExpr.cpp
  SILGenLValue.cpp
  SILGenStmt.cpp
  TypeInfo.cpp
  DEPENDS swiftSIL)

