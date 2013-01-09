add_swift_library(swiftSILGen
  Cleanup.cpp
  Condition.cpp
  SILGen.cpp
  SILGenDecl.cpp
  SILGenExpr.cpp
  SILGenLValue.cpp
  SILGenStmt.cpp
  TypeLoweringInfo.cpp
  DEPENDS swiftSIL)

