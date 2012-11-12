add_swift_library(swiftSILGen
  Cleanup.cpp
  Cleanup.h
  Condition.cpp
  Condition.h
  JumpDest.h
  Scope.h
  SILGen.cpp
  SILGen.h
  SILGenDecl.cpp
  SILGenExpr.cpp
  SILGenStmt.cpp
  DEPENDS swiftSIL)

