add_swift_library(swiftSILGen
  Cleanup.cpp
  Condition.cpp
  CFGGen.cpp
  CFGGenDecl.cpp
  CFGGenExpr.cpp
  CFGGenStmt.cpp
  DEPENDS swiftSIL)

