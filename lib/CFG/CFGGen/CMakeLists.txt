add_swift_library(swiftCFGGen
  Cleanup.cpp
  Condition.cpp
  CFGGen.cpp
  CFGGenDecl.cpp
  CFGGenExpr.cpp
  CFGGenStmt.cpp
  DEPENDS swiftCFG)

