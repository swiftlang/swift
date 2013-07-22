add_swift_library(swiftSILPasses
  DataflowDiagnostics.cpp
  AllocBoxToStack.cpp
  StackToSSA.cpp
  DEPENDS swiftSIL)

