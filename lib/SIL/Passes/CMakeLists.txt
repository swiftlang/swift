add_swift_library(swiftSILPasses
  DataflowDiagnostics.cpp
  DeadCodeElimination.cpp
  AllocBoxToStack.cpp
  StackToSSA.cpp
  DEPENDS swiftSIL)

