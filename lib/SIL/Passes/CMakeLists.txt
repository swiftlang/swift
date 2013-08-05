add_swift_library(swiftSILPasses
  DataflowDiagnostics.cpp
  DeadCodeElimination.cpp
  AllocBoxToStack.cpp
  MemoryPromotion.cpp
  StackToSSA.cpp
  DEPENDS swiftSIL)

