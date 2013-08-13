add_swift_library(swiftSILPasses
  AllocBoxToStack.cpp
  ConstantPropagation.cpp
  DataflowDiagnostics.cpp
  DeadCodeElimination.cpp
  MemoryPromotion.cpp
  StackToSSA.cpp
  DEPENDS swiftSIL)

