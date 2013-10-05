add_subdirectory(Utils)
add_swift_library(swiftSILPasses
  AllocBoxToStack.cpp
  ConstantPropagation.cpp
  DataflowDiagnostics.cpp
  DeadCodeElimination.cpp
  Link.cpp
  MandatoryInlining.cpp
  DefiniteInitialization.cpp
  SILCleanup.cpp
  StackToSSA.cpp
  DEPENDS swiftSILPassesUtils)

