add_subdirectory(Utils)
add_swift_library(swiftSILPasses
  AllocBoxToStack.cpp
  CapturePromotion.cpp
  ConstantPropagation.cpp
  DataflowDiagnostics.cpp
  DeadCodeElimination.cpp
  InOutDeshadowing.cpp
  Link.cpp
  MandatoryInlining.cpp
  DefiniteInitialization.cpp
  SILCleanup.cpp
  SILCombine.cpp
  SimplifyCFG.cpp
  DEPENDS swiftSILPassesUtils)

