add_subdirectory(Utils)
add_swift_library(swiftSILPasses
  AllocBoxToStack.cpp
  CapturePromotion.cpp
  ConstantPropagation.cpp
  CSE.cpp
  DataflowDiagnostics.cpp
  DeadCodeElimination.cpp
  InOutDeshadowing.cpp
  Link.cpp
  MandatoryInlining.cpp
  PredictableMemOpt.cpp
  DefiniteInitialization.cpp
  DIMemoryUseCollector.cpp
  SILCleanup.cpp
  SILCombine.cpp
  SimplifyCFG.cpp
  DEPENDS swiftSILPassesUtils)

