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
  Passes.cpp
  PredictableMemOpt.cpp
  DefiniteInitialization.cpp
  DIMemoryUseCollector.cpp
  SILCleanup.cpp
  SILCombine.cpp
  SILMem2Reg.cpp
  Specializer.cpp
  SimplifyCFG.cpp
  PerformanceInliner.cpp
  SILLowerAggregateInstrs.cpp
  DEPENDS swiftSILPassesUtils)

