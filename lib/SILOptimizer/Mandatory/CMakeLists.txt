target_sources(swiftSILOptimizer PRIVATE
  AccessEnforcementSelection.cpp
  AccessMarkerElimination.cpp
  AddressLowering.cpp
  ClosureLifetimeFixup.cpp
  ConstantPropagation.cpp
  DefiniteInitialization.cpp
  # SWIFT_ENABLE_TENSORFLOW
  Differentiation.cpp
  # SWIFT_ENABLE_TENSORFLOW END
  DIMemoryUseCollector.cpp
  DataflowDiagnostics.cpp
  DiagnoseInfiniteRecursion.cpp
  DiagnoseInvalidEscapingCaptures.cpp
  DiagnoseStaticExclusivity.cpp
  DiagnoseUnreachable.cpp
  Differentiation.cpp
  GuaranteedARCOpts.cpp
  IRGenPrepare.cpp
  MandatoryInlining.cpp
  PredictableMemOpt.cpp
  PMOMemoryUseCollector.cpp
  RawSILInstLowering.cpp
  SILGenCleanup.cpp
  YieldOnceCheck.cpp
  MandatoryCombine.cpp
  OSLogOptimization.cpp
  OwnershipModelEliminator.cpp)
