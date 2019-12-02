silopt_register_sources(
  BasicBlockOptUtils.cpp
  CFGOptUtils.cpp
  CanonicalizeInstruction.cpp
  CastOptimizer.cpp
  CheckedCastBrJumpThreading.cpp
  ConstantFolding.cpp
  ConstExpr.cpp
  # SWIFT_ENABLE_TENSORFLOW
  DerivativeLookup.cpp
  Differentiation.cpp
  # SWIFT_ENABLE_TENSORFLOW END
  Devirtualize.cpp
  Existential.cpp
  GenericCloner.cpp
  Generics.cpp
  InstOptUtils.cpp
  LoadStoreOptUtils.cpp
  LoopUtils.cpp
  OptimizerStatsUtils.cpp
  PerformanceInlinerUtils.cpp
  SILInliner.cpp
  SILSSAUpdater.cpp
  SpecializationMangler.cpp
  StackNesting.cpp
  ValueLifetime.cpp
)
