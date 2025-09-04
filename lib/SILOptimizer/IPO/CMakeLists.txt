target_sources(swiftSILOptimizer PRIVATE
  ClosureSpecializer.cpp
  CrossModuleOptimization.cpp
  DeadFunctionElimination.cpp
  GlobalPropertyOpt.cpp
  LetPropertiesOpts.cpp
  UsePrespecialized.cpp)
