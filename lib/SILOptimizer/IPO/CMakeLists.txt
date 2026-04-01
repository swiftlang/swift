target_sources(swiftSILOptimizer PRIVATE
  CrossModuleOptimization.cpp
  DeadFunctionElimination.cpp
  GlobalPropertyOpt.cpp
  LetPropertiesOpts.cpp
  UsePrespecialized.cpp)
