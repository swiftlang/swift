add_swift_library(swiftSILPassesSILCombiner
  SILCombine.cpp
  SILCombinerApplyVisitors.cpp
  SILCombinerBuiltinVisitors.cpp
  SILCombinerCastVisitors.cpp
  SILCombinerMiscVisitors.cpp

  LINK_LIBRARIES
  swiftSILPassesPassManager
  swiftSILPassesUtils
  swiftSILAnalysis)
