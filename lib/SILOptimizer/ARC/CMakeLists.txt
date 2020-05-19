target_sources(swiftSILOptimizer PRIVATE
  ARCBBState.cpp
  ARCLoopOpts.cpp
  ARCMatchingSet.cpp
  ARCRegionState.cpp
  ARCSequenceOpts.cpp
  GlobalARCSequenceDataflow.cpp
  GlobalLoopARCSequenceDataflow.cpp
  RCStateTransition.cpp
  RCStateTransitionVisitors.cpp
  RefCountState.cpp)
