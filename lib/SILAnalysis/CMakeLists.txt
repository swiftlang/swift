add_swift_library(swiftSILAnalysis
  ARCAnalysis.cpp
  AliasAnalysis.cpp
  Analysis.cpp
  ColdBlockInfo.cpp
  ValueTracking.cpp
  GlobalARCPairingAnalysis.cpp
  GlobalARCSequenceDataflow.cpp
  LoopAnalysis.cpp
  ClassHierarchyAnalysis.cpp
  IVAnalysis.cpp
  CallGraphAnalysis.cpp
  SimplifyInstruction.cpp
  RCIdentityAnalysis.cpp
  DestructorAnalysis.cpp
  CFG.cpp
  ArraySemantic.cpp
  LINK_LIBRARIES swiftSILPassesUtils)
