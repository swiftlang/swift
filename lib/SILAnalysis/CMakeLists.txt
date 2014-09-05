add_swift_library(swiftSILAnalysis
  ARCAnalysis.cpp
  AliasAnalysis.cpp
  Analysis.cpp
  ColdBlockInfo.cpp
  ValueTracking.cpp
  GlobalARCPairingAnalysis.cpp
  GlobalARCSequenceDataflow.cpp
  SILLoopInfo.cpp
  ClassHierarchyAnalysis.cpp
  IVAnalysis.cpp
  CallGraphAnalysis.cpp
  SimplifyInstruction.cpp
  DEPENDS swiftSILPassesUtils)
