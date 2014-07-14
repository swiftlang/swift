add_swift_library(swiftSILAnalysis
  ARCAnalysis.cpp
  AliasAnalysis.cpp
  Analysis.cpp
  ColdBlockInfo.cpp
  ValueTracking.cpp
  GlobalARCPairingAnalysis.cpp
  GlobalARCSequenceDataflow.cpp
  SILLoopInfo.cpp
  IVAnalysis.cpp
  DEPENDS swiftSILPassesUtils)

