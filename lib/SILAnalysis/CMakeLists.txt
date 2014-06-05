add_swift_library(swiftSILAnalysis
  ARCAnalysis.cpp
  AliasAnalysis.cpp
  Analysis.cpp
  ValueTracking.cpp
  GlobalARCPairingAnalysis.cpp
  GlobalARCSequenceDataflow.cpp
  SILLoopInfo.cpp
  DEPENDS swiftSILPassesUtils)

