add_swift_library(swiftSILAnalysis
  ARCAnalysis.cpp
  AliasAnalysis.cpp
  Analysis.cpp
  ValueTracking.cpp
  GlobalARCPairingAnalysis.cpp
  GlobalARCSequenceDataflow.cpp
  DEPENDS swiftSILPassesUtils)

