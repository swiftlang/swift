add_swift_library(swiftSILAnalysis
  ARCAnalysis.cpp
  AliasAnalysis.cpp
  Analysis.cpp
  DEPENDS swiftSILPassesUtils)

