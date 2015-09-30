add_swift_library(swiftSILPassesPassManager
  Passes.cpp
  PassManager.cpp
  PrettyStackTrace.cpp

  LINK_LIBRARIES
  swiftSIL
  swiftSILAnalysis)
