add_swift_library(swiftSILPassesUtils
  CFG.cpp
  Local.cpp
  SILInliner.cpp
  SILSSAUpdater.cpp
  ConstantFolding.cpp
  Generics.cpp
  LINK_LIBRARIES swiftSIL swiftSILAnalysis)

