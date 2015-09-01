add_swift_library(swiftSILPassesUtils
  CFG.cpp
  Local.cpp
  SILInliner.cpp
  SILSSAUpdater.cpp
  ConstantFolding.cpp
  GenericCloner.cpp
  Generics.cpp
  Devirtualize.cpp
  CheckedCastBrJumpThreading.cpp
  LoopUtils.cpp
  LINK_LIBRARIES swiftSIL swiftSILAnalysis)

