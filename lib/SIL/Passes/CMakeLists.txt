add_swift_library(swiftSILPasses
  DataflowDiagnostics.cpp
  AllocBoxPromotion.cpp
  StackToSSA.cpp
  DEPENDS swiftSIL)

