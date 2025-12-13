target_sources(swiftSIL PRIVATE
  DebugInfoVerifier.cpp
  FlowSensitiveVerifier.cpp
  LinearLifetimeChecker.cpp
  MemoryLifetimeVerifier.cpp
  GuaranteedPhiVerifier.cpp
  SILOwnershipVerifier.cpp
  SILVerifier.cpp)
