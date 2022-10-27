target_sources(swiftSIL PRIVATE
  DebugInfoVerifier.cpp
  LoadBorrowImmutabilityChecker.cpp
  LinearLifetimeChecker.cpp
  MemoryLifetimeVerifier.cpp
  GuaranteedPhiVerifier.cpp
  SILOwnershipVerifier.cpp
  SILVerifier.cpp)
