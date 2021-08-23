target_sources(swiftSIL PRIVATE
  DebugInfoVerifier.cpp
  LoadBorrowImmutabilityChecker.cpp
  LinearLifetimeChecker.cpp
  MemoryLifetimeVerifier.cpp
  ReborrowVerifier.cpp
  SILOwnershipVerifier.cpp
  SILVerifier.cpp)
