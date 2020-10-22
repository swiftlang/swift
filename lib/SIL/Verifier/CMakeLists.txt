target_sources(swiftSIL PRIVATE
  LoadBorrowImmutabilityChecker.cpp
  LinearLifetimeChecker.cpp
  MemoryLifetime.cpp
  ReborrowVerifier.cpp
  SILOwnershipVerifier.cpp
  SILVerifier.cpp)
