target_sources(swiftSIL PRIVATE
  LoadBorrowImmutabilityChecker.cpp
  LinearLifetimeChecker.cpp
  MemoryLifetimeVerifier.cpp
  ReborrowVerifier.cpp
  SILOwnershipVerifier.cpp
  SILVerifier.cpp)
