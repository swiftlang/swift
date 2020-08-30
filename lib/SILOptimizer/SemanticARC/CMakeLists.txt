target_sources(swiftSILOptimizer PRIVATE
  SemanticARCOpts.cpp
  OwnershipLiveRange.cpp
  LoadCopyToLoadBorrowOpt.cpp
  BorrowScopeOpts.cpp)
