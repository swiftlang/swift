// RUN: %target-typecheck-verify-swift

// expected-error@+1:5{{'hasFeature' requires a single unlabeled argument for the feature}}
#if hasFeature(17)
#endif
