// RUN: %target-typecheck-verify-swift

// expected-error@+1{{unexpected platform condition argument: expected feature name}}
#if hasFeature(17)
#endif
