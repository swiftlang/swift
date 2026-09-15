// RUN: %target-typecheck-verify-swift -enable-experimental-feature TargetFeaturePredicate

// REQUIRES: swift_feature_TargetFeaturePredicate

#if _hasTargetFeature(avx2) // expected-error {{'_hasTargetFeature' requires a single unlabeled argument for the target feature name}}
#endif

#if _hasTargetFeature("") // expected-error {{'_hasTargetFeature' requires a single unlabeled argument for the target feature name}}
#endif

#if _hasTargetFeature() // expected-error {{'_hasTargetFeature' requires a single unlabeled argument for the target feature name}}
#endif

#if _hasTargetFeature("avx2", "avx512f") // expected-error {{'_hasTargetFeature' requires a single unlabeled argument for the target feature name}}
#endif
