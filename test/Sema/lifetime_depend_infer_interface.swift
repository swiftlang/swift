// RUN: %target-typecheck-verify-swift \
// RUN:   -I %S/Inputs \
// RUN:   -enable-experimental-feature LifetimeDependence \
// RUN:   -enable-experimental-lifetime-dependence-inference

// REQUIRES: swift_feature_LifetimeDependence

// Test that type checking continues to handle inference of lifetime
// dependencies that may be required in older (early
// 2025) .swiftinterface files. Source-level type checking is stricter.

import lifetime_depend_infer
