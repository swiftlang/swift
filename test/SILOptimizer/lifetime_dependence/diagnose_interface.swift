// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -I %S/Inputs \
// RUN:   -verify \
// RUN:   -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence

// Test that lifetime dependence diagnostics continues to older (early
// 2025) .swiftinterface files. Source-level diagnostics are stricter.

import lifetime_depend_diagnose
