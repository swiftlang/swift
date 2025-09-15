// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_feature_LifetimeDependence

struct S: ~Escapable {}

func hello(_ t: some Escapable, _ u: any Escapable) {}

protocol Whatever: Escapable {}
