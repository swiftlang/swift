// Test command-line flags for enforcement of the law of exclusivity.

// RUN: %target-swift-frontend -enforce-exclusivity=checked %s -emit-silgen
// RUN: %target-swift-frontend -enforce-exclusivity=unchecked %s -emit-silgen

// Staging flags; eventually these will not be accepted.
// RUN: %target-swift-frontend -enforce-exclusivity=dynamic-only %s -emit-silgen
// RUN: %target-swift-frontend -enforce-exclusivity=none %s -emit-silgen

// RUN: not %target-swift-frontend -enforce-exclusivity=other %s -emit-silgen 2>&1 | %FileCheck -check-prefix=EXCLUSIVITY_UNRECOGNIZED %s
// EXCLUSIVITY_UNRECOGNIZED: unsupported argument 'other' to option '-enforce-exclusivity='
