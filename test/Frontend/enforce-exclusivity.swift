// Test command-line flags for enforcement of the law of exclusivity.

// RUN: %swift -enforce-exclusivity=checked %s -emit-silgen
// RUN: %swift -enforce-exclusivity=unchecked %s -emit-silgen

// Staging flags; eventually these will not be accepted.
// RUN: %swift -enforce-exclusivity=dynamic-only %s -emit-silgen
// RUN: %swift -enforce-exclusivity=none %s -emit-silgen

// RUN: not %swift -enforce-exclusivity=other %s -emit-silgen 2>&1 | %FileCheck -check-prefix=EXCLUSIVITY_UNRECOGNIZED %s
// EXCLUSIVITY_UNRECOGNIZED: unsupported argument 'other' to option '-enforce-exclusivity='
