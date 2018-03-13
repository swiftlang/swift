// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name main %s -o %t/hash
// RUN: (export -n %env-SWIFT_DETERMINISTIC_HASHING; %target-run %t/hash && %target-run %t/hash) | %FileCheck %s
// REQUIRES: executable_test

// This check verifies that the hash seed is randomly generated on every
// execution of a Swift program. There is a minuscule chance that the same seed
// is generated on two separate executions; however, a test failure here is more
// likely to indicate an issue with the random number generator or the testing
// environment.

print("Deterministic: \(_Hasher._isDeterministic)")
print("Seed: \(_Hasher._seed)")
print("Hash values: <\(0.hashValue), \(1.hashValue)>")

// On the first run, remember the seed and hash value.
//   CHECK: Deterministic: false
//   CHECK-NEXT: Seed: [[SEED0:\([0-9]+, [0-9]+\)]]
//   CHECK-NEXT: Hash values: [[HASH0:<-?[0-9]+, -?[0-9]+>]]

// Check that the values are different on the second run.
//   CHECK-NEXT: Deterministic: false
//   CHECK-NEXT: Seed:
//   CHECK-NOT: [[SEED0]]
//   CHECK-NEXT: Hash values:
//   CHECK-NOT: [[HASH0]]
