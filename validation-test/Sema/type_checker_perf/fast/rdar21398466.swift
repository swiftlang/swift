// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: OS=macosx,no_asan
// REQUIRES: asserts,no_asan
// REQUIRES: rdar48061151,no_asan

// This problem is related to renaming,
// as soon as `init(truncatingBitPattern:)` is changed
// to `init(truncatingIfNeeded:)` this example is no longer "too complex"
func getUInt8(u: UInt) -> [UInt8]
{
  let values: [UInt8]

  values = [
    UInt8(truncatingBitPattern: (u >> 1) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 2) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 3) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 4) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 1) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 2) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 3) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 4) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 1) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 2) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 3) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 4) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 1) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 2) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 3) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 4) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 1) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 2) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 3) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 4) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 1) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 2) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 3) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 4) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 1) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 2) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 3) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 4) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 1) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 2) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 3) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
    UInt8(truncatingBitPattern: (u >> 4) & 0xf), // expected-error {{'init(truncatingBitPattern:)' has been renamed to 'init(truncatingIfNeeded:)'}}
  ]

  return values
}
