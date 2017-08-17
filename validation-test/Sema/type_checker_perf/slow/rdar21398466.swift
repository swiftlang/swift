// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -swift-version 4
// REQUIRES: tools-release,no_asserts

// This problem is related to renaming,
// as soon as `init(truncatingBitPattern:)` is changed
// to `init(truncatingIfNeeded:)` this example is no longer "too complex"
func getUInt8(u: UInt) -> [UInt8]
{
  let values: [UInt8]

  values = [
    // expected-error@-1 {{expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions}}
    UInt8(truncatingBitPattern: (u >> 1) & 0xf),
    UInt8(truncatingBitPattern: (u >> 2) & 0xf),
    UInt8(truncatingBitPattern: (u >> 3) & 0xf),
    UInt8(truncatingBitPattern: (u >> 4) & 0xf),
    UInt8(truncatingBitPattern: (u >> 1) & 0xf),
    UInt8(truncatingBitPattern: (u >> 2) & 0xf),
    UInt8(truncatingBitPattern: (u >> 3) & 0xf),
    UInt8(truncatingBitPattern: (u >> 4) & 0xf),
    UInt8(truncatingBitPattern: (u >> 1) & 0xf),
    UInt8(truncatingBitPattern: (u >> 2) & 0xf),
    UInt8(truncatingBitPattern: (u >> 3) & 0xf),
    UInt8(truncatingBitPattern: (u >> 4) & 0xf),
    UInt8(truncatingBitPattern: (u >> 1) & 0xf),
    UInt8(truncatingBitPattern: (u >> 2) & 0xf),
    UInt8(truncatingBitPattern: (u >> 3) & 0xf),
    UInt8(truncatingBitPattern: (u >> 4) & 0xf),
    UInt8(truncatingBitPattern: (u >> 1) & 0xf),
    UInt8(truncatingBitPattern: (u >> 2) & 0xf),
    UInt8(truncatingBitPattern: (u >> 3) & 0xf),
    UInt8(truncatingBitPattern: (u >> 4) & 0xf),
    UInt8(truncatingBitPattern: (u >> 1) & 0xf),
    UInt8(truncatingBitPattern: (u >> 2) & 0xf),
    UInt8(truncatingBitPattern: (u >> 3) & 0xf),
    UInt8(truncatingBitPattern: (u >> 4) & 0xf),
    UInt8(truncatingBitPattern: (u >> 1) & 0xf),
    UInt8(truncatingBitPattern: (u >> 2) & 0xf),
    UInt8(truncatingBitPattern: (u >> 3) & 0xf),
    UInt8(truncatingBitPattern: (u >> 4) & 0xf),
    UInt8(truncatingBitPattern: (u >> 1) & 0xf),
    UInt8(truncatingBitPattern: (u >> 2) & 0xf),
    UInt8(truncatingBitPattern: (u >> 3) & 0xf),
    UInt8(truncatingBitPattern: (u >> 4) & 0xf),
  ]

  return values
}
