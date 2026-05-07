// RUN: not %target-swift-frontend -disable-availability-checking -emit-ir %s 2>&1 | %FileCheck %s

// https://github.com/swiftlang/swift/issues/88549
// FixedTypeInfo packs the byte size into a 32-bit field, so a fixed array
// whose total size exceeds 2^32 bytes cannot be represented. Reject these
// eagerly instead of taking time proportional to the array's byte size in
// IRGen.

func tooLarge() {
  _ = InlineArray<1_000_000_000, Int>(repeating: 0)
}
// CHECK: error: size of value of type 'Builtin.FixedArray<1000000000, Int>' exceeds the maximum supported size of 4 GiB