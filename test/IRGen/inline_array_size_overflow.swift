// RUN: not %target-swift-frontend -disable-availability-checking -emit-ir %s 2>&1 | %FileCheck %s

// https://github.com/swiftlang/swift/issues/88549
// FixedTypeInfo packs the byte size into a 32-bit field, so a fixed array
// whose total size exceeds 2^32 bytes cannot be represented. Reject these
// eagerly instead of taking time proportional to the array's byte size in
// IRGen.
//
// Use `Int64` rather than `Int` for the element type so the total overflows
// on 32-bit targets as well (where `Int` is 4 bytes); 1_000_000_000 × 8 =
// ~8 GiB either way.

func tooLarge() {
  _ = InlineArray<1_000_000_000, Int64>(repeating: 0)
}
// CHECK: error: size of value of type 'Builtin.FixedArray<1000000000, Int64>' exceeds the maximum supported size of 4 GiB