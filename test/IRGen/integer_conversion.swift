// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -O -emit-ir | %FileCheck %s
// REQUIRES: CPU=x86_64 || CPU=arm64

// https://github.com/swiftlang/swift/issues/78501
public struct PcgRandom {
  private var state: UInt64 = 0;

  // CHECK-LABEL: define{{.*}}swiftcc i32 @"$s18integer_conversion9PcgRandomV6next32s6UInt32VyF"
  public mutating func next32() -> UInt32 {
    // CHECK-NOT: sSUss17FixedWidthIntegerRzrlEyxqd__cSzRd__lufC
    // CHECK-NOT: sSZss17FixedWidthIntegerRzrlEyxqd__cSzRd__lufC
    // CHECK: ret i32
    let oldstate : UInt64 = state
    state = oldstate &* 6364136223846793005 &+ 1;
    let shifted = oldstate >> 18
    let xor = shifted ^ oldstate
    let xorshifted64 = xor >> 27
    let xorshifted = UInt32((xorshifted64 << 32) >> 32)
    let rot : UInt32 = UInt32(oldstate >> 59)
    let nrot : UInt32 = UInt32(bitPattern: -Int32(rot))
    return (xorshifted >> rot) | (xorshifted << (nrot & 31))
  }

  init() {}
}
