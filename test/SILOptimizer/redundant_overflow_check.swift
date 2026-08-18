// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// `x - min(a, x)` can't overflow, so the `-` overflow check is redundant and
// gets removed, leaving only the `precondition` trap. rdar://138518973

public struct Buf {
  let count: Int

  // CHECK-LABEL: sil {{.*}}@$s24redundant_overflow_check3BufV8clampLowyS2iF :
  // CHECK: cond_fail {{.*}}"precondition failure"
  // CHECK-NOT: "arithmetic overflow"
  // CHECK: } // end sil function '$s24redundant_overflow_check3BufV8clampLowyS2iF'
  public func clampLow(_ k: Int) -> Int {
    precondition(k >= 0)
    let dc = min(k, count)
    return count - dc
  }

  // `x - max(a, x)` can underflow, so the check stays.
  // CHECK-LABEL: sil {{.*}}@$s24redundant_overflow_check3BufV9clampHighyS2iF :
  // CHECK: "arithmetic overflow"
  // CHECK: } // end sil function '$s24redundant_overflow_check3BufV9clampHighyS2iF'
  public func clampHigh(_ k: Int) -> Int {
    precondition(k >= 0)
    let dc = max(k, count)
    return count - dc
  }
}

public struct UBuf {
  let count: UInt

  // Unsigned `x - min(a, x)` never underflows, no precondition needed.
  // CHECK-LABEL: sil {{.*}}@$s24redundant_overflow_check4UBufV8clampLowyS2uF :
  // CHECK-NOT: "arithmetic overflow"
  // CHECK: } // end sil function '$s24redundant_overflow_check4UBufV8clampLowyS2uF'
  public func clampLow(_ k: UInt) -> UInt {
    let dc = min(k, count)
    return count - dc
  }
}

// `i + c` (c > 0) as an array index can't observably overflow: the bounds check traps first.

// CHECK-LABEL: sil {{.*}}@$s24redundant_overflow_check9nextValueySiSaySiG_SitF :
// CHECK-NOT: "arithmetic overflow"
// CHECK: "Index out of range"
// CHECK: } // end sil function '$s24redundant_overflow_check9nextValueySiSaySiG_SitF'
public func nextValue(_ a: [Int], _ i: Int) -> Int {
  return a[i + 1]
}

// A negative constant can underflow into a valid index, so the check stays.
// CHECK-LABEL: sil {{.*}}@$s24redundant_overflow_check13previousValueySiSaySiG_SitF :
// CHECK: "arithmetic overflow"
// CHECK: } // end sil function '$s24redundant_overflow_check13previousValueySiSaySiG_SitF'
public func previousValue(_ a: [Int], _ i: Int) -> Int {
  return a[i - 1]
}
