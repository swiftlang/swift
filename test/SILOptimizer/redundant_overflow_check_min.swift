// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

// `x - min(a, x)` can't overflow, so the `-` overflow check is redundant and
// gets removed, leaving only the `precondition` trap. rdar://138518973

public struct Buf {
  let count: Int

  // CHECK-LABEL: sil {{.*}}8clampLowyS2iF
  // CHECK: cond_fail {{.*}}"precondition failure"
  // CHECK-NOT: "arithmetic overflow"
  // CHECK: return
  public func clampLow(_ k: Int) -> Int {
    precondition(k >= 0)
    let dc = min(k, count)
    return count - dc
  }

  // `x - max(a, x)` can underflow, so the check stays.
  // CHECK-LABEL: sil {{.*}}9clampHighyS2iF
  // CHECK: "arithmetic overflow"
  public func clampHigh(_ k: Int) -> Int {
    precondition(k >= 0)
    let dc = max(k, count)
    return count - dc
  }
}

public struct UBuf {
  let count: UInt

  // Unsigned `x - min(a, x)` never underflows, no precondition needed.
  // CHECK-LABEL: sil {{.*}}UBufV8clampLowyS2uF
  // CHECK-NOT: "arithmetic overflow"
  // CHECK: return
  public func clampLow(_ k: UInt) -> UInt {
    let dc = min(k, count)
    return count - dc
  }
}
