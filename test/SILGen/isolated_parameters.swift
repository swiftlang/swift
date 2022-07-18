// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5 | %FileCheck %s
// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
public actor A {
  // CHECK: sil{{.*}} [ossa] @$s4test1AC6methodyyF
  public func method() { }
}

// CHECK: sil{{.*}} [ossa] @$s4test13takesIsolatedyyAA1ACYiF
@available(SwiftStdlib 5.1, *)
public func takesIsolated(_: isolated A) { }

@available(SwiftStdlib 5.1, *)
public func takeClosureWithIsolatedParam(body: (isolated A) async -> Void) { }

// Emit the unnamed parameter when it's isolated, so that we can hop to it.
// CHECK-LABEL: sil private [ossa] @$s4test0A24ClosureWithIsolatedParamyyFyAA1ACYiYaXEfU_ : $@convention(thin) @async (@guaranteed A)
// CHECK: bb0(%0 : @guaranteed $A):
// CHECK: [[COPY:%.*]] = copy_value %0 : $A
// CHECK-NEXT: [[BORROW:%.*]] = begin_borrow [[COPY]] : $A
// CHECK-NEXT:  hop_to_executor [[BORROW]] : $A
@available(SwiftStdlib 5.1, *)
public func testClosureWithIsolatedParam() {
  takeClosureWithIsolatedParam { _ in }
}
