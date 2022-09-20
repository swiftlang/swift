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

// CHECK-LABEL: sil{{.*}} [ossa] @$s4test0A19IsolatedExistentialyyScA_pYiYaF
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $any Actor)
@available(SwiftStdlib 5.1, *)
public func testIsolatedExistential(_ a: isolated Actor) async {
  // CHECK: [[ACTOR_COPY:%.*]] = copy_value [[ACTOR]] : $any Actor
  // CHECK: [[ACTOR_BORROW:%.*]] = begin_borrow [[ACTOR_COPY]] : $any Actor
  // CHECK: [[ACTOR_OPENED:%.*]] = open_existential_ref [[ACTOR_BORROW]] : $any Actor to $@opened("{{.*}}", any Actor) Self
  // CHECK: hop_to_executor [[ACTOR_OPENED]] : $@opened("{{.*}}", any Actor) Self
  // CHECK: return
}
