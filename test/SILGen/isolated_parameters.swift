// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name test -swift-version 5 | %FileCheck %s
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
// CHECK-LABEL: sil private [ossa] @$s4test0A24ClosureWithIsolatedParamyyFyAA1ACYiYaXEfU_ : $@convention(thin) @async (@sil_isolated @guaranteed A)
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
  // CHECK: hop_to_executor [[ACTOR_BORROW]] : $any Actor
  // CHECK: return
}

@available(SwiftStdlib 5.1, *)
nonisolated func suspend() async {}

// CHECK-LABEL: sil{{.*}} [ossa] @$s4test0A16OptionalIsolatedyyAA1ACSgYiYaF
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<A>)
@available(SwiftStdlib 5.1, *)
public func testOptionalIsolated(_ a: isolated A?) async {
  await suspend()
  // CHECK: [[ACTOR_COPY:%.*]] = copy_value [[ACTOR]] : $Optional<A>
  // CHECK: [[ACTOR_BORROW:%.*]] = begin_borrow [[ACTOR_COPY]] : $Optional<A>
  // CHECK: hop_to_executor [[ACTOR_BORROW]] : $Optional<A>
  // CHECK: [[SUSPEND:%.*]] = function_ref @$s4test7suspendyyYaF : $@convention(thin) @async () -> ()
  // CHECK: apply [[SUSPEND]]() : $@convention(thin) @async () -> ()
  // CHECK: hop_to_executor [[ACTOR_BORROW]] : $Optional<A>
  // CHECK: return
}

// CHECK-LABEL: sil{{.*}} [ossa] @$s4test0A27OptionalIsolatedExistentialyyScA_pSgYiYaF
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>)
@available(SwiftStdlib 5.1, *)
public func testOptionalIsolatedExistential(_ a: isolated (any Actor)?) async {
  await suspend()
  // CHECK: [[ACTOR_COPY:%.*]] = copy_value [[ACTOR]] : $Optional<any Actor>
  // CHECK: [[ACTOR_BORROW:%.*]] = begin_borrow [[ACTOR_COPY]] : $Optional<any Actor>
  // CHECK: hop_to_executor [[ACTOR_BORROW]] : $Optional<any Actor>
  // CHECK: [[SUSPEND:%.*]] = function_ref @$s4test7suspendyyYaF : $@convention(thin) @async () -> ()
  // CHECK: apply [[SUSPEND]]() : $@convention(thin) @async () -> ()
  // CHECK: hop_to_executor [[ACTOR_BORROW]] : $Optional<any Actor>
  // CHECK: return
}
