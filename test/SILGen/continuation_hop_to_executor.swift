// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name test -swift-version 5 -target %target-swift-5.1-abi-triple -parse-stdlib -sil-verify-all | %FileCheck %s
// REQUIRES: concurrency

import Swift
import _Concurrency

// rdar://173371163 — Verify that hop_to_executor is emitted after
// await_async_continuation in the resume (and error) blocks.
// Without this hop, nonisolated(nonsending) functions that use continuation
// builtins would return on whichever thread resumed the continuation, violating
// the CallerIsolationInheriting contract.

// All functions must hop back to their right executor once the continuation was resumed.

// === Nonisolated free function (Swift 5 default) — hops to generic executor ===

// CHECK-LABEL: // nonisolatedFree()
// CHECK-NEXT:  // Isolation: unspecified
// CHECK-LABEL: sil [ossa] @$s4test15nonisolatedFreeSiyYaF
// CHECK:   [[GLOBAL:%[0-9]+]] = enum $Optional<any Actor>, #Optional.none
// CHECK:   await_async_continuation {{%[0-9]+}} : $Builtin.RawUnsafeContinuation, resume [[RESUME:bb[0-9]+]]
// CHECK: [[RESUME]]:
// CHECK-NEXT: hop_to_executor [[GLOBAL]] : $Optional<any Actor>
public func nonisolatedFree() async -> Int {
  return await Builtin.withUnsafeContinuation { c in }
}

// === nonisolated(nonsending) free function — hops to caller's implicit actor ===

// CHECK-LABEL: // nonsendingFree()
// CHECK-NEXT:  // Isolation: caller_isolation_inheriting
// CHECK-LABEL: sil [ossa] @$s4test14nonsendingFreeSiyYaF
// CHECK: bb0([[IMPLICIT:%[0-9]+]] : @guaranteed $Builtin.ImplicitActor):
// CHECK:   await_async_continuation {{%[0-9]+}} : $Builtin.RawUnsafeContinuation, resume [[RESUME:bb[0-9]+]]
// CHECK: [[RESUME]]:
// CHECK-NEXT: hop_to_executor [[IMPLICIT]] : $Builtin.ImplicitActor
public nonisolated(nonsending) func nonsendingFree() async -> Int {
  return await Builtin.withUnsafeContinuation { c in }
}

// === @concurrent free function — hops to generic executor ===

// CHECK-LABEL: // concurrentFree()
// CHECK-NEXT:  // Isolation: nonisolated
// CHECK-LABEL: sil [ossa] @$s4test14concurrentFreeSiyYaF
// CHECK:   [[GLOBAL:%[0-9]+]] = enum $Optional<any Actor>, #Optional.none
// CHECK:   await_async_continuation {{%[0-9]+}} : $Builtin.RawUnsafeContinuation, resume [[RESUME:bb[0-9]+]]
// CHECK: [[RESUME]]:
// CHECK-NEXT: hop_to_executor [[GLOBAL]] : $Optional<any Actor>
@concurrent
public func concurrentFree() async -> Int {
  return await Builtin.withUnsafeContinuation { c in }
}

// === @MainActor class method — hops to MainActor ===

@MainActor
public class MainActorClass {
  // CHECK-LABEL: // MainActorClass.mainActorMethod()
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  // CHECK-LABEL: sil [ossa] @$s4test14MainActorClassC04mainC6MethodSiyYaF
  // CHECK:   [[MA:%[0-9]+]] = begin_borrow {{%[0-9]+}} : $MainActor
  // CHECK:   await_async_continuation {{%[0-9]+}} : $Builtin.RawUnsafeContinuation, resume [[RESUME:bb[0-9]+]]
  // CHECK: [[RESUME]]:
  // CHECK-NEXT: hop_to_executor [[MA]] : $MainActor
  public func mainActorMethod() async -> Int {
    return await Builtin.withUnsafeContinuation { c in }
  }
}

// === actor method — hops to actor executor ===

public actor MyActor {
  // CHECK-LABEL: // MyActor.actorMethod()
  // CHECK-NEXT:  // Isolation: actor_instance. name: 'self'
  // CHECK-LABEL: sil [ossa] @$s4test7MyActorC11actorMethodSiyYaF
  // CHECK: bb0([[SELF:%[0-9]+]] : @guaranteed $MyActor):
  // CHECK:   await_async_continuation {{%[0-9]+}} : $Builtin.RawUnsafeContinuation, resume [[RESUME:bb[0-9]+]]
  // CHECK: [[RESUME]]:
  // CHECK-NEXT: hop_to_executor [[SELF]] : $MyActor
  public func actorMethod() async -> Int {
    return await Builtin.withUnsafeContinuation { c in }
  }
}

// === Throwing: nonisolated(nonsending) — hops in both resume and error blocks ===

// CHECK-LABEL: // nonsendingThrowing()
// CHECK-NEXT:  // Isolation: caller_isolation_inheriting
// CHECK-LABEL: sil [ossa] @$s4test18nonsendingThrowingSiyYaKF
// CHECK: bb0([[IMPLICIT:%[0-9]+]] : @guaranteed $Builtin.ImplicitActor):
// CHECK:   await_async_continuation {{%[0-9]+}} : $Builtin.RawUnsafeContinuation, resume [[RESUME:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
// CHECK: [[RESUME]]:
// CHECK-NEXT: hop_to_executor [[IMPLICIT]] : $Builtin.ImplicitActor
// CHECK: [[ERROR]]({{%[0-9]+}} : @owned $any Error):
// CHECK-NEXT: hop_to_executor [[IMPLICIT]] : $Builtin.ImplicitActor
public nonisolated(nonsending) func nonsendingThrowing() async throws -> Int {
  return try await Builtin.withUnsafeThrowingContinuation { c in }
}
