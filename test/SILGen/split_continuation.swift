// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name test -swift-version 5 -target %target-swift-5.1-abi-triple -parse-stdlib -sil-verify-all | %FileCheck %s
// REQUIRES: concurrency

import Swift
import _Concurrency

// withSplitContinuation allocates the result buffer with an ordinary
// alloc_stack, and the continuation is a scope inside it. The function runs on
// the caller's isolation, and the continuation is destroyed on both the normal
// and the error path.
// CHECK-LABEL: sil hidden [ossa] @$s4test21withSplitContinuationyS2iYaKF
// CHECK: [[BUF:%.*]] = alloc_stack $Builtin.Int32
// CHECK: [[CONT:%.*]] = builtin "createSplitContinuation"<Builtin.Int32>([[BUF]] : $*Builtin.Int32) : $Builtin.RawUnsafeContinuation
// CHECK: try_apply {{%.*}}({{%.*}}, [[CONT]]) : $@caller_isolated @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, Builtin.RawUnsafeContinuation) -> (Int, @error any Error), normal [[NORMAL:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
// CHECK: [[NORMAL]]({{%.*}} : $Int):
// CHECK:   builtin "destroySplitContinuation"([[CONT]] : $Builtin.RawUnsafeContinuation) : $()
// CHECK:   dealloc_stack [[BUF]]
// CHECK:   return
// CHECK: [[ERROR]]({{%.*}} : @owned $any Error):
// CHECK:   builtin "destroySplitContinuation"([[CONT]] : $Builtin.RawUnsafeContinuation) : $()
// CHECK:   dealloc_stack [[BUF]]
// CHECK:   throw
func withSplitContinuation(_ value: Int) async throws -> Int {
  return try await Builtin.withSplitContinuation(Builtin.Int32.self) {
    (continuation) async throws -> Int in
    _ = continuation
    return value
  }
}

// The handlers are optional non-escaping closures, and a handler the await
// doesn't have is nil.
// CHECK-LABEL: sil hidden [ossa] @$s4test13awaitThrowing{{.*}} : $@convention(thin) @async
// CHECK-DAG: [[SLOT:%.*]] = alloc_stack $Builtin.Int32
// CHECK-DAG: [[CANCEL:%.*]] = enum $Optional<@noescape @callee_guaranteed (UInt8) -> ()>, #Optional.some!enumelt
// CHECK-DAG: [[ESCALATE:%.*]] = enum $Optional<@noescape @callee_guaranteed (UInt8, UInt8) -> ()>, #Optional.none!enumelt
// CHECK: [[CONT:%.*]] = builtin "getSplitContinuationAddr"<Builtin.Int32>({{%.*}} : $Builtin.RawUnsafeContinuation, [[SLOT]] : $*Builtin.Int32, [[CANCEL]] : $Optional<@noescape @callee_guaranteed (UInt8) -> ()>, [[ESCALATE]] : $Optional<@noescape @callee_guaranteed (UInt8, UInt8) -> ()>)
// CHECK: await_async_continuation [[CONT]] : $Builtin.RawUnsafeContinuation, resume [[RESUME:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
// CHECK: [[ERROR]]([[ERR:%.*]] : @owned $any Error):
// CHECK: throw [[ERR]]
func awaitThrowing(
  _ continuation: Builtin.RawUnsafeContinuation,
  onCancel: (UInt8) -> Void
) async throws -> Builtin.Int32 {
  return try await Builtin.awaitSplitThrowingContinuation(
    continuation, onCancel, nil)
}
