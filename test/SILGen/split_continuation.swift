// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name test -swift-version 5 -target %target-swift-5.1-abi-triple -parse-stdlib -sil-verify-all -enable-experimental-feature SplitContinuations | %FileCheck %s
// REQUIRES: concurrency
// REQUIRES: swift_feature_SplitContinuations

// SILGen for the split (split) continuation builtins: creating the
// continuation, awaiting it, and destroying it.
//
// Awaiting reuses the ordinary `await_async_continuation` terminator; the token,
// the resume buffer and the handler records are carried by the
// `getSplitContinuationAddr` that produces the awaited continuation.  Resuming
// likewise uses the ordinary resumeContinuation* builtins, with no change to the
// code they emit: a split token is the task-shaped header of the continuation,
// and the runtime entry points those builtins call tell the two flavours apart
// out of line by reading a marker word.

import Swift
import _Concurrency

// CHECK-LABEL: sil hidden [ossa] @$s4test6createBcyF
// CHECK: [[MT:%.*]] = metatype $@thin Builtin.Int32.Type
// CHECK: [[TOKEN:%.*]] = builtin "createSplitContinuation"<Builtin.Int32>([[MT]] : $@thin Builtin.Int32.Type) : $Builtin.RawUnsafeContinuation
// CHECK: return [[TOKEN]]
func create() -> Builtin.RawUnsafeContinuation {
  return Builtin.createSplitContinuation(Builtin.Int32.self)
}

// The await has both a resume and an error successor, and the error successor
// takes the error as a terminator result.  The second operand of
// getSplitContinuationAddr is the resume buffer.
//
// There is only a throwing await builtin: a `Failure == Never` caller uses it too
// and simply never sees the error successor.
//
// CHECK-LABEL: sil hidden [ossa] @$s4test13awaitThrowingyBi32_BcYaKF
// CHECK: [[SLOT:%.*]] = alloc_stack $Builtin.Int32
// CHECK: [[CONT:%.*]] = builtin "getSplitContinuationAddr"<Builtin.Int32>({{%.*}} : $Builtin.RawUnsafeContinuation, [[SLOT]] : $*Builtin.Int32)
// CHECK: await_async_continuation [[CONT]] : $Builtin.RawUnsafeContinuation, resume [[RESUME:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
// CHECK: [[ERROR]]([[ERR:%.*]] : @owned $any Error):
// CHECK: throw [[ERR]]
func awaitThrowing(
  _ token: Builtin.RawUnsafeContinuation
) async throws -> Builtin.Int32 {
  return try await Builtin.awaitSplitThrowingContinuation(token)
}

// CHECK-LABEL: sil hidden [ossa] @$s4test7destroyyyBcF
// CHECK: builtin "destroySplitContinuation"({{%.*}} : $Builtin.RawUnsafeContinuation) : $()
func destroy(_ token: Builtin.RawUnsafeContinuation) {
  Builtin.destroySplitContinuation(token)
}
