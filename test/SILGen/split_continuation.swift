// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name test -swift-version 5 -target %target-swift-5.1-abi-triple -parse-stdlib -sil-verify-all | %FileCheck %s
// REQUIRES: concurrency

import Swift
import _Concurrency

// The continuation is a stack allocation, so it is created and destroyed in
// the same function.
// CHECK-LABEL: sil hidden [ossa] @$s4test16createAndDestroyyyF
// CHECK: [[MT:%.*]] = metatype $@thin Builtin.Int32.Type
// CHECK: [[CONTINUATION:%.*]] = builtin "createSplitContinuation"<Builtin.Int32>([[MT]] : $@thin Builtin.Int32.Type) : $Builtin.RawUnsafeContinuation
// CHECK: [[BINDING:%.*]] = move_value [var_decl] [[CONTINUATION]]
// CHECK: builtin "destroySplitContinuation"([[BINDING]] : $Builtin.RawUnsafeContinuation) : $()
func createAndDestroy() {
  let continuation = Builtin.createSplitContinuation(Builtin.Int32.self)
  Builtin.destroySplitContinuation(continuation)
}

// CHECK-LABEL: sil hidden [ossa] @$s4test13awaitThrowingyBi32_BcYaKF
// CHECK: [[SLOT:%.*]] = alloc_stack $Builtin.Int32
// CHECK: [[CONT:%.*]] = builtin "getSplitContinuationAddr"<Builtin.Int32>({{%.*}} : $Builtin.RawUnsafeContinuation, [[SLOT]] : $*Builtin.Int32)
// CHECK: await_async_continuation [[CONT]] : $Builtin.RawUnsafeContinuation, resume [[RESUME:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
// CHECK: [[ERROR]]([[ERR:%.*]] : @owned $any Error):
// CHECK: throw [[ERR]]
func awaitThrowing(
  _ continuation: Builtin.RawUnsafeContinuation
) async throws -> Builtin.Int32 {
  return try await Builtin.awaitSplitThrowingContinuation(continuation)
}
