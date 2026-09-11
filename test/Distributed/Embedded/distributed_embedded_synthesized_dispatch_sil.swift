// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift | %FileCheck %s

// Check we're not accidentally using String in Distributed synthesized code:
// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift | %FileCheck %s --check-prefix STRINGFREE

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed

// Verify the compiler synthesizes `_executeDistributedTarget(target:invocationDecoder:resultHandler:)`.

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

distributed actor Greeter {
  distributed func square(_ x: Int) -> Int {
    return x * x
  }
  distributed func notify(_ code: Int) {
    _ = code
  }
  distributed func unusedLog(_ code: Int) {
    _ = code
  }
}

@main struct Main {
  static func main() async {
    let system = EmbeddedFakeRoundtripActorSystem()
    let greeter = Greeter(actorSystem: system)
    var decoder = EmbeddedFakeInvocationDecoder(buffer: CallBuffer())
    let handler = EmbeddedFakeResultHandler(buffer: ResultBuffer())
    let target = RemoteCallTarget("not-a-real-target")

#if false
    // Never reached, so `unusedLog` has no call site anywhere in the program
    // dispatch is dynamic (via _executeDistributedTarget), so DCE must not
    // strip its thunk / decode / onReturn machinery just because it is unused
    try! await greeter.unusedLog(0)
#endif
  }
}

// `_executeDistributedTarget` is synthesized on the actor.
// CHECK-LABEL: sil{{.*}} @${{.+}}GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandler

// The target match goes through 'RemoteCallTarget.identifierByteCount' and
// 'identifierEquals(_: StaticString)', comparing raw bytes -- never through
// String or its UTF8 view, which would pull ~18KB of __text into the binary.
// CHECK-DAG: function_ref @${{.+}}RemoteCallTargetV{{[0-9]+}}identifierByteCount
// CHECK-DAG: function_ref @${{.+}}RemoteCallTargetV{{[0-9]+}}identifierEquals

// The synthesized body references the single generic decode / onReturn
// members, specialized for the distributed func's concrete type (`Si_Tg5`
// for Int) - not per-type overloads.
// CHECK-DAG: function_ref @${{.+}}EmbeddedFakeInvocationDecoderV18decodeNextArgument{{.+}}SerializationRequirement{{.+}}Si_Tg5
// CHECK-DAG: function_ref @${{.+}}EmbeddedFakeResultHandlerV8onReturn{{.+}}SerializationRequirement{{.+}}Si_Tg5
// CHECK-DAG: function_ref @${{.+}}EmbeddedFakeResultHandlerV12onReturnVoidyyYaKF

// And references each distributed func's distributed thunk (TE), which
// in turn handles the isRemote check and the local-vs-remote dispatch.
// CHECK-DAG: function_ref @${{.+}}GreeterC6square{{.+}}TE
// CHECK-DAG: function_ref @${{.+}}GreeterC6notify{{.+}}TE

// `unusedLog` is never called from source (its only mention is inside an
// `#if false` block), yet its thunk must still be emitted and referenced
// dispatch is dynamic, so DCE may not eliminate it just because there is no
// direct call site
// CHECK-DAG: function_ref @${{.+}}GreeterC9unusedLog{{.+}}TE

// ==== -----------------------------------------------------------------------
// MARK: String-free dispatch

// `Greeter` uses only `Int`, so its synthesized dispatch must reference none of
// String's UTF8-view / element-comparison machinery -- the target comparison
// goes through 'identifierEquals(_: StaticString)' on raw bytes. The CHECK-NOT
// lines are bounded to this one function by the trailing 'end sil function', so
// any String machinery elsewhere in the same SIL dump does not trip them.
// STRINGFREE-LABEL: sil{{.*}} @${{.+}}GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandler
// STRINGFREE-NOT: UTF8View
// STRINGFREE-NOT: elementsEqual
// STRINGFREE-NOT: _StringGutsSlice
// STRINGFREE: end sil function {{.*}}GreeterC25_executeDistributedTarget
