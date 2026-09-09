// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed

// Verify the compiler synthesizes `_executeDistributedTarget(target:invocationDecoder:resultHandler:)`.

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

distributed actor Greeter {
  distributed func hello(name: String) -> String {
    return "Hello, \(name)!"
  }
  distributed func square(_ x: Int) -> Int {
    return x * x
  }
  distributed func notify(_ message: String) {
    _ = message
  }
  distributed func unusedLog(_ message: String) {
    _ = message
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
    try! await greeter.unusedLog("never called")
#endif
  }
}

// `_executeDistributedTarget` is synthesized on the actor.
// CHECK-LABEL: sil{{.*}} @${{.+}}GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandler

// The synthesized body references the single generic decode / onReturn
// members, specialized for each distributed func's concrete types (`SS_Tg5`
// for String, `Si_Tg5` for Int) - not per-type overloads.
// CHECK-DAG: function_ref @${{.+}}EmbeddedFakeInvocationDecoderV18decodeNextArgument{{.+}}SerializationRequirement{{.+}}SS_Tg5
// CHECK-DAG: function_ref @${{.+}}EmbeddedFakeInvocationDecoderV18decodeNextArgument{{.+}}SerializationRequirement{{.+}}Si_Tg5
// CHECK-DAG: function_ref @${{.+}}EmbeddedFakeResultHandlerV8onReturn{{.+}}SerializationRequirement{{.+}}SS_Tg5
// CHECK-DAG: function_ref @${{.+}}EmbeddedFakeResultHandlerV8onReturn{{.+}}SerializationRequirement{{.+}}Si_Tg5
// CHECK-DAG: function_ref @${{.+}}EmbeddedFakeResultHandlerV12onReturnVoidyyYaKF

// And references each distributed func's distributed thunk (TE), which
// in turn handles the isRemote check and the local-vs-remote dispatch.
// CHECK-DAG: function_ref @${{.+}}GreeterC5hello4nameS2S_tYaKFTE
// CHECK-DAG: function_ref @${{.+}}GreeterC6squareyS2iYaKFTE
// CHECK-DAG: function_ref @${{.+}}GreeterC6notifyyySSYaKFTE

// `unusedLog` is never called from source (its only mention is inside an
// `#if false` block), yet its thunk must still be emitted and referenced
// dispatch is dynamic, so DCE may not eliminate it just because there is no
// direct call site
// CHECK-DAG: function_ref @${{.+}}GreeterC9unusedLogyySSYaKFTE
