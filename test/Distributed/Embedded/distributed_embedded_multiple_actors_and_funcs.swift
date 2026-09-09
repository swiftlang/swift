// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift | %FileCheck %s --check-prefix IR
// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift | %FileCheck %s --check-prefix SIL

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

// Several actors sharing one system.
distributed actor Greeter {
  distributed func hello(name: String) -> String { "Hello, \(name)!" }
}

distributed actor Farewell {
  distributed func goodbye(name: String) -> String { "Bye, \(name)!" }
}

// One actor with multiple funcs of varying signatures: String/Int params, a
// multi-arg func, and a Void return (which routes through `remoteCallVoid`).
distributed actor MultiFuncActor {
  distributed func hello(name: String) -> String { "Hello, \(name)!" }
  distributed func square(_ x: Int) -> Int { x * x }
  distributed func repeated(_ s: String, count n: Int) -> Int { s.count * n }
  distributed func notify(_ message: String) { _ = message }
}

@main struct Main {
  static func main() async {
    let system = EmbeddedFakeRoundtripActorSystem()
    let g = Greeter(actorSystem: system)
    let f = Farewell(actorSystem: system)
    let m = MultiFuncActor(actorSystem: system)
    _ = try? await g.hello(name: "x")
    _ = try? await f.goodbye(name: "y")
    _ = try? await m.hello(name: "z")
    _ = try? await m.square(3)
    _ = try? await m.repeated("a", count: 2)
    try? await m.notify("n")
  }
}

// Each actor gets its own thunk in IR, plus the `remoteCall<Actor, Never, String>`
// specialization (`s5NeverO` is the `Err` for a non-throwing target, `SS` the String `Res`).
// IR-DAG: @"$e{{.+}}GreeterC5hello4nameS2S_tYaKFTE"
// IR-DAG: @"$e{{.+}}FarewellC7goodbye4nameS2S_tYaKFTE"
// IR-DAG: $e{{.+}}EmbeddedFakeRoundtripActorSystemC10remoteCall{{.+}}GreeterC_s5NeverOSS{{.+}}g5
// IR-DAG: $e{{.+}}EmbeddedFakeRoundtripActorSystemC10remoteCall{{.+}}FarewellC_s5NeverOSS{{.+}}g5

// Remote-proxy allocation goes through the embedded-only entry point (with a
// compiler-computed allocSize / alignMask), never the non-embedded runtime one,
// and no accessible-function-table section is emitted in Embedded Swift.
// IR-NOT: call swiftcc {{.*}}@swift_distributedActor_remote_initialize(
// IR: call swiftcc ptr @swift_distributedActor_remote_initialize_embedded(ptr {{.*}}, i64 {{[0-9]+}}, i64 {{[0-9]+}})
// IR-NOT: __swift5_acfuncs

// Each distributed func has its own TE thunk.
// SIL-DAG: sil{{( hidden)?}} [thunk] [distributed]{{.*}}MultiFuncActorC5hello4nameS2S_tYaKFTE
// SIL-DAG: sil{{( hidden)?}} [thunk] [distributed]{{.*}}MultiFuncActorC6squareyS2iYaKFTE
// SIL-DAG: sil{{( hidden)?}} [thunk] [distributed]{{.*}}MultiFuncActorC8repeated_5countSiSS_SitYaKFTE
// SIL-DAG: sil{{( hidden)?}} [thunk] [distributed]{{.*}}MultiFuncActorC6notifyyySSYaKFTE

// Both String and Int decode specializations of the single generic
// `decodeNextArgument` are emitted on EmbeddedFakeInvocationDecoder.
// SIL-DAG: sil{{.*}}@${{.+}}EmbeddedFakeInvocationDecoderV18decodeNextArgument{{.+}}SerializationRequirement{{.+}}SS_Tg5
// SIL-DAG: sil{{.*}}@${{.+}}EmbeddedFakeInvocationDecoderV18decodeNextArgument{{.+}}SerializationRequirement{{.+}}Si_Tg5
