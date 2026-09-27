// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library -wmo -module-name main -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift -o %t/out.ll
// RUN: %FileCheck %s < %t/out.ll
// RUN: %FileCheck %s --check-prefix=NOFUNCLET < %t/out.ll
// RUN: %target-swift-frontend -emit-ir -O -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library -wmo -module-name main -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift -o %t/out-O.ll
// RUN: %FileCheck %s --check-prefix=OPT < %t/out-O.ll
// RUN: %FileCheck %s --check-prefix=NOFUNCLET < %t/out-O.ll

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed
// REQUIRES: swift_feature_OnewayNowait
// REQUIRES: optimized_stdlib

// In Embedded Swift neither the distributed thunk of a 'oneway' func with a
// synchronous body, nor a 'nowait' caller of it, nor the operation of the task
// that enqueues a local call has any async suspend/resume funclets
// ('TQ<n>_' / 'TY<n>_' partial functions)

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

distributed actor Greeter {
  distributed func greet(_ n: Int) oneway {
    _ = n
  }
}

// Not inlined, so that the '-O' run checks the caller itself
@inline(never)
func send(_ g: Greeter) throws {
  try nowait g.greet(1)
}

@main struct Main {
  static func main() async {
    let system = EmbeddedFakeRoundtripActorSystem()
    let greeter = Greeter(actorSystem: system)
    try? send(greeter)
  }
}

// The thunk and the caller exist, and are plain synchronous functions
// CHECK-DAG: define {{.*}}@{{"?}}$e4main7GreeterC5greet{{[^"(]*}}TE{{"?}}(
// CHECK-DAG: define {{.*}}@{{"?}}$e4main4send{{[^"(]*}}F{{"?}}(
// OPT: define {{.*}}@{{"?}}$e4main4send{{[^"(]*}}F{{"?}}(

// NOFUNCLET-NOT: GreeterC5greet{{[^" (,]*}}T{{[QY]}}{{[0-9]+}}_
// NOFUNCLET-NOT: 4main4send{{[^" (,]*}}T{{[QY]}}{{[0-9]+}}_
// The enqueue task's operation itself never suspends, and the generic
// specializer needs no async reabstraction thunk ('TG5') around it, which
// would suspend in its call to the specialization
// NOFUNCLET-NOT: onewayOperation{{[^" (,]*}}_T{{[gG]q?}}5T{{[QY]}}{{[0-9]+}}_
// NOFUNCLET-NOT: onewayOperation{{[^" (,]*}}_TG5
// Its only remaining funclet is the resume partial of the async partial
// application forwarder ('TATQ0_') that binds the captured body and value to
// it: IRGen emits every async partial application forwarder as a suspending
// call into the partially applied function
// NOFUNCLET-NOT: onewayOperation{{[^" (,]*}}_T{{[gG]q?}}5TAT{{[QY]}}{{[1-9]}}
// NOFUNCLET-NOT: onewayOperation{{[^" (,]*}}_T{{[gG]q?}}5TATY
//
// The 'remoteCallVoidOneway' default implementation and its 'Task.immediate'
// reabstraction thunks do have funclets: the fake actor system does not
// implement 'remoteCallVoidOneway'. See
// distributed_embedded_oneway_sync_system_no_funclets_ir.swift for a system
// that does
