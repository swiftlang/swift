// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library -wmo -module-name main -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift -o %t/out.ll
// RUN: %FileCheck %s < %t/out.ll
// RUN: %FileCheck %s --check-prefix=NOFUNCLET < %t/out.ll
// RUN: %target-swift-frontend -emit-ir -O -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library -wmo -module-name main -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift -o %t/out-O.ll
// RUN: %FileCheck %s --check-prefix=OPT < %t/out-O.ll
// RUN: %FileCheck %s --check-prefix=NOFUNCLET < %t/out-O.ll

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SendCalls
// REQUIRES: optimized_stdlib

// In Embedded Swift neither the synchronous distributed thunk of a
// '@oneway' func with a synchronous body, nor a 'send' caller of it,
// nor the operation of the task that enqueues a local call has any async
// suspend/resume funclets ('TQ<n>_' / 'TY<n>_' partial functions). The await
// thunk of the func, which a call that is not a 'send' calls, does

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

distributed actor Greeter {
  @oneway distributed func greet(_ n: Int) {
    _ = n
  }
}

// Not inlined, so that the '-O' run checks the caller itself
@inline(never)
func send(_ g: Greeter) throws {
  try send g.greet(1)
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

// NOFUNCLET-NOT: GreeterC5greetyySi{{[^" (,]*}}T{{[QY]}}{{[0-9]+}}_
// NOFUNCLET-NOT: 4main4send{{[^" (,]*}}T{{[QY]}}{{[0-9]+}}_
// The enqueue task's operation is a synchronous closure
// ('createSynchronousDiscardingTask'), so nothing in the '_enqueueOneway'
// family is async: no async operation, reabstraction thunk or partial
// application forwarder, and so no suspend/resume funclet. The async fallback
// operation '_unsafeInheritExecutor_onewayOperation', for runtimes which
// cannot run a synchronous task operation, is not emitted at all
// NOFUNCLET-NOT: _unsafeInheritExecutor_oneway
// NOFUNCLET-NOT: _enqueueOneway{{[^" (,]*}}Ya
// NOFUNCLET-NOT: _enqueueOneway{{[^" (,]*}}T{{[QY]}}{{[0-9]+}}_
//
// The 'remoteCallOneway' of the fake actor system does have funclets, it
// runs the call on the receiving side from a 'Task.immediate'. See
// distributed_embedded_oneway_sync_system_no_funclets_ir.swift for a system
// whose send path has none
