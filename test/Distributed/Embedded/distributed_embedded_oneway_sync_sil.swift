// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library -wmo -module-name main -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift -o %t/out.sil
// RUN: %FileCheck %s --check-prefix=THUNK < %t/out.sil
// RUN: %FileCheck %s --check-prefix=THUNK-NOHOP < %t/out.sil
// RUN: %FileCheck %s --check-prefix=CALLER < %t/out.sil
// RUN: %FileCheck %s --check-prefix=DISPATCH < %t/out.sil

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed
// REQUIRES: swift_feature_OnewayNowait
// REQUIRES: optimized_stdlib

// In Embedded Swift the distributed thunk of a 'oneway' func with a
// synchronous body is 'throws' but not 'async': the remote branch calls the
// synchronous 'remoteCallVoidOneway' and the local branch enqueues the call via
// '_enqueueOnewayDistributed'. A 'nowait' of it from a synchronous function is
// a plain 'try_apply' of the thunk, without any task creation

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

distributed actor Greeter {
  distributed func greet(_ n: Int) oneway {
    _ = n
  }
}

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

// ==== -----------------------------------------------------------------------
// MARK: The distributed thunk is synchronous

// THUNK-LABEL: sil {{.*}}@$e4main7GreeterC5greet{{.*}}TE : $@convention(method) ({{.*}}) -> @error any Error {
// THUNK-DAG: function_ref @{{.*}}20remoteCallVoidOneway
// THUNK-DAG: // function_ref {{.*}}_enqueueOnewayDistributed<A>(on:_:)
// THUNK: end sil function '$e4main7GreeterC5greet{{.*}}TE'

// THUNK-NOHOP-LABEL: sil {{.*}}@$e4main7GreeterC5greet{{.*}}TE : $@convention(method)
// THUNK-NOHOP-NOT: hop_to_executor
// THUNK-NOHOP-NOT: await_async_continuation
// THUNK-NOHOP-NOT: createAsyncTask
// THUNK-NOHOP: end sil function '$e4main7GreeterC5greet{{.*}}TE'

// ==== -----------------------------------------------------------------------
// MARK: A 'nowait' from a synchronous function is a direct call of the thunk

// CALLER-LABEL: sil {{.*}}@$e4main4send{{.*}}F : $@convention(thin) (@guaranteed Greeter) -> @error any Error {
// CALLER-NOT: createAsyncTask
// CALLER-NOT: hop_to_executor
// CALLER: function_ref @$e4main7GreeterC5greet{{.*}}TE
// CALLER-NOT: createAsyncTask
// CALLER-NOT: hop_to_executor
// CALLER: try_apply
// CALLER-NOT: createAsyncTask
// CALLER-NOT: hop_to_executor
// CALLER: end sil function '$e4main4send{{.*}}F'

// ==== -----------------------------------------------------------------------
// MARK: The receive dispatcher enqueues 'oneway' calls without a reply

// DISPATCH-LABEL: sil{{.*}} @${{.+}}GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandler
// DISPATCH: // function_ref {{.*}}_enqueueOnewayDistributed<A>(on:_:)
// DISPATCH-NOT: onReturnVoid
// DISPATCH: end sil function '${{.+}}GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandler
