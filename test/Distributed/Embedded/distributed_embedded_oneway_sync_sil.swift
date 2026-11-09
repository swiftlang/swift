// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library -wmo -module-name main -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift -o %t/out.sil
// RUN: %FileCheck %s --check-prefix=THUNK --implicit-check-not='string_literal utf8 "$e4main7GreeterC5greetyySiKFTE"' < %t/out.sil
// RUN: %FileCheck %s --check-prefix=THUNK-NOHOP < %t/out.sil
// RUN: %FileCheck %s --check-prefix=CALLER < %t/out.sil
// RUN: %FileCheck %s --check-prefix=DISPATCH < %t/out.sil

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SendCalls
// REQUIRES: optimized_stdlib

// In Embedded Swift the distributed thunk of a '@oneway' func with a
// synchronous body is 'throws' but not 'async': the remote branch calls the
// synchronous 'remoteCallOneway' of the '_DistributedOnewayActorSystem' and
// the local branch enqueues the call via
// '_enqueueOnewayDistributed'. A 'send' of it from a synchronous function is
// a plain 'try_apply' of the thunk, without any task creation. Its remote calls
// use the identifier of the func's await thunk, like awaited calls, and the
// receive dispatcher matches that identifier for both

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

distributed actor Greeter {
  @oneway distributed func greet(_ n: Int) {
    _ = n
  }
}

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

// ==== -----------------------------------------------------------------------
// MARK: The distributed thunk is synchronous

// THUNK-LABEL: sil {{.*}}@$e4main7GreeterC5greet{{.*}}TE : $@convention(method) ({{.*}}) -> @error any Error {
// THUNK-DAG: string_literal utf8 "$e4main7GreeterC5greetyySiYaKFTE"
// THUNK-DAG: function_ref @{{.*}}16remoteCallOneway2on6target10invocation
// THUNK-DAG: // function_ref {{.*}}_enqueueOnewayDistributed<A>(on:_:)
// THUNK: end sil function '$e4main7GreeterC5greet{{.*}}TE'

// THUNK-NOHOP-LABEL: sil {{.*}}@$e4main7GreeterC5greet{{.*}}TE : $@convention(method)
// THUNK-NOHOP-NOT: hop_to_executor
// THUNK-NOHOP-NOT: await_async_continuation
// THUNK-NOHOP-NOT: createAsyncTask
// THUNK-NOHOP: end sil function '$e4main7GreeterC5greet{{.*}}TE'

// ==== -----------------------------------------------------------------------
// MARK: A 'send' from a synchronous function is a direct call of the thunk

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
// MARK: The receive dispatcher enqueues 'oneway' remote calls without a reply

// Whether a reply is expected is a property of the call, which the actor
// system passes in 'isOnewayRemoteCall' of the target: only a 'oneway' remote
// call is enqueued without a reply, any other call is made on the actor and
// reported to the result handler (see
// distributed_embedded_oneway_sync_dispatch_reply_sil.swift)
// DISPATCH-LABEL: sil{{.*}} @${{.+}}GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandler
// DISPATCH: string_literal utf8 "$e4main7GreeterC5greetyySiYaKFTE"
// DISPATCH: // function_ref RemoteCallTarget.isOnewayRemoteCall.getter
// DISPATCH: cond_br {{%[0-9]+}}, [[ONEWAY_BB:bb[0-9]+]], [[REPLY_BB:bb[0-9]+]]
// DISPATCH: [[ONEWAY_BB]]:
// DISPATCH-NOT: onReturnVoid
// DISPATCH: // function_ref {{.*}}_enqueueOnewayDistributed<A>(on:_:)
// DISPATCH-NOT: onReturnVoid
// DISPATCH: [[REPLY_BB]]:
// DISPATCH: // function_ref EmbeddedFakeResultHandler.onReturnVoid()
// DISPATCH: end sil function '${{.+}}GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandler
