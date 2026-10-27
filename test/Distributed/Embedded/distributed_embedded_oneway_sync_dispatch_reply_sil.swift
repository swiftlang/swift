// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library -wmo -module-name main -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift -o %t/out.sil
// RUN: %FileCheck %s --check-prefix=DISPATCH < %t/out.sil
// RUN: %FileCheck %s --check-prefix=NOARGS < %t/out.sil
// RUN: %FileCheck %s --check-prefix=REGULAR < %t/out.sil

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SendCalls
// REQUIRES: optimized_stdlib

// Whether the caller of a remote call waits for a reply is a property of the
// call, which the recipient's actor system passes into the receive dispatcher
// as 'RemoteCallTarget.isOnewayRemoteCall', never of the declaration of the
// target. The receive dispatcher of a synchronous '@oneway' func only
// enqueues the call without a reply when the call is a 'oneway' remote call.
// Any other call of it, e.g. a call of a client compiled against a version of
// the func which was not '@oneway', which waits for a reply, is made
// on the actor and then reported to the result handler, like the call of any
// other distributed func

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

distributed actor Greeter {
  @oneway distributed func greet(_ n: Int) {
    _ = n
  }

  @oneway distributed func ping() {}

  distributed func regular() {}
}

func sendAll(_ g: Greeter) throws {
  try send g.greet(1)
  try send g.ping()
}

@main struct Main {
  static func main() async {
    let system = EmbeddedFakeRoundtripActorSystem()
    let greeter = Greeter(actorSystem: system)
    try? sendAll(greeter)
    try? await greeter.regular()
  }
}

// ==== -----------------------------------------------------------------------
// MARK: A '@oneway' func with arguments

// DISPATCH-LABEL: sil @$e4main7GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandlery0D0010RemoteCallE0V_AA022EmbeddedFakeInvocationH0VzAA0mn6ResultJ0VtYaKF :

// The arguments are decoded once, before checking the kind of the call
// DISPATCH: decodeNextArgument
// DISPATCH: // function_ref {{.*}}_onewayDecodedArgument<A>(_:)
// DISPATCH-NOT: {{^}}bb{{[0-9]+}}
// DISPATCH: // function_ref RemoteCallTarget.isOnewayRemoteCall.getter
// DISPATCH-NEXT: [[GETTER:%[0-9]+]] = function_ref @$e11Distributed16RemoteCallTargetV08isOnewaybC0Sbvg
// DISPATCH-NEXT: [[ISONEWAY:%[0-9]+]] = apply [[GETTER]](%1)
// DISPATCH: cond_br {{%[0-9]+}}, [[ONEWAY_BB:bb[0-9]+]], [[REPLY_BB:bb[0-9]+]]

// A 'oneway' remote call is enqueued on the actor, and no result is reported
// DISPATCH: [[ONEWAY_BB]]:
// DISPATCH-NOT: onReturnVoid
// DISPATCH: // function_ref {{.*}}_enqueueOnewayDistributed<A>(on:_:)
// DISPATCH-NOT: onReturnVoid
// DISPATCH: br [[DONE_BB:bb[0-9]+]]

// Any other call is made on the actor, then reported to the result handler.
// The func itself is called, not its distributed thunk, which would only
// enqueue the call on the local actor without waiting for it
// DISPATCH: [[REPLY_BB]]:
// DISPATCH-NOT: {{^}}bb{{[0-9]+}}
// DISPATCH: function_ref @$e4main7GreeterC5greetyySiF :
// DISPATCH-NEXT: hop_to_executor %4
// DISPATCH-NEXT: apply {{%[0-9]+}}({{%[0-9]+}}, %4)
// DISPATCH-NOT: {{^}}bb{{[0-9]+}}
// DISPATCH: // function_ref EmbeddedFakeResultHandler.onReturnVoid()
// DISPATCH-NEXT: [[ONRETURN:%[0-9]+]] = function_ref @$e4main25EmbeddedFakeResultHandlerV12onReturnVoidyyYaKF
// DISPATCH-NEXT: try_apply [[ONRETURN]](%3)
// DISPATCH: end sil function '$e4main7GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandlery0D0010RemoteCallE0V_AA022EmbeddedFakeInvocationH0VzAA0mn6ResultJ0VtYaKF'

// ==== -----------------------------------------------------------------------
// MARK: A '@oneway' func without arguments

// NOARGS-LABEL: sil @$e4main7GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandlery0D0010RemoteCallE0V_AA022EmbeddedFakeInvocationH0VzAA0mn6ResultJ0VtYaKF :
// NOARGS: // function_ref RemoteCallTarget.isOnewayRemoteCall.getter
// NOARGS: // function_ref RemoteCallTarget.isOnewayRemoteCall.getter
// NOARGS: cond_br {{%[0-9]+}}, [[ONEWAY_BB:bb[0-9]+]], [[REPLY_BB:bb[0-9]+]]
// NOARGS: [[ONEWAY_BB]]:
// NOARGS-NOT: onReturnVoid
// NOARGS: // function_ref {{.*}}_enqueueOnewayDistributed<A>(on:_:)
// NOARGS-NOT: onReturnVoid
// NOARGS: [[REPLY_BB]]:
// NOARGS-NOT: {{^}}bb{{[0-9]+}}
// NOARGS: function_ref @$e4main7GreeterC4pingyyF :
// NOARGS-NEXT: hop_to_executor %4
// NOARGS-NEXT: apply {{%[0-9]+}}(%4)
// NOARGS-NOT: {{^}}bb{{[0-9]+}}
// NOARGS: // function_ref EmbeddedFakeResultHandler.onReturnVoid()
// NOARGS: end sil function '$e4main7GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandlery0D0010RemoteCallE0V_AA022EmbeddedFakeInvocationH0VzAA0mn6ResultJ0VtYaKF'

// ==== -----------------------------------------------------------------------
// MARK: A regular distributed func is unchanged

// The regular func does not check 'isOnewayRemoteCall', it is called through
// its distributed thunk and always reported to the result handler
// REGULAR-LABEL: sil @$e4main7GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandlery0D0010RemoteCallE0V_AA022EmbeddedFakeInvocationH0VzAA0mn6ResultJ0VtYaKF :
// REGULAR: function_ref @$e4main7GreeterC7regular{{.*}}TE :
// REGULAR-NOT: {{^}}bb{{[0-9]+}}
// REGULAR: try_apply {{%[0-9]+}}({{.*}}, normal [[NORMAL_BB:bb[0-9]+]], error
// REGULAR: [[NORMAL_BB]]({{%[0-9]+}} : $()):
// REGULAR-NOT: isOnewayRemoteCall
// REGULAR: // function_ref EmbeddedFakeResultHandler.onReturnVoid()
// REGULAR: end sil function '$e4main7GreeterC25_executeDistributedTarget6target17invocationDecoder13resultHandlery0D0010RemoteCallE0V_AA022EmbeddedFakeInvocationH0VzAA0mn6ResultJ0VtYaKF'
