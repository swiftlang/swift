// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library -wmo -module-name main -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift -o %t/out.ll
// RUN: %FileCheck %s < %t/out.ll
// RUN: %FileCheck %s --check-prefix=NODEFAULT < %t/out.ll
// RUN: grep -oE '@"?\$e[^" (]*(T[QY][0-9]+_|Tg5TA)' %t/out.ll | grep -v EmbeddedFakeResultHandler | grep -v GreeterC25_executeDistributedTarget | grep -v GreeterC5greetyySiYaKFTE | grep -v SyncOnewaySystemC14remoteCallVoid | grep -v EmbeddedFakeRoundtripActorSystemC16remoteCallOneway | sort -u | %FileCheck %s --check-prefix=FUNCLETS

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SendCalls
// REQUIRES: optimized_stdlib

// In Embedded Swift, with a '_DistributedOnewayActorSystem', whose
// 'remoteCallOneway' is synchronous, a 'try send' of a '@oneway' distributed func with a
// synchronous body from synchronous code emits no async code on the whole send
// path: the thunk, the caller, the actor system's send and the local enqueue.
// The enqueue task's operation is a synchronous closure, so its partial
// application forwarder ('Tg5TA') is synchronous too, and there is no async
// suspend/resume funclet ('TQ<n>_' / 'TY<n>_') on the send path at all. The
// public async methods of the input file's 'EmbeddedFakeResultHandler' are
// not part of the send path and are filtered out, and so is the receive
// dispatcher '_executeDistributedTarget', which awaits a call which is not a
// 'oneway' remote call and replies to it, the await thunk of the func, which
// a call that is not a 'send' calls, with the 'remoteCallVoid' that it calls,
// and the 'Task.immediate' of the input file's other actor system,
// 'EmbeddedFakeRoundtripActorSystem', which this test does not use

import _Concurrency
@_spi(OnewaySend) import Distributed

typealias DefaultDistributedActorSystem = SyncOnewaySystem

// An actor system whose synchronous 'remoteCallOneway' sends without any task
final class SyncOnewaySystem: _DistributedOnewayActorSystem, @unchecked Sendable {
  typealias ActorID = EmbeddedFakeActorID
  typealias SerializationRequirement = EmbeddedSerializationRequirement
  typealias InvocationEncoder = EmbeddedFakeInvocationEncoder
  typealias InvocationDecoder = EmbeddedFakeInvocationDecoder
  typealias ResultHandler = EmbeddedFakeResultHandler

  var sent: [[UInt8]] = []
  var nextID: UInt64 = 1

  init() {}

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor, Act.ActorSystem == SyncOnewaySystem {
    nil
  }
  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor, Act.ActorSystem == SyncOnewaySystem {
    defer { nextID += 1 }
    return ActorID(id: nextID)
  }
  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ActorSystem == SyncOnewaySystem {}
  func resignID(_ id: ActorID) {}
  func makeInvocationEncoder() -> InvocationEncoder { .init(buffer: CallBuffer()) }

  func remoteCall<Act, Err, Res>(
    on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder,
    throwing: Err.Type, returning: Res.Type
  ) async throws -> Res
      where Act: DistributedActor, Act.ID == ActorID, Err: Error,
            Res: EmbeddedSerializationRequirement {
    fatalError("unexpected remoteCall")
  }
  func remoteCallVoid<Act, Err>(
    on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
      where Act: DistributedActor, Act.ID == ActorID, Err: Error {
    fatalError("unexpected remoteCallVoid")
  }
  func remoteCallOneway<Act>(
    on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder
  ) throws
      where Act: DistributedActor, Act.ActorSystem == SyncOnewaySystem {
    sent.append(invocation.buffer.argBytes)
  }
}

distributed actor Greeter {
  @oneway distributed func greet(_ n: Int) {
    _ = n
  }
}

func send(_ g: Greeter) throws {
  try send g.greet(1)
}

@main struct Main {
  static func main() {
    let system = SyncOnewaySystem()
    let greeter = Greeter(actorSystem: system)
    try? send(greeter)
  }
}

// The thunk and the caller exist
// CHECK-DAG: define {{.*}}@{{"?}}$e4main7GreeterC5greet{{[^"(]*}}TE{{"?}}(
// CHECK-DAG: define {{.*}}@{{"?}}$e4main4send{{[^"(]*}}F{{"?}}(

// There is no 'Task.immediate' on the send path
// NODEFAULT-NOT: $eScTss5NeverORs_rlE9immediate

// The enqueue task's operation and its partial application forwarder are
// synchronous ('yycfU_', 'swiftcc' rather than 'swifttailcc')
// CHECK-DAG: define {{.*}} swiftcc void @"$es23_enqueueOnewayUnchecked{{[^"]*}}yycfU_4main7GreeterC_Tg5TA"(
// NODEFAULT-NOT: _unsafeInheritExecutor_oneway

// The forwarder is the one line of the filtered output: no funclet before or
// after it
// FUNCLETS-NOT: T{{[QY]}}{{[0-9]+}}_
// FUNCLETS: {{^}}@"$es23_enqueueOnewayUnchecked{{[^"]*}}yycfU_4main7GreeterC_Tg5TA{{$}}
// FUNCLETS-NOT: T{{[QY]}}{{[0-9]+}}_
