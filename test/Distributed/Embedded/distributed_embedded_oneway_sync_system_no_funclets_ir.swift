// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library -wmo -module-name main -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift -o %t/out.ll
// RUN: %FileCheck %s < %t/out.ll
// RUN: %FileCheck %s --check-prefix=NODEFAULT < %t/out.ll
// RUN: grep -oE '@"?\$e[^" (]*T[QY][0-9]+_' %t/out.ll | grep -v EmbeddedFakeResultHandler | sort -u | %FileCheck %s --check-prefix=FUNCLETS

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed
// REQUIRES: swift_feature_OnewayNowait
// REQUIRES: optimized_stdlib

// In Embedded Swift, with an actor system that implements the synchronous
// 'remoteCallVoidOneway', a 'try nowait' of a 'oneway' distributed func with a
// synchronous body from synchronous code emits no async code on the whole send
// path: the thunk, the caller, the actor system's send and the local enqueue.
// The only async suspend/resume funclet ('TQ<n>_' / 'TY<n>_') in the module
// is the one of the async partial application forwarder that binds the
// enqueue task's operation to its captures, since IRGen emits every such
// forwarder as a suspending call. The public async methods of the input
// file's 'EmbeddedFakeResultHandler' are not part of the send path and are
// filtered out

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = SyncOnewaySystem

// An actor system which implements the synchronous 'remoteCallVoidOneway', so
// no 'Task.immediate' fallback is used
final class SyncOnewaySystem: DistributedActorSystem, @unchecked Sendable {
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
  func remoteCallVoidOneway<Act, Err>(
    on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) throws
      where Act: DistributedActor, Act.ActorSystem == SyncOnewaySystem, Err: Error {
    sent.append(invocation.buffer.argBytes)
  }
}

distributed actor Greeter {
  distributed func greet(_ n: Int) oneway {
    _ = n
  }
}

func send(_ g: Greeter) throws {
  try nowait g.greet(1)
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

// The 'Task.immediate' default implementation of 'remoteCallVoidOneway' is
// not used
// NODEFAULT-NOT: $e11Distributed0A11ActorSystemPAAE20remoteCallVoidOneway
// NODEFAULT-NOT: $eScTss5NeverORs_rlE9immediate

// FUNCLETS: {{^}}@"$es23_enqueueOnewayUnchecked{{[^"]*}}onewayOperation{{[^"]*}}4main7GreeterC_Tg5TATQ0_
// FUNCLETS-NOT: T{{[QY]}}{{[0-9]+}}_
