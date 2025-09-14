// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

final class CrashActorSystem: DistributedActorSystem {
  typealias ActorID = CrashActorID
  typealias InvocationEncoder = CrashInvocationEncoder
  typealias InvocationDecoder = CrashInvocationDecoder
  typealias ResultHandler = CrashResultHandler

  typealias SerializationRequirement = Any

  func assignID<Actor: DistributedActor>(_: Actor.Type) -> ActorID where Actor.ID == ActorID {
    fatalError()
  }

  func resignID(_: ActorID) {
    // nothing
  }

  func actorReady<Actor: DistributedActor>(_: Actor) where Actor.ID == ActorID { }

  func resolve<Actor: DistributedActor>(id _: ActorID, as _: Actor.Type) throws -> Actor? where Actor.ID == ActorID {
    return nil
  }

  func makeInvocationEncoder() -> InvocationEncoder {
    return InvocationEncoder()
  }

  func remoteCallVoid<Actor: DistributedActor, Failure: Error>(
    on _: Actor,
    target _: RemoteCallTarget,
    invocation _: inout InvocationEncoder,
    throwing _: Failure.Type
  ) async throws where Actor.ID == ActorID {
    return
  }

  func remoteCall<Actor: DistributedActor, Failure: Error, Success: SerializationRequirement>(
    on _: Actor,
    target _: RemoteCallTarget,
    invocation _: inout InvocationEncoder,
    throwing _: Failure.Type,
    returning _: Success.Type
  ) async throws -> Success where Actor.ID == ActorID {
    fatalError()
  }

}

struct CrashActorID: Hashable {
  func hash(into hasher: inout Hasher) { }
}

struct CrashInvocationEncoder: DistributedTargetInvocationEncoder {
  typealias SerializationRequirement = CrashActorSystem.SerializationRequirement

  func recordGenericSubstitution<GenericSubstitution>(_: GenericSubstitution.Type) throws { }
  mutating func recordArgument<Argument: SerializationRequirement>(_: RemoteCallArgument<Argument>) throws { }
  mutating func recordReturnType<Success: SerializationRequirement>(_: Success.Type) throws { }
  func recordErrorType<Failure: Error>(_: Failure.Type) throws { }
  func doneRecording() throws { }
}

struct CrashInvocationDecoder: DistributedTargetInvocationDecoder {
  typealias SerializationRequirement = CrashActorSystem.SerializationRequirement

  func decodeGenericSubstitutions() throws -> [Any.Type] {
    return []
  }

  mutating func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument {
    fatalError()
  }

  func decodeReturnType() throws -> Any.Type? {
    return nil
  }

  func decodeErrorType() throws -> Any.Type? {
    return nil
  }

}

struct CrashResultHandler: DistributedTargetInvocationResultHandler {
  typealias SerializationRequirement = CrashActorSystem.SerializationRequirement

  func onReturn<Success: SerializationRequirement>(value _: Success) async throws { }
  func onReturnVoid() async throws { }
  func onThrow<Failure: Error>(error _: Failure) async throws { }
}