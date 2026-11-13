// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple
// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple -Wwarning StrictMemorySafety
// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple -strict-memory-safety
// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple -D FILE_DEFAULT

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

// The synthesized 'invokeHandlerOnReturn' loads the result from a raw buffer,
// and acknowledges that with 'unsafe'.

#if FILE_DEFAULT
default @diagnose(StrictMemorySafety, as: error)
#endif

import Distributed

// Synthesize the handler and make sure we don't get a diagnostic.
final class TestActorSystem: DistributedActorSystem {
  typealias ActorID = LocalTestingActorID
  typealias InvocationEncoder = LocalTestingInvocationEncoder
  typealias InvocationDecoder = LocalTestingInvocationDecoder
  typealias ResultHandler = LocalTestingInvocationResultHandler
  typealias SerializationRequirement = Codable

  func resolve<Act: DistributedActor>(id: ActorID, as: Act.Type) throws -> Act?
      where Act.ID == ActorID {
    nil
  }

  func assignID<Act: DistributedActor>(_: Act.Type) -> ActorID
      where Act.ID == ActorID {
    fatalError()
  }

  func actorReady<Act: DistributedActor>(_: Act) where Act.ID == ActorID { }

  func resignID(_: ActorID) { }

  func makeInvocationEncoder() -> InvocationEncoder { fatalError() }

  func remoteCall<Act: DistributedActor, Err: Error, Res: SerializationRequirement>(
    on: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder,
    throwing: Err.Type, returning: Res.Type
  ) async throws -> Res where Act.ID == ActorID {
    fatalError()
  }

  func remoteCallVoid<Act: DistributedActor, Err: Error>(
    on: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws where Act.ID == ActorID { }
}
