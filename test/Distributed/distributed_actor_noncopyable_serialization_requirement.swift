// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -D WORKAROUND %s
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -D EXPLICIT_COPYABLE %s
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -D NONE %s

// REQUIRES: concurrency
// REQUIRES: distributed

// https://github.com/swiftlang/swift/issues/93021
// A SerializationRequirement that is a ~Copyable protocol (e.g. NewCodable's JSONCodable)
// must still let the encoder, decoder, result handler and actor system conform

import Distributed

protocol Serializable: ~Copyable {}

#if WORKAROUND
protocol Transferable: Serializable {}
#elseif EXPLICIT_COPYABLE
typealias Transferable = Serializable & Copyable
#else
typealias Transferable = Serializable
#endif

struct Name: Transferable {
  var value: String
}

distributed actor Greeter {
  typealias ActorSystem = TransferableActorSystem

  distributed func greet(_ name: Name) -> Name {
    name
  }

  distributed func ping(_ name: Name) {}
}

final class TransferableActorSystem: DistributedActorSystem {
  typealias ActorID = Int
  typealias InvocationEncoder = TransferableInvocationEncoder
  typealias InvocationDecoder = TransferableInvocationDecoder
  typealias ResultHandler = TransferableResultHandler
  typealias SerializationRequirement = Transferable

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor, Act.ID == ActorID {
    fatalError()
  }

  func resignID(_ id: ActorID) {}

  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ID == ActorID {}

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor, Act.ID == ActorID {
    nil
  }

  func makeInvocationEncoder() -> InvocationEncoder {
    InvocationEncoder()
  }

  func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
      where Act: DistributedActor, Act.ID == ActorID,
            Err: Error, Res: Transferable {
    fatalError()
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
      where Act: DistributedActor, Act.ID == ActorID, Err: Error {
    fatalError()
  }
}

struct TransferableInvocationEncoder: DistributedTargetInvocationEncoder {
  typealias SerializationRequirement = Transferable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Value: Transferable>(
    _ argument: RemoteCallArgument<Value>
  ) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func recordReturnType<R: Transferable>(_ type: R.Type) throws {}
  mutating func doneRecording() throws {}
}

struct TransferableInvocationDecoder: DistributedTargetInvocationDecoder {
  typealias SerializationRequirement = Transferable

  mutating func decodeGenericSubstitutions() throws -> [Any.Type] {
    []
  }
  mutating func decodeNextArgument<Argument: Transferable>() throws -> Argument {
    fatalError()
  }
  mutating func decodeErrorType() throws -> Any.Type? {
    nil
  }
  mutating func decodeReturnType() throws -> Any.Type? {
    nil
  }
}

struct TransferableResultHandler: DistributedTargetInvocationResultHandler {
  typealias SerializationRequirement = Transferable

  func onReturn<Success: Transferable>(value: Success) async throws {}
  func onReturnVoid() async throws {}
  func onThrow<Err: Error>(error: Err) async throws {}
}
