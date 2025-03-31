// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-irgen -swift-version 6 -O -I %t %s

// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

// NOTE: None of the ad-hoc protocol requirement implementations

public protocol Transferable: Sendable {}

// NOT final on purpose
public class TheSpecificResultHandlerWhichIsANonFinalClass: DistributedTargetInvocationResultHandler {
  public typealias SerializationRequirement = Transferable

  public func onReturn<Success>(value: Success) async throws where Success: Transferable {
  }

  public func onReturnVoid() async throws {
    fatalError()
  }

  public func onThrow<Err>(error: Err) async throws where Err : Error {
    fatalError()
  }
}

// NOT final on purpose
public class FakeInvocationDecoder: DistributedTargetInvocationDecoder {
  public typealias SerializationRequirement = Transferable

  public func decodeGenericSubstitutions() throws -> [Any.Type] {
    []
  }

  public func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument {
    fatalError()
  }

  public func decodeErrorType() throws -> Any.Type? {
    nil
  }

  public func decodeReturnType() throws -> Any.Type? {
    nil
  }
}

// NOT final on purpose
public class FakeInvocationEncoder : DistributedTargetInvocationEncoder {
  public typealias SerializationRequirement = Transferable

  public func recordArgument<Value: SerializationRequirement>(
    _ argument: RemoteCallArgument<Value>) throws {
  }

  public func recordGenericSubstitution<T>(_ type: T.Type) throws {
  }

  public func recordErrorType<E: Error>(_ type: E.Type) throws {
  }

  public func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {
  }

  public func doneRecording() throws {
  }
}

// NOT final on purpose
public class NotFinalActorSystemForAdHocRequirementTest: DistributedActorSystem, @unchecked Sendable {
  public typealias ActorID = String
  public typealias InvocationEncoder = FakeInvocationEncoder
  public typealias InvocationDecoder = FakeInvocationDecoder
  public typealias SerializationRequirement = Transferable
  public typealias ResultHandler = TheSpecificResultHandlerWhichIsANonFinalClass

  public init() {}

  public func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    fatalError()
  }

  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    fatalError()
  }

  public func actorReady<Act>(_ actor: Act) where Act: DistributedActor, Act.ID == ActorID {
    fatalError()
  }

  public func resignID(_ id: ActorID) {
    fatalError()
  }

  public func makeInvocationEncoder() -> InvocationEncoder {
    fatalError()
  }

  public func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing errorType: Err.Type,
    returning returnType: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error,
          Res: SerializationRequirement {
    fatalError()
  }

  public func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing errorType: Err.Type
  ) async throws
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error {
    fatalError()
  }
}
