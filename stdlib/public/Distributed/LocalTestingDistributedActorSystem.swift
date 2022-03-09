//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// TODO(distributed): not thread safe...
public final class LocalTestingDistributedActorSystem: DistributedActorSystem, @unchecked Sendable {
  public typealias ActorID = ActorAddress
  public typealias InvocationEncoder = LocalInvocationEncoder
  public typealias InvocationDecoder = LocalInvocationDecoder
  public typealias SerializationRequirement = Codable

  private var activeActors: [ActorID: any DistributedActor] = [:]

  private var idProvider: ActorIDProvider = ActorIDProvider()
  private var assignedIDs: Set<ActorID> = []

  public init() {}

  public func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    guard let anyActor = self.activeActors[id] else {
      throw LocalTestingDistributedActorSystemError(message: "Unable to locate id '\(id)' locally")
    }
    guard let actor = anyActor as? Act else {
      throw LocalTestingDistributedActorSystemError(message: "Failed to resolve id '\(id)' as \(Act.Type)")
    }
    return actor
  }

  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    let id = ActorAddress(parse: "\(self.idProvider.next())")
    print("| assign id: \(id) for \(actorType)")
    self.assignedIDs.insert(id)
    return id
  }

  public func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {
    guard self.assignedIDs.contains(actor.id) else {
      fatalError("Attempted to mark an unknown actor '\(actor.id)' ready")
    }
    print("| actor ready: \(actor)")
    self.activeActors[actor.id] = actor
  }

  public func resignID(_ id: ActorID) {
    guard self.assignedIDs.contains(actor.id) else {
      fatalError("Attempted to resign unknown id '\(id)'")
    }
    self.activeActors.removeValue(forKey: id)
  }

  public func makeInvocationEncoder() -> InvocationEncoder {
    .init()
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
    fatalError("Attempted to make remote call on actor \(actor) in a local-only actor system")
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
    fatalError("Attempted to make remote call on actor \(actor) in a local-only actor system")
  }

  // TODO(distributed): not thread safe...
  private struct ActorIDProvider {
    private var counter: Int = 0

    init() {}

    mutating func next() -> ActorAddress {
      self.counter += 1
      return ActorAddress(parse: "\(self.counter)")
    }
  }
}

public struct LocalInvocationEncoder: DistributedTargetInvocationEncoder {
  public typealias SerializationRequirement = Codable

  public mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {
    fatalError("Attempted to call encoder method in a local-only actor system")
  }

  public mutating func recordArgument<Argument: SerializationRequirement>(_ argument: Argument) throws {
    fatalError("Attempted to call encoder method in a local-only actor system")
  }

  public mutating func recordErrorType<E: Error>(_ type: E.Type) throws {
    fatalError("Attempted to call encoder method in a local-only actor system")
  }

  public mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {
    fatalError("Attempted to call encoder method in a local-only actor system")
  }

  public mutating func doneRecording() throws {
    fatalError("Attempted to call encoder method in a local-only actor system")
  }
}

public class LocalInvocationDecoder : DistributedTargetInvocationDecoder {
  public typealias SerializationRequirement = Codable

  public func decodeGenericSubstitutions() throws -> [Any.Type] {
    fatalError("Attempted to call decoder method in a local-only actor system")
  }

  public func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument {
    fatalError("Attempted to call decoder method in a local-only actor system")
  }

  public func decodeErrorType() throws -> Any.Type? {
    fatalError("Attempted to call decoder method in a local-only actor system")
  }

  public func decodeReturnType() throws -> Any.Type? {
    fatalError("Attempted to call decoder method in a local-only actor system")
  }
}

// === errors ----------------------------------------------------------------

@available(SwiftStdlib 5.7, *)
public struct LocalTestingDistributedActorSystemError: DistributedActorSystemError {
  public let message: String

  public init(message: String) {
    self.message = message
  }
}
