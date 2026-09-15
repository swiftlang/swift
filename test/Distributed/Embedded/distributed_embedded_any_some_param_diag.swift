// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -target %target-cpu-apple-macos14 -plugin-path %swift-plugin-dir %s

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed

// Embedded `@Resolvable` support mirrors the shapes non-embedded accepts:
// `any P` / `some P` parameters and `any P` returns where `P` is `@Resolvable`
// get the wire-level `$P` stub substitution at the synthesized thunk's call
// site (the `$P` stub conforms to the system's `SerializationRequirement`, so
// the standard coverage check accepts them).

import _Concurrency
import Distributed

public struct MyActorID: Sendable, Hashable {
  public let id: UInt64
}

// The system's serialization requirement; we're just type-checking here, no requirements.
public protocol MySerializationRequirement {}
extension String: MySerializationRequirement {}

public struct MyEncoder: DistributedTargetInvocationEncoder {
  public init() {}
  public mutating func doneRecording() throws {}
}
extension MyEncoder {
  public mutating func recordArgument<Value: MySerializationRequirement>(
      _ argument: RemoteCallArgument<Value>) throws {}
}

public struct MyDecoder: DistributedTargetInvocationDecoder {
  public init() {}
}
extension MyDecoder {
  public mutating func decodeNextArgument<Argument: MySerializationRequirement>() throws -> Argument {
    fatalError()
  }
}

public struct MyResultHandler: DistributedTargetInvocationResultHandler {
  public init() {}
  public func onReturnVoid() async throws {}
  public func onThrow(error: any Error) async throws {}
}
extension MyResultHandler {
  public func onReturn<Success: MySerializationRequirement>(value: Success) async throws {}
}

public final class MySystem: DistributedActorSystem, @unchecked Sendable {
  public typealias ActorID = MyActorID
  public typealias SerializationRequirement = MySerializationRequirement
  public typealias InvocationEncoder = MyEncoder
  public typealias InvocationDecoder = MyDecoder
  public typealias ResultHandler = MyResultHandler

  public init() {}

  public func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor, Act.ID == ActorID { return nil }
  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor, Act.ID == ActorID { return MyActorID(id: 0) }
  public func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ID == ActorID {}
  public func resignID(_ id: ActorID) {}

  public func makeInvocationEncoder() -> InvocationEncoder { .init() }

  public func remoteCall<Act, Err, Res>(
    on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder,
    throwing: Err.Type, returning: Res.Type
  ) async throws -> Res
      where Act: DistributedActor, Act.ID == ActorID,
            Err: Error, Res: MySerializationRequirement { fatalError() }

  public func remoteCallVoid<Act, Err>(
    on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
      where Act: DistributedActor, Act.ID == ActorID, Err: Error { fatalError() }
}

typealias DefaultDistributedActorSystem = MySystem

// A protocol refining DistributedActor that a Phase 2 implementation
// would use with @Resolvable.
public protocol Worker: DistributedActor where ActorSystem == MySystem {
  distributed func work(name: String) -> String
}

// A `@Resolvable` protocol: the macro emits a `$Worker` stub. `any Worker`
// parameters/returns are accepted; the synthesized distributed thunk
// substitutes `$Worker` at the wire-level call site.
@Resolvable
public protocol RWorker: DistributedActor where ActorSystem == MySystem {
  distributed func work(name: String) -> String
}

// Conform the `$RWorker` wire stub to the serialization requirement so the
// standard coverage check accepts `any RWorker` parameters and returns.
extension $RWorker: MySerializationRequirement {}

distributed actor Hub {
  // expected-error@+1{{parameter 'to' of type 'any Worker' in distributed instance method is not supported in Embedded Swift; only concrete types, or 'any'/'some' types of an '@Resolvable protocol' may appear in 'distributed func' signatures}}
  distributed func sendAny(to worker: any Worker) -> String {
    return "sent"
  }

  distributed func sendSome(to worker: some RWorker) -> String {
    return "sent"
  }

  // expected-error@+1{{parameter 'to' of type 'some Worker' in distributed instance method is not supported in Embedded Swift; only concrete types, or 'any'/'some' types of an '@Resolvable protocol' may appear in 'distributed func' signatures}}
  distributed func sendSomeNonResolvable(to worker: some Worker) -> String {
    return "sent"
  }

  // expected-error@+1{{'some' return type 'some RWorker' of distributed instance method is not supported in Embedded Swift; use 'any RWorker' instead}}
  distributed func replySome() -> some RWorker {
    return try! $RWorker.resolve(id: self.id, using: self.actorSystem)
  }

  // `any P` return where P is `@Resolvable`: accepted
  distributed func pickResolvable() -> any RWorker {
    fatalError()
  }

  // expected-error@+1{{return type 'any Worker' of distributed instance method is not supported in Embedded Swift; only concrete types, or 'any'/'some' types of an '@Resolvable' protocol may appear in 'distributed func' signatures}}
  distributed func pickWorker() -> any Worker {
    fatalError()
  }

  distributed func sendResolvableAny(to worker: any RWorker) -> String {
    return "sent"
  }

  // User-written generic distributed func: rejected up front
  // expected-error@+1{{generic 'distributed func' is not supported in Embedded Swift; use concrete parameter and return types in distributed instance method}}
  distributed func sendGeneric<T: Sendable>(_ value: T) -> String {
    return "sent"
  }

  // Equivalent to 'some RWorker'
  distributed func sendGenericWorker<W: RWorker>(_ value: W) -> String {
    return "sent"
  }

  // Ok
  distributed func hello(name: String) -> String {
    return "Hello, \(name)!"
  }
}
