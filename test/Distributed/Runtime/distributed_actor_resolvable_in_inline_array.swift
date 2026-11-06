// RUN: %empty-directory(%t)

// RUN: %target-build-swift -module-name main -target %target-swift-6.2-abi-triple -plugin-path %swift-plugin-dir -enable-experimental-feature DistributedAnyResolvableInContainers -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// RUN: %target-build-swift -O -module-name main -target %target-swift-6.2-abi-triple -plugin-path %swift-plugin-dir -enable-experimental-feature DistributedAnyResolvableInContainers -parse-as-library %s -o %t/a-O.out
// RUN: %target-codesign %t/a-O.out
// RUN: %target-run %t/a-O.out | %FileCheck %s
//
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_swift_parser, asserts
// REQUIRES: swift_feature_DistributedAnyResolvableInContainers
//
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// `InlineArray<N, any P>` of a `@Resolvable protocol` existential. InlineArray
// is not Codable, so this uses an actor system whose serialization
// requirement it can satisfy, and which passes values by reference.
// Unlike `[any P]`, an `InlineArray<N, $P>` doesn't implicitly upcast to
// `InlineArray<N, any P>`, so both directions are converted element-wise

import Distributed

// ==== -----------------------------------------------------------------------
// MARK: Pass-by-reference actor system

public protocol Passable {}

extension String: Passable {}
extension Int: Passable {}
extension InlineArray: Passable where Element: Passable {}
extension Array: Passable where Element: Passable {}

public struct PassID: Hashable, Sendable, CustomStringConvertible {
  let value: Int
  public var description: String { "id-\(value)" }
}

public final class PassthroughActorSystem: DistributedActorSystem, @unchecked Sendable {
  public typealias ActorID = PassID
  public typealias InvocationEncoder = PassInvocationEncoder
  public typealias InvocationDecoder = PassInvocationDecoder
  public typealias SerializationRequirement = Passable
  public typealias ResultHandler = PassResultHandler

  var nextID = 0
  var activeActors: [ActorID: any DistributedActor] = [:]

  public init() {}

  public func resolve<Act>(id: ActorID, as actorType: Act.Type)
      throws -> Act? where Act: DistributedActor {
    nil
  }

  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor {
    nextID += 1
    return PassID(value: nextID)
  }

  public func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ID == ActorID {
    activeActors[actor.id] = actor
  }

  public func resignID(_ id: ActorID) {}

  public func makeInvocationEncoder() -> InvocationEncoder { .init() }

  public func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
      where Act: DistributedActor, Act.ID == ActorID,
            Err: Error, Res: Passable {
    let handler = PassResultHandler()
    try await execute(on: actor.id, target: target, invocation, handler)
    return handler.value as! Res
  }

  public func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
      where Act: DistributedActor, Act.ID == ActorID, Err: Error {
    try await execute(on: actor.id, target: target, invocation, PassResultHandler())
  }

  private func execute(on id: ActorID, target: RemoteCallTarget,
                       _ invocation: InvocationEncoder,
                       _ handler: PassResultHandler) async throws {
    guard let active = activeActors[id] else {
      fatalError("No active actor for \(id)")
    }
    var decoder = PassInvocationDecoder(invocation)
    func doIt<A: DistributedActor>(_ active: A) async throws {
      try await executeDistributedTarget(
        on: active, target: target, invocationDecoder: &decoder,
        handler: handler)
    }
    try await _openExistential(active, do: doIt)
  }
}

public struct PassInvocationEncoder: DistributedTargetInvocationEncoder {
  public typealias SerializationRequirement = Passable

  var substitutions: [Any.Type] = []
  var arguments: [Any] = []

  public mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {
    substitutions.append(type)
  }
  public mutating func recordArgument<Value: Passable>(
      _ argument: RemoteCallArgument<Value>) throws {
    arguments.append(argument.value)
  }
  public mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  public mutating func recordReturnType<R: Passable>(_ type: R.Type) throws {}
  public mutating func doneRecording() throws {}
}

public struct PassInvocationDecoder: DistributedTargetInvocationDecoder {
  public typealias SerializationRequirement = Passable

  let substitutions: [Any.Type]
  let arguments: [Any]
  var index = 0

  init(_ encoder: PassInvocationEncoder) {
    self.substitutions = encoder.substitutions
    self.arguments = encoder.arguments
  }

  public mutating func decodeGenericSubstitutions() throws -> [Any.Type] {
    substitutions
  }
  public mutating func decodeNextArgument<Argument: Passable>() throws -> Argument {
    defer { index += 1 }
    return arguments[index] as! Argument
  }
  public mutating func decodeErrorType() throws -> Any.Type? { nil }
  public mutating func decodeReturnType() throws -> Any.Type? { nil }
}

public final class PassResultHandler: DistributedTargetInvocationResultHandler, @unchecked Sendable {
  public typealias SerializationRequirement = Passable

  var value: Any? = nil

  public func onReturn<Success: Passable>(value: Success) async throws {
    self.value = value
  }
  public func onReturnVoid() async throws {}
  public func onThrow<Err: Error>(error: Err) async throws {
    fatalError("Unexpected error: \(error)")
  }
}

// The `$Greeter` stub travels by reference too
extension $Greeter: Passable {}

// ==== -----------------------------------------------------------------------
// MARK: Actors

typealias DefaultDistributedActorSystem = PassthroughActorSystem

@Resolvable
protocol Greeter: DistributedActor where ActorSystem == PassthroughActorSystem {
  distributed func sayHi() -> String
}

distributed actor GreeterImpl: Greeter {
  let name: String

  init(name: String, actorSystem: ActorSystem) {
    self.name = name
    self.actorSystem = actorSystem
  }

  distributed func sayHi() -> String { "Hi from \(name)" }
}

distributed actor Pair {
  let first: GreeterImpl
  let second: GreeterImpl

  init(_ first: GreeterImpl, _ second: GreeterImpl, actorSystem: ActorSystem) {
    self.first = first
    self.second = second
    self.actorSystem = actorSystem
  }

  distributed func one() -> any Greeter {
    first
  }

  distributed func both() -> InlineArray<2, any Greeter> {
    [first, second]
  }

  distributed func greetBoth(_ gs: InlineArray<2, any Greeter>) async throws -> [String] {
    var result: [String] = []
    for i in gs.indices {
      print("greetBoth element type: \(type(of: gs[i]))")
      result.append(try await gs[i].sayHi())
    }
    return result
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Main

@main struct Main {
  static func main() async throws {
    let system = PassthroughActorSystem()
    let alice = GreeterImpl(name: "alice", actorSystem: system)
    let bob = GreeterImpl(name: "bob", actorSystem: system)
    let local = Pair(alice, bob, actorSystem: system)
    let pair = try Pair.resolve(id: local.id, using: system)

    // Like XPC, this system's 'decodeReturnType()' returns nil, so the
    // recipient takes the result type from the target's mangled name
    print("--- top-level result")
    // CHECK-LABEL: --- top-level result
    let one = try await pair.one()
    let oneHi = try await one.sayHi()
    print("one type: \(type(of: one)), says: \(oneHi)")
    // CHECK: one type: $Greeter, says: Hi from alice

    print("--- result")
    // CHECK-LABEL: --- result
    let both = try await pair.both()
    for i in both.indices {
      let hi = try await both[i].sayHi()
      print("both element type: \(type(of: both[i])), says: \(hi)")
    }
    // CHECK: both element type: $Greeter, says: Hi from alice
    // CHECK: both element type: $Greeter, says: Hi from bob

    print("--- param")
    // CHECK-LABEL: --- param
    let greetings = try await pair.greetBoth([bob, alice])
    // CHECK: greetBoth element type: $Greeter
    // CHECK: greetBoth element type: $Greeter
    print("greetBoth: \(greetings)")
    // CHECK: greetBoth: ["Hi from bob", "Hi from alice"]

    print("--- done")
    // CHECK: --- done
  }
}
