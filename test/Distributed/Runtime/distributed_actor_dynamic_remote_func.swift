// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import _Distributed

distributed actor LocalWorker {
  typealias Transport = FakeActorSystem

  init(system: FakeActorSystem) {}

  distributed func function() async throws -> String {
    "local:"
  }

  distributed func echo(name: String) async throws -> String {
    "local:\(name)"
  }
}

extension LocalWorker {
  @_dynamicReplacement(for: _remote_function())
  // TODO(distributed): @_remoteDynamicReplacement(for: function()) - could be a nicer spelling, hiding that we use dynamic under the covers
  func _cluster_remote_function() async throws -> String {
    "\(#function):"
  }

  @_dynamicReplacement(for: _remote_echo(name:))
  // TODO(distributed): @_remoteDynamicReplacement(for: hello(name:)) - could be a nicer spelling, hiding that we use dynamic under the covers
  func _cluster_remote_echo(name: String) async throws -> String {
    "\(#function):\(name)"
  }
}

// ==== Fake Transport ---------------------------------------------------------

struct ActorAddress: Sendable, Hashable, Codable {
  let address: String
  init(parse address : String) {
    self.address = address
  }
}

struct FakeActorSystem: DistributedActorSystem {
  typealias ActorID = ActorAddress
  typealias Invocation = FakeInvocation
  typealias SerializationRequirement = Codable

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor,
      Act.ID == ActorID {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor,
      Act.ID == ActorID {
    let id = ActorAddress(parse: "xxx")
    print("assign type:\(actorType), id:\(id)")
    return id
  }

  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor,
      Act.ID == ActorID {
    print("ready actor:\(actor), id:\(actor.id)")
  }

  func resignID(_ id: ActorID) {
    print("ready id:\(id)")
  }

  func makeInvocation() -> Invocation {
    .init()
  }
}

struct FakeInvocation: DistributedTargetInvocation {
  typealias ArgumentDecoder = FakeArgumentDecoder
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(mangledType: T.Type) throws {}
  mutating func recordArgument<Argument: SerializationRequirement>(argument: Argument) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(mangledType: R.Type) throws {}
  mutating func recordErrorType<E: Error>(mangledType: E.Type) throws {}
  mutating func doneRecording() throws {}

  // === Receiving / decoding -------------------------------------------------

  mutating func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  mutating func argumentDecoder() -> FakeArgumentDecoder { .init() }
  mutating func decodeReturnType() throws -> Any.Type? { nil }
  mutating func decodeErrorType() throws -> Any.Type? { nil }

  struct FakeArgumentDecoder: DistributedTargetInvocationArgumentDecoder {
    typealias SerializationRequirement = Codable
  }
}


@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== Execute ----------------------------------------------------------------

func test_local() async throws {
  let system = FakeActorSystem()

  let worker = LocalWorker(system: system)
  let x = try await worker.function()
  print("call: \(x)")
  // CHECK: assign type:LocalWorker, id:[[ADDRESS:.*]]
  // CHECK: ready actor:main.LocalWorker, id:[[ADDRESS]]
  // CHECK: call: local:
}

func test_remote() async throws {
  let address = ActorAddress(parse: "")
  let system = FakeActorSystem()

  let worker = try LocalWorker.resolve(id: address, using: system)
  let x = try await worker.function()
  print("call: \(x)")
  // CHECK: call: _cluster_remote_function():

  let e = try await worker.echo(name: "Charlie")
  print("call: \(e)")
  // CHECK: call: _cluster_remote_echo(name:):Charlie
}

@main struct Main {
  static func main() async {
    try! await test_local()
    try! await test_remote()
  }
}
