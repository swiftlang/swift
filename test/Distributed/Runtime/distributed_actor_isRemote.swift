// RUN: %target-run-simple-swift(-Onone -Xfrontend -g -Xfrontend -enable-experimental-distributed -Xfrontend -disable-availability-checking -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import _Distributed

distributed actor SomeSpecificDistributedActor {
  distributed func hello() async throws -> String {
    "local impl"
  }
}

extension SomeSpecificDistributedActor {

  @_dynamicReplacement(for: _remote_hello())
  nonisolated func _remote_impl_hello() async throws -> String {
    return "remote impl (address: \(self.id))"
  }
}

// ==== Fake Transport ---------------------------------------------------------

struct FakeActorID: Sendable, Hashable, Codable {
  let id: UInt64
}

enum FakeActorSystemError: DistributedActorSystemError {
  case unsupportedActorIdentity(Any)
}

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
            Act.ID == ActorID  {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor {
    let id = ActorAddress(parse: "xxx")
    print("assignID type:\(actorType), id:\(id)")
    return id
  }

  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor,
      Act.ID == ActorID {
    print("actorReady actor:\(actor), id:\(actor.id)")
  }

  func resignID(_ id: ActorID) {
    print("assignID id:\(id)")
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

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== Execute ----------------------------------------------------------------

@_silgen_name("swift_distributed_actor_is_remote")
func __isRemoteActor(_ actor: AnyObject) -> Bool

func __isLocalActor(_ actor: AnyObject) -> Bool {
  return !__isRemoteActor(actor)
}

// ==== Execute ----------------------------------------------------------------

func test_remote() async {
  let address = ActorAddress(parse: "sact://127.0.0.1/example#1234")
  let system = FakeActorSystem()

  let local = SomeSpecificDistributedActor(system: system)
  assert(__isLocalActor(local) == true, "should be local")
  assert(__isRemoteActor(local) == false, "should be local")
  print("isRemote(local) = \(__isRemoteActor(local))") // CHECK: isRemote(local) = false
  print("local.id = \(local.id)") // CHECK: local.id = ActorAddress(address: "xxx")
  print("local.system = \(local.actorSystem)") // CHECK: local.system = FakeActorSystem()

  // assume it always makes a remote one
  let remote = try! SomeSpecificDistributedActor.resolve(id: address, using: system)
  assert(__isLocalActor(remote) == false, "should be remote")
  assert(__isRemoteActor(remote) == true, "should be remote")
  print("isRemote(remote) = \(__isRemoteActor(remote))") // CHECK: isRemote(remote) = true

  // Check the id and system are the right values, and not trash memory
  print("remote.id = \(remote.id)") // CHECK: remote.id = ActorAddress(address: "sact://127.0.0.1/example#1234")
  print("remote.system = \(remote.actorSystem)") // CHECK: remote.system = FakeActorSystem()

  print("done") // CHECK: done
}

@main struct Main {
  static func main() async {
    await test_remote()
  }
}

