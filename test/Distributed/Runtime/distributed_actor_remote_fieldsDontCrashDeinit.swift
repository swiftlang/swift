// RUN: %target-run-simple-swift(-Onone -Xfrontend -g -Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import _Distributed

@available(SwiftStdlib 5.6, *)
distributed actor SomeSpecificDistributedActor {
  typealias Transport = FakeActorSystem

  let name: String
  let surname: String
  let age: Int

  init(name: String, transport: FakeActorSystem) {
    self.name = name
    self.surname = "Surname"
    self.age = 42
  }

  deinit {
    print("deinit \(self.id)")
  }

  distributed func hello() async throws -> String {
    "Hello, from \(name)"
  }
}

// ==== Fake Transport ---------------------------------------------------------

@available(SwiftStdlib 5.6, *)
struct FakeActorID: Sendable, Hashable, Codable {
  let id: UInt64
}

@available(SwiftStdlib 5.6, *)
enum FakeActorSystemError: DistributedActorSystemError {
  case unsupportedActorIdentity(AnyActorIdentity)
}

@available(SwiftStdlib 5.6, *)
struct ActorAddress: Sendable, Hashable, Codable {
  let address: String
  init(parse address : String) {
    self.address = address
  }
}

@available(SwiftStdlib 5.6, *)
struct FakeActorSystem: DistributedActorSystem {

  func resolve<Act>(id: ID, as actorType: Act.Type) throws -> Act? where Act: DistributedActor {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> AnyActorIdentity
      where Act: DistributedActor {
    let id = ActorAddress(parse: "xxx")
    print("assignID type:\(actorType), id:\(id)")
    return .init(id)
  }

  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor,
      Act.ID == ActorID {
    print("actorReady actor:\(actor), id:\(actor.id)")
  }

  func resignID(_ id: AnyActorIdentity) {
    print("assignID id:\(id)")
  }
}

@available(SwiftStdlib 5.6, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== Execute ----------------------------------------------------------------

@available(SwiftStdlib 5.6, *)
func test_remote() async {
  let address = ActorAddress(parse: "sact://127.0.0.1/example#1234")
  let system = FakeActorSystem()

  var remote: SomeSpecificDistributedActor? =
      try! SomeSpecificDistributedActor.resolve(.init(address), using: transport)
  // Check the id and transport are the right values, and not trash memory
  print("remote.id = \(remote!.id)") // CHECK: remote.id = AnyActorIdentity(ActorAddress(address: "sact://127.0.0.1/example#1234"))
  print("remote.transport = \(remote!.actorSystem)") // CHECK: remote.transport = FakeActorSystem()

  remote = nil // CHECK: deinit AnyActorIdentity(ActorAddress(address: "sact://127.0.0.1/example#1234"))
  print("done") // CHECK: done
}

@available(SwiftStdlib 5.6, *)
@main struct Main {
  static func main() async {
    await test_remote()
  }
}

