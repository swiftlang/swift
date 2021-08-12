// RUN: %target-run-simple-swift(-Onone -Xfrontend -g -Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// REQUIRES: rdar78290608

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor SomeSpecificDistributedActor {
  let name: String
  let surname: String
  let age: Int

  init(name: String, transport: ActorTransport) {
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

@available(SwiftStdlib 5.5, *)
struct FakeActorID: ActorIdentity {
  let id: UInt64
}

@available(SwiftStdlib 5.5, *)
enum FakeTransportError: ActorTransportError {
  case unsupportedActorIdentity(AnyActorIdentity)
}

@available(SwiftStdlib 5.5, *)
struct ActorAddress: ActorIdentity {
  let address: String
  init(parse address : String) {
    self.address = address
  }
}

@available(SwiftStdlib 5.5, *)
struct FakeTransport: ActorTransport {
  func decodeIdentity(from decoder: Decoder) throws -> AnyActorIdentity {
    fatalError("not implemented:\(#function)")
  }

  func resolve<Act>(_ identity: AnyActorIdentity, as actorType: Act.Type)
  throws -> ActorResolved<Act>
      where Act: DistributedActor {
    .makeProxy
  }

  func assignIdentity<Act>(_ actorType: Act.Type) -> AnyActorIdentity
      where Act: DistributedActor {
    let id = ActorAddress(parse: "xxx")
    print("assignIdentity type:\(actorType), id:\(id)")
    return .init(id)
  }

  func actorReady<Act>(_ actor: Act) where Act: DistributedActor {
    print("actorReady actor:\(actor), id:\(actor.id)")
  }

  func resignIdentity(_ id: AnyActorIdentity) {
    print("resignIdentity id:\(id)")
  }
}

// ==== Execute ----------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
func test_remote() async {
  let address = ActorAddress(parse: "sact://127.0.0.1/example#1234")
  let transport = FakeTransport()

  var remote: SomeSpecificDistributedActor? =
      try! SomeSpecificDistributedActor.resolve(.init(address), using: transport)
  // Check the id and transport are the right values, and not trash memory
  print("remote.id = \(remote!.id)") // CHECK: remote.id = AnyActorIdentity(ActorAddress(address: "sact://127.0.0.1/example#1234"))
  print("remote.transport = \(remote!.actorTransport)") // CHECK: remote.transport = FakeTransport()

  remote = nil // CHECK: deinit AnyActorIdentity(ActorAddress(address: "sact://127.0.0.1/example#1234"))
  print("done") // CHECK: done
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await test_remote()
  }
}

