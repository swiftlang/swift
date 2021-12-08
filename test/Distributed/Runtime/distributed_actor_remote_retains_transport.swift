// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -Xfrontend -disable-availability-checking -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import _Distributed

distributed actor SomeSpecificDistributedActor {
  deinit {
    print("deinit \(self.id)")
  }
}

// ==== Fake Transport ---------------------------------------------------------

struct FakeActorID: Sendable, Hashable, Codable {
  let id: UInt64
}

enum FakeActorSystemError: DistributedActorSystemError {
  case unsupportedActorIdentity(AnyActorIdentity)
}

struct ActorAddress: Sendable, Hashable, Codable {
  let address: String
  init(parse address : String) {
    self.address = address
  }
}

final class FakeActorSystem: DistributedActorSystem {

  deinit {
    print("deinit \(self)")
  }

  func resolve<Act>(id: ID, as actorType: Act.Type)
  throws -> Act?
      where Act: DistributedActor {
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

func test_remote() async {
  let address = ActorAddress(parse: "sact://127.0.0.1/example#1234")
  var transport: FakeActorSystem? = FakeActorSystem()

  let remote = try! SomeSpecificDistributedActor.resolve(.init(address), using: transport!)

  transport = nil
  print("done") // CHECK: done

  print("remote.id = \(remote.id)") // CHECK: remote.id = AnyActorIdentity(ActorAddress(address: "sact://127.0.0.1/example#1234"))
  print("remote.transport = \(remote.actorSystem)") // CHECK: remote.transport = main.FakeActorSystem

  // only once we exit the function and the remote is released, the transport has no more references
  // CHECK-DAG: deinit AnyActorIdentity(ActorAddress(address: "sact://127.0.0.1/example#1234"))
  // transport must deinit after the last actor using it does deinit
  // CHECK-DAG: deinit main.FakeActorSystem
}

@main struct Main {
  static func main() async {
    await test_remote()
  }
}

