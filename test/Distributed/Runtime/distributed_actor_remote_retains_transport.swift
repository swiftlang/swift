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

struct FakeActorID: ActorIdentity {
  let id: UInt64
}

enum FakeTransportError: ActorTransportError {
  case unsupportedActorIdentity(AnyActorIdentity)
}

struct ActorAddress: ActorIdentity {
  let address: String
  init(parse address : String) {
    self.address = address
  }
}

final class FakeTransport: ActorTransport {

  deinit {
    print("deinit \(self)")
  }

  func decodeIdentity(from decoder: Decoder) throws -> AnyActorIdentity {
    fatalError("not implemented:\(#function)")
  }

  func resolve<Act>(_ identity: AnyActorIdentity, as actorType: Act.Type)
  throws -> Act?
      where Act: DistributedActor {
    return nil
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

func test_remote() async {
  let address = ActorAddress(parse: "sact://127.0.0.1/example#1234")
  var transport: ActorTransport? = FakeTransport()

  let remote = try! SomeSpecificDistributedActor.resolve(.init(address), using: transport!)

  transport = nil
  print("done") // CHECK: done

  print("remote.id = \(remote.id)") // CHECK: remote.id = AnyActorIdentity(ActorAddress(address: "sact://127.0.0.1/example#1234"))
  print("remote.transport = \(remote.actorTransport)") // CHECK: remote.transport = main.FakeTransport

  // only once we exit the function and the remote is released, the transport has no more references
  // CHECK-DAG: deinit AnyActorIdentity(ActorAddress(address: "sact://127.0.0.1/example#1234"))
  // transport must deinit after the last actor using it does deinit
  // CHECK-DAG: deinit main.FakeTransport
}

@main struct Main {
  static func main() async {
    await test_remote()
  }
}

