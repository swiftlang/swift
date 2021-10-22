// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor PickATransport1 {
  init(kappa transport: ActorTransport, other: Int) {}
}

@available(SwiftStdlib 5.5, *)
distributed actor PickATransport2 {
  init(other: Int, theTransport: ActorTransport) async {}
}

@available(SwiftStdlib 5.5, *)
distributed actor LocalWorker {
  init(transport: ActorTransport) {}
}

// ==== Fake Transport ---------------------------------------------------------

@available(SwiftStdlib 5.5, *)
struct ActorAddress: ActorIdentity {
  let address: String
  init(parse address: String) {
    self.address = address
  }
}

@available(SwiftStdlib 5.5, *)
struct FakeTransport: ActorTransport {
  func decodeIdentity(from decoder: Decoder) throws -> AnyActorIdentity {
    fatalError("not implemented:\(#function)")
  }

  func resolve<Act>(_ identity: AnyActorIdentity, as actorType: Act.Type)
      throws -> Act? where Act: DistributedActor {
    fatalError("not implemented:\(#function)")
  }

  func assignIdentity<Act>(_ actorType: Act.Type) -> AnyActorIdentity
      where Act: DistributedActor {
    let id = ActorAddress(parse: "xxx")
    print("assign type:\(actorType), id:\(id)")
    return .init(id)
  }

  func actorReady<Act>(_ actor: Act) where Act: DistributedActor {
    print("ready actor:\(actor), id:\(actor.id)")
  }

  func resignIdentity(_ id: AnyActorIdentity) {
    print("resign id:\(id)")
  }
}

// ==== Execute ----------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
func test() async {
  let transport = FakeTransport()

  _ = LocalWorker(transport: transport)
  // CHECK: assign type:LocalWorker, id:ActorAddress(address: "[[ID:.*]]")
  // CHECK: ready actor:main.LocalWorker, id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))
  // CHECK: resign id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))

  _ = PickATransport1(kappa: transport, other: 0)
  // CHECK: assign type:PickATransport1, id:ActorAddress(address: "[[ID:.*]]")
  // CHECK: ready actor:main.PickATransport1, id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))
  // CHECK: resign id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))

  _ = await PickATransport2(other: 1, theTransport: transport)
  // CHECK: assign type:PickATransport2, id:ActorAddress(address: "[[ID:.*]]")
  // CHECK: ready actor:main.PickATransport2, id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))
  // CHECK: resign id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await test()
  }
}
