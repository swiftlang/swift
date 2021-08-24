// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// REQUIRES: rdar78290608

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor LocalWorker {
  init(transport: ActorTransport) {
    defer { transport.actorReady(self) } // FIXME(distributed): rdar://81783599 this should be injected automatically
  }
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
func test() {
  let transport = FakeTransport()

  _ = LocalWorker(transport: transport)
  // CHECK: assign type:LocalWorker, id:ActorAddress(address: "[[ID:.*]]")
  // CHECK: ready actor:main.LocalWorker, id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))
  // CHECK: resign id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    test()
  }
}
