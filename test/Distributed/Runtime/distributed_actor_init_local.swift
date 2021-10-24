// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): we need to revisit what's going on on windows with distributed actors rdar://84574311
// UNSUPPORTED: OS=windows-msvc

// Disabled temporarily until we figure out why the test is flaky.
// REQUIRES: rdar84586299

import _Distributed

enum MyError: Error {
  case test
}

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

@available(SwiftStdlib 5.5, *)
distributed actor Bug_CallsReadyTwice {
  var x: Int
  init(transport: ActorTransport, wantBug: Bool) async {
    if wantBug {
      self.x = 1
    }
    self.x = 2
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor Throwy {
  init(transport: ActorTransport, doThrow: Bool) throws {
    if doThrow {
      throw MyError.test
    }
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor ThrowBeforeFullyInit {
  var x: Int
  init(transport: ActorTransport, doThrow: Bool) throws {
    if doThrow {
      throw MyError.test
    }
    self.x = 0
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

  _ = try? Throwy(transport: transport, doThrow: false)
  // CHECK: assign type:Throwy, id:ActorAddress(address: "[[ID:.*]]")
  // CHECK: ready actor:main.Throwy, id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))
  // CHECK: resign id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))

  _ = try? Throwy(transport: transport, doThrow: true)
  // CHECK: assign type:Throwy, id:ActorAddress(address: "[[ID:.*]]")
  // CHECK-NOT: ready
  // CHECK: resign id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))

  _ = try? ThrowBeforeFullyInit(transport: transport, doThrow: true)
  // CHECK: assign type:ThrowBeforeFullyInit, id:ActorAddress(address: "[[ID:.*]]")
  // FIXME: The two checks below should work, but do not currently, so they're disabled (rdar://84533820).
  // MISSING-CHECK-NOT: ready actor:main.ThrowBeforeFullyInit
  // MISSING-CHECK: resign id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))

  _ = await PickATransport2(other: 1, theTransport: transport)
  // CHECK: assign type:PickATransport2, id:ActorAddress(address: "[[ID:.*]]")
  // CHECK: ready actor:main.PickATransport2, id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))

  // FIXME: The checks for this initializer should NOT pass, but currently do. (rdar://84533820)
  _ = await Bug_CallsReadyTwice(transport: transport, wantBug: true)
  // CHECK: assign type:Bug_CallsReadyTwice, id:ActorAddress(address: "[[ID:.*]]")
  // CHECK:      ready actor:main.Bug_CallsReadyTwice, id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))
  // CHECK-NEXT: ready actor:main.Bug_CallsReadyTwice, id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))

  // TODO: it's not obvious why the resigns happen later for the async ones.
  // might need to find a way to force the deallocation at a specific point,
  // or just use check-dag or something.

  // CHECK: resign id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))
  // CHECK: resign id:AnyActorIdentity(ActorAddress(address: "[[ID]]"))
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await test()
  }
}
