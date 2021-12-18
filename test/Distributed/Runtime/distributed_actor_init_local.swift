// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Flaky CI test
// REQUIRES: radar84649015

import _Distributed

enum MyError: Error {
  case test
}

@available(SwiftStdlib 5.6, *)
distributed actor PickATransport1 {
  init(kappa transport: FakeTransport, other: Int) {}
}

@available(SwiftStdlib 5.6, *)
distributed actor PickATransport2 {
  init(other: Int, theTransport: FakeTransport) async {}
}

@available(SwiftStdlib 5.6, *)
distributed actor LocalWorker {
  init(transport: FakeTransport) {}
}

@available(SwiftStdlib 5.6, *)
distributed actor Bug_CallsReadyTwice {
  var x: Int
  init(transport: FakeTransport, wantBug: Bool) async {
    if wantBug {
      self.x = 1
    }
    self.x = 2
  }
}

@available(SwiftStdlib 5.6, *)
distributed actor Throwy {
  init(transport: FakeTransport, doThrow: Bool) throws {
    if doThrow {
      throw MyError.test
    }
  }
}

@available(SwiftStdlib 5.6, *)
distributed actor ThrowBeforeFullyInit {
  var x: Int
  init(transport: FakeTransport, doThrow: Bool) throws {
    if doThrow {
      throw MyError.test
    }
    self.x = 0
  }
}

// ==== Fake Transport ---------------------------------------------------------

@available(SwiftStdlib 5.6, *)
struct ActorAddress: ActorIdentity {
  let address: String
  init(parse address: String) {
    self.address = address
  }
}

// global to track available IDs
var nextID: Int = 1

@available(SwiftStdlib 5.6, *)
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
    let id = ActorAddress(parse: "\(nextID)")
    nextID += 1
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

@available(SwiftStdlib 5.6, *)
func test() async {
  let transport = FakeTransport()

  // NOTE: All allocated distributed actors should be saved in this array, so
  // that they will be deallocated together at the end of this test!
  // This convention helps ensure that the test is not flaky.
  var test: [DistributedActor?] = []

  test.append(LocalWorker(transport: transport))
  // CHECK: assign type:LocalWorker, id:ActorAddress(address: "[[ID1:.*]]")
  // CHECK: ready actor:main.LocalWorker, id:AnyActorIdentity(ActorAddress(address: "[[ID1]]"))

  test.append(PickATransport1(kappa: transport, other: 0))
  // CHECK: assign type:PickATransport1, id:ActorAddress(address: "[[ID2:.*]]")
  // CHECK: ready actor:main.PickATransport1, id:AnyActorIdentity(ActorAddress(address: "[[ID2]]"))

  test.append(try? Throwy(transport: transport, doThrow: false))
  // CHECK: assign type:Throwy, id:ActorAddress(address: "[[ID3:.*]]")
  // CHECK: ready actor:main.Throwy, id:AnyActorIdentity(ActorAddress(address: "[[ID3]]"))

  test.append(try? Throwy(transport: transport, doThrow: true))
  // CHECK: assign type:Throwy, id:ActorAddress(address: "[[ID4:.*]]")
  // CHECK-NOT: ready

  test.append(try? ThrowBeforeFullyInit(transport: transport, doThrow: true))
  // CHECK: assign type:ThrowBeforeFullyInit, id:ActorAddress(address: "[[ID5:.*]]")
  // CHECK-NOT: ready

  test.append(await PickATransport2(other: 1, theTransport: transport))
  // CHECK: assign type:PickATransport2, id:ActorAddress(address: "[[ID6:.*]]")
  // CHECK: ready actor:main.PickATransport2, id:AnyActorIdentity(ActorAddress(address: "[[ID6]]"))

  test.append(await Bug_CallsReadyTwice(transport: transport, wantBug: true))
    // CHECK: assign type:Bug_CallsReadyTwice, id:ActorAddress(address: "[[ID7:.*]]")
    // CHECK:      ready actor:main.Bug_CallsReadyTwice, id:AnyActorIdentity(ActorAddress(address: "[[ID7]]"))
    // CHECK-NEXT: ready actor:main.Bug_CallsReadyTwice, id:AnyActorIdentity(ActorAddress(address: "[[ID7]]"))

  // CHECK-DAG: resign id:AnyActorIdentity(ActorAddress(address: "[[ID1]]"))
  // CHECK-DAG: resign id:AnyActorIdentity(ActorAddress(address: "[[ID2]]"))
  // CHECK-DAG: resign id:AnyActorIdentity(ActorAddress(address: "[[ID3]]"))
  // MISSING-CHECK-DAG: resign id:AnyActorIdentity(ActorAddress(address: "[[ID4]]")) // FIXME: should eventually work (rdar://84533820).
  // MISSING-CHECK-DAG: resign id:AnyActorIdentity(ActorAddress(address: "[[ID5]]")) // FIXME: should eventually work (rdar://84533820).
  // CHECK-DAG: resign id:AnyActorIdentity(ActorAddress(address: "[[ID6]]"))
  // CHECK-DAG: resign id:AnyActorIdentity(ActorAddress(address: "[[ID7]]"))
}

@available(SwiftStdlib 5.6, *)
@main struct Main {
  static func main() async {
    await test()
  }
}
