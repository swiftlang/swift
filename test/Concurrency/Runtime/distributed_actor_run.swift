// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

import Dispatch
import _Concurrency

//protocol DA {
//  var address: ActorAddress { get }
//
//}
//
//class MANUAL: DA {
//  // @derived
//  let address: ActorAddress
//
//  init( actorAddress: ActorAddress) {
//    self.address = actorAddress
//  }
//}


distributed actor class SomeSpecificDistributedActor {
//  // @derived let actorTransport: ActorTransport
//  // @derived let actorAddress: ActorAddress

//  // @derived
//  required init(transport: ActorTransport) {
//    self.actorTransport = transport
//    self.actorAddress = ActorAddress(parse: "xxx")
//  }
//  // @derived
//  required init(resolve address: ActorAddress, using transport: ActorTransport) {
//    self.actorAddress = address
//    self.actorTransport = transport
//  }

//  distributed func hello() async throws {
//    // print("hello from \(self.actorAddress)")
//  }
}

// ==== Fake Transport ---------------------------------------------------------

struct FakeTransport: ActorTransport {
  func resolve<Act>(address: ActorAddress, as actorType: Act.Type)
    throws -> ActorResolved<Act> where Act: DistributedActor {
    fatalError()
  }
  func assignAddress<Act>(
    _ actorType: Act.Type
//    ,
//    onActorCreated: (Act) -> ()
  ) -> ActorAddress where Act : DistributedActor {
    fatalError()
  }
}

// ==== Execute ----------------------------------------------------------------
let address = ActorAddress(parse: "")
let transport = FakeTransport()

func test_initializers() {
  _ = SomeSpecificDistributedActor(transport: transport)
  _ = SomeSpecificDistributedActor(resolve: address, using: transport)
}

func test_address() {
  let actor = SomeSpecificDistributedActor(transport: transport)
  _ = actor.actorAddress
}

func test_run() async {
  print("before") // CHECK: before
//  try! await actor.hello()
  print("after") // CHECK: after
}

runAsyncAndBlock(test_run)
