// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

import Dispatch
import _Concurrency

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

  distributed func hello() async throws {
    _ = self.actorTransport
    _ = self.actorAddress
    print("hello from \(self.actorAddress)")
  }
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
  func send<Message>(_ message: Message, to recipient: ActorAddress) async throws where Message : Decodable, Message : Encodable {
    fatalError()
  }
  func request<Request, Reply>(replyType: Reply.Type, _ request: Request, from recipient: ActorAddress) async throws where Request : Decodable, Request : Encodable, Reply : Decodable, Reply : Encodable {
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

func test_run() {
  print("before")
//  try! await actor.hello() // CHECK: hell
  print("after")
}

//runAsyncAndBlock(run)
