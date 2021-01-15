// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

// Synthesis of distributed actor classes.

distributed actor class D1 {
  var x: Int = 17

  // ==== DistributedActor state -----------------------------------------------
//  let actorTransport: ActorTransport // TODO; make it automagically
//  let actorAddress: ActorAddress // TODO; make it automagically
  var actorTransport: ActorTransport { fatalError() }
  var actorAddress: ActorAddress { fatalError() }
  // ==== End of DistributedActor state ----------------------------------------

//  required init(transport: ActorTransport) {
//     self.actorTransport = transport
//     self.actorAddress = transport.makeAddress(forType: Self.self)
//     transport.register(self)
//  }

//  deinit {
//    transport.unregister(self)
//  }

  distributed func hello(name: String) async throws {
    if __isRemoteActor(self) {
      try await __hello$distributed(name: name)
    } else {
      // <<actual logic>>
      print("Hello there!")
    }
  }
  /*generated*/ private var __fakeTransport: FakeTransport {
  /*generated*/   actorTransport as! FakeTransport
  /*generated*/ }
  /*generated*/
  /*generated*/ private func __hello$distributed(name: String) async throws {
  /*generated*/   let message = Message.hello(name: name)
  /*generated*/   return try await actorTransport.send(message, to: self.actorAddress)
  /*generated*/ }

}

/*generated*/ extension D1 {
/*generated*/   enum Message: Codable {
/*generated*/   case hello(name: String)
/*generated*/   }
/*generated*/ }
/*generated*/
/*generated*/ extension D1.Message {
/*generated*/   func encode(to encoder: Encoder) throws { fatalError() }
/*generated*/   init(from decoder: Decoder) throws { fatalError() }
/*generated*/ }

// ==== "Fake" impls -----------------------------------------------------------

struct FakeTransport: ActorTransport {
  func resolve<A>(address: ActorAddress, as actorType: A.Type)
    where A : DistributedActor {
    fatalError("\(#function) is not implemented yet")
  }

  func send<Message>(
    _ message: Message,
    to recipient: ActorAddress
  ) async throws where Message: Codable {
    fatalError("\(#function) is not implemented yet")
  }

  func request<Request, Reply>(
    replyType: Reply.Type,
    _ request: Request,
    from recipient: ActorAddress
  ) async throws where Request: Codable, Reply: Codable {
    fatalError("\(#function) is not implemented yet")
  }
}
struct FakeAddress: ActorAddress {
  static let `protocol` = "fake"

  var host: String? = "localhost"
  var port: Int? = nil

  var uid: UInt64 = 1
}

// ==== Tests ------------------------------------------------------------------

// Make sure the conformances actually happen.
func acceptActor<Act: DistributedActor>(_: Act.Type) { }

func testConformance() {
  acceptActor(D1.self)
}
