// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

// Synthesis of distributed actor classes.

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

distributed actor class D1 {
  var x: Int = 17

  // ==== Distributed requirements
//  let actorTransport: ActorTransport // TODO; make it automagically
//  let actorAddress: ActorAddress // TODO; make it automagically
  var actorTransport: ActorTransport { fatalError() }
  var actorAddress: ActorAddress { fatalError() }

  required init(transport: ActorTransport) { // TODO: Forbid implementing this manually
//     self.actorTransport = FakeTransport() // FIXME: make it real
//     self.actorAddress = FakeAddress() // FIXME: make it real
  }

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

extension D1.Message {
  func encode(to encoder: Encoder) throws { fatalError() }
  init(from decoder: Decoder) throws { fatalError() }
}

// Make sure the conformances actually happen.
func acceptActor<Act: DistributedActor>(_: Act.Type) { }

func testConformance() {
  acceptActor(D1.self)
}
