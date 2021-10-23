// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

distributed actor OK0 { }

distributed actor OK1 {
  var x: Int = 1
  // ok, since all fields are initialized, the constructor can be synthesized
}

// TODO(distributed): test all the FIXITs in this file

distributed actor Bad1 {
  init() {
    // expected-error@-1 {{designated distributed actor initializer 'init()' is missing required ActorTransport parameter}}
  }
}

distributed actor Bad12 {
  init(x: String) {
    // expected-error@-1 {{designated distributed actor initializer 'init(x:)' is missing required ActorTransport parameter}}
  }
}

distributed actor OK2 {
  var x: Int

  init(x: Int, transport: ActorTransport) { // ok
    self.x = x
  }
}

distributed actor Bad2 {
  var x: Int = 1

  init(transport: ActorTransport, too many: ActorTransport) {
    // expected-error@-1{{designated distributed actor initializer 'init(transport:too:)' must accept exactly one ActorTransport parameter, found 2}}
  }
}

distributed actor OK3 {
  var x: Int

  init(y: Int, transport: ActorTransport) {
    self.x = y
  }
}

distributed actor OKMulti {

  convenience init(y: Int, transport: ActorTransport) { // ok
    self.init(transport: transport)
  }

}

distributed actor OKMultiDefaultValues {

  convenience init(y: Int, transport: ActorTransport, x: Int = 1234) { // ok
    self.init(transport: transport)
  }

}

// ==== ------------------------------------------------------------------------
// MARK: Specific transport

struct ActorAddress: ActorIdentity {
  let address: String
  init(parse address : String) {
    self.address = address
  }
}

struct FakeTransport: ActorTransport {
  func decodeIdentity(from decoder: Decoder) throws -> AnyActorIdentity {
    fatalError("not implemented \(#function)")
  }

  func resolve<Act>(_ identity: AnyActorIdentity, as actorType: Act.Type) throws -> Act?
          where Act: DistributedActor {
    return nil
  }

  func assignIdentity<Act>(_ actorType: Act.Type) -> AnyActorIdentity
          where Act: DistributedActor {
    .init(ActorAddress(parse: ""))
  }

  public func actorReady<Act>(_ actor: Act)
          where Act: DistributedActor {
    print("\(#function):\(actor)")
  }

  func resignIdentity(_ id: AnyActorIdentity) {}
}

distributed actor OKSpecificTransportType {

  init(y: Int, transport fake: FakeTransport) { // ok
    // nothing
  }

}
