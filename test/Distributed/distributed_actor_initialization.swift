// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

/// Use the existential wrapper as the default actor transport.
typealias DefaultDistributedActorSystem = AnyDistributedActorSystem

distributed actor OK0 { }

distributed actor OK1 {
  var x: Int = 1
  // ok, since all fields are initialized, the constructor can be synthesized
}

// TODO(distributed): test all the FIXITs in this file

distributed actor Bad1 {
  init() {
    // expected-error@-1 {{designated distributed actor initializer 'init()' is missing required DistributedActorSystem parameter}}
  }
}

distributed actor Bad12 {
  init(x: String) {
    // expected-error@-1 {{designated distributed actor initializer 'init(x:)' is missing required DistributedActorSystem parameter}}
  }
}

distributed actor OK2 {
  var x: Int

  init(x: Int, transport: AnyDistributedActorSystem) { // ok
    self.x = x
  }
}

distributed actor Bad2 {
  var x: Int = 1

  init(transport: AnyDistributedActorSystem, too many: AnyDistributedActorSystem) {
    // expected-error@-1{{designated distributed actor initializer 'init(transport:too:)' must accept exactly one DistributedActorSystem parameter, found 2}}
  }
}

distributed actor OK3 {
  var x: Int

  init(y: Int, transport: AnyDistributedActorSystem) {
    self.x = y
  }
}

distributed actor OKMulti {

  convenience init(y: Int, transport: AnyDistributedActorSystem) { // ok
    self.init(transport: transport)
  }

}

distributed actor OKMultiDefaultValues {

  convenience init(y: Int, transport: AnyDistributedActorSystem, x: Int = 1234) { // ok
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

struct FakeActorSystem: DistributedActorSystem {
  typealias ActorID = ActorAddress

  func resolve<Act>(id: ID, as actorType: Act.Type) throws -> Act?
          where Act: DistributedActor, Act.ID == ActorID {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorAddress
          where Act: DistributedActor, Act.ID == ActorID {
    ActorAddress(parse: "")
  }

  public func actorReady<Act>(_ actor: Act)
          where Act: DistributedActor, Act.ID == ActorID {
    print("\(#function):\(actor)")
  }

  func resignID(_ id: ActorAddress) {}
}

distributed actor OKSpecificTransportType {
  typealias Transport = FakeActorSystem

  init(y: Int, transport fake: FakeActorSystem) { // ok
    // nothing
  }

}
