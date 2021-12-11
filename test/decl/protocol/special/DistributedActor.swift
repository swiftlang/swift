// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -disable-availability-checking -verify-ignore-unknown
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

/// Use the existential wrapper as the default actor system.
typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor D1 {
  var x: Int = 17
}

distributed actor D2 {
  // expected-error@-1{{actor 'D2' has no initializers}}
  let actorSystem: String
  // expected-error@-1{{property 'actorSystem' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
  // expected-error@-2{{invalid redeclaration of synthesized implementation for protocol requirement 'actorSystem'}}
  // expected-note@-3{{stored property 'actorSystem' without initial value prevents synthesized initializers}}
}

distributed actor D3 {
  // expected-error@-1{{protocol 'DistributedActor' is broken; cannot derive conformance for type 'D3'}}
  // expected-error@-2{{protocol 'DistributedActor' is broken; cannot derive conformance for type 'D3'}} // FIXME(distributed): duplicate errors
  // expected-error@-3{{type 'D3' does not conform to protocol 'Identifiable'}}
  // expected-error@-4{{type 'D3' does not conform to protocol 'DistributedActor'}}
  // expected-error@-5{{type 'D3' does not conform to protocol 'DistributedActor'}}

  var id: Int { 0 }
  // expected-error@-1{{property 'id' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
  // expected-error@-2{{invalid redeclaration of synthesized property 'id'}}
  // expected-note@-3{{matching requirement 'id' to this declaration inferred associated type to 'Int'}}
}

struct OtherActorIdentity: Sendable, Hashable, Codable {}

distributed actor D4 {
  // expected-error@-1{{actor 'D4' has no initializers}}
  // expected-error@-2{{type 'D4' does not conform to protocol 'DistributedActor'}} // FIXME(distributed): duplicated errors
  // expected-error@-3{{type 'D4' does not conform to protocol 'DistributedActor'}}
  // expected-error@-4{{protocol 'DistributedActor' is broken; cannot derive conformance for type 'D4'}} // FIXME(distributed): duplicated errors
  // expected-error@-5{{protocol 'DistributedActor' is broken; cannot derive conformance for type 'D4'}}
  // expected-error@-6{{type 'D4' does not conform to protocol 'Identifiable'}}
  let actorSystem: String
  // expected-error@-1{{invalid redeclaration of synthesized property 'actorSystem'}}
  // expected-error@-2{{property 'actorSystem' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
  // expected-note@-3{{stored property 'actorSystem' without initial value prevents synthesized initializers}}
  let id: OtherActorIdentity
  // expected-error@-1{{property 'id' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
  // expected-error@-2{{invalid redeclaration of synthesized property 'id'}}
  // expected-note@-3{{stored property 'id' without initial value prevents synthesized initializers}}
  // expected-note@-4{{matching requirement 'id' to this declaration inferred associated type to 'OtherActorIdentity'}}
}

protocol P1: DistributedActor {
  distributed func dist() -> String
  // expected-note@-1{{distributed instance method requirement 'dist()' declared here}}
}

distributed actor D5: P1 {
  func dist() -> String { "" }
  // expected-error@-1{{distributed actor-isolated instance method 'dist()' cannot be used to satisfy a protocol requirement}}
  // expected-note@-2{{add 'distributed' to 'dist()' to make this instance method witness the protocol requirement}}{{3-3=distributed }}
}

// ==== Tests ------------------------------------------------------------------

// Make sure the conformances have been added implicitly.
func acceptDistributedActor<Act: DistributedActor>(_: Act.Type) { }
func acceptAnyActor<Act: AnyActor>(_: Act.Type) { }

func testConformance() {
  acceptDistributedActor(D1.self)
  acceptAnyActor(D1.self)
}


// ==== Fake Transport ---------------------------------------------------------

struct ActorAddress: Sendable, Hashable, Codable {
  let address: String
  init(parse address: String) {
    self.address = address
  }
}

// global to track available IDs
var nextID: Int = 1

struct FakeActorSystem: DistributedActorSystem {
  public typealias ActorID = ActorAddress
  public typealias Invocation = FakeInvocation
  public typealias SerializationRequirement = Codable

  init() {
    print("Initialized new FakeActorSystem")
  }

  public func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor,
      Act.ID == ActorID  {
    fatalError("not implemented:\(#function)")
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor {
    let id = ActorAddress(parse: "\(nextID)")
    nextID += 1
    print("assign type:\(actorType), id:\(id)")
    return id
  }

  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor,
      Act.ID == ActorID {
    print("ready actor:\(actor), id:\(actor.id)")
  }

  func resignID(_ id: ActorID) {
    print("resign id:\(id)")
  }


  public func makeInvocation() -> Invocation {
    .init()
  }
}

public struct FakeInvocation: DistributedTargetInvocation {
  public typealias ArgumentDecoder = FakeArgumentDecoder
  public typealias SerializationRequirement = Codable

  public mutating func recordGenericSubstitution<T>(mangledType: T.Type) throws {}
  public mutating func recordArgument<Argument: SerializationRequirement>(argument: Argument) throws {}
  public mutating func recordReturnType<R: SerializationRequirement>(mangledType: R.Type) throws {}
  public mutating func recordErrorType<E: Error>(mangledType: E.Type) throws {}
  public mutating func doneRecording() throws {}

  // === Receiving / decoding -------------------------------------------------

  public mutating func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  public mutating func argumentDecoder() -> FakeArgumentDecoder { .init() }
  public mutating func decodeReturnType() throws -> Any.Type? { nil }
  public mutating func decodeErrorType() throws -> Any.Type? { nil }

  public struct FakeArgumentDecoder: DistributedTargetInvocationArgumentDecoder {
    public typealias SerializationRequirement = Codable
  }
}

