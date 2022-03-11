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
  // expected-error@-1{{type 'D3' does not conform to protocol 'Identifiable'}}
  // expected-error@-2{{type 'D3' does not conform to protocol 'DistributedActor'}}
  // Codable synthesis also will fail since the ID mismatch:
  // expected-error@-4{{type 'D3' does not conform to protocol 'Decodable'}}
  // expected-error@-5{{type 'D3' does not conform to protocol 'Encodable'}}

  var id: Int { 0 }
  // expected-error@-1{{property 'id' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
  // expected-error@-2{{invalid redeclaration of synthesized property 'id'}}
  // expected-note@-3{{matching requirement 'id' to this declaration inferred associated type to 'Int'}}
}

struct OtherActorIdentity: Sendable, Hashable, Codable {}

distributed actor D4 {
  // expected-error@-1{{actor 'D4' has no initializers}}
  // expected-error@-2{{type 'D4' does not conform to protocol 'DistributedActor'}}
  // expected-error@-3{{type 'D4' does not conform to protocol 'Identifiable'}}
  // Codable synthesis also will fail since the ID errors:
  // expected-error@-5{{type 'D4' does not conform to protocol 'Decodable'}}
  // expected-error@-6{{type 'D4' does not conform to protocol 'Encodable'}}

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
  public typealias InvocationDecoder = FakeInvocationDecoder
  public typealias InvocationEncoder = FakeInvocationEncoder
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


  public func makeInvocationEncoder() -> InvocationEncoder {
    .init()
  }

  func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing errorType: Err.Type,
    returning returnType: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error,
          Res: SerializationRequirement {
    print("remoteCall: on:\(actor), target:\(target), invocation:\(invocationEncoder), throwing:\(errorType), returning:\(returnType)")
    return "<REMOTE CALL>" as! Res
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing errorType: Err.Type
  ) async throws
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error {
    print("remoteCallVoid: on:\(actor), target:\(target), invocation:\(invocationEncoder), throwing:\(errorType)")
    return ()
  }
}

// === Sending / encoding -------------------------------------------------
struct FakeInvocationEncoder: DistributedTargetInvocationEncoder {
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Argument: SerializationRequirement>(_ argument: Argument) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

// === Receiving / decoding -------------------------------------------------
class FakeInvocationDecoder : DistributedTargetInvocationDecoder {
  typealias SerializationRequirement = Codable

  func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument { fatalError() }
  func decodeReturnType() throws -> Any.Type? { nil }
  func decodeErrorType() throws -> Any.Type? { nil }
}
