// RUN: %target-typecheck-verify-swift -swift-version 6

class NS {} // expected-note 9 {{class 'NS' does not conform to the 'Sendable' protocol}}

protocol AssocParam {
  associatedtype Value
  func process(_ value: Value) async
}

actor ActorNonSendableAssocParam: AssocParam {
  typealias Value = NS
  func process(_ value: NS) async {}
  // expected-error@-1 {{non-Sendable parameter type 'ActorNonSendableAssocParam.Value' (aka 'NS') cannot be sent from caller of protocol requirement 'process' into actor-isolated implementation}}
}

actor ActorSendableAssocParam: AssocParam {
  typealias Value = Int
  func process(_ value: Int) async {}
}

@MainActor
struct MainActorStructNonSendableAssocParam: AssocParam {
  typealias Value = NS
  func process(_ value: NS) async {}
  // expected-error@-1 {{non-Sendable parameter type 'MainActorStructNonSendableAssocParam.Value' (aka 'NS') cannot be sent from caller of protocol requirement 'process' into main actor-isolated implementation}}
}

protocol SendableConstrainedAssocParam {
  associatedtype Value: Sendable
  func process(_ value: Value) async
}

actor ActorSendableConstrainedAssocParam: SendableConstrainedAssocParam {
  typealias Value = Int
  func process(_ value: Int) async {}
}

actor ActorNonSendableConstrainedAssocParam: SendableConstrainedAssocParam { // expected-error {{type 'ActorNonSendableConstrainedAssocParam.Value' (aka 'NS') does not conform to the 'Sendable' protocol}}
  typealias Value = NS
  func process(_ value: NS) async {}
  // expected-error@-1 {{non-Sendable parameter type 'ActorNonSendableConstrainedAssocParam.Value' (aka 'NS') cannot be sent from caller of protocol requirement 'process' into actor-isolated implementation}}
}

protocol SelfParam {
  func accept(_ other: Self) async
}

actor ActorSelfParam: SelfParam {
  func accept(_ other: ActorSelfParam) async {}
}

protocol ArrayAssocParam {
  associatedtype Element
  func processAll(_ elements: [Element]) async
}

actor ActorNonSendableArrayAssocParam: ArrayAssocParam {
  typealias Element = NS
  func processAll(_ elements: [NS]) async {}
  // expected-error@-1 {{non-Sendable parameter type '[ActorNonSendableArrayAssocParam.Element]' (aka 'Array<NS>') cannot be sent from caller of protocol requirement 'processAll' into actor-isolated implementation}}
}

protocol MixedAssocParams {
  associatedtype Key
  associatedtype Payload
  func store(_ key: Key, _ payload: Payload) async
}

@MainActor
struct MainActorStructMixedAssocParams: MixedAssocParams {
  typealias Key = String
  typealias Payload = NS
  func store(_ key: String, _ payload: NS) async {}
  // expected-error@-1 {{non-Sendable parameter type 'MainActorStructMixedAssocParams.Payload' (aka 'NS') cannot be sent from caller of protocol requirement 'store' into main actor-isolated implementation}}
}

protocol GenericAndAssocParam {
  associatedtype Config
  func apply<T>(_ config: Config, to value: T) async
  // expected-note@-1 {{consider making generic parameter 'T' conform to the 'Sendable' protocol}}
}

actor ActorGenericAndAssocParam: GenericAndAssocParam {
  typealias Config = Int
  func apply<T>(_ config: Int, to value: T) async {}
  // expected-error@-1 {{non-Sendable parameter type 'T' cannot be sent from caller of protocol requirement 'apply(_:to:)' into actor-isolated implementation}}
}

struct StructWithIsolatedMethod: AssocParam {
  typealias Value = NS
  @MainActor func process(_ value: NS) async {}
  // expected-error@-1 {{non-Sendable parameter type 'StructWithIsolatedMethod.Value' (aka 'NS') cannot be sent from caller of protocol requirement 'process' into main actor-isolated implementation}}
}

protocol Entity {
  associatedtype ID
  var id: ID { get }
}

protocol Query {
  associatedtype E: Entity
  func fetch(for identifiers: [E.ID]) async
}

struct NSEntity: Entity {
  let id: NS
}

actor ActorQueryNonSendableNestedAssoc: Query {
  typealias E = NSEntity
  func fetch(for identifiers: [NS]) async {}
  // expected-error@-1 {{non-Sendable parameter type '[NS]' cannot be sent from caller of protocol requirement 'fetch(for:)' into actor-isolated implementation}}
}

protocol AssocReturn {
  associatedtype Result
  func produce() async -> Result
}

actor ActorNonSendableAssocReturn: AssocReturn {
  typealias Result = NS
  func produce() async -> NS { NS() }
  // expected-error@-1 {{non-Sendable type 'NS' cannot be returned from actor-isolated implementation to caller of protocol requirement 'produce()'}}
}
