// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

/// Use the existential wrapper as the default actor system.
typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== -----------------------------------------------------------------------
// MARK: Distributed actor protocols

protocol WrongDistFuncs {
    distributed func notDistActor() // expected-error{{'distributed' method can only be declared within 'distributed actor'}}{{5-17=}} {{25-25=: DistributedActor}}
}

protocol DistProtocol: DistributedActor {
  // FIXME(distributed): avoid issuing these warnings, these originate from the call on the DistProtocol where we marked this func as dist isolated,
  func local() -> String
  // (the note appears a few times, because we misuse the call many times)
  // expected-note@-2{{distributed actor-isolated instance method 'local()' declared here}}
  // expected-note@-3{{distributed actor-isolated instance method 'local()' declared here}}
  // expected-note@-4{{distributed actor-isolated instance method 'local()' declared here}}

  distributed func dist() -> String
  distributed func dist(string: String) -> String

  distributed func distAsync() async -> String
  distributed func distThrows() throws -> String
  distributed func distAsyncThrows() async throws -> String
}

distributed actor SpecificDist: DistProtocol {

  nonisolated func local() -> String { "hi" }

  distributed func dist() -> String { "dist!" }
  distributed func dist(string: String) -> String { string }

  distributed func distAsync() async -> String { "dist!" }
  distributed func distThrows() throws -> String { "dist!" }
  distributed func distAsyncThrows() async throws -> String { "dist!" }

  func inside() async throws {
    _ = self.local() // ok

    _ = self.dist() // ok
    _ = self.dist(string: "") // ok
    _ = await self.distAsync() // ok
    _ = try self.distThrows() // ok
    _ = try await self.distAsyncThrows() // ok
  }
}

func outside_good(dp: SpecificDist) async throws {
  _ = dp.local()

  _ = try await dp.dist() // implicit async throws
  _ = try await dp.dist(string: "") // implicit async throws
  _ = try await dp.distAsync() // implicit throws
  _ = try await dp.distThrows() // implicit async
  _ = try await dp.distAsyncThrows() // ok
}

func outside_good_generic<DP: DistProtocol>(dp: DP) async throws {
  _ = dp.local() // expected-error{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}
  _ = await dp.local() // expected-error{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}
  // the below warning is expected because we don't apply the "implicitly async" to the not-callable func
  // expected-warning@-2{{no 'async' operations occur within 'await' expression}}

  _ = try dp.local() // expected-error{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}
  // the below warning is expected because we don't apply the "implicitly throwing" to the not-callable func
  // expected-warning@-2{{no calls to throwing functions occur within 'try' expression}}

  _ = try await dp.dist() // implicit async throws
  _ = try await dp.dist(string: "") // implicit async throws
  _ = try await dp.distAsync() // implicit throws
  _ = try await dp.distThrows() // implicit async
  _ = try await dp.distAsyncThrows() // ok
}

func outside_good_ext<DP: DistProtocol>(dp: DP) async throws {
  _ = try await dp.dist() // implicit async throws
  _ = try await dp.dist(string: "") // implicit async throws
  _ = try await dp.distAsync() // implicit throws
  _ = try await dp.distThrows() // implicit async
  _ = try await dp.distAsyncThrows() // ok
}

// ==== ------------------------------------------------------------------------
// MARK: General protocols implemented by distributed actors

/// A distributed actor could only conform to this by making everything 'nonisolated':
protocol StrictlyLocal {
  func local()
  // expected-note@-1 2{{mark the protocol requirement 'local()' 'async throws' to allow actor-isolated conformances}}{{15-15= async throws}}

  func localThrows() throws
  // expected-note@-1 2{{mark the protocol requirement 'localThrows()' 'async' to allow actor-isolated conformances}}{{22-22=async }}
  
  // TODO: localAsync
}

distributed actor Nope1_StrictlyLocal: StrictlyLocal {
  func local() {}
  // expected-error@-1{{distributed actor-isolated instance method 'local()' cannot be used to satisfy nonisolated protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'local()' to make this instance method not isolated to the actor}}
  func localThrows() throws {}
  // expected-error@-1{{distributed actor-isolated instance method 'localThrows()' cannot be used to satisfy nonisolated protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'localThrows()' to make this instance method not isolated to the actor}}
}
distributed actor Nope2_StrictlyLocal: StrictlyLocal {
  distributed func local() {}
  // expected-error@-1{{actor-isolated distributed instance method 'local()' cannot be used to satisfy nonisolated protocol requirement}}
  distributed func localThrows() throws {}
  // expected-error@-1{{actor-isolated distributed instance method 'localThrows()' cannot be used to satisfy nonisolated protocol requirement}}
}
distributed actor OK_StrictlyLocal: StrictlyLocal {
  nonisolated func local() {}
  nonisolated func localThrows() throws {}
}

protocol Server {
  func send<Message: Codable>(message: Message) async throws -> String
}
actor MyServer : Server {
  func send<Message: Codable>(message: Message) throws -> String { "" }  // expected-warning{{non-sendable type 'Message' in parameter of actor-isolated instance method 'send(message:)' satisfying non-isolated protocol requirement cannot cross actor boundary}}
}

protocol AsyncThrowsAll {
  func maybe(param: String, int: Int) async throws -> Int
  // expected-note@-1{{'maybe(param:int:)' declared here}}
}

actor LocalOK_AsyncThrowsAll: AsyncThrowsAll {
  func maybe(param: String, int: Int) async throws -> Int { 1111 }
}

actor LocalOK_Implicitly_AsyncThrowsAll: AsyncThrowsAll {
  func maybe(param: String, int: Int) throws -> Int { 1111 }
}

distributed actor Nope1_AsyncThrowsAll: AsyncThrowsAll {
  func maybe(param: String, int: Int) async throws -> Int { 111 }
  // expected-error@-1{{distributed actor-isolated instance method 'maybe(param:int:)' cannot be used to satisfy nonisolated protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'maybe(param:int:)' to make this instance method not isolated to the actor}}
  // expected-note@-3{{add 'distributed' to 'maybe(param:int:)' to make this instance method satisfy the protocol requirement}}
}

distributed actor OK_AsyncThrowsAll: AsyncThrowsAll {
  distributed func maybe(param: String, int: Int) async throws -> Int { 222 }
}
distributed actor OK_Implicitly_AsyncThrowsAll: AsyncThrowsAll {
  distributed func maybe(param: String, int: Int) -> Int { 333 }
}

func testAsyncThrowsAll(p: AsyncThrowsAll,
                        dap: OK_AsyncThrowsAll,
                        dapi: OK_Implicitly_AsyncThrowsAll) async throws {
  _ = try await p.maybe(param: "", int: 0)
  _ = try await dap.maybe(param: "", int: 0)
  _ = try await dapi.maybe(param: "", int: 0)

  // Such conversion is sound:
  let pp: AsyncThrowsAll = dapi
  _ = try await pp.maybe(param: "", int: 0)
}

// ==== ------------------------------------------------------------------------
// MARK: Error cases

protocol ErrorCases: DistributedActor {
  distributed func unexpectedAsyncThrows() -> String
  // expected-note@-1{{protocol requires function 'unexpectedAsyncThrows()' with type '() -> String'; do you want to add a stub?}}
}

distributed actor BadGreeter: ErrorCases {
  // expected-error@-1{{type 'BadGreeter' does not conform to protocol 'ErrorCases'}}

  distributed func unexpectedAsyncThrows() async throws -> String { "" }
  // expected-note@-1{{candidate is 'async', but protocol requirement is not}}
}

// ==== ------------------------------------------------------------------------
// MARK: Distributed Actor requiring protocol witnessing async throws requirements

struct Salsa: Codable, Sendable {}

protocol TacoPreparation {
  func makeTacos(with salsa: Salsa) async throws
}

protocol DistributedTacoMaker: DistributedActor, TacoPreparation {
}

extension DistributedTacoMaker {
  distributed func makeTacos(with: Salsa) {}
}

extension TacoPreparation {
  distributed func makeSalsa() -> Salsa {}
  // expected-error@-1{{'distributed' method can only be declared within 'distributed actor'}}
}

distributed actor TacoWorker: DistributedTacoMaker {} // implemented in extensions

extension DistributedTacoMaker where SerializationRequirement == Codable {
  distributed func makeGreatTacos(with: Salsa) {}
}
