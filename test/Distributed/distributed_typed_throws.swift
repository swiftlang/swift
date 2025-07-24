// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

protocol MyError: Error, Codable { }

struct DistributedBoom: Error, Codable {
  var message: String
}

distributed actor ThrowingActor {

  distributed func nope() throws(DistributedBoom) { // expected-error{{distributed instance method cannot declare typed throw}}
    throw DistributedBoom(message: "A message!")
  }

  distributed func nope2<E: MyError>() throws(E) { // expected-error{{distributed instance method cannot declare typed throw}}
    fatalError()
  }

  distributed func ok1() throws {
    throw DistributedBoom(message: "A message!")
  }

  distributed func ok2() throws(Error) {
    fatalError()
  }

  distributed func never() throws(Never) {
    return
  }

  distributed func ok2<E: Error>() throws(E) {
    fatalError()
  }
}

distributed actor Robo<E: Error, ErrBad: MyError> {

  distributed var ok0: String {
    get async throws {}
  }

  distributed var ok1: String {
    get async throws(Error) {}
  }

  distributed var ok2: String {
    get async throws(E) {}
  }

  distributed var bad1: String {
    get async throws(ErrBad) {} // expected-error{{distributed getter cannot declare typed throw}}
  }

  distributed var bad2: String {
    get async throws(DistributedBoom) {} // expected-error{{distributed getter cannot declare typed throw}}
  }

}
