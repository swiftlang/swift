// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== ------------------------------------------------------------------------

distributed actor Overloader {

  func overloaded() {}
  func overloaded() async {}

  distributed func overloadedDistA() {}
  // expected-note@-1{{'overloadedDistA()' previously declared here, cannot overload distributed methods on effect only}}
  distributed func overloadedDistA() async {}
  // expected-error@-1{{invalid redeclaration of 'overloadedDistA()'}}

  distributed func overloadedDistT() throws {}
  // expected-note@-1{{'overloadedDistT()' previously declared here, cannot overload distributed methods on effect only}}
  distributed func overloadedDistT() async throws {}
  // expected-error@-1{{invalid redeclaration of 'overloadedDistT()'}}

  distributed func overloadedDistST(string: String) throws {}
  // expected-note@-1{{'overloadedDistST(string:)' previously declared here, cannot overload distributed methods on effect only}}
  distributed func overloadedDistST(string: String) async throws {}
  // expected-error@-1{{invalid redeclaration of 'overloadedDistST(string:)'}}

  // Throws overloads are not legal anyway, but let's check for them here too:
  distributed func overloadedDistThrows() {}
  // expected-note@-1{{'overloadedDistThrows()' previously declared here}}
  distributed func overloadedDistThrows() throws {}
  // expected-error@-1{{invalid redeclaration of 'overloadedDistThrows()'}}

  distributed func overloadedDistAsync() async {}
  // expected-note@-1{{'overloadedDistAsync()' previously declared here}}
  distributed func overloadedDistAsync() async throws {}
  // expected-error@-1{{invalid redeclaration of 'overloadedDistAsync()'}}

  // overloads differing by parameter type are allowed,
  // since the mangled identifier includes full type information:
  distributed func overloadedDistParams(param: String) async {} // ok
  distributed func overloadedDistParams(param: Int) async {} // ok

  distributed func overloadedDistParams() async {} // also ok

  distributed func overloadedDistParams<A: Sendable & Codable>(param: A) async {} // ok
}

