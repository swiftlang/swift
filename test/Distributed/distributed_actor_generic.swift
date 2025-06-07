// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

typealias Message = Sendable & Codable

distributed actor GreeterX<A: Message> {
  distributed func generic<V: Message>(_ value: V) -> String {
    return "\(value)"
  }

  distributed func generic2<B: Message>(
    strict: Double, _ value: A, _ bs: [B]) -> String {
    return "\(value) \(bs)"
  }
}
