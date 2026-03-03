// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main  -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

struct Outer: Sendable, Codable {
  distributed actor Greeter<Element> where Element: Sendable & Codable {
    distributed func hello() -> String {
      return "Hello, \(Element.self)!"
    }
  }
}

struct OuterGeneric<Element>: Sendable, Codable where Element: Sendable & Codable {
  distributed actor Greeter {
    distributed func hello() -> String {
      return "Hello, \(Element.self)!"
    }
  }
}

func test() async throws {
  let system = DefaultDistributedActorSystem()

  do {
    let local = Outer.Greeter<String>(actorSystem: system)
    let ref = try Outer.Greeter<String>.resolve(id: local.id, using: system)
    let response = try await ref.hello()
    // CHECK: > encode generic sub: Swift.String
    // CHECK: >> remoteCall: on:main.Outer.Greeter<Swift.String>, target:Greeter.hello(), invocation:FakeInvocationEncoder(genericSubs: [Swift.String], arguments: [], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String
    print("response 1: \(response)")
    // CHECK: response 1: Hello, String!
  }

  do {
    let local = OuterGeneric<String>.Greeter(actorSystem: system)
    let ref = try OuterGeneric<String>.Greeter.resolve(id: local.id, using: system)
    let response = try await ref.hello()
    // CHECK: > encode generic sub: Swift.String
    // CHECK: >> remoteCall: on:main.OuterGeneric<Swift.String>.Greeter, target:Greeter.hello(), invocation:FakeInvocationEncoder(genericSubs: [Swift.String], arguments: [], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String
    print("response 2: \(response)")
    // CHECK: response 2: Hello, String!
  }



}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
