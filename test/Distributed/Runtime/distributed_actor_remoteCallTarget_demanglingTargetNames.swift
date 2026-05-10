// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

protocol SomeProtocol {}
extension String: SomeProtocol {}

distributed actor Greeter {
  distributed func noParams() {}
  distributed func noParamsThrows() throws {}
  distributed func noLabel(_ value: String) {}
  distributed func noLabels2(_ value: String, _ value2: String) {}
  distributed func noLabels3(_ value: String, _ value2: String, _ value3: String) {}
  distributed func oneLabel(value: String, _ value2: String, _ value3: String) {}
  distributed func parameterSingle(first: String) {}
  distributed func parameterPair(first: String, second: Int) {}
  distributed func generic<A: Codable & Sendable>(first: A) {}
  distributed func genericThree<A: Codable & Sendable & SomeProtocol>(first: A) {}
  distributed func genericThreeTwo<A: Codable & Sendable, B: Codable & SomeProtocol>(first: A, second: B) {}
}
extension Greeter {
  distributed func parameterTriple(first: String, second: Int, third: Double) {}
}

func test() async throws {
  let system = DefaultDistributedActorSystem()
  let g = Greeter(actorSystem: system)
  let greeter = try Greeter.resolve(id: g.id, using: system)

  try await greeter.noParams()
  // CHECK: >> remoteCallVoid: on:main.Greeter, target:main.Greeter.noParams()

  _ = try await greeter.parameterSingle(first: "X")
  // CHECK: >> remoteCallVoid: on:main.Greeter, target:main.Greeter.parameterSingle(first:)

  try await greeter.noLabel("")
  // CHECK: >> remoteCallVoid: on:main.Greeter, target:main.Greeter.noLabel(_:)

  try await greeter.noLabels2("", "")
  // CHECK: >> remoteCallVoid: on:main.Greeter, target:main.Greeter.noLabels2(_:_:)

  try await greeter.noLabels3("", "", "")
  // CHECK: >> remoteCallVoid: on:main.Greeter, target:main.Greeter.noLabels3(_:_:_:)

  try await greeter.oneLabel(value: "", "", "")
  // CHECK: >> remoteCallVoid: on:main.Greeter, target:main.Greeter.oneLabel(value:_:_:)

  _ = try await greeter.parameterPair(first: "X", second: 2)
  // CHECK: >> remoteCallVoid: on:main.Greeter, target:main.Greeter.parameterPair(first:second:)

  _ = try await greeter.parameterTriple(first: "X", second: 2, third: 3.0)
  // CHECK: >> remoteCallVoid: on:main.Greeter, target:main.Greeter.parameterTriple(first:second:third:)

  _ = try await greeter.generic(first: "X")
  // CHECK: >> remoteCallVoid: on:main.Greeter, target:main.Greeter.generic(first:)

  _ = try await greeter.genericThree(first: "X")
  // CHECK: >> remoteCallVoid: on:main.Greeter, target:main.Greeter.genericThree(first:)

  _ = try await greeter.genericThreeTwo(first: "X", second: "SecondValue")
  // CHECK: >> remoteCallVoid: on:main.Greeter, target:main.Greeter.genericThreeTwo(first:second:)

  print("done")
  // CHECK: done
}

@main struct Main {
  static func main() async {
    do {
      try await test()
    } catch {
      print("ERROR: \(error)")
    }
  }
}
