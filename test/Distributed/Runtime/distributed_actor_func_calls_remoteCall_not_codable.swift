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

// UNSUPPORTED: OS=windows-msvc

import Distributed

extension UInt8: CustomSerializationProtocol {
  public func toBytes() throws -> [UInt8] {
    [self]
  }
  public static func fromBytes(_ bytes: [UInt8]) throws -> Self {
    bytes.first!
  }
}

distributed actor Tester {
  typealias ActorSystem = FakeCustomSerializationRoundtripActorSystem

  distributed func echo(param: UInt8) -> UInt8 {
    param
  }
}

// ==== ------------------------------------------------------------------------

func test() async throws {
  let system = FakeCustomSerializationRoundtripActorSystem()

  let local = Tester(actorSystem: system)
  let ref = try Tester.resolve(id: local.id, using: system)

  let reply = try await ref.echo(param: 2)
  // CHECK: >> remoteCall: on:main.Tester, target:main.Tester.echo(param:), invocation:FakeCustomSerializationInvocationEncoder(genericSubs: [], arguments: [2], returnType: Optional(Swift.UInt8), errorType: nil), throwing:Swift.Never, returning:Swift.UInt8

  // CHECK: << remoteCall return: 2
  print("reply: \(reply)")
  // CHECK: reply: 2
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
