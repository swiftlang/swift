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

// https://github.com/swiftlang/swift/issues/93021
// The SerializationRequirement of FakeCustomSerializationRoundtripActorSystem,
// CustomSerializationProtocol, is a ~Copyable protocol. The values that
// conform to it are Copyable, so arguments and results roundtrip through the
// encoder, the decoder and the result handler.

import Distributed

struct Point: CustomSerializationProtocol, Equatable {
  var x: UInt8
  var y: UInt8

  func toBytes() throws -> [UInt8] {
    [x, y]
  }
  static func fromBytes(_ bytes: [UInt8]) throws -> Self {
    Point(x: bytes[0], y: bytes[1])
  }
}

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

  distributed func mirror(_ point: Point) -> Point {
    Point(x: point.y, y: point.x)
  }

  distributed func sum(_ a: UInt8, _ b: UInt8) -> UInt8 {
    a &+ b
  }
}

// ==== ------------------------------------------------------------------------

func test() async throws {
  let system = FakeCustomSerializationRoundtripActorSystem()

  let local = Tester(actorSystem: system)
  let ref = try Tester.resolve(id: local.id, using: system)

  let mirrored = try await ref.mirror(Point(x: 1, y: 2))
  // CHECK: >> remoteCall: on:main.Tester, target:main.Tester.mirror(_:), invocation:FakeCustomSerializationInvocationEncoder(genericSubs: [], arguments: [main.Point(x: 1, y: 2)], returnType: Optional(main.Point), errorType: nil), throwing:Swift.Never, returning:main.Point
  print("mirrored: \(mirrored)")
  // CHECK: mirrored: Point(x: 2, y: 1)

  let total = try await ref.sum(40, 2)
  // CHECK: >> remoteCall: on:main.Tester, target:main.Tester.sum(_:_:), invocation:FakeCustomSerializationInvocationEncoder(genericSubs: [], arguments: [40, 2], returnType: Optional(Swift.UInt8), errorType: nil), throwing:Swift.Never, returning:Swift.UInt8
  print("sum: \(total)")
  // CHECK: sum: 42
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
