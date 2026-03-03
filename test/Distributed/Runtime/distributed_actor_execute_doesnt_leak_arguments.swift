// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeCodableForDistributedTests.swiftmodule -module-name FakeCodableForDistributedTests -target %target-swift-5.7-abi-triple %S/../Inputs/FakeCodableForDistributedTests.swift
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// XXX: %target-build-swift -emit-silgen -module-name main -Xfrontend -enable-experimental-distributed -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeCodableForDistributedTests.swift %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -Xfrontend -enable-experimental-distributed -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeCodableForDistributedTests.swift %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Distributed
import FakeDistributedActorSystems
import FakeCodableForDistributedTests

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

class Sentinel {
  let str: String

  init(_ str: String) {
    self.str = str
    print("\(str).init: \(Unmanaged.passUnretained(self).toOpaque())")
  }

  deinit {
    print("\(str).deinit: \(Unmanaged.passUnretained(self).toOpaque())")
  }
}

struct InnerStruct1 {
  let sentinel: Sentinel
  let innerStruct2: InnerStruct2

  init() {
    self.sentinel = Sentinel("\(Self.self)")
    self.innerStruct2 = InnerStruct2()
  }
}

struct InnerStruct2 {
  let sentinel: Sentinel

  init() {
    self.sentinel = Sentinel("\(Self.self)")
  }
}

enum InnerEnum {
  case v1(String)
  case v2(InnerStruct1)
}

struct ArgumentType: Codable {
  let sentinel: Sentinel
  let value: Int
  let innerEnum: InnerEnum

  init(_ value: Int) {
    self.sentinel = Sentinel("ArgumentType")
    self.value = value
    self.innerEnum = .v2(InnerStruct1())
  }

  init(from decoder: Decoder) throws {
    self.sentinel = Sentinel("ArgumentType")
    self.value = 100
    self.innerEnum = .v2(InnerStruct1())
  }

  func encode(to encoder: Encoder) throws {
    print("ArgumentType.encode")
  }
}

distributed actor TestActor {
  public distributed func testFunc(arg: ArgumentType) {
    print("value=\(arg.value)")
  }
}

@main
struct Main {

  static func main() async throws {
    let system = DefaultDistributedActorSystem()

    let instance = TestActor(actorSystem: system)
    let resolved = try TestActor.resolve(id: instance.id, using: system)

    // CHECK: ArgumentType.init: [[P1:0x[0-9]+]]
    // CHECK: InnerStruct1.init: [[P2:0x[0-9]+]]
    // CHECK: InnerStruct2.init: [[P3:0x[0-9]+]]

    // CHECK: ArgumentType.deinit: [[P1]]
    // CHECK: InnerStruct1.deinit: [[P2]]
    // CHECK: InnerStruct2.deinit: [[P3]]

    let arg = ArgumentType(100)
    try await resolved.testFunc(arg: arg)
  }
}
