// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main  -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Distributed
import FakeDistributedActorSystems

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

protocol SomeProtocol {
  static var isInteger: Bool { get }
}

distributed actor TestActor<Value> where Value: Codable & Identifiable, Value.ID: SomeProtocol {
  distributed func requirementsFromActor(value: Value) async throws {
    print("OK: \(value)")
  }
  distributed func requirementsFromActorAndMethod(value: Value) async throws where Value: VerySpecific {
    print("OK: \(value)")
  }
}

protocol VerySpecific {}

struct TheValue: Codable, Identifiable, VerySpecific {
  let id: String
  init() {
    self.id = "id"
  }
}
extension String: SomeProtocol {
  static var isInteger: Bool { false }
}

@main struct Main {
  static func main() async throws {
    let ta: TestActor<TheValue> = TestActor(actorSystem: .init())
    try await ta.requirementsFromActor(value: TheValue())
    try await ta.requirementsFromActorAndMethod(value: TheValue())
    // CHECK: OK
  }
}

// FIXME: repro testing
// export VER=5.9; swiftly install $VER && swiftly use $VER; swiftly list | grep use; swift build --build-tests; if [[ "$?" -eq 0 ]]; then echo "$VER: OK" >> ../checks; else echo "$VER: broken" >> ../checks; fi