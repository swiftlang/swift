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

// FIXME(distributed): Distributed has some issues on windows currently, see also: rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

protocol Key {
  static var isInteger: Bool { get }
}

distributed actor TestActor<Object> where Object: Codable & Identifiable, Object.ID: Key {
  public init(actorSystem: ActorSystem) {
    self.actorSystem = actorSystem
  }

  public distributed func handleObject(_ object: Object) async throws {
    print("self.id = \(object.id)")
  }
}

struct SomeKey: Codable, Key, Hashable {
  static var isInteger: Bool { false }
}

struct Something: Codable, Identifiable {
  typealias ID = SomeKey
  var id: SomeKey = .init()
}

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

// ==== Execute ----------------------------------------------------------------
@main struct Main {
  static func main() async {
    let system = DefaultDistributedActorSystem()

    let actor: TestActor<Something> = TestActor(actorSystem: system)
    let resolved: TestActor<Something> = try! .resolve(id: actor.id, using: system)
    try! await resolved.handleObject(Something())

    // CHECK: self.id = SomeKey()

    // CHECK: OK
    print("OK")
  }
}
