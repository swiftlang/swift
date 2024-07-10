// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// REQUIRES: objc_interop

import Distributed
import Foundation

actor A: NSObject { }

protocol NSSomeDelegate: NSObject {
  func willDoThings()
}

distributed actor DA: NSObject, NSSomeDelegate { // OK
  typealias ActorSystem = LocalTestingDistributedActorSystem

  private let identifier: UUID

  nonisolated public func handleAction(for identifier: UUID) {
    fatalError()
  }

  distributed public func distributedHandleAction(for identifier: UUID) async throws {
    fatalError()
  }

  public init(identifier: UUID, actorSystem: ActorSystem) {
    self.actorSystem = actorSystem
    self.identifier = identifier
  }

  nonisolated func willDoThings() {}
}

@main struct Main {
  static func main() async {
    let system = LocalTestingDistributedActorSystem()

    let worker = DA(identifier: .init(), actorSystem: system)

    print("OK") // CHECK: OK
  }
}
