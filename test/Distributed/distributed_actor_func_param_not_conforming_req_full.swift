// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/Inputs/FakeDistributedActorSystems.swift 2> %t/output.txt || echo 'failed expectedly'
// RUN: %FileCheck %s < %t/output.txt

// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Distributed

// Notes:
// This test specifically is not just a -typecheck -verify test but attempts to generate the whole module.
// This is because we may be emitting errors but otherwise still attempt to emit a thunk for an "error-ed"
// distributed function, which would then crash in later phases of compilation when we try to get types
// of the `func` the THUNK is based on.

typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

distributed actor Service {
}

extension Service {
  distributed func boombox(_ id: Box) async throws {}
  // CHECK: parameter '' of type 'Box' in distributed instance method does not conform to serialization requirement 'Codable'

  distributed func boxIt() async throws -> Box { fatalError() }
  // CHECK: result type 'Box' of distributed instance method 'boxIt' does not conform to serialization requirement 'Codable'
}

public enum Box: Hashable { case boom }

@main struct Main {
  static func main() async {
    try? await Service(actorSystem: .init()).boombox(Box.boom)
  }
}

