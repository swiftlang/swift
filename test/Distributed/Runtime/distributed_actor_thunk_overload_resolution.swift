// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name main -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// https://github.com/swiftlang/swift/issues/92795
// The distributed thunk must call the exact method it was synthesized for,
// not whichever overload the type checker prefers in the (async) thunk body.
// The overloads here deliberately do not forward to the method, so a wrong
// pick shows up as a FileCheck mismatch instead of infinite recursion

import Distributed

protocol Echoing: DistributedActor where ActorSystem == LocalTestingDistributedActorSystem {
  distributed func echo(_ text: String) async throws -> String
}

protocol GenericEchoing: DistributedActor where ActorSystem == LocalTestingDistributedActorSystem {
  distributed func generic<T: Codable & Sendable>(_ value: T) async -> String
}

distributed actor Echo: Echoing, GenericEchoing {
  typealias ActorSystem = LocalTestingDistributedActorSystem

  // The reported case: an async overload matching through a defaulted param
  distributed func echo(_ text: String) throws -> String {
    "echo(_:)"
  }

  distributed func echo(_ text: String, loudly: Bool = false) async throws -> String {
    "echo(_:loudly:)"
  }

  distributed func generic<T: Codable & Sendable>(_ value: T) -> String {
    "generic(_:)"
  }

  func generic<T: Codable & Sendable>(_ value: T, extra: Int = 0) async -> String {
    "generic(_:extra:)"
  }
}

func callEcho(_ echo: some Echoing) async throws -> String {
  try await echo.echo("hello")
}

func callGeneric(_ echo: some GenericEchoing) async throws -> String {
  try await echo.generic(42)
}

@main struct Main {
  static func main() async throws {
    let system = LocalTestingDistributedActorSystem()
    let echo = Echo(actorSystem: system)

    // CHECK: echo: echo(_:)
    print("echo: \(try await callEcho(echo))")

    // CHECK: echo loudly: echo(_:loudly:)
    print("echo loudly: \(try await echo.echo("hello", loudly: true))")

    // CHECK: generic: generic(_:)
    print("generic: \(try await callGeneric(echo))")

    // CHECK: resolved echo: echo(_:)
    let resolved = try Echo.resolve(id: echo.id, using: system)
    print("resolved echo: \(try await callEcho(resolved))")
  }
}
