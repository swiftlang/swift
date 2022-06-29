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

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

protocol Greeting: DistributedActor {
  distributed func greeting() -> String // async throw
}

extension Greeting {
  func greetLocal(name: String) {
    print("\(greeting()), \(name)!") // okay, we're on the actor
  }
}

extension Greeting where SerializationRequirement == Codable {
  // okay, uses Codable to transfer arguments.
  distributed func greetDistributed(name: String) {
    // okay, we're on the actor
    greetLocal(name: name)
  }
}

extension Greeting where SerializationRequirement == Codable {
  nonisolated func greetAliceALot() async throws {
    try await greetDistributed(name: "Alice") // okay, via Codable
    let rawGreeting = try await greeting() // okay, via Self's serialization requirement
//    greetLocal(name: "Alice") // expected-error{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}
  }
}

distributed actor Greeter: Greeting {
  distributed func direct() -> String { "direct" }
  distributed func greeting() -> String {
    "Hello"
  }
}

// 1. Teach SILWitnessVisitor to add the asDistributed version of the SILDeclRef for
//   distributed functions. See addAutoDiffDerivativeMethodsIfRequired for an approach to doing this.
//
// 2. SILGenWitnessTable will need to learn to distinction between the distributed and
//   non-distributed SILDeclRefs and pick the appropriate witnesses.
//
// 3. Make sure that SILGenApply.cpp sets the asDistributed bit in the SILDeclRef when
//   calling a distributed func in a DA protocol, even though there is no witness thunk declaration.

func test() async throws {
  let system = DefaultDistributedActorSystem()
  let g = Greeter(actorSystem: system)

  let direct = try await g.greeting()
  print("direct(): \(direct)") // CHECK: direct(): direct
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
