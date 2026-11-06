// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems %S/../Inputs/FakeDistributedActorSystems.swift

// RUN: %target-build-swift -module-name main -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// RUN: %target-build-swift -O -module-name main -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a-O.out
// RUN: %target-codesign %t/a-O.out
// RUN: %target-run %t/a-O.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_swift_parser

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Non-generic requirements of a @Resolvable protocol returning 'any P', called
// through the '$P' stub.
//
// Such a call is executed on the recipient by the requirement's distributed
// target accessor. Its record used to report the declared 'any Greeter' result
// type, so the synthesized 'invokeHandlerOnReturn' trapped casting
// '(any Greeter).Type' to the serialization requirement, as an existential
// does not conform to 'Codable'

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

@Resolvable
protocol Greeter: DistributedActor where ActorSystem == FakeRoundtripActorSystem {
  distributed func name() -> String
  distributed func friend() -> any Greeter
  distributed func echo(_ g: any Greeter) async throws -> any Greeter
  distributed var buddy: any Greeter { get }
}

distributed actor GreeterImpl: Greeter {
  let _name: String
  let _buddy: GreeterImpl?

  init(name: String, buddy: GreeterImpl?, actorSystem: ActorSystem) {
    self._name = name
    self._buddy = buddy
    self.actorSystem = actorSystem
  }

  distributed func name() -> String { _name }

  distributed func friend() -> any Greeter {
    _buddy ?? self
  }

  distributed func echo(_ g: any Greeter) async throws -> any Greeter {
    print("echo got: \(type(of: g))")
    return g
  }

  distributed var buddy: any Greeter {
    _buddy ?? self
  }
}

@main struct Main {
  static func main() async throws {
    let system = FakeRoundtripActorSystem()
    let bob = GreeterImpl(name: "bob", buddy: nil, actorSystem: system)
    let alice = GreeterImpl(name: "alice", buddy: bob, actorSystem: system)

    let stub: any Greeter = try $Greeter.resolve(id: alice.id, using: system)

    print("--- friend")
    // CHECK-LABEL: --- friend
    let friend = try await stub.friend()
    print("friend: \(type(of: friend)), name: \(try await friend.name())")
    // CHECK: friend: $Greeter, name: bob

    print("--- echo")
    // CHECK-LABEL: --- echo
    let echoed = try await stub.echo(bob)
    // CHECK: echo got: $Greeter
    print("echoed: \(type(of: echoed)), name: \(try await echoed.name())")
    // CHECK: echoed: $Greeter, name: bob

    print("--- buddy")
    // CHECK-LABEL: --- buddy
    let buddy = try await stub.buddy
    print("buddy: \(type(of: buddy)), name: \(try await buddy.name())")
    // CHECK: buddy: $Greeter, name: bob

    print("--- done")
    // CHECK: --- done
  }
}
