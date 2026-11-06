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

// Generic requirements of a @Resolvable protocol called through the '$P' stub.
//
// The caller passes the generic substitutions of the stub's thunk, which start
// with 'Self := $Greeter', while the recipient's requirement accessor gets
// 'Self' from the actual actor. The accessor must load each generic argument
// from its position in the runtime buffers rather than positionally

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

struct Payload: Codable, Sendable, CustomStringConvertible {
  let number: Int
  let text: String
  var description: String { "Payload(\(number), \(text))" }
}

@Resolvable
protocol Greeter: DistributedActor where ActorSystem == FakeRoundtripActorSystem {
  distributed func name() -> String
  distributed func echo<T: Codable & Sendable>(_ value: T) -> T
  distributed func describe<A: Codable & Sendable, B: Codable & Sendable>(_ a: A, _ b: B) -> String
  distributed func friendTagged<T: Codable & Sendable>(_ tag: T) -> any Greeter
}

distributed actor GreeterImpl: Greeter {
  let _name: String
  let buddy: GreeterImpl?

  init(name: String, buddy: GreeterImpl?, actorSystem: ActorSystem) {
    self._name = name
    self.buddy = buddy
    self.actorSystem = actorSystem
  }

  distributed func name() -> String { _name }

  distributed func echo<T: Codable & Sendable>(_ value: T) -> T { value }

  distributed func describe<A: Codable & Sendable, B: Codable & Sendable>(_ a: A, _ b: B) -> String {
    "\(_name) got \(A.self)=\(a) and \(B.self)=\(b)"
  }

  distributed func friendTagged<T: Codable & Sendable>(_ tag: T) -> any Greeter {
    buddy ?? self
  }
}

@main struct Main {
  static func main() async throws {
    let system = FakeRoundtripActorSystem()
    let bob = GreeterImpl(name: "bob", buddy: nil, actorSystem: system)
    let alice = GreeterImpl(name: "alice", buddy: bob, actorSystem: system)

    print("--- concrete")
    let concrete = try GreeterImpl.resolve(id: alice.id, using: system)
    print("echo Int: \(try await concrete.echo(42))")
    // CHECK-LABEL: --- concrete
    // CHECK: echo Int: 42

    print("--- through stub")
    let stub: any Greeter = try $Greeter.resolve(id: alice.id, using: system)

    print("echo Int: \(try await stub.echo(42))")
    // CHECK-LABEL: --- through stub
    // CHECK: echo Int: 42

    print("echo String: \(try await stub.echo("hello"))")
    // CHECK: echo String: hello

    print("echo Payload: \(try await stub.echo(Payload(number: 7, text: "seven")))")
    // CHECK: echo Payload: Payload(7, seven)

    print("describe: \(try await stub.describe(1, "two"))")
    // CHECK: describe: alice got Int=1 and String=two

    print("describe: \(try await stub.describe(Payload(number: 3, text: "three"), [1, 2]))")
    // CHECK: describe: alice got Payload=Payload(3, three) and Array<Int>=[1, 2]

    let friend = try await stub.friendTagged("tag")
    print("friendTagged: \(type(of: friend)), name: \(try await friend.name())")
    // CHECK: friendTagged: $Greeter, name: bob

    print("--- done")
    // CHECK: --- done
  }
}
