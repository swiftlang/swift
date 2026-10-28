// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.0-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift -plugin-path %swift-plugin-dir
// RUN: %target-build-swift -module-name main -target %target-swift-6.0-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -plugin-path %swift-plugin-dir -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc

// The distributed thunks of the '$Greeter' stub only contain the remote branch,
// make sure calls through 'any Greeter' and 'some Greeter' still reach the system,
// and that calling into a locally initialized stub traps with the same message
// the stub bodies use

import StdlibUnittest
import Distributed
import FakeDistributedActorSystems

@Resolvable
protocol Greeter: DistributedActor where ActorSystem == FakeRoundtripActorSystem {
  distributed func greet(name: String) -> String
  distributed func ping()
  distributed var count: Int { get }
}

distributed actor GreeterImpl: Greeter {
  distributed func greet(name: String) -> String {
    "Hello, \(name)!"
  }

  distributed func ping() {
    print("ping on \(Self.self)")
  }

  distributed var count: Int {
    42
  }
}

func callSome(_ greeter: some Greeter) async throws -> String {
  try await greeter.greet(name: "some")
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("ResolvableStubRemoteOnlyThunk")

    tests.test("calls on a resolved stub reach the actor system") {
      let system = FakeRoundtripActorSystem()
      let real = GreeterImpl(actorSystem: system)

      let greeter: any Greeter = try! $Greeter.resolve(id: real.id, using: system)

      let reply = try! await greeter.greet(name: "any")
      // CHECK: >> remoteCall: on:main.$Greeter, target:main.$Greeter.greet(name:)
      expectEqual(reply, "Hello, any!")

      let someReply = try! await callSome(greeter)
      // CHECK: >> remoteCall: on:main.$Greeter, target:main.$Greeter.greet(name:)
      expectEqual(someReply, "Hello, some!")

      try! await greeter.ping()
      // CHECK: >> remoteCallVoid: on:main.$Greeter, target:main.$Greeter.ping()
      // CHECK: ping on GreeterImpl

      let count = try! await greeter.count
      // CHECK: >> remoteCall: on:main.$Greeter, target:main.$Greeter.count
      expectEqual(count, 42)
    }

    tests.test("calling a distributed func on a local stub traps") {
      let system = FakeRoundtripActorSystem()
      let stub = $Greeter(actorSystem: system)
      expectCrashLater(withMessage: "Unexpected invocation of distributed method 'greet(name:)' stub!")
      _ = try? await stub.greet(name: "local")
    }

    tests.test("reading a distributed var on a local stub traps") {
      let system = FakeRoundtripActorSystem()
      let stub = $Greeter(actorSystem: system)
      expectCrashLater(withMessage: "Unexpected invocation of distributed method 'count' stub!")
      _ = try? await stub.count
    }

    await runAllTestsAsync()
  }
}

// CHECK: ResolvableStubRemoteOnlyThunk: All tests passed
