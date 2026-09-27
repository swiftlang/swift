// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name main -target %target-swift-6.0-abi-triple -j2 -parse-as-library %s -plugin-path %swift-plugin-dir -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc

// A '$Greeter' stub can be initialized locally, but its distributed thunks
// have no local branch, so calling into a local stub traps with the same
// message the stub bodies use

import StdlibUnittest
import Distributed

@Resolvable
protocol Greeter: DistributedActor where ActorSystem == LocalTestingDistributedActorSystem {
  distributed func greet(name: String) -> String
  distributed var count: Int { get }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("ResolvableStubLocalInstance")

    tests.test("calling a distributed func on a local stub traps") {
      let system = LocalTestingDistributedActorSystem()
      let stub = $Greeter(actorSystem: system)
      expectCrashLater(withMessage: "Unexpected invocation of distributed method 'greet(name:)' stub!")
      _ = try? await stub.greet(name: "local")
    }

    tests.test("reading a distributed var on a local stub traps") {
      let system = LocalTestingDistributedActorSystem()
      let stub = $Greeter(actorSystem: system)
      expectCrashLater(withMessage: "Unexpected invocation of distributed method 'count' stub!")
      _ = try? await stub.count
    }

    await runAllTestsAsync()
  }
}
