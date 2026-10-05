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

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

distributed actor Greeter {
  distributed func greet(name: String) -> String {
      "Hello, \(name)!"
  }
}

// Build a well-formed distributed-thunk mangled name whose single parameter is Swift.Int
// wrapped `depth` times in Array/Optional layers selected by the bits of `pattern`.
//
// Each pattern would demangle into a distinct nested-generic parameter type.
func syntheticTargetName(pattern: Int, depth: Int) -> String {
  var inner = "Si" // Swift.Int
  for j in 0..<depth {
    if (pattern >> j) & 1 == 1 {
      inner = "Say" + inner + "G" // Array<inner>
    } else {
      inner = inner + "Sg" // Optional<inner>
    }
  }
  return "$s4main7GreeterC1f1vy" + inner + "_tYaKFTE"
}

func test() async throws {
  let system = DefaultDistributedActorSystem()
  let local = Greeter(actorSystem: system)
  let ref = try Greeter.resolve(id: local.id, using: system)

  // Make sure normal call works ok:
  let greeting = try await ref.greet(name: "Caplin")
  print("\(greeting)")
  // CHECK: Hello, Caplin!

  let correctTargetIdentifier = "$s4main7GreeterC5greet4nameS2S_tYaKFTE"
  _ = correctTargetIdentifier
  let badModuleTargetIdentifier = "$s9BADMODULE7GreeterC5greet4nameS2S_tYaKFTE"
  // the BADMODULE is a bad module, and we won't be able to find the distributed accessor
  // this should result in a failed call, but not hang the call.

  var invocation = Greeter.ActorSystem.InvocationEncoder()
  invocation.arguments = ["BadCall"]
  invocation.returnType = String.self

  let badTarget = RemoteCallTarget(badModuleTargetIdentifier)

  do {
    // CHECK: >> remoteCall: on:main.Greeter, target:BADMODULE.Greeter.greet(name:)
    _ = try await system.remoteCall(
      on: local,
      target: badTarget,
      invocation: &invocation,
      throwing: Never.self,
      returning: String.self
    )
  } catch {
    // An unknown target is now rejected before dispatch,
    // so the error doesn't end up in onThrow in this specific implementation
    print("caught error: \(error)")
    print("call target was: \(badTarget.identifier)")
    // CHECK: caught error: ExecuteDistributedTargetError(errorCode: Distributed.ExecuteDistributedTargetError.ErrorCode.targetAccessorNotFound, message: "Failed to locate distributed function accessor")
    // CHECK: call target was: $s9BADMODULE7GreeterC5greet4nameS2S_tYaKFTE
  }
}

func testRejectUnknownTargets() async throws {
  let system = DefaultDistributedActorSystem()
  let greeter = Greeter(actorSystem: system)
  let handler = FakeRoundtripResultHandler({ _ in }, onError: { _ in })

  // 256 distinct well-formed nested-generic identifiers, none of which name a
  // real accessor, plus a handful of malformed identifiers.
  var names = [String]()
  for i in 0..<256 {
    names.append(syntheticTargetName(pattern: i, depth: 8))
  }
  names += [
    "",
    "not a mangled name",
    "$s4main7GreeterC7missingyyYaKFTE",
    "$s99999",
  ]

  var rejected = 0
  var unexpected = 0
  for name in names {
    var decoder = FakeInvocationDecoder(args: [], substitutions: [])
    do {
      try await system.executeDistributedTarget(
        on: greeter,
        target: RemoteCallTarget(name),
        invocationDecoder: &decoder,
        handler: handler)
      unexpected += 1
      print("UNEXPECTED: returned for \(name)")
    } catch let e as ExecuteDistributedTargetError
              where e.errorCode == .targetAccessorNotFound {
      rejected += 1
    } catch {
      unexpected += 1
      print("UNEXPECTED error for \(name): \(error)")
    }
  }

  print("rejected=\(rejected) unexpected=\(unexpected)")
  // CHECK: rejected=260 unexpected=0
}

@main struct Main {
  static func main() async {
    try! await test()
    try! await testRejectUnknownTargets()
  }
}
