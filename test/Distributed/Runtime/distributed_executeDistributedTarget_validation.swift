// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-future-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

struct S<T: Codable>: Codable { var data: T }

distributed actor Greeter {
  // Four key generic parameters, eight protocol requirements.
  distributed func generic5<A: Codable, B: Codable, C: Codable, D: Codable>(
    a: A, b: S<B>, c: C, d: D
  ) {
    print("SHOULD NOT REACH")
  }
}

// Mangled distributed-thunk name for Greeter.generic5 in module `main`.
let generic5Name =
  "$s4main7GreeterC8generic51a1b1c1dyx_AA1SVyq_Gq0_q1_tYaKSeRzSERzSeR_SER_SeR0_SER0_SeR1_SER1_r2_lFTE"

@main
struct Main {
  static func main() async {
    let system = FakeActorSystem()
    let greeter = Greeter(actorSystem: system)
    let handler = FakeRoundtripResultHandler({ _ in }, onError: { _ in })

    // ==== -------------------------------------------------------------------
    // Check that wrong count of substitutions is rejected properly:

    var decoder = FakeInvocationDecoder(
      args: [],
      substitutions: [Int.self, String.self])
    do {
      try await system.executeDistributedTarget(
        on: greeter,
        target: RemoteCallTarget(generic5Name),
        invocationDecoder: &decoder,
        handler: handler)
      print("UNEXPECTED: returned")
    } catch let e as ExecuteDistributedTargetError {
      print("threw errorCode=\(e.errorCode)")
    } catch {
      print("threw: \(error)")
    }
    // CHECK: threw errorCode=invalidGenericSubstitutions

    // ==== -------------------------------------------------------------------
    // Check that empty substitutions is also handled explicitly:

    var emptyDecoder = FakeInvocationDecoder(args: [], substitutions: [])
    do {
      try await system.executeDistributedTarget(
        on: greeter,
        target: RemoteCallTarget(generic5Name),
        invocationDecoder: &emptyDecoder,
        handler: handler)
      print("UNEXPECTED: empty returned")
    } catch let e as ExecuteDistributedTargetError {
      print("empty threw errorCode=\(e.errorCode)")
    } catch {
      print("empty threw: \(error)")
    }

    // CHECK: empty threw errorCode=missingGenericSubstitutions
  }
}
