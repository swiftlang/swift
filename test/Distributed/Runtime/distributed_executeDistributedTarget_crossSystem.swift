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

// A distributed actor pinned to a *different* ActorSystem whose associated
// InvocationDecoder and ResultHandler types differ from the receiving system.
distributed actor OtherSystemGreeter {
  typealias ActorSystem = FakeCustomSerializationRoundtripActorSystem

  distributed func hello() {}
}

@main
struct Main {
  static func main() async {
    let system = FakeActorSystem()
    let handler = FakeRoundtripResultHandler({ _ in }, onError: { _ in })

    // executeDistributedTarget must reject invocations on an actor whose
    // declared ActorSystem's associated decoder / result handler types do
    // not match the receiving system.

    let otherSystem = FakeCustomSerializationRoundtripActorSystem()
    let otherGreeter = OtherSystemGreeter(actorSystem: otherSystem)
    var crossSystemDecoder = FakeInvocationDecoder(args: [])
    do {
      try await system.executeDistributedTarget(
        on: otherGreeter,
        target: RemoteCallTarget("$s4main18OtherSystemGreeterC5helloyyYaKFTE"),
        invocationDecoder: &crossSystemDecoder,
        handler: handler)
      print("UNEXPECTED: cross-system returned")
    } catch let e as ExecuteDistributedTargetError {
      print("crossSystem threw errorCode=\(e.errorCode)")
    } catch {
      print("crossSystem threw: \(error)")
    }
    // CHECK: crossSystem threw errorCode=incompatibleInvocationDecoder
  }
}
