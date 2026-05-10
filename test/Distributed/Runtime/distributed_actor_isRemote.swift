// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main  -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Distributed
import FakeDistributedActorSystems

distributed actor SomeSpecificDistributedActor {
  distributed func hello() async throws -> String {
    "local impl"
  }
}

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== Execute ----------------------------------------------------------------

func test_remote() async {
  let address = ActorAddress(parse: "sact://127.0.0.1/example#1234")
  let system = DefaultDistributedActorSystem()

  let local = SomeSpecificDistributedActor(actorSystem: system)
  assert(__isLocalActor(local) == true, "should be local")
  assert(__isRemoteActor(local) == false, "should be local")
  print("isRemote(local) = \(__isRemoteActor(local))") // CHECK: isRemote(local) = false
  print("local.id = \(local.id)") // CHECK: local.id = ActorAddress(address: "xxx")
  print("local.system = \(local.actorSystem)") // CHECK: local.system = FakeActorSystem()

  // assume it always makes a remote one
  let remote = try! SomeSpecificDistributedActor.resolve(id: address, using: system)
  assert(__isLocalActor(remote) == false, "should be remote")
  assert(__isRemoteActor(remote) == true, "should be remote")
  print("isRemote(remote) = \(__isRemoteActor(remote))") // CHECK: isRemote(remote) = true

  // Check the id and system are the right values, and not trash memory
  print("remote.id = \(remote.id)") // CHECK: remote.id = ActorAddress(address: "sact://127.0.0.1/example#1234")
  print("remote.system = \(remote.actorSystem)") // CHECK: remote.system = FakeActorSystem()

  print("done") // CHECK: done
}

@main struct Main {
  static func main() async {
    await test_remote()
  }
}

