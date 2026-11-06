// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-6.2-abi-triple -Xfrontend -disable-availability-checking -enable-experimental-feature SendCalls -plugin-path %swift-plugin-dir -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out > %t/out.txt
// RUN: %FileCheck %s < %t/out.txt
// RUN: %FileCheck %s --check-prefix=RECIPIENT < %t/out.txt

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SendCalls

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

// A '@oneway' distributed requirement can be witnessed by a function that is
// not '@oneway'. The witness table then holds the 'oneway witness adapter' of
// the witness, a synchronous thunk like the one of a '@oneway' function:
// - a remote call is handed to 'remoteCallOneway' right away, with the
//   identifier of the witness itself, so that the recipient runs the witness
// - a local call 'send's the call of the witness to the actor
// - a '@Resolvable' stub of the protocol makes the same remote call, which the
//   recipient runs through the witness table of the actor

import Distributed
import FakeDistributedActorSystems

func waitUntil(_ done: () async throws -> Bool) async rethrows {
  for _ in 0 ..< 1_000_000 {
    if try await done() {
      return
    }
    await Task.yield()
  }
  fatalError("timed out waiting for the 'send' calls to run")
}

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

@Resolvable
protocol Worker: DistributedActor where ActorSystem == FakeRoundtripActorSystem {
  @oneway distributed func stop(_ reason: String)
  distributed func received() -> [String]
}

// The witness of 'stop' is not '@oneway'
distributed actor Plain: Worker {
  var log: [String] = []

  distributed func stop(_ reason: String) {
    log.append("stop \(reason)")
    print("[swift] Plain.stop \(reason)")
  }

  distributed func received() -> [String] { log }
}

// The witness of 'stop' is '@oneway'
distributed actor Exact: Worker {
  var log: [String] = []

  @oneway distributed func stop(_ reason: String) {
    log.append("oneway stop \(reason)")
    print("[swift] Exact.oneway stop \(reason)")
  }

  distributed func received() -> [String] { log }
}

func sendExistential(_ w: any Worker, _ reason: String) throws {
  try send w.stop(reason)
}

func sendGeneric<W: Worker>(_ w: W, _ reason: String) throws {
  try send w.stop(reason)
}

@main struct Main {
  static func main() async throws {
    FakeRecipientTargetPrinting.isEnabled = true
    // The fake roundtrip system gives all of its actors the same id, so every
    // test uses a system of its own
    print("[swift] test_local")
    let local = Plain(actorSystem: FakeRoundtripActorSystem())
    try sendExistential(local, "a")
    try sendGeneric(local, "b")
    try await waitUntil { try await local.received().count >= 2 }
    print("[swift] local received: \(try await local.received())")
    // CHECK-LABEL: [swift] test_local{{$}}
    // CHECK-NOT: remoteCallOneway
    // CHECK: [swift] local received: ["stop a", "stop b"]

    print("[swift] test_remote")
    let system = FakeRoundtripActorSystem()
    let host = Plain(actorSystem: system)
    let remote = try Plain.resolve(id: host.id, using: system)
    try sendExistential(remote, "1")
    try sendGeneric(remote, "2")
    let stub = try $Worker.resolve(id: host.id, using: system)
    try send stub.stop("3")
    try await waitUntil { try await host.received().count >= 3 }
    print("[swift] host received: \(try await host.received())")
    // The calls to the actor are made with the identifier of the witness.
    // They are 'oneway' remote calls, but their target is not '@oneway',
    // unlike the call through the '@Resolvable' stub, whose 'stop' is
    // CHECK-LABEL: [swift] test_remote
    // CHECK: >> remoteCallOneway: on:main.Plain, target:main.Plain.stop(_:), {{.*}}arguments: ["1"]{{.*}}, oneway: true, onewayTarget: false
    // CHECK: >> remoteCallOneway: on:main.Plain, target:main.Plain.stop(_:), {{.*}}arguments: ["2"]{{.*}}, oneway: true, onewayTarget: false
    // CHECK: >> remoteCallOneway: on:main.$Worker, target:main.$Worker.stop(_:), {{.*}}arguments: ["3"]{{.*}}, oneway: true, onewayTarget: true
    // The recipient sees the flags of the sender: a oneway remote call of a
    // target which is not '@oneway' for the witness, both for the stub's one
    // RECIPIENT-LABEL: [swift] test_remote
    // RECIPIENT-DAG: >> executeDistributedTarget: $s4main5PlainC4stopyySSYaKFTE oneway: true target: false
    // RECIPIENT-DAG: >> executeDistributedTarget: $s4main5PlainC4stopyySSYaKFTE oneway: true target: false
    // RECIPIENT-DAG: >> executeDistributedTarget: $s4main7$WorkerC4stopyySSYaKFTE oneway: true target: true
    // CHECK: [swift] host received: ["stop 1", "stop 2", "stop 3"]

    print("[swift] test_exact")
    let exactSystem = FakeRoundtripActorSystem()
    let exactHost = Exact(actorSystem: exactSystem)
    let exactRemote = try Exact.resolve(id: exactHost.id, using: exactSystem)
    try sendExistential(exactRemote, "x")
    try sendExistential(exactHost, "y")
    try await waitUntil { try await exactHost.received().count >= 2 }
    print("[swift] exact received: \(try await exactHost.received())")
    // CHECK-LABEL: [swift] test_exact
    // CHECK-DAG: >> remoteCallOneway: on:main.Exact, target:main.Exact.stop(_:), {{.*}}arguments: ["x"]{{.*}}, oneway: true, onewayTarget: true
    // CHECK-DAG: [swift] Exact.oneway stop x
    // CHECK-DAG: [swift] Exact.oneway stop y
    // RECIPIENT-LABEL: [swift] test_exact
    // RECIPIENT: >> executeDistributedTarget: $s4main5ExactC4stopyySSYaKFTE oneway: true target: true
    // CHECK: [swift] exact received:

    print("[swift] done")
    // CHECK: [swift] done
  }
}
