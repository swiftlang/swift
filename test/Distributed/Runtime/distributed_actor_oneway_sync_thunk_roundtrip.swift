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

// 'try send' of a '@oneway' distributed function calls its synchronous
// distributed thunk, also from a synchronous function:
// - a local call runs on the actor without the caller waiting for it, in the
//   order the calls were made
// - a local call copies the task locals of the caller
// - a remote call is handed to 'remoteCallOneway' of the
//   '_DistributedOnewayActorSystem' right away, in the order the calls were
//   made, with the identifier of the function's regular 'async' thunk, and
//   the recipient executes them through the distributed target accessor of
//   that thunk
// - the same holds for an 'async' '@oneway' function, through an optional chain,
//   a generic or existential distributed protocol, a '@Resolvable' stub, a
//   distributed actor which is generic over its actor system, a '@Resolvable'
//   protocol which is generic over a '_DistributedOnewayActorSystem', and for
//   'LocalTestingDistributedActorSystem'

@_spi(OnewaySend) import Distributed
import FakeDistributedActorSystems

// Polls until 'done' returns true, and fails instead of hanging forever if the
// fire-and-forget calls never run
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

enum Locals {
  @TaskLocal static var requestID: String = "<unset>"
}

// ==== -----------------------------------------------------------------------
// MARK: Actors and protocols

@Resolvable
protocol Worker: DistributedActor where ActorSystem == FakeRoundtripActorSystem {
  @oneway distributed func stop(_ reason: String)
}

distributed actor Greeter: Worker {
  var log: [String] = []

  @oneway distributed func greet(_ name: String) {
    log.append("greet \(name)")
    print("[swift] greet \(name)")
  }

  @oneway distributed func greetLater(_ name: String) async {
    await Task.yield()
    log.append("greetLater \(name)")
    print("[swift] greetLater \(name)")
  }

  @oneway distributed func stop(_ reason: String) {
    log.append("stop \(reason)")
    print("[swift] stop \(reason)")
  }

  @oneway distributed func observe(_ tag: String) {
    log.append("\(tag): requestID=\(Locals.requestID)")
  }

  distributed func received() -> [String] { log }
}

// A distributed actor which is generic over its actor system
distributed actor GenericGreeter<ActorSystem> where ActorSystem: _DistributedOnewayActorSystem<any Codable> {
  var count = 0

  @oneway distributed func greet(_ name: String) {
    count += 1
    print("[swift] GenericGreeter.greet \(name)")
  }

  distributed func received() -> Int { count }
}

// A '@Resolvable' protocol whose actor system is only constrained to be a
// '_DistributedOnewayActorSystem'
@Resolvable
protocol Service: DistributedActor where ActorSystem: _DistributedOnewayActorSystem<any Codable> {
  @oneway distributed func shutdown(_ reason: String)
  distributed func status() -> String
}

distributed actor ServiceImpl: Service {
  typealias ActorSystem = FakeRoundtripActorSystem
  var log: [String] = []

  @oneway distributed func shutdown(_ reason: String) {
    log.append(reason)
    print("[swift] ServiceImpl.shutdown \(reason)")
  }

  distributed func status() -> String { "shutdowns: \(log)" }
}

// A local-only actor system supports '@oneway' functions too
distributed actor LocalOnly {
  typealias ActorSystem = LocalTestingDistributedActorSystem
  var count = 0

  @oneway distributed func ping(_ n: Int) {
    count += 1
    print("[swift] LocalOnly.ping \(n)")
  }

  distributed func received() -> Int { count }
}

// ==== -----------------------------------------------------------------------
// MARK: Synchronous senders

func sendGreet(_ g: Greeter, _ name: String) throws {
  try send g.greet(name)
}

func sendOptional(_ g: Greeter?, _ name: String) throws {
  try send g?.greet(name)
}

func sendGeneric<W: Worker>(_ w: W, _ reason: String) throws {
  try send w.stop(reason)
}

func sendExistential(_ w: any Worker, _ reason: String) throws {
  try send w.stop(reason)
}

// ==== -----------------------------------------------------------------------
// MARK: Tests

@main struct Main {
  static func main() async throws {
    FakeRecipientTargetPrinting.isEnabled = true
    // The fake roundtrip system gives all of its actors the same id, so every
    // test uses a system of its own
    print("[swift] test_local")
    let local = Greeter(actorSystem: FakeRoundtripActorSystem())
    try sendGreet(local, "a")
    try sendGreet(local, "b")
    try send local.greetLater("c")
    try await waitUntil { try await local.received().count >= 3 }
    print("[swift] local received: \(try await local.received())")
    // CHECK-LABEL: [swift] test_local{{$}}
    // CHECK-NOT: remoteCallOneway
    // CHECK: [swift] local received: ["greet a", "greet b", "greetLater c"]

    // The task of a local call copies the task locals of the caller, and
    // only those: the call made outside of the binding does not see it. This
    // is not checked for remote calls, where the actor system decides what
    // the recipient sees
    print("[swift] test_local_task_locals")
    let observer = Greeter(actorSystem: FakeRoundtripActorSystem())
    try Locals.$requestID.withValue("req-1") {
      try send observer.observe("bound")
    }
    try send observer.observe("unbound")
    try await waitUntil { try await observer.received().count >= 2 }
    print("[swift] observer received: \(try await observer.received())")
    // CHECK-LABEL: [swift] test_local_task_locals
    // CHECK: [swift] observer received: ["bound: requestID=req-1", "unbound: requestID=<unset>"]

    print("[swift] test_remote")
    let system = FakeRoundtripActorSystem()
    let host = Greeter(actorSystem: system)
    let remote = try Greeter.resolve(id: host.id, using: system)
    try sendGreet(remote, "1")
    try sendGreet(remote, "2")
    try send remote.greetLater("3")
    try sendOptional(remote, "4")
    try sendOptional(nil, "never")
    try await waitUntil { try await host.received().count >= 4 }
    print("[swift] host received: \(try await host.received())")
    // The calls are handed to the actor system in the order they were made,
    // and the recipient executed them in that order
    // CHECK-LABEL: [swift] test_remote
    // CHECK: >> remoteCallOneway: on:main.Greeter, target:main.Greeter.greet(_:), invocation:FakeInvocationEncoder({{.*}}arguments: ["1"]{{.*}}), oneway: true
    // CHECK: >> remoteCallOneway: on:main.Greeter, target:main.Greeter.greet(_:), invocation:FakeInvocationEncoder({{.*}}arguments: ["2"]{{.*}}), oneway: true
    // CHECK: >> remoteCallOneway: on:main.Greeter, target:main.Greeter.greetLater(_:), invocation:FakeInvocationEncoder({{.*}}arguments: ["3"]{{.*}}), oneway: true
    // CHECK: >> remoteCallOneway: on:main.Greeter, target:main.Greeter.greet(_:), invocation:FakeInvocationEncoder({{.*}}arguments: ["4"]{{.*}}), oneway: true
    // Nothing is sent through a 'nil' optional chain
    // CHECK-NOT: never
    // CHECK: [swift] host received: ["greet 1", "greet 2", "greetLater 3", "greet 4"]
    // The recipient sees the flags of the sender
    // RECIPIENT-LABEL: [swift] test_remote
    // RECIPIENT-DAG: >> executeDistributedTarget: $s4main7GreeterC5greetyySSYaKFTE oneway: true target: true
    // RECIPIENT-DAG: >> executeDistributedTarget: $s4main7GreeterC5greetyySSYaKFTE oneway: true target: true
    // RECIPIENT-DAG: >> executeDistributedTarget: $s4main7GreeterC10greetLateryySSYaKFTE oneway: true target: true
    // RECIPIENT-DAG: >> executeDistributedTarget: $s4main7GreeterC5greetyySSYaKFTE oneway: true target: true

    print("[swift] test_protocols")
    try sendGeneric(remote, "generic")
    try sendExistential(remote, "existential")
    let stub = try $Worker.resolve(id: host.id, using: system)
    try send stub.stop("stub")
    try await waitUntil { try await host.received().count >= 7 }
    print("[swift] host received: \(try await host.received())")
    // CHECK-LABEL: [swift] test_protocols
    // CHECK: >> remoteCallOneway: on:main.Greeter, target:main.Greeter.stop(_:), {{.*}}arguments: ["generic"]{{.*}}, oneway: true
    // CHECK: >> remoteCallOneway: on:main.Greeter, target:main.Greeter.stop(_:), {{.*}}arguments: ["existential"]{{.*}}, oneway: true
    // CHECK: >> remoteCallOneway: on:main.$Worker, target:main.$Worker.stop(_:), {{.*}}arguments: ["stub"]{{.*}}, oneway: true
    // CHECK: [swift] host received: ["greet 1", "greet 2", "greetLater 3", "greet 4", "stop generic", "stop existential", "stop stub"]
    // RECIPIENT-LABEL: [swift] test_protocols
    // RECIPIENT-DAG: >> executeDistributedTarget: $s4main7GreeterC4stopyySSYaKFTE oneway: true target: true
    // RECIPIENT-DAG: >> executeDistributedTarget: $s4main7GreeterC4stopyySSYaKFTE oneway: true target: true
    // RECIPIENT-DAG: >> executeDistributedTarget: $s4main7$WorkerC4stopyySSYaKFTE oneway: true target: true

    print("[swift] test_generic_actor")
    let genericSystem = FakeRoundtripActorSystem()
    let genericHost = GenericGreeter<FakeRoundtripActorSystem>(actorSystem: genericSystem)
    let genericRemote = try GenericGreeter<FakeRoundtripActorSystem>.resolve(
      id: genericHost.id, using: genericSystem)
    try send genericRemote.greet("x")
    try send genericHost.greet("y")
    try await waitUntil { try await genericHost.received() >= 2 }
    // CHECK-LABEL: [swift] test_generic_actor
    // CHECK-DAG: >> remoteCallOneway: on:main.GenericGreeter<main.FakeRoundtripActorSystem>, target:main.GenericGreeter.greet(_:), {{.*}}oneway: true
    // CHECK-DAG: [swift] GenericGreeter.greet x
    // CHECK-DAG: [swift] GenericGreeter.greet y
    // RECIPIENT-LABEL: [swift] test_generic_actor
    // RECIPIENT: >> executeDistributedTarget: $s4main14GenericGreeterC5greetyySSYaKFTE oneway: true target: true

    print("[swift] test_resolvable_generic")
    let serviceSystem = FakeRoundtripActorSystem()
    let service = ServiceImpl(actorSystem: serviceSystem)
    let serviceStub = try $Service<FakeRoundtripActorSystem>.resolve(
      id: service.id, using: serviceSystem)
    try send serviceStub.shutdown("now")
    try await waitUntil { try await serviceStub.status() != "shutdowns: []" }
    print("[swift] \(try await serviceStub.status())")
    // CHECK-LABEL: [swift] test_resolvable_generic
    // CHECK: >> remoteCallOneway: on:main.$Service<main.FakeRoundtripActorSystem>, target:main.$Service.shutdown(_:), {{.*}}oneway: true
    // CHECK: [swift] ServiceImpl.shutdown now
    // CHECK: [swift] shutdowns: ["now"]
    // RECIPIENT-LABEL: [swift] test_resolvable_generic
    // RECIPIENT: >> executeDistributedTarget: $s4main8$ServiceC8shutdownyySSYaKFTE oneway: true target: true

    print("[swift] test_local_only_system")
    let localOnly = LocalOnly(actorSystem: LocalTestingDistributedActorSystem())
    try send localOnly.ping(1)
    try send localOnly.ping(2)
    try await waitUntil { try await localOnly.received() >= 2 }
    // CHECK-LABEL: [swift] test_local_only_system
    // CHECK-NEXT: [swift] LocalOnly.ping 1
    // CHECK-NEXT: [swift] LocalOnly.ping 2

    print("[swift] done")
    // CHECK: [swift] done
  }
}
