// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeCodableForDistributedTests.swiftmodule -module-name FakeCodableForDistributedTests -target %target-swift-5.7-abi-triple %S/../Inputs/FakeCodableForDistributedTests.swift
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeOptionsDistributedActorSystem.swiftmodule -module-name FakeOptionsDistributedActorSystem -target %target-swift-5.7-abi-triple -I %t %S/../Inputs/FakeOptionsDistributedActorSystem.swift %S/../Inputs/FakeOptionsDistributedActorSystem+CallOptions.swift
// RUN: %target-build-swift -module-name main -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeCodableForDistributedTests.swift %S/../Inputs/FakeOptionsDistributedActorSystem.swift %S/../Inputs/FakeOptionsDistributedActorSystem+CallOptions.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeOptionsDistributedActorSystem

typealias DefaultDistributedActorSystem = FakeOptionsActorSystem

@available(SwiftStdlib 5.7, *)
distributed actor Greeter {

  distributed func greet(_ name: String) -> String {
    "Hello \(name) from \(actorSystem.node)"
  }

  /// What the receiving node sees of an actor passed as an argument.
  distributed func inspect(_ other: Greeter) -> String {
    "received \(other.id), local: \(__isLocalActor(other)), options: \(other.callOptions?.description ?? "none")"
  }
}

@available(SwiftStdlib 5.7, *)
distributed actor Relay {
  typealias ActorSystem = FakeOptionsActorSystem

  let downstream: Greeter

  init(downstream: Greeter, actorSystem: ActorSystem) {
    self.downstream = downstream
    self.actorSystem = actorSystem
  }

  /// Derives a retrying proxy of `downstream` while handling this call.
  distributed func greetDownstream(_ name: String) async throws -> String {
    let retrying = downstream.withCallOptions { $0.automaticRetries = 2 }
    return try await retrying.greet(name)
  }
}

@available(SwiftStdlib 5.7, *)
func derivedProxies() async throws {
  print("=== derived proxies")
  let client = FakeOptionsActorSystem(node: "client")
  let server = FakeOptionsActorSystem(node: "server")
  let connection = client.connect(to: server)

  let greeter = Greeter(actorSystem: server)
  let base = try client.makeProxy(Greeter.self, for: greeter.id, over: connection)

  let retrying = base.withCallOptions {
    $0.automaticRetries = 2
    $0.label = "retrying"
  }
  print("base options: \(base.callOptions?.description ?? "none")")
  print("retrying options: \(retrying.callOptions?.description ?? "none")")
  print("same object: \(base === retrying), equal: \(base == retrying), same hash: \(base.hashValue == retrying.hashValue), set count: \(Set([base, retrying]).count)")
  print("retrying is remote: \(__isRemoteActor(retrying)), route: \(client.route(for: retrying.id))")
  // CHECK: === derived proxies
  // CHECK: base options: none
  // CHECK: retrying options: [automaticRetries: 2, label: retrying, onEvent: nil]
  // CHECK: same object: false, equal: true, same hash: true, set count: 1
  // CHECK: retrying is remote: true, route: client->server

  print(try await base.greet("Alice"))
  // CHECK: >> client: call #0 main.Greeter.greet(_:) on server/Greeter-0 over client->server, options: none
  // CHECK: wire: recipient server/Greeter-0; arguments ["Alice;"]
  // CHECK: << server: execute main.Greeter.greet(_:) on server/Greeter-0, from client
  // CHECK: Hello Alice from server

  print(try await retrying.greet("Bob"))
  // CHECK: >> client: call #1 main.Greeter.greet(_:) on server/Greeter-0 over client->server, options: [automaticRetries: 2, label: retrying, onEvent: nil]
  // CHECK: wire: recipient server/Greeter-0; arguments ["Bob;"]
  // CHECK: Hello Bob from server

  print("--- layering")
  let traced = retrying.withCallOptions {
    $0.label = "traced"
    $0.onEvent = { event in print("     \(event)") }
  }
  print("traced options: \(traced.callOptions?.description ?? "none")")
  print("retrying options: \(retrying.callOptions?.description ?? "none")")
  print(try await traced.greet("Carol"))
  // CHECK: --- layering
  // CHECK: traced options: [automaticRetries: 2, label: traced, onEvent: set]
  // CHECK: retrying options: [automaticRetries: 2, label: retrying, onEvent: nil]
  // CHECK: >> client: call #2 {{.*}} options: [automaticRetries: 2, label: traced, onEvent: set]
  // CHECK: event #2 sent
  // CHECK: << server: execute
  // CHECK: event #2 finished
  // CHECK: Hello Carol from server

  print("--- connection failures")
  // Each call meets two failing connection attempts.
  for (name, proxy) in [("base", base), ("retrying", retrying), ("traced", traced)] {
    connection.failingAttempts = 2
    do {
      print("\(name): \(try await proxy.greet("Dave"))")
    } catch {
      print("\(name) error: \(error)")
    }
  }
  // CHECK: --- connection failures
  // CHECK: >> client: call #3 {{.*}} options: none
  // CHECK-NEXT: client: connection attempt 1 to server failed
  // CHECK-NEXT: base error: connection to server failed after 1 attempt
  // CHECK: >> client: call #4 {{.*}} label: retrying
  // CHECK-NEXT: client: connection attempt 1 to server failed
  // CHECK-NEXT: client: connection attempt 2 to server failed
  // CHECK-NEXT: wire: recipient server/Greeter-0; arguments ["Dave;"]
  // CHECK: retrying: Hello Dave from server
  // CHECK: >> client: call #5 {{.*}} label: traced
  // CHECK-NEXT: client: connection attempt 1 to server failed
  // CHECK-NEXT: event #5 retrying(attempt 2)
  // CHECK-NEXT: client: connection attempt 2 to server failed
  // CHECK-NEXT: event #5 retrying(attempt 3)
  // CHECK-NEXT: wire: recipient server/Greeter-0; arguments ["Dave;"]
  // CHECK-NEXT: event #5 sent
  // CHECK: event #5 finished
  // CHECK-NEXT: traced: Hello Dave from server

  // Three failing attempts are one more than two retries can cover.
  connection.failingAttempts = 3
  do {
    _ = try await retrying.greet("Dave")
  } catch {
    print("retrying error: \(error)")
  }
  // CHECK: >> client: call #6 {{.*}} label: retrying
  // CHECK-NEXT: client: connection attempt 1 to server failed
  // CHECK-NEXT: client: connection attempt 2 to server failed
  // CHECK-NEXT: client: connection attempt 3 to server failed
  // CHECK-NEXT: retrying error: connection to server failed after 3 attempts

  print("--- options stay off the wire")
  print(try await base.inspect(retrying))
  // CHECK: --- options stay off the wire
  // CHECK: wire: recipient server/Greeter-0; arguments ["server/Greeter-0;"]
  // CHECK: received server/Greeter-0, local: true, options: none

  print("--- local actors ignore options")
  let local = Greeter(actorSystem: client)
  let localWithOptions = local.withCallOptions { $0.label = "ignored" }
  print("same object: \(local === localWithOptions), options: \(localWithOptions.callOptions?.description ?? "none")")
  // CHECK: --- local actors ignore options
  // CHECK: same object: true, options: none
}

/// Node `a` calls `relay`, whose handler derives a retrying proxy to `b`'s
/// greeter. Without `protectsDerivedIDs`, resolving the derived ID during the
/// inbound call routes it, and the proxy it came from, over `relay->a`.
@available(SwiftStdlib 5.7, *)
func deriveDuringInboundCall(protectsDerivedIDs: Bool) async throws {
  print("=== derive during inbound call, protectsDerivedIDs: \(protectsDerivedIDs)")
  let a = FakeOptionsActorSystem(node: "a")
  let relayNode = FakeOptionsActorSystem(node: "relay")
  let b = FakeOptionsActorSystem(node: "b")
  relayNode.protectsDerivedIDs = protectsDerivedIDs

  let greeterOnB = Greeter(actorSystem: b)
  let toB = try relayNode.makeProxy(
    Greeter.self, for: greeterOnB.id, over: relayNode.connect(to: b))
  let relay = Relay(downstream: toB, actorSystem: relayNode)
  let relayFromA = try a.makeProxy(
    Relay.self, for: relay.id, over: a.connect(to: relayNode))

  print("route before: \(relayNode.route(for: toB.id))")
  do {
    print(try await relayFromA.greetDownstream("Erin"))
  } catch {
    print("error: \(error)")
  }
  print("route after: \(relayNode.route(for: toB.id))")
  do {
    print("base: \(try await toB.greet("Frank"))")
  } catch {
    print("base error: \(error)")
  }
}

// CHECK: === derive during inbound call, protectsDerivedIDs: false
// CHECK: route before: relay->b
// CHECK: << relay: execute main.Relay.greetDownstream(_:) on relay/Relay-0, from a
// CHECK: | relay: resolve b/Greeter-0, route over relay->a
// CHECK: >> relay: call #0 main.Greeter.greet(_:) on b/Greeter-0 over relay->a
// CHECK: error: a has no actor b/Greeter-0
// CHECK: route after: relay->a
// CHECK: base error: a has no actor b/Greeter-0

// CHECK: === derive during inbound call, protectsDerivedIDs: true
// CHECK: route before: relay->b
// CHECK: | relay: resolve b/Greeter-0 with options, keep route relay->b
// CHECK: >> relay: call #0 main.Greeter.greet(_:) on b/Greeter-0 over relay->b
// CHECK: Hello Erin from b
// CHECK: route after: relay->b
// CHECK: base: Hello Frank from b

// CHECK: done

@available(SwiftStdlib 5.7, *)
@main struct Main {
  static func main() async throws {
    try await derivedProxies()
    try await deriveDuringInboundCall(protectsDerivedIDs: false)
    try await deriveDuringInboundCall(protectsDerivedIDs: true)
    print("done")
  }
}
