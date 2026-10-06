// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// ASan run to verify we don't read out-of-bounds:
// RUN: %if asan_runtime %{ %target-build-swift -module-name main -target %target-swift-5.7-abi-triple -j2 -parse-as-library -sanitize=address -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a-asan.out %}
// RUN: %if asan_runtime %{ %target-codesign %t/a-asan.out %}
// RUN: %if asan_runtime %{ %target-run %t/a-asan.out | %FileCheck %s %}

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Freeing a local actor whose `next` is a remote proxy must not read the
// proxy's (non-existent) stored properties

import Distributed
import FakeDistributedActorSystems

distributed actor Node {
  typealias ActorSystem = FakeActorSystem

  var next: Node?
  var payload = [1, 2, 3]
}

// The remote proxy must be referenced only by the `next` of the last local
// node, otherwise the is_unique check stops the deinit loop before it would
// read from the proxy

func makeRemote(_ id: String) -> Node {
  try! Node.resolve(id: .init(parse: id), using: FakeActorSystem())
}

func oneLocalThenRemote() async {
  let node = Node(actorSystem: FakeActorSystem())
  await node.whenLocal { $0.next = makeRemote("remote-1") }
}

func localChainThenRemote() async {
  var head = Node(actorSystem: FakeActorSystem())
  await head.whenLocal { $0.next = makeRemote("remote-2") }
  for _ in 0..<3 {
    let node = Node(actorSystem: FakeActorSystem())
    await node.whenLocal { [head] in $0.next = head }
    head = node
  }
}

@main struct Main {
  static func main() async {
    await oneLocalThenRemote()
    print("one local, then remote: done")
    // CHECK: one local, then remote: done

    await localChainThenRemote()
    print("local chain, then remote: done")
    // CHECK: local chain, then remote: done
  }
}
