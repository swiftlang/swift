// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeCodableForDistributedTests.swiftmodule -module-name FakeCodableForDistributedTests -disable-availability-checking %S/../Inputs/FakeCodableForDistributedTests.swift
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// XXX: %target-build-swift -emit-silgen -module-name main -Xfrontend -enable-experimental-distributed -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeCodableForDistributedTests.swift %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -Xfrontend -enable-experimental-distributed -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeCodableForDistributedTests.swift %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Distributed
import FakeDistributedActorSystems
import FakeCodableForDistributedTests

distributed actor Worker: CustomStringConvertible {
  nonisolated var description: Swift.String {
    "Worker(\(id))"
  }
}

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

// ==== Execute ----------------------------------------------------------------
@main struct Main {
  static func main() async {
    let system = DefaultDistributedActorSystem()

    let actor = Worker(actorSystem: system)
    // CHECK: assign id: ActorAddress(address: "<unique-id>") for Worker

    // compilation check:
    let _: Encodable = actor
    let _: Decodable = actor

    // round trip check:
    let json = FakeEncoder()
    let encoded = try! json.encode(actor)
    print("encoded = \(encoded)")
    // CHECK: encoded = <unique-id>;

    let decoder = FakeDecoder()
    decoder.userInfo[.actorSystemKey] = system
    let back = try! decoder.decode(encoded, as: Worker.self)
    // CHECK: | resolve ActorAddress(address: "<unique-id>")

    print("back = \(back)")
    // CHECK: back = Worker(ActorAddress(address: "<unique-id>"))
  }
}
