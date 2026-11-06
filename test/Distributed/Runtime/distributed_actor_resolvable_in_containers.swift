// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems %S/../Inputs/FakeDistributedActorSystems.swift

// RUN: %target-build-swift -module-name main -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -enable-experimental-feature DistributedAnyResolvableInContainers -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// RUN: %target-build-swift -O -module-name main -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -enable-experimental-feature DistributedAnyResolvableInContainers -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a-O.out
// RUN: %target-codesign %t/a-O.out
// RUN: %target-run %t/a-O.out | %FileCheck %s
//
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_swift_parser, asserts
// REQUIRES: swift_feature_DistributedAnyResolvableInContainers
//
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Wire round-trip of distributed methods and properties taking and returning
// containers of a `@Resolvable protocol` existential: `[any P]`, `(any P)?`
// and `[K: any P]`. Every element must arrive as the `$P` stub, resolved to
// the right actor

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

@Resolvable
protocol Greeter: DistributedActor where ActorSystem == FakeRoundtripActorSystem {
  distributed func sayHi() -> String

  // Containers in requirements, so they are also called through `$Greeter`
  distributed func friends() -> [any Greeter]
  distributed func bestFriend() -> (any Greeter)?
  distributed func friendsByName() -> [String: any Greeter]
  distributed func introduce(_ others: [any Greeter]) async throws -> [String]
}

distributed actor GreeterImpl: Greeter {
  let name: String
  var _friends: [String: any Greeter] = [:]

  init(name: String, actorSystem: ActorSystem) {
    self.name = name
    self.actorSystem = actorSystem
  }

  func befriend(_ others: [String: any Greeter]) {
    _friends = others
  }

  distributed func sayHi() -> String {
    "Hi from \(name)"
  }

  distributed func friends() -> [any Greeter] {
    _friends.keys.sorted().map { _friends[$0]! }
  }

  distributed func bestFriend() -> (any Greeter)? {
    _friends.keys.sorted().first.map { _friends[$0]! }
  }

  distributed func friendsByName() -> [String: any Greeter] {
    _friends
  }

  distributed func introduce(_ others: [any Greeter]) async throws -> [String] {
    var result: [String] = []
    for other in others {
      print("introduce element type: \(type(of: other))")
      let hi = try await other.sayHi()
      result.append("\(name) meets \(hi)")
    }
    return result
  }
}

struct DirectoryError: Error {}

distributed actor Directory {
  let greeters: [String: GreeterImpl]

  init(greeters: [String: GreeterImpl], actorSystem: ActorSystem) {
    self.greeters = greeters
    self.actorSystem = actorSystem
  }

  // ==== Results

  distributed func all() -> [any Greeter] {
    greeters.keys.sorted().map { greeters[$0]! }
  }

  distributed func none() -> [any Greeter] {
    []
  }

  distributed func find(_ name: String) -> (any Greeter)? {
    greeters[name]
  }

  distributed func byName() -> [String: any Greeter] {
    greeters
  }

  distributed func allOrThrow(_ shouldThrow: Bool) throws -> [any Greeter] {
    if shouldThrow { throw DirectoryError() }
    return greeters.keys.sorted().map { greeters[$0]! }
  }

  distributed var first: (any Greeter)? {
    greeters.keys.sorted().first.map { greeters[$0]! }
  }

  distributed var everyone: [any Greeter] {
    greeters.keys.sorted().map { greeters[$0]! }
  }

  // ==== Parameters

  distributed func greetAll(_ gs: [any Greeter]) async throws -> [String] {
    var result: [String] = []
    for g in gs {
      print("greetAll element type: \(type(of: g))")
      result.append(try await g.sayHi())
    }
    return result
  }

  distributed func greetMaybe(_ g: (any Greeter)?) async throws -> String {
    guard let g else { return "nobody" }
    print("greetMaybe type: \(type(of: g))")
    return try await g.sayHi()
  }

  distributed func greetEach(_ gs: [String: any Greeter]) async throws -> [String] {
    var result: [String] = []
    for key in gs.keys.sorted() {
      print("greetEach \(key) type: \(type(of: gs[key]!))")
      result.append(try await gs[key]!.sayHi())
    }
    return result
  }

  // ==== Parameter and result, mixed with plain Codable values

  distributed func reversed(_ gs: [any Greeter], tag: String) -> [any Greeter] {
    print("reversed tag: \(tag), count: \(gs.count)")
    return gs.reversed()
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Helpers

func describe(_ label: String, _ gs: [any Greeter]) async throws {
  print("\(label) count: \(gs.count)")
  for g in gs {
    let hi = try await g.sayHi()
    print("\(label) element type: \(type(of: g)), says: \(hi)")
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Tests

func test_results(_ directory: Directory) async throws {
  print("--- results")
  // CHECK-LABEL: --- results

  try await describe("all", directory.all())
  // CHECK: all count: 2
  // CHECK: all element type: $Greeter, says: Hi from alice
  // CHECK: all element type: $Greeter, says: Hi from bob

  try await describe("none", directory.none())
  // CHECK: none count: 0

  let found = try await directory.find("bob")
  let foundHi = try await found!.sayHi()
  print("found type: \(type(of: found!)), says: \(foundHi)")
  // CHECK: found type: $Greeter, says: Hi from bob

  let missing = try await directory.find("carol")
  print("missing: \(missing == nil)")
  // CHECK: missing: true

  let byName = try await directory.byName()
  for key in byName.keys.sorted() {
    let hi = try await byName[key]!.sayHi()
    print("byName \(key) type: \(type(of: byName[key]!)), says: \(hi)")
  }
  // CHECK: byName alice type: $Greeter, says: Hi from alice
  // CHECK: byName bob type: $Greeter, says: Hi from bob

  try await describe("allOrThrow", directory.allOrThrow(false))
  // CHECK: allOrThrow count: 2
  // CHECK: allOrThrow element type: $Greeter, says: Hi from alice
  // CHECK: allOrThrow element type: $Greeter, says: Hi from bob

  do {
    _ = try await directory.allOrThrow(true)
    print("allOrThrow(true) did not throw")
  } catch {
    print("allOrThrow(true) threw: \(error)")
  }
  // CHECK: allOrThrow(true) threw: DirectoryError()

  let first = try await directory.first
  let firstHi = try await first!.sayHi()
  print("first type: \(type(of: first!)), says: \(firstHi)")
  // CHECK: first type: $Greeter, says: Hi from alice

  try await describe("everyone", directory.everyone)
  // CHECK: everyone count: 2
  // CHECK: everyone element type: $Greeter, says: Hi from alice
  // CHECK: everyone element type: $Greeter, says: Hi from bob
}

func test_params(_ directory: Directory, alice: GreeterImpl, bob: GreeterImpl) async throws {
  print("--- params")
  // CHECK-LABEL: --- params

  let greetings = try await directory.greetAll([alice, bob])
  // CHECK: greetAll element type: $Greeter
  // CHECK: greetAll element type: $Greeter
  print("greetAll: \(greetings)")
  // CHECK: greetAll: ["Hi from alice", "Hi from bob"]

  let empty = try await directory.greetAll([])
  print("greetAll empty: \(empty)")
  // CHECK: greetAll empty: []

  let maybe = try await directory.greetMaybe(bob)
  // CHECK: greetMaybe type: $Greeter
  print("greetMaybe: \(maybe)")
  // CHECK: greetMaybe: Hi from bob

  let nobody = try await directory.greetMaybe(nil)
  print("greetMaybe nil: \(nobody)")
  // CHECK: greetMaybe nil: nobody

  let each = try await directory.greetEach(["a": alice, "b": bob])
  // CHECK: greetEach a type: $Greeter
  // CHECK: greetEach b type: $Greeter
  print("greetEach: \(each)")
  // CHECK: greetEach: ["Hi from alice", "Hi from bob"]
}

func test_paramAndResult(_ directory: Directory, alice: GreeterImpl, bob: GreeterImpl) async throws {
  print("--- param and result")
  // CHECK-LABEL: --- param and result

  try await describe("reversed", directory.reversed([alice, bob], tag: "x"))
  // CHECK: reversed tag: x, count: 2
  // CHECK: reversed count: 2
  // CHECK: reversed element type: $Greeter, says: Hi from bob
  // CHECK: reversed element type: $Greeter, says: Hi from alice
}

func test_forwardReceivedProxies(_ directory: Directory) async throws {
  print("--- forward received proxies")
  // CHECK-LABEL: --- forward received proxies

  // Elements received as `$Greeter` are sent back as `[any Greeter]`
  let received = try await directory.all()
  let greetings = try await directory.greetAll(received)
  // CHECK: greetAll element type: $Greeter
  // CHECK: greetAll element type: $Greeter
  print("forwarded: \(greetings)")
  // CHECK: forwarded: ["Hi from alice", "Hi from bob"]
}

func test_throughStub(_ directory: Directory) async throws {
  print("--- through stub")
  // CHECK-LABEL: --- through stub

  // `alice` is a `$Greeter`, so these calls go through the stub's thunks
  let alice = try await directory.find("alice")!

  try await describe("friends", alice.friends())
  // CHECK: friends count: 1
  // CHECK: friends element type: $Greeter, says: Hi from bob

  let best = try await alice.bestFriend()
  let bestHi = try await best!.sayHi()
  print("bestFriend type: \(type(of: best!)), says: \(bestHi)")
  // CHECK: bestFriend type: $Greeter, says: Hi from bob

  let byName = try await alice.friendsByName()
  for key in byName.keys.sorted() {
    let hi = try await byName[key]!.sayHi()
    print("friendsByName \(key) type: \(type(of: byName[key]!)), says: \(hi)")
  }
  // CHECK: friendsByName bob type: $Greeter, says: Hi from bob

  let bob = try await directory.find("bob")!
  let intro = try await alice.introduce([bob])
  // CHECK: introduce element type: $Greeter
  print("introduce: \(intro)")
  // CHECK: introduce: ["alice meets Hi from bob"]
}

// ==== -----------------------------------------------------------------------
// MARK: Main

@main struct Main {
  static func main() async throws {
    let system = FakeRoundtripActorSystem()
    let alice = GreeterImpl(name: "alice", actorSystem: system)
    let bob = GreeterImpl(name: "bob", actorSystem: system)
    await alice.whenLocal { $0.befriend(["bob": bob]) }

    let local = Directory(
      greeters: ["alice": alice, "bob": bob], actorSystem: system)
    let directory = try Directory.resolve(id: local.id, using: system)

    try await test_results(directory)
    try await test_params(directory, alice: alice, bob: bob)
    try await test_paramAndResult(directory, alice: alice, bob: bob)
    try await test_forwardReceivedProxies(directory)
    try await test_throughStub(directory)

    print("--- done")
    // CHECK: --- done
  }
}
