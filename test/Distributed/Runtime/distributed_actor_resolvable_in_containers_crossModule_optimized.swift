// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the fake actor systems lib
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -parse-as-library -emit-library                                    \
// RUN:     -emit-module-path %t/FakeDistributedActorSystems.swiftmodule       \
// RUN:     -module-name FakeDistributedActorSystems                           \
// RUN:     -plugin-path %swift-plugin-dir                                     \
// RUN:      %S/../Inputs/FakeDistributedActorSystems.swift                    \
// RUN:     -enable-library-evolution                                          \
// RUN:     -O                                                                 \
// RUN:     -Xfrontend -validate-tbd-against-ir=all                            \
// RUN:     -o %t/%target-library-name(FakeDistributedActorSystems)

/// Build the GreeterAPILib (defines the @Resolvable protocol)
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -parse-as-library -emit-library                                    \
// RUN:     -emit-module-path %t/GreeterAPILib.swiftmodule                     \
// RUN:     -emit-module-interface-path %t/GreeterAPILib.swiftinterface        \
// RUN:     -module-name GreeterAPILib                                         \
// RUN:     -I %t                                                              \
// RUN:     -L %t                                                              \
// RUN:     -plugin-path %swift-plugin-dir                                     \
// RUN:     -enable-experimental-feature DistributedAnyResolvableInContainers  \
// RUN:     %t/src/GreeterAPILib.swift                                         \
// RUN:     -lFakeDistributedActorSystems                                      \
// RUN:     -enable-library-evolution                                          \
// RUN:     -O                                                                 \
// RUN:     -Xfrontend -validate-tbd-against-ir=all                            \
// RUN:     -o %t/%target-library-name(GreeterAPILib)

/// The container requirements are guarded by the feature in the interface
// RUN: %FileCheck %s --check-prefix=INTERFACE < %t/GreeterAPILib.swiftinterface

/// Build the GreeterImplLib (the implementing distributed actor)
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -parse-as-library -emit-library                                    \
// RUN:     -emit-module-path %t/GreeterImplLib.swiftmodule                    \
// RUN:     -module-name GreeterImplLib                                        \
// RUN:     -I %t                                                              \
// RUN:     -L %t                                                              \
// RUN:     -plugin-path %swift-plugin-dir                                     \
// RUN:     -enable-experimental-feature DistributedAnyResolvableInContainers  \
// RUN:     %t/src/GreeterImplLib.swift                                        \
// RUN:     -lFakeDistributedActorSystems                                      \
// RUN:     -lGreeterAPILib                                                    \
// RUN:     -enable-library-evolution                                          \
// RUN:     -O                                                                 \
// RUN:     -Xfrontend -validate-tbd-against-ir=all                            \
// RUN:     -o %t/%target-library-name(GreeterImplLib)

/// Build the client at -O too
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -parse-as-library                                                  \
// RUN:     -lFakeDistributedActorSystems                                      \
// RUN:     -lGreeterAPILib                                                    \
// RUN:     -lGreeterImplLib                                                   \
// RUN:     -module-name main                                                  \
// RUN:     -I %t                                                              \
// RUN:     -L %t                                                              \
// RUN:     -plugin-path %swift-plugin-dir                                     \
// RUN:     %t/src/Main.swift                                                  \
// RUN:     -enable-library-evolution                                          \
// RUN:     -O                                                                 \
// RUN:     -Xfrontend -validate-tbd-against-ir=all                            \
// RUN:     -o %t/a.out

// RUN: %target-codesign %t/a.out
// RUN: %target-codesign %t/%target-library-name(FakeDistributedActorSystems)
// RUN: %target-codesign %t/%target-library-name(GreeterAPILib)
// RUN: %target-codesign %t/%target-library-name(GreeterImplLib)

// RUN: %target-run %t/a.out                                                   \
// RUN:     %t/%target-library-name(FakeDistributedActorSystems)               \
// RUN:     %t/%target-library-name(GreeterAPILib)                             \
// RUN:     %t/%target-library-name(GreeterImplLib)                            \
// RUN:     2>&1                                                               \
// RUN:     | %FileCheck %s --color --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_swift_parser, asserts
// REQUIRES: swift_feature_DistributedAnyResolvableInContainers

// UNSUPPORTED: OS=linux-gnu || OS=freebsd
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: remote_run || device_run

// Cross-module, fully-optimized (`-O`, library-evolution) round-trip for a
// `@Resolvable` protocol whose requirements take and return containers of
// `any P`. The recipient goes through the `$distributedProxyAdapter$<base>`
// thunk in GreeterImplLib, the caller through the `$Greeter` stub thunks in
// GreeterAPILib

// INTERFACE: #if compiler(>=5.3) && $DistributedAnyResolvableInContainers
// INTERFACE-NEXT: distributed func friends() -> [any GreeterAPILib::Greeter]

//--- GreeterAPILib.swift

import Distributed
import FakeDistributedActorSystems

@Resolvable
@available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *)
public protocol Greeter: DistributedActor
where ActorSystem == FakeRoundtripActorSystem {
  distributed func sayHi() -> String
  distributed func friends() -> [any Greeter]
  distributed func bestFriend() -> (any Greeter)?
  distributed func friendsByName() -> [String: any Greeter]
  distributed func greetAll(_ gs: [any Greeter]) async throws -> [String]
}

//--- GreeterImplLib.swift

import GreeterAPILib

import Distributed
import FakeDistributedActorSystems

@available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *)
public distributed actor GreeterImpl: Greeter {
  public typealias ActorSystem = FakeRoundtripActorSystem

  let name: String
  var _friends: [String: GreeterImpl] = [:]

  public init(name: String, actorSystem: ActorSystem) {
    self.name = name
    self.actorSystem = actorSystem
  }

  public func befriend(_ others: [String: GreeterImpl]) {
    _friends = others
  }

  public distributed func sayHi() -> String { "Hi from \(name)" }

  public distributed func friends() -> [any Greeter] {
    _friends.keys.sorted().map { _friends[$0]! }
  }

  public distributed func bestFriend() -> (any Greeter)? {
    _friends.keys.sorted().first.map { _friends[$0]! }
  }

  public distributed func friendsByName() -> [String: any Greeter] { _friends }

  public distributed func greetAll(_ gs: [any Greeter]) async throws -> [String] {
    var result: [String] = []
    for g in gs {
      print("greetAll element type: \(type(of: g))")
      result.append(try await g.sayHi())
    }
    return result
  }
}

//--- Main.swift

import GreeterAPILib
import GreeterImplLib

import Distributed
import FakeDistributedActorSystems

@main
struct Main {
  static func main() async throws {
    if #available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *) {
      let system = FakeRoundtripActorSystem()
      let alice = GreeterImpl(name: "alice", actorSystem: system)
      let bob = GreeterImpl(name: "bob", actorSystem: system)
      let carol = GreeterImpl(name: "carol", actorSystem: system)
      await alice.whenLocal { $0.befriend(["bob": bob, "carol": carol]) }

      let proxy = try GreeterImpl.resolve(id: alice.id, using: system)

      print("--- friends")
      // CHECK-LABEL: --- friends
      let friends = try await proxy.friends()
      for f in friends {
        let hi = try await f.sayHi()
        print("friend type: \(type(of: f)), says: \(hi)")
      }
      // CHECK: friend type: $Greeter, says: Hi from bob
      // CHECK: friend type: $Greeter, says: Hi from carol

      print("--- bestFriend")
      // CHECK-LABEL: --- bestFriend
      let best = try await proxy.bestFriend()
      let bestHi = try await best!.sayHi()
      print("bestFriend type: \(type(of: best!)), says: \(bestHi)")
      // CHECK: bestFriend type: $Greeter, says: Hi from bob

      print("--- friendsByName")
      // CHECK-LABEL: --- friendsByName
      let byName = try await proxy.friendsByName()
      for key in byName.keys.sorted() {
        let hi = try await byName[key]!.sayHi()
        print("friendsByName \(key): \(hi)")
      }
      // CHECK: friendsByName bob: Hi from bob
      // CHECK: friendsByName carol: Hi from carol

      print("--- greetAll")
      // CHECK-LABEL: --- greetAll
      let greetings = try await proxy.greetAll([bob, carol])
      // CHECK: greetAll element type: $Greeter
      // CHECK: greetAll element type: $Greeter
      print("greetAll: \(greetings)")
      // CHECK: greetAll: ["Hi from bob", "Hi from carol"]

      print("--- through stub")
      // CHECK-LABEL: --- through stub
      // `friends[0]` is a `$Greeter`, so this goes through the stub thunks
      let stubFriends = try await friends[0].friends()
      print("stub friends count: \(stubFriends.count)")
      // CHECK: stub friends count: 0

      print("DONE")
      // CHECK: DONE
    }
  }
}
