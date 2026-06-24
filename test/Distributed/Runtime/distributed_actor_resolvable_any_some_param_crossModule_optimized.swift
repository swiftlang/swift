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

/// Build the GreeterAPILib (defines the @Resolvable protocol).
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -parse-as-library -emit-library                                    \
// RUN:     -emit-module-path %t/GreeterAPILib.swiftmodule                     \
// RUN:     -module-name GreeterAPILib                                         \
// RUN:     -I %t                                                              \
// RUN:     -L %t                                                              \
// RUN:     -plugin-path %swift-plugin-dir                                     \
// RUN:     %t/src/GreeterAPILib.swift                                         \
// RUN:     -lFakeDistributedActorSystems                                      \
// RUN:     -enable-library-evolution                                          \
// RUN:     -O                                                                 \
// RUN:     -Xfrontend -validate-tbd-against-ir=all                            \
// RUN:     -o %t/%target-library-name(GreeterAPILib)

/// Build the GreeterImplLib (the implementing distributed actor).
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -parse-as-library -emit-library                                    \
// RUN:     -emit-module-path %t/GreeterImplLib.swiftmodule                    \
// RUN:     -module-name GreeterImplLib                                        \
// RUN:     -I %t                                                              \
// RUN:     -L %t                                                              \
// RUN:     -plugin-path %swift-plugin-dir                                     \
// RUN:     %t/src/GreeterImplLib.swift                                        \
// RUN:     -lFakeDistributedActorSystems                                      \
// RUN:     -lGreeterAPILib                                                    \
// RUN:     -enable-library-evolution                                          \
// RUN:     -O                                                                 \
// RUN:     -Xfrontend -validate-tbd-against-ir=all                            \
// RUN:     -o %t/%target-library-name(GreeterImplLib)

/// Build the client at -O too.
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

// Library construction in this layout has been flaky on Linux historically;
// macOS is what we primarily care about for this regression test.
// UNSUPPORTED: OS=linux-gnu || OS=freebsd
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: remote_run || device_run

// Cross-module, fully-optimized (`-O`, library-evolution) round-trip for a
// `@Resolvable` protocol that uses `any P` / `some P` parameters and an
// `any P` result. Exercises the recipient path through the synthesized
// `$distributedProxyAdapter$<base>` thunk, which would crash with
// "Unexpected invocation of distributed method '...' stub!" if either the
// adapter thunk or the `$Greeter` distributed-thunk witness gets DFE'd by
// the per-module optimizer.

//--- GreeterAPILib.swift

import Distributed
import FakeDistributedActorSystems

@Resolvable
@available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *)
public protocol Greeter: DistributedActor, Codable
where ActorSystem == FakeRoundtripActorSystem {
  distributed func sayHi() -> String
  distributed func sendAnyGreeter(_ g: any Greeter) async throws -> String
  distributed func sendSomeGreeter(_ g: some Greeter) async throws -> String
  distributed func echoActor(_ g: any Greeter) async throws -> any Greeter
}

//--- GreeterImplLib.swift

import GreeterAPILib

import Distributed
import FakeDistributedActorSystems

@available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *)
public distributed actor GreeterImpl: Greeter {
  public typealias ActorSystem = FakeRoundtripActorSystem

  public init(actorSystem: ActorSystem) {
    self.actorSystem = actorSystem
  }

  public distributed func sayHi() -> String { "Hi from \(self.id)" }

  public distributed func sendAnyGreeter(_ g: any Greeter) async throws -> String {
    print("sendAnyGreeter type: \(type(of: g))")
    return try await g.sayHi()
  }

  public distributed func sendSomeGreeter(_ g: some Greeter) async throws -> String {
    print("sendSomeGreeter type: \(type(of: g))")
    return try await g.sayHi()
  }

  public distributed func echoActor(_ g: any Greeter) async throws -> any Greeter {
    print("echoActor type: \(type(of: g))")
    return g
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
      let local = GreeterImpl(actorSystem: system)
      let proxy = try GreeterImpl.resolve(id: local.id, using: system)

      print("any Greeter")
      // CHECK-LABEL: any Greeter
      let r1 = try await proxy.sendAnyGreeter(local)
      // CHECK: sendAnyGreeter type: $Greeter
      print("r1: \(r1)")
      // CHECK: r1: Hi from

      print("some Greeter")
      // CHECK-LABEL: some Greeter
      let r2 = try await proxy.sendSomeGreeter(local)
      // CHECK: sendSomeGreeter type: $Greeter
      print("r2: \(r2)")
      // CHECK: r2: Hi from

      print("echo any Greeter -> any Greeter")
      // CHECK-LABEL: echo any Greeter -> any Greeter
      let echoed = try await proxy.echoActor(local)
      // CHECK: echoActor type: $Greeter
      print("echoed type: \(type(of: echoed))")
      // CHECK: echoed type: $Greeter
      let r3 = try await echoed.sayHi()
      print("r3: \(r3)")
      // CHECK: r3: Hi from

      print("DONE")
      // CHECK: DONE
    }
  }
}
