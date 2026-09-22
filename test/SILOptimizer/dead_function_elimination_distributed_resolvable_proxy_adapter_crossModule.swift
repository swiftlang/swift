// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// Build the FakeDistributedActorSystems dep module (with library evolution).
// RUN: %target-swift-frontend -emit-module                                    \
// RUN:     -emit-module-path %t/FakeDistributedActorSystems.swiftmodule       \
// RUN:     -module-name FakeDistributedActorSystems                           \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -plugin-path %swift-plugin-dir                                     \
// RUN:     -enable-library-evolution                                          \
// RUN:     %S/../Distributed/Inputs/FakeDistributedActorSystems.swift

// Build GreeterAPILib (the @Resolvable protocol module) at -O with evolution.
// RUN: %target-swift-frontend -emit-module                                    \
// RUN:     -emit-module-path %t/GreeterAPILib.swiftmodule                     \
// RUN:     -module-name GreeterAPILib                                         \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -plugin-path %swift-plugin-dir                                     \
// RUN:     -enable-library-evolution                                          \
// RUN:     -I %t                                                              \
// RUN:     -O                                                                 \
// RUN:     %t/src/GreeterAPILib.swift

// Emit SIL for GreeterAPILib at -O and verify the protocol-stub
// $distributedProxyAdapter$ thunks survive DFE.
// RUN: %target-swift-frontend -emit-sil -O                                    \
// RUN:     -module-name GreeterAPILib                                         \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -plugin-path %swift-plugin-dir                                     \
// RUN:     -enable-library-evolution                                          \
// RUN:     -I %t                                                              \
// RUN:     %t/src/GreeterAPILib.swift                                         \
// RUN:     | %FileCheck --check-prefix=CHECK-API %s

// Emit SIL for GreeterImplLib at -O and verify the impl-actor
// $distributedProxyAdapter$ thunks survive DFE.
// RUN: %target-swift-frontend -emit-sil -O                                    \
// RUN:     -module-name GreeterImplLib                                        \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -plugin-path %swift-plugin-dir                                     \
// RUN:     -enable-library-evolution                                          \
// RUN:     -I %t                                                              \
// RUN:     %t/src/GreeterImplLib.swift                                        \
// RUN:     | %FileCheck --check-prefix=CHECK-IMPL %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_swift_parser, asserts

// UNSUPPORTED: OS=windows-msvc

// Cross-module + library-evolution variant of
// dead_function_elimination_distributed_resolvable_proxy_adapter.swift.
// Verifies the synthesized `$distributedProxyAdapter$<base>` thunks survive
// per-module DFE at -O when:
//   - the @Resolvable protocol lives in one library-evolution module
//   - the implementing distributed actor lives in another
// Both ends must survive DFE: the protocol's `_DistributedActorStub`
// extension thunks (referenced by the `$Greeter` proxy stub witnesses) and
// the impl actor's own adapter thunks (called by the IRGen-emitted accessor).

//--- GreeterAPILib.swift

import Distributed
import FakeDistributedActorSystems

@Resolvable
@available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *)
public protocol Greeter: DistributedActor, Codable
where ActorSystem == FakeRoundtripActorSystem {
  distributed func sendAnyGreeter(_ g: any Greeter) async throws -> String
  distributed func sendSomeGreeter(_ g: some Greeter) async throws -> String
  distributed func echoActor(_ g: any Greeter) async throws -> any Greeter
  distributed var currentSelf: any Greeter { get }
}

// API module: adapter thunks on the protocol's _DistributedActorStub
// extension must survive DFE; the $Greeter proxy stub's witnesses
// dispatch through them.

// CHECK-API-LABEL: Greeter<>.$distributedProxyAdapter$sendAnyGreeter
// CHECK-API: [distributed_proxy_adapter_thunk]

// CHECK-API-LABEL: Greeter<>.$distributedProxyAdapter$sendSomeGreeter
// CHECK-API: [distributed_proxy_adapter_thunk]

// CHECK-API-LABEL: Greeter<>.$distributedProxyAdapter$echoActor
// CHECK-API: [distributed_proxy_adapter_thunk]

// CHECK-API-LABEL: Greeter<>.$distributedProxyAdapter$currentSelf
// CHECK-API: [distributed_proxy_adapter_thunk]

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

  public distributed func sendAnyGreeter(_ g: any Greeter) async throws -> String { "" }
  public distributed func sendSomeGreeter(_ g: some Greeter) async throws -> String { "" }
  public distributed func echoActor(_ g: any Greeter) async throws -> any Greeter { g }
  public distributed var currentSelf: any Greeter {
    try! $Greeter.resolve(id: id, using: actorSystem)
  }
}

// Impl module: adapter thunks on the implementing actor must survive DFE;
// the IR-only distributed accessor dispatches through them at IRGen time.

// CHECK-IMPL-LABEL: GreeterImpl.$distributedProxyAdapter$sendAnyGreeter
// CHECK-IMPL: [distributed_proxy_adapter_thunk]

// CHECK-IMPL-LABEL: GreeterImpl.$distributedProxyAdapter$sendSomeGreeter
// CHECK-IMPL: [distributed_proxy_adapter_thunk]

// CHECK-IMPL-LABEL: GreeterImpl.$distributedProxyAdapter$echoActor
// CHECK-IMPL: [distributed_proxy_adapter_thunk]

// CHECK-IMPL-LABEL: GreeterImpl.$distributedProxyAdapter$currentSelf
// CHECK-IMPL: [distributed_proxy_adapter_thunk]
