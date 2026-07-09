// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.0-abi-triple %S/../Distributed/Inputs/FakeDistributedActorSystems.swift -plugin-path %swift-plugin-dir
// RUN: %target-swift-frontend %s -O -emit-sil -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -I %t | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_swift_parser, asserts

// Verify that dead function elimination does NOT remove the synthesized
// `$distributedProxyAdapter$<base>` thunk that the IR-only distributed
// accessor dispatches through when a `distributed func` / `distributed var`
// has a `@Resolvable any P` / `some P` parameter or result. The accessor
// references the adapter thunk by name at IRGen time (no SIL-level caller),
// so DFE must treat it as alive — same reasoning that already protects
// regular distributed thunks (see dead_function_elimination_distributed.swift).

import Distributed
import FakeDistributedActorSystems

@Resolvable
protocol Greeter: DistributedActor, Codable
where ActorSystem == FakeRoundtripActorSystem {
  distributed func sendAnyGreeter(_ g: any Greeter) async throws -> String
  distributed func sendSomeGreeter(_ g: some Greeter) async throws -> String
  distributed func echoActor(_ g: any Greeter) async throws -> any Greeter
  distributed var currentSelf: any Greeter { get }
}

distributed actor GreeterImpl: Greeter {
  distributed func sendAnyGreeter(_ g: any Greeter) async throws -> String { "" }
  distributed func sendSomeGreeter(_ g: some Greeter) async throws -> String { "" }
  distributed func echoActor(_ g: any Greeter) async throws -> any Greeter { g }
  distributed var currentSelf: any Greeter {
    try! $Greeter.resolve(id: id, using: actorSystem)
  }
}

// ==== -----------------------------------------------------------------------
// On Impl actor: adapter thunks must survive DFE.

// CHECK-LABEL: GreeterImpl.$distributedProxyAdapter$sendAnyGreeter
// CHECK: sil hidden [distributed_proxy_adapter_thunk]
// CHECK-SAME: $@convention(method) @async (@sil_sending @guaranteed $Greeter, @guaranteed GreeterImpl) -> (@owned String, @error any Error)

// CHECK-LABEL: GreeterImpl.$distributedProxyAdapter$sendSomeGreeter
// CHECK: sil hidden [distributed_proxy_adapter_thunk]
// CHECK-SAME: <τ_0_0 where τ_0_0 : Greeter> (@sil_sending @guaranteed $Greeter, @guaranteed GreeterImpl) -> (@owned String, @error any Error)

// CHECK-LABEL: GreeterImpl.$distributedProxyAdapter$echoActor
// CHECK: sil hidden [distributed_proxy_adapter_thunk]
// CHECK-SAME: $@convention(method) @async (@sil_sending @guaranteed $Greeter, @guaranteed GreeterImpl) -> (@owned $Greeter, @error any Error)

// CHECK-LABEL: GreeterImpl.$distributedProxyAdapter$currentSelf
// CHECK: sil hidden [distributed_proxy_adapter_thunk]
// CHECK-SAME: $@convention(method) @async (@guaranteed GreeterImpl) -> (@owned $Greeter, @error any Error)

// ==== -----------------------------------------------------------------------

// On the protocol's `_DistributedActorStub` extension:
// adapter thunks must also survive, because
// the `$Greeter` proxy stub's witnesses dispatch through them.

// CHECK-LABEL: Greeter<>.$distributedProxyAdapter$sendAnyGreeter
// CHECK: sil hidden [distributed_proxy_adapter_thunk]
// CHECK-SAME: <Self where Self : _DistributedActorStub, Self : Greeter> (@sil_sending @guaranteed $Greeter, @guaranteed Self) -> (@owned String, @error any Error)

// CHECK-LABEL: Greeter<>.$distributedProxyAdapter$sendSomeGreeter
// CHECK: sil hidden [distributed_proxy_adapter_thunk]
// CHECK-SAME: <Self where Self : _DistributedActorStub, Self : Greeter><τ_1_0 where τ_1_0 : Greeter> (@sil_sending @guaranteed $Greeter, @guaranteed Self) -> (@owned String, @error any Error)

// CHECK-LABEL: Greeter<>.$distributedProxyAdapter$echoActor
// CHECK: sil hidden [distributed_proxy_adapter_thunk]
// CHECK-SAME: <Self where Self : _DistributedActorStub, Self : Greeter> (@sil_sending @guaranteed $Greeter, @guaranteed Self) -> (@owned $Greeter, @error any Error)

// CHECK-LABEL: Greeter<>.$distributedProxyAdapter$currentSelf
// CHECK: sil hidden [distributed_proxy_adapter_thunk]
// CHECK-SAME: <Self where Self : _DistributedActorStub, Self : Greeter> (@guaranteed Self) -> (@owned $Greeter, @error any Error)
