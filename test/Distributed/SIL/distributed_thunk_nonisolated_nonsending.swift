// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.0-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeNonsendingActorSystems.swiftmodule -module-name FakeNonsendingActorSystems -target %target-swift-6.0-abi-triple -I %t %S/../Inputs/FakeNonsendingActorSystems.swift
// RUN: %target-swift-emit-silgen %s -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -I %t | %FileCheck %s
// REQUIRES: swift_swift_parser
// REQUIRES: concurrency
// REQUIRES: distributed

// Verify that a synthesized distributed thunk mirrors the isolation of the
// concrete actor-system's remoteCall/remoteCallVoid witness:

import Distributed
import FakeDistributedActorSystems
import FakeNonsendingActorSystems

// ==== ------------------------------------------------------------------------
// MARK: Plain distributed actors

// Hopping system: thunk stays `nonisolated` + `@concurrent`
//
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}6HopperC5greetSSyYaKFTE : $@convention(method) @async (@guaranteed Hopper) -> (@owned String, @error any Error)
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}6HopperC4pingyyYaKFTE : $@convention(method) @async (@guaranteed Hopper) -> @error any Error
distributed actor Hopper {
  typealias ActorSystem = FakeActorSystem

  distributed func greet() -> String { "hi" } // uses 'remoteCall'
  distributed func ping() {}                  // uses 'remoteCallVoid'
}

// Nonsending system: thunk becomes `@caller_isolated`
//
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}10LessHopperC5greetSSyYaKFTE : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed LessHopper) -> (@owned String, @error any Error)
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}10LessHopperC4pingyyYaKFTE : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed LessHopper) -> @error any Error
distributed actor LessHopper {
  typealias ActorSystem = FakeNonsendingActorSystem

  distributed func greet() -> String { "hi" }
  distributed func ping() {}
}

// ==== ------------------------------------------------------------------------
// MARK: A `@Resolvable protocol` with a concrete actor system`where ActorSystem == ...`.

// Legacy @concurrency remoteCall
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}9HopWorkerPAA11Distributed01_{{.*}}ActorStubRzrlE4workSSyYaKFTE : $@convention(method) @async <Self where
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}9HopWorkerPAA11Distributed01_{{.*}}ActorStubRzrlE5nudgeyyYaKFTE : $@convention(method) @async <Self where
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}9HopWorkerPAA11Distributed01_{{.*}}ActorStubRzrlE5labelSSyYaKFTE : $@convention(method) @async <Self where
//
// CHECK-DAG: sil private [transparent] [distributed_thunk] {{.*}}@$s{{.*}}10$HopWorkerCAA{{.*}}4workSSyYaKFTWTE : $@convention(witness_method: HopWorker) @async (
// CHECK-DAG: sil private [transparent] [distributed_thunk] {{.*}}@$s{{.*}}10$HopWorkerCAA{{.*}}5nudgeyyYaKFTWTE : $@convention(witness_method: HopWorker) @async (
// CHECK-DAG: sil private [transparent] [distributed_thunk] {{.*}}@$s{{.*}}10$HopWorkerCAA{{.*}}5labelSSvgTWTE : $@convention(witness_method: HopWorker) @async (
@Resolvable
@available(SwiftStdlib 6.0, *)
protocol HopWorker: DistributedActor where ActorSystem == FakeActorSystem {
  distributed func work() -> String
  distributed func nudge()
  distributed var label: String { get }
}

// New nonisolated(nonsending) remoteCall
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}13LessHopWorkerPAA11Distributed01_{{.*}}ActorStubRzrlE4workSSyYaKFTE : $@convention(method) @caller_isolated @async <Self where
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}13LessHopWorkerPAA11Distributed01_{{.*}}ActorStubRzrlE5nudgeyyYaKFTE : $@convention(method) @caller_isolated @async <Self where
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}13LessHopWorkerPAA11Distributed01_{{.*}}ActorStubRzrlE5labelSSyYaKFTE : $@convention(method) @caller_isolated @async <Self where
//
// CHECK-DAG: sil private [transparent] [distributed_thunk] {{.*}}@$s{{.*}}14$LessHopWorkerCAA{{.*}}4workSSyYaKFTWTE : $@convention(witness_method: LessHopWorker) @caller_isolated @async (
// CHECK-DAG: sil private [transparent] [distributed_thunk] {{.*}}@$s{{.*}}14$LessHopWorkerCAA{{.*}}5nudgeyyYaKFTWTE : $@convention(witness_method: LessHopWorker) @caller_isolated @async (
// CHECK-DAG: sil private [transparent] [distributed_thunk] {{.*}}@$s{{.*}}14$LessHopWorkerCAA{{.*}}5labelSSvgTWTE : $@convention(witness_method: LessHopWorker) @caller_isolated @async (
@Resolvable
@available(SwiftStdlib 6.0, *)
protocol LessHopWorker: DistributedActor where ActorSystem == FakeNonsendingActorSystem {
  distributed func work() -> String
  distributed func nudge()
  distributed var label: String { get }
}

// Legacy @concurrency remoteCall
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}13HopWorkerImplC4workSSyYaKFTE : $@convention(method) @async (@guaranteed HopWorkerImpl) -> (@owned String, @error any Error)
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}13HopWorkerImplC5nudgeyyYaKFTE : $@convention(method) @async (@guaranteed HopWorkerImpl) -> @error any Error
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}13HopWorkerImplC5labelSSyYaKFTE : $@convention(method) @async (@guaranteed HopWorkerImpl) -> (@owned String, @error any Error)
//
// CHECK-DAG: sil private [transparent] [distributed_thunk] {{.*}}@$s{{.*}}13HopWorkerImplCAA{{.*}}4workSSyYaKFTWTE : $@convention(witness_method: HopWorker) @async (
// CHECK-DAG: sil private [transparent] [distributed_thunk] {{.*}}@$s{{.*}}13HopWorkerImplCAA{{.*}}5labelSSvgTWTE : $@convention(witness_method: HopWorker) @async (
@available(SwiftStdlib 6.0, *)
distributed actor HopWorkerImpl: HopWorker {
  typealias ActorSystem = FakeActorSystem
  distributed func work() -> String { "w" }
  distributed func nudge() {}
  distributed var label: String { "l" }
}

// New nonisolated(nonsending) remoteCall
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}17LessHopWorkerImplC4workSSyYaKFTE : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed LessHopWorkerImpl) -> (@owned String, @error any Error)
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}17LessHopWorkerImplC5nudgeyyYaKFTE : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed LessHopWorkerImpl) -> @error any Error
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}17LessHopWorkerImplC5labelSSyYaKFTE : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed LessHopWorkerImpl) -> (@owned String, @error any Error)
//
// CHECK-DAG: sil private [transparent] [distributed_thunk] {{.*}}@$s{{.*}}17LessHopWorkerImplCAA{{.*}}4workSSyYaKFTWTE : $@convention(witness_method: LessHopWorker) @caller_isolated @async (
// CHECK-DAG: sil private [transparent] [distributed_thunk] {{.*}}@$s{{.*}}17LessHopWorkerImplCAA{{.*}}5nudgeyyYaKFTWTE : $@convention(witness_method: LessHopWorker) @caller_isolated @async (
// CHECK-DAG: sil private [transparent] [distributed_thunk] {{.*}}@$s{{.*}}17LessHopWorkerImplCAA{{.*}}5labelSSvgTWTE : $@convention(witness_method: LessHopWorker) @caller_isolated @async (
@available(SwiftStdlib 6.0, *)
distributed actor LessHopWorkerImpl: LessHopWorker {
  typealias ActorSystem = FakeNonsendingActorSystem
  distributed func work() -> String { "w" }
  distributed func nudge() {}
  distributed var label: String { "l" }
}

// ==== ------------------------------------------------------------------------
// MARK: 'resolvable proxy adapter' thunk
//
// That adapter is deliberately NOT `@caller_isolated`: it is invoked by the
// hand-built distributed target accessor in IRGen (GenDistributed.cpp), which
// does not pass the implicit `Builtin.ImplicitActor` leading parameter. Until
// that accessor learns the caller-isolated ABI the adapter must stay
// `@concurrent`, matching the TODO in CodeSynthesisDistributedActor.cpp.

// The ordinary caller-side thunk still follows the witness
//
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}9ProxyUserC9introduce2toSSAA13LessHopWorker_p_tYaKFTE : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor,

// The adapter takes `$LessHopWorker` and stays plain `@async`
//
// CHECK-DAG: sil hidden [distributed_proxy_adapter_thunk] {{.*}}@$s{{.*}}9ProxyUserC{{.*}}Adapter$introduce{{.*}} : $@convention(method) @async (@sil_sending @guaranteed $LessHopWorker, @guaranteed ProxyUser) -> (@owned String, @error any Error)
@available(SwiftStdlib 6.0, *)
distributed actor ProxyUser {
  typealias ActorSystem = FakeNonsendingActorSystem
  distributed func introduce(to other: any LessHopWorker) -> String { "x" }
}

// ==== ------------------------------------------------------------------------
// MARK: Generic-system distributed actor
//
// When the actor's `ActorSystem` is generic we don't know at thunk emission
// if we can use the nonsending(nonisolated) path, so we use the legacy shape.
//
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}13GenericWorkerC5greetSSyYaKFTE : $@convention(method) @async <ActorSystem where ActorSystem : DistributedActorSystem, {{.*}}> (@guaranteed GenericWorker<ActorSystem>) -> (@owned String, @error any Error)
// CHECK-DAG: sil hidden [thunk] [distributed] {{.*}}@$s{{.*}}13GenericWorkerC4pingyyYaKFTE : $@convention(method) @async <ActorSystem where ActorSystem : DistributedActorSystem, {{.*}}> (@guaranteed GenericWorker<ActorSystem>) -> @error any Error

// The thunk's remote branch reaches `remoteCall` via `witness_method` on
// `DistributedActorSystem`. The slot's ABI is fixed by the protocol -- plain
// `@async`, no implicit leading actor
//
// CHECK-DAG: witness_method $ActorSystem, #DistributedActorSystem.remoteCall {{.*}}: $@convention(witness_method: DistributedActorSystem) @async
// CHECK-DAG: witness_method $ActorSystem, #DistributedActorSystem.remoteCallVoid {{.*}}: $@convention(witness_method: DistributedActorSystem) @async
@available(SwiftStdlib 6.0, *)
distributed actor GenericWorker<ActorSystem>
    where ActorSystem: DistributedActorSystem<any Codable> {
  distributed func greet() -> String { "hi" } // uses 'remoteCall'
  distributed func ping() {}                  // uses 'remoteCallVoid'
}
