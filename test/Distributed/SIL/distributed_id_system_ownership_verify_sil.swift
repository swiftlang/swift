// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -verify -Xllvm -sil-print-types -emit-sil -module-name main -target %target-swift-5.7-abi-triple -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_in_compiler

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

/// This type uses a non loadable (size not known at compile time) ID which forces the actor's init SILGen
/// to make use of the isAddressOnly handling of the ID assignments.
///
/// This test covers rdar://104583893 / https://github.com/apple/swift/issues/62898
/// And used to fail with:
///     SIL memory lifetime failure in @$s18DistributedCluster0B16EventStreamActorC11actorSystemAcA0bG0C_tcfc: memory is initialized, but shouldn't be
typealias DefaultDistributedActorSystem = FakeNotLoadableAddressActorSystem

distributed actor Greeter {

  init(actorSystem: ActorSystem) {
    self.actorSystem = actorSystem
  }

  distributed func echo(name: String) -> String {
    return "Echo: \(name)"
  }
}

// CHECK: sil hidden @$s4main7GreeterC7resolve2id5usingAcA23NotLoadableActorAddressV_AA04FakefgiH6SystemVtKFZ : $@convention(method) (@in_guaranteed NotLoadableActorAddress, @guaranteed FakeNotLoadableAddressActorSystem, @thick Greeter.Type) -> (@owned Greeter, @error any Error) {
// CHECK: bb0([[ADDRESS_ARG:%[0-9]+]] : $*NotLoadableActorAddress, [[SYSTEM_ARG:%[0-9]+]] : $FakeNotLoadableAddressActorSystem, [[TYPE_ARG:%[0-9]+]] : $@thick Greeter.Type):
// CHECK: [[TYPE:%[0-9]+]] = metatype $@thick Greeter.Type

// CHECK: [[INSTANCE:%[0-9]+]] = builtin "initializeDistributedRemoteActor"([[TYPE]] : $@thick Greeter.Type) : $Greeter
// CHECK: [[ID_PROPERTY:%[0-9]+]] = ref_element_addr [immutable] [[INSTANCE]] : $Greeter, #Greeter.id
// Note specifically that we don't [take] in the below copy_addr:
// CHECK: copy_addr [[ADDRESS_ARG]] to [init] [[ID_PROPERTY]] : $*NotLoadableActorAddress

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = Greeter(actorSystem: system)
  _ = try await local.echo(name: "Caplin")
}
