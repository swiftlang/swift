// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name default_deinit -primary-file %s -Xllvm -sil-print-types -emit-sil -target %target-swift-5.7-abi-triple -I %t | %FileCheck %s --enable-var-scope --dump-input=fail
// REQUIRES: concurrency
// REQUIRES: distributed

// REQUIRES: swift_in_compiler

/// The convention in this test is that the Swift declaration comes before its FileCheck lines.

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== ----------------------------------------------------------------------------------------------------------------

class SomeClass {}

enum Err : Error {
  case blah
}

distributed actor MyDistActor {
  var localOnlyField: SomeClass

  init?(system_async_fail_throws: FakeActorSystem, cond: Bool) async throws {
    self.actorSystem = system_async_fail_throws
    guard cond else { throw Err.blah }
    self.localOnlyField = SomeClass()
  }

  // CHECK-LABEL: // MyDistActor.init(system_async_fail_throws:cond:)
  // CHECK: sil hidden @$s14default_deinit11MyDistActorC24system_async_fail_throws4condACSg015FakeDistributedE7Systems0kE6SystemV_SbtYaKcfc : $@convention(method) @async (@owned FakeActorSystem, Bool, @sil_isolated @owned MyDistActor) -> (@owned Optional<MyDistActor>, @error any Error) {
  // CHECK: bb0([[SYSTEM:%[0-9]+]] : $FakeActorSystem, [[COND:%[0-9]+]] : $Bool, [[SELF:%[0-9]+]] : $MyDistActor):
  // CHECK:   cond_br {{%[0-9]+}}, [[SUCCESS_BB:bb[0-9]+]], [[FAIL_BB:bb[0-9]+]]

  // CHECK: [[SUCCESS_BB]]:
  // CHECK:   hop_to_executor {{%[0-9]+}}
  // CHECK:   [[READY_FN:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0aC6SystemV10actorReadyyyx0B00bC0RzAA0C7AddressV2IDRtzlF : $@convention(method) <τ_0_0 where τ_0_0 : DistributedActor, τ_0_0.ID == ActorAddress> (@guaranteed τ_0_0, @guaranteed FakeActorSystem) -> ()
  // CHECK:   = apply [[READY_FN]]
  // CHECK:   return

  // CHECK: [[FAIL_BB]]:
  // CHECK:   [[RESIGN_FN:%[0-9]+]] = function_ref @$s27FakeDistributedActorSystems0aC6SystemV8resignIDyyAA0C7AddressVF : $@convention(method) (@guaranteed ActorAddress, @guaranteed FakeActorSystem) -> ()
  // CHECK:   = apply [[RESIGN_FN]]
  // CHECK:   builtin "destroyDefaultActor"
  // CHECK:   throw {{%[0-9]+}} : $any Error
  // CHECK: } // end sil function '$s14default_deinit11MyDistActorC24system_async_fail_throws4condACSg015FakeDistributedE7Systems0kE6SystemV_SbtYaKcfc'

}
