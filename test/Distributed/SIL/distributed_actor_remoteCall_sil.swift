// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name remoteCall -primary-file %s -emit-sil -enable-experimental-distributed -disable-availability-checking -I %t | %FileCheck %s --enable-var-scope --color --dump-input=always
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor MyDistActor {
  distributed func test() {}

}

// CHECK-LABEL: sil hidden [thunk] [distributed] [ossa] @$s14default_deinit11MyDistActorC4testyyFTE : $@convention(method) @async (@guaranteed MyDistActor) -> @error Error {
// CHECK: // [[SELF:%[0-9]+]] "self"
//
// CHECK: X
