// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name remoteCall -primary-file %s -dump-ast -enable-experimental-distributed -disable-availability-checking -I %t | %FileCheck %s --enable-var-scope --color --dump-input=always
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor MyDistActor {
  distributed func test() {}
}

// TODO(distributed): very naive test, SIL for discussion and cleanup

// MyDistActor.test()
// CHECK: (func_decl implicit
