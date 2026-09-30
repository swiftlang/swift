// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -emit-silgen -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -I %t %s -module-name main | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_OnewayNowait

// Without Embedded, 'nowait' of a synchronous 'oneway' func keeps the
// task-based lowering: a discarding 'createAsyncTask' whose operation closure
// makes the call. Only Embedded lowers it to a synchronous enqueue, and only
// Embedded gives a distributed func a synchronous thunk

// CHECK-NOT: _enqueueOneway

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

actor Mailbox {
  func tell(_ n: Int) oneway {}
}

@globalActor actor Background {
  static let shared = Background()
}

@Background func note(_ n: Int) oneway {}

// The distributed thunk stays 'async'
// CHECK-LABEL: sil hidden [thunk] [distributed]{{.*}} @$s4main7GreeterC5greetyySiYaYoKFTE : $@convention(method) @async (@sil_sending Int, @guaranteed Greeter) -> @error any Error {
distributed actor Greeter {
  distributed func greet(_ n: Int) oneway {}
}

// CHECK-LABEL: sil hidden [ossa] @$s4main9sendPlainyyAA7MailboxCF : $@convention(thin) (@guaranteed Mailbox) -> () {
// CHECK: builtin "createAsyncTask"
// CHECK: } // end sil function '$s4main9sendPlainyyAA7MailboxCF'
// CHECK-LABEL: sil private [ossa] @$s4main9sendPlainyyAA7MailboxCFyyYaKcfU_ : $@convention(thin) @async (@guaranteed Mailbox) -> @error any Error {
// CHECK: class_method {{%.*}}, #Mailbox.tell
func sendPlain(_ m: Mailbox) {
  nowait m.tell(1)
}

// CHECK-LABEL: sil hidden [ossa] @$s4main10sendGlobalyyF : $@convention(thin) () -> () {
// CHECK: builtin "createAsyncTask"
// CHECK: } // end sil function '$s4main10sendGlobalyyF'
// CHECK-LABEL: sil private [ossa] @$s4main10sendGlobalyyFyyYaKcfU_ : $@convention(thin) @async () -> @error any Error {
// CHECK: function_ref @$s4main4noteyySiYoF
func sendGlobal() {
  nowait note(2)
}

// CHECK-LABEL: sil hidden [ossa] @$s4main15sendDistributedyyAA7GreeterCKF : $@convention(thin) (@guaranteed Greeter) -> @error any Error {
// CHECK: builtin "createAsyncTask"
// CHECK: } // end sil function '$s4main15sendDistributedyyAA7GreeterCKF'
// CHECK-LABEL: sil private [ossa] @$s4main15sendDistributedyyAA7GreeterCKFyyYaKcfU_ : $@convention(thin) @async (@guaranteed Greeter) -> @error any Error {
// CHECK: function_ref @$s4main7GreeterC5greetyySiYaYoKFTE : $@convention(method) @async
func sendDistributed(_ g: Greeter) throws {
  nowait g.greet(3)
}
