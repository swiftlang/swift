// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-5.7-abi-triple -j1 -parse-as-library -typecheck -dump-ast -I %t %s %S/Inputs/FakeDistributedActorSystems.swift 2> %t.ast.txt
// RUN: %FileCheck %s < %t.ast.txt

// REQUIRES: concurrency
// REQUIRES: distributed

// REQUIRES: rdar114453974

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

// FIXME(distributed): Distributed actors currently have some issues on windows rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import StdlibUnittest
import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

distributed actor DefaultWorker {
  // empty on purpose, default executor
}

// Check DefaultWorker, the DefaultActor version of the synthesis:
// CHECK:  (class_decl range=[{{.*}}] "DefaultWorker" interface_type="DefaultWorker.Type" access=internal non_resilient distributed actor
// The unowned executor property:
// CHECK:    (var_decl implicit "unownedExecutor" interface_type="UnownedSerialExecutor" access=internal final readImpl=getter immutable

// We guard the rest of the body; we only return a default executor if the actor is local:
// CHECK:       (guard_stmt implicit
// CHECK:         (call_expr implicit type="Bool" nothrow
// CHECK:           (declref_expr implicit type="@_NO_EXTINFO (AnyObject) -> Bool" decl="Distributed.(file).__isLocalActor" function_ref=unapplied)

// Check that we create the "remote reference" executor:
// CHECK: (return_stmt implicit
// CHECK:   (call_expr implicit type="UnownedSerialExecutor" nothrow
// CHECK:     (declref_expr implicit type="(DefaultWorker) -> UnownedSerialExecutor" decl="Distributed.(file).buildDefaultDistributedRemoteActorExecutor [with (substitution_map generic_signature='<Act where Act : DistributedActor>' 'Act -> DefaultWorker')]"

// Check the default executor synthesis for local actor otherwise:
// CHECK: (return_stmt implicit
// CHECK:   (call_expr implicit type="Builtin.Executor" nothrow
// CHECK:     (declref_expr implicit type="(DefaultWorker) -> Builtin.Executor" decl="Builtin.(file).buildDefaultActorExecutorRef [with (substitution_map generic_signature='<T where T : AnyObject>' 'T -> DefaultWorker')]" function_ref=unapplied)
