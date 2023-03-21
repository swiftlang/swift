// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -Xfrontend -disable-availability-checking -j2 -parse-as-library -typecheck -dump-ast -I %t %s %S/Inputs/FakeDistributedActorSystems.swift 2>&1 | %FileCheck %s --dump-input=fail

// REQUIRES: concurrency
// REQUIRES: distributed

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
// CHECK:  (class_decl range=[{{.*}}] "DefaultWorker" interface type='DefaultWorker.Type' access=internal non-resilient actor
// The unowned executor property:
// CHECK:    (var_decl implicit "localUnownedExecutor" type='Optional<UnownedSerialExecutor>' interface type='Optional<UnownedSerialExecutor>' access=internal final readImpl=getter immutable
// CHECK:     (accessor_decl implicit 'anonname={{.*}}' interface type='(DefaultWorker) -> () -> Optional<UnownedSerialExecutor>' access=internal final get_for=localUnownedExecutor
// CHECK:       (parameter "self" type='DefaultWorker' interface type='DefaultWorker')
// CHECK:       (parameter_list)
// CHECK:       (brace_stmt implicit
// We guard the rest of the body; we only return a default executor if the actor is local:
// CHECK:       (guard_stmt implicit
// CHECK:         (call_expr implicit type='Bool' nothrow
// CHECK:           (declref_expr implicit type='@_NO_EXTINFO (AnyObject) -> Bool' decl=Distributed.(file).__isLocalActor function_ref=unapplied)
// CHECK:           (argument_list implicit
// CHECK:             (argument
// CHECK:               (erasure_expr implicit type='AnyObject'
// CHECK:                 (declref_expr implicit type='DefaultWorker' decl=main.(file).DefaultWorker.<anonymous>.self function_ref=unapplied)))))
// CHECK:           (brace_stmt implicit
// CHECK:             (return_stmt implicit
// CHECK:               (nil_literal_expr implicit type='Optional<UnownedSerialExecutor>' initializer=**NULL**))))
// If the actor is not local, we return a default executor for it, same as normal actors:
// CHECK:         (return_stmt implicit
// CHECK:           (inject_into_optional implicit type='Optional<UnownedSerialExecutor>'
// CHECK:             (call_expr implicit type='UnownedSerialExecutor' nothrow
// CHECK:               (constructor_ref_call_expr implicit type='(Builtin.Executor) -> UnownedSerialExecutor' nothrow
// CHECK:                 (declref_expr implicit type='(UnownedSerialExecutor.Type) -> (Builtin.Executor) -> UnownedSerialExecutor' decl=_Concurrency.(file).UnownedSerialExecutor.init(_:) function_ref=unapplied)
// CHECK:                 (argument_list implicit
// CHECK:                   (argument
// CHECK:                     (type_expr implicit type='UnownedSerialExecutor.Type' typerepr='<<NULL>>'))))
// CHECK:               (argument_list implicit
// CHECK:                 (argument
// CHECK:                   (call_expr implicit type='Builtin.Executor' nothrow
// CHECK:                     (declref_expr implicit type='(DefaultWorker) -> Builtin.Executor' decl=Builtin.(file).buildDefaultActorExecutorRef [with (substitution_map generic_signature=<T where T : AnyObject> (substitution T -> DefaultWorker))] function_ref=unapplied)
// CHECK:                     (argument_list implicit
// CHECK:                       (argument
// CHECK:                         (declref_expr implicit type='DefaultWorker' decl=main.(file).DefaultWorker.<anonymous>.self function_ref=unapplied))))))))))))
// CHECK:   (pattern_binding_decl implicit
// CHECK:     (pattern_typed implicit type='Optional<UnownedSerialExecutor>'
// CHECK:       (pattern_named implicit type='Optional<UnownedSerialExecutor>' 'localUnownedExecutor')))

