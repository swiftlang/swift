// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module %S/Inputs/dynamic_witness_other_module_other.swift -emit-module-path %t

// RUN: %target-swift-emit-silgen %s -I %t | %FileCheck %s
// RUN: %target-swift-emit-ir %s -I %t > /dev/null

// REQUIRES: objc_interop

import dynamic_witness_other_module_other

protocol EvenMoreExtremeLateBindingCounter {
  var counter: Int { get set }
}

extension ExtremeLateBindingCounter : EvenMoreExtremeLateBindingCounter {}

// Make sure we emit a direct reference to the witness's materializeForSet
// instead of dispatching via class_method.

// CHECK-LABEL: sil private [transparent] [thunk] @$S029dynamic_witness_other_module_C025ExtremeLateBindingCounterC0a1_b1_c1_D008EvenMoreefgH0A2dEP7counterSivmTW : $@convention(witness_method: EvenMoreExtremeLateBindingCounter) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout ExtremeLateBindingCounter) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
// CHECK: function_ref @$S029dynamic_witness_other_module_C025ExtremeLateBindingCounterC7counterSivm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed ExtremeLateBindingCounter) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
// CHECK: return
