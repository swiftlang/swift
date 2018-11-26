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

// CHECK-LABEL: sil private [transparent] [thunk] @$s029dynamic_witness_other_module_C025ExtremeLateBindingCounterC0a1_b1_c1_D008EvenMoreefgH0A2dEP7counterSivMTW : $@yield_once @convention(witness_method: EvenMoreExtremeLateBindingCounter) (@inout ExtremeLateBindingCounter) -> @yields @inout Int {
// CHECK: function_ref @$s029dynamic_witness_other_module_C025ExtremeLateBindingCounterC7counterSivM : $@yield_once @convention(method) (@guaranteed ExtremeLateBindingCounter) -> @yields @inout Int
// CHECK: return
