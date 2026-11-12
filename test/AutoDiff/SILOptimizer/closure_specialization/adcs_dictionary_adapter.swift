// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name dictionary_adapter %S/../../validation-test/closure_specialization/adcs_dictionary_adapter.swift -o %t.sil
// RUN: %FileCheck %s < %t.sil

// CHECK: enum _AD__{{.*}}lookup{{.*}}_spec_bb1_1{{.*}} {
// CHECK: case bb1((predecessor: {{.*}}, (_: @callee_guaranteed (@in_guaranteed Optional<Double>.TangentVector) -> @owned Dictionary<String, Double>)))
// CHECK-LABEL: // reverse-mode derivative of lookup(_:_:)
// CHECK: // function_ref specialized pullback of lookup(_:_:)
// CHECK-LABEL: // specialized pullback of lookup(_:_:)
// CHECK: %[[CAPTURES:[0-9]+]] = tuple_extract %{{[0-9]+}}, 1
// CHECK: %[[CALLBACK:[0-9]+]] = tuple_extract %[[CAPTURES]], 0
// CHECK: apply %[[CALLBACK]]({{.*}}) : $@callee_guaranteed (@in_guaranteed Optional<Double>.TangentVector) -> @owned Dictionary<String, Double>
