// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name ADCSMP %S/../../validation-test/closure_specialization/adcs_merged_payload.swift -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s --check-prefix=INPUT < %t.passes
// RUN: %FileCheck %s --check-prefix=REWRITE < %t.passes
// RUN: %FileCheck %S/../../validation-test/closure_specialization/adcs_merged_payload.swift --check-prefix=OPT < %t.sil

// Both incoming edges forward the entire extracted payload. The join
// destructures it before calling its saved pullback.
// INPUT-LABEL: // pullback of mergedPayload(_:_:)
// INPUT: %[[LEFT:[0-9]+]] = unchecked_enum_data
// INPUT-NEXT: br [[JOIN:bb[0-9]+]]({{.*}}, %[[LEFT]])
// INPUT: %[[RIGHT:[0-9]+]] = unchecked_enum_data
// INPUT-NEXT: br [[JOIN]]({{.*}}, %[[RIGHT]])
// INPUT: [[JOIN]]({{.*}}, %[[PAYLOAD:[0-9]+]] : @owned $(predecessor: {{.*}}, @callee_guaranteed (Float) -> (Float, Float))):
// INPUT-NEXT: (%{{[0-9]+}}, %[[CALLBACK:[0-9]+]]) = destructure_tuple %[[PAYLOAD]]
// INPUT: %{{[0-9]+}} = apply %[[CALLBACK]]

// The clone keeps the whole-payload join and passes its specialized tuple.
// REWRITE-LABEL: // specialized pullback of mergedPayload(_:_:)
// REWRITE: %[[LEFT:[0-9]+]] = unchecked_enum_data {{.*}}_spec
// REWRITE-NEXT: br [[JOIN:bb[0-9]+]]({{.*}}, %[[LEFT]])
// REWRITE: %[[RIGHT:[0-9]+]] = unchecked_enum_data {{.*}}_spec
// REWRITE-NEXT: br [[JOIN]]({{.*}}, %[[RIGHT]])
// REWRITE: [[JOIN]]({{.*}}, %[[PAYLOAD:[0-9]+]] : $(predecessor: {{.*}}_spec{{.*}}, ())):
// REWRITE-NEXT: (%{{[0-9]+}}, %{{[0-9]+}}) = destructure_tuple %[[PAYLOAD]]
