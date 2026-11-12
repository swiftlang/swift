// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name NoneCopies %S/../../validation-test/closure_specialization/adcs_duplicated_optional_none.swift -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.log
// RUN: %FileCheck %s < %t.log

// The first failed call's none value dominates the second call. By the second
// ADCS visit, CSE and jump threading have produced two error tape constructors
// with distinct payload tuples but the same bound case and saved SSA values.
// CHECK: stage MidLevel,Function, pass
// CHECK: // reverse-mode derivative of recover(_:_:_:_:)
// CHECK: [[NONE:%[0-9]+]] = enum $Optional<@callee_guaranteed (Float) -> Float>, #Optional.none!enumelt
// CHECK-NEXT: [[FIRST_PAYLOAD:%[0-9]+]] = tuple ([[NONE]])
// CHECK-NEXT: [[PRED:%[0-9]+]] = enum ${{_AD__[^, ]+}}, {{.*}}[[FIRST_PAYLOAD]]
// CHECK: [[PAYLOAD1:%[0-9]+]] = tuple $(predecessor: {{.*}}, Optional<@callee_guaranteed (Float) -> Float>) ([[PRED]], [[NONE]])
// CHECK-NEXT: {{%[0-9]+}} = enum $[[ERROR_TYPE:_AD__[^, ]+]], #[[ERROR_TYPE]].[[CASE:bb[0-9]+]]!enumelt, [[PAYLOAD1]]
// CHECK: [[PAYLOAD2:%[0-9]+]] = tuple $(predecessor: {{.*}}, Optional<@callee_guaranteed (Float) -> Float>) ([[PRED]], [[NONE]])
// CHECK-NEXT: {{%[0-9]+}} = enum $[[ERROR_TYPE]], #[[ERROR_TYPE]].[[CASE]]!enumelt, [[PAYLOAD2]]

// A known some callback remains present, so this exercises optional matching
// before the existing live-optional specialization boundary is checked.
// CHECK: // function_ref closure #1 in squareVJP(_:_:)
// CHECK-NEXT: [[FN:%[0-9]+]] = function_ref
// CHECK-NEXT: [[CALLBACK:%[0-9]+]] = partial_apply [callee_guaranteed] [[FN]]
// CHECK-NEXT: [[SOME:%[0-9]+]] = enum $Optional<@callee_guaranteed (Float) -> Float>, #Optional.some!enumelt, [[CALLBACK]]
// CHECK-NEXT: [[NORMAL_PAYLOAD:%[0-9]+]] = tuple $(predecessor: {{.*}}, Optional<@callee_guaranteed (Float) -> Float>) ([[PRED]], [[SOME]])
// CHECK-NEXT: {{%[0-9]+}} = enum $[[NORMAL_TYPE:_AD__[^, ]+]], #[[NORMAL_TYPE]].[[CASE]]!enumelt, [[NORMAL_PAYLOAD]]
// CHECK: return
