// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-as-library -o %t %S/Inputs/ModuleA.swift
// RUN: %target-swift-frontend -emit-module -parse-as-library -o %t %S/Inputs/ModuleB.swift
// RUN: %target-swift-emit-sil -parse-as-library -I%t %s -Xllvm -sil-disable-pass=GlobalOpt -O | %FileCheck %s

import ModuleA
import ModuleB

var mygg = 29

// Check if we have one token: from mygg.
// Initializers from other modules are never fragile.

// CHECK: sil_global private{{.*}} @globalinit_[[T3:.*]]_token0

//@inlinable
public func sum() -> Int {
  return mygg + get_gg_a() + get_gg_b()
}

// Check if all the addressors are inlined.

// CHECK-LABEL: sil {{.*}}@$S15fragile_globals3sumSiyF
// CHECK-DAG: global_addr @globalinit_[[T1:.*]]_token0
// CHECK-DAG: function_ref @globalinit_[[T1]]_func0
// CHECK-DAG: global_addr @$S15fragile_globals4myggSivp
// CHECK-DAG: function_ref @$S7ModuleA2ggSivau
// CHECK-DAG: function_ref @$S7ModuleB2ggSivau
// CHECK: return

