// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -parse-as-library -sil-serialize-all -o %t %S/Inputs/ModuleA.swift
// RUN: %target-swift-frontend -emit-module -parse-as-library -sil-serialize-all -o %t %S/Inputs/ModuleB.swift
// RUN: %target-swift-frontend -parse-as-library -I%t %s -Xllvm -enable-static-init=false -O -emit-sil | FileCheck %s

import ModuleA
import ModuleB

var mygg = 29

// Check if we have three tokens: 2 from the imported modules, one from mygg.

// CHECK: sil_global private{{.*}} @globalinit_[[T1:.*]]_token0
// CHECK: sil_global private{{.*}} @globalinit_[[T2:.*]]_token0
// CHECK: sil_global private{{.*}} @globalinit_[[T3:.*]]_token0

public func sum() -> Int {
  return mygg + get_gg_a() + get_gg_b()
}

// Check if all the addressors are inlined.

// CHECK-LABEL: sil @_TF15fragile_globals3sumFT_Si
// CHECK-DAG: global_addr @_Tv15fragile_globals4myggSi
// CHECK-DAG: global_addr @_Tv7ModuleA{{.*}}gg
// CHECK-DAG: global_addr @_Tv7ModuleA{{.*}}gg
// CHECK-DAG: global_addr @globalinit_[[T1]]_token0
// CHECK-DAG: global_addr @globalinit_[[T2]]_token0
// CHECK-DAG: global_addr @globalinit_[[T3]]_token0
// CHECK-DAG: function_ref @globalinit_[[T1]]_func0
// CHECK-DAG: function_ref @globalinit_[[T2]]_func0
// CHECK-DAG: function_ref @globalinit_[[T3]]_func0
// CHECK: return

