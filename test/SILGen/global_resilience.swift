// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_global.swiftmodule -module-name=resilient_global %S/../Inputs/resilient_global.swift
// RUN: %target-swift-frontend -I %t -emit-silgen -enable-resilience -parse-as-library %s | %FileCheck %s
// RUN: %target-swift-frontend -I %t -emit-sil -O -enable-resilience -parse-as-library %s | %FileCheck --check-prefix=CHECK-OPT %s

import resilient_global

public struct MyEmptyStruct {}

// CHECK-LABEL: sil_global @_T017global_resilience13myEmptyGlobalAA02MyD6StructVv : $MyEmptyStruct

public var myEmptyGlobal = MyEmptyStruct()

// CHECK-LABEL: sil_global @_T017global_resilience19myFixedLayoutGlobalAA13MyEmptyStructVv : $MyEmptyStruct

@_fixed_layout public var myFixedLayoutGlobal = MyEmptyStruct()

// Mutable addressor for resilient global (should not be public?)

// CHECK-LABEL: sil [global_init] @_T017global_resilience13myEmptyGlobalAA02MyD6StructVfau : $@convention(thin) () -> Builtin.RawPointer
// CHECK:         global_addr @_T017global_resilience13myEmptyGlobalAA02MyD6StructVv
// CHECK:         return

// Synthesized getter and setter for our resilient global variable

// CHECK-LABEL: sil @_T017global_resilience13myEmptyGlobalAA02MyD6StructVfg
// CHECK:         function_ref @_T017global_resilience13myEmptyGlobalAA02MyD6StructVfau
// CHECK:         return

// CHECK-LABEL: sil @_T017global_resilience13myEmptyGlobalAA02MyD6StructVfs
// CHECK:         function_ref @_T017global_resilience13myEmptyGlobalAA02MyD6StructVfau
// CHECK:         return

// Mutable addressor for fixed-layout global

// CHECK-LABEL: sil [global_init] @_T017global_resilience19myFixedLayoutGlobalAA13MyEmptyStructVfau
// CHECK:         global_addr @_T017global_resilience19myFixedLayoutGlobalAA13MyEmptyStructVv
// CHECK:         return

// CHECK-OPT-LABEL: sil private @globalinit_{{.*}}_func0
// CHECK-OPT:     alloc_global @_T017global_resilience19myFixedLayoutGlobalAA13MyEmptyStructVv
// CHECK-OPT:     return

// CHECK-OPT-LABEL: sil [global_init] @_T017global_resilience19myFixedLayoutGlobalAA13MyEmptyStructVfau
// CHECK-OPT:     global_addr @_T017global_resilience19myFixedLayoutGlobalAA13MyEmptyStructVv
// CHECK-OPT:     function_ref @globalinit_{{.*}}_func0
// CHECK-OPT:     return

// Accessing resilient global from our resilience domain --
// call the addressor directly

// CHECK-LABEL: sil @_T017global_resilience16getMyEmptyGlobalAA0dE6StructVyF
// CHECK:         function_ref @_T017global_resilience13myEmptyGlobalAA02MyD6StructVfau
// CHECK:         return
public func getMyEmptyGlobal() -> MyEmptyStruct {
  return myEmptyGlobal
}

// Accessing resilient global from a different resilience domain --
// access it with accessors

// CHECK-LABEL: sil @_T017global_resilience14getEmptyGlobal010resilient_A00D15ResilientStructVyF
// CHECK:         function_ref @_T016resilient_global11emptyGlobalAA20EmptyResilientStructVfg
// CHECK:         return
public func getEmptyGlobal() -> EmptyResilientStruct {
  return emptyGlobal
}

// Accessing fixed-layout global from a different resilience domain --
// call the addressor directly

// CHECK-LABEL: sil @_T017global_resilience20getFixedLayoutGlobal010resilient_A020EmptyResilientStructVyF
// CHECK:         function_ref @_T016resilient_global17fixedLayoutGlobalAA20EmptyResilientStructVfau
// CHECK:         return
public func getFixedLayoutGlobal() -> EmptyResilientStruct {
  return fixedLayoutGlobal
}
