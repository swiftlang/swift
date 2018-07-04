// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-sil-ownership -enable-resilience -emit-module-path=%t/resilient_global.swiftmodule -module-name=resilient_global %S/../Inputs/resilient_global.swift
// RUN: %target-swift-emit-silgen -I %t -enable-resilience -enable-sil-ownership -parse-as-library %s | %FileCheck %s
// RUN: %target-swift-emit-sil -I %t -O -enable-resilience -parse-as-library %s | %FileCheck --check-prefix=CHECK-OPT %s

import resilient_global

public struct MyEmptyStruct {}

// CHECK-LABEL: sil_global @$S17global_resilience13myEmptyGlobalAA02MyD6StructVvp : $MyEmptyStruct

public var myEmptyGlobal = MyEmptyStruct()

// CHECK-LABEL: sil_global @$S17global_resilience19myFixedLayoutGlobalAA13MyEmptyStructVvp : $MyEmptyStruct

@_fixed_layout public var myFixedLayoutGlobal = MyEmptyStruct()

// Mutable addressor for resilient global

// CHECK-LABEL: sil hidden [global_init] @$S17global_resilience13myEmptyGlobalAA02MyD6StructVvau : $@convention(thin) () -> Builtin.RawPointer
// CHECK:         global_addr @$S17global_resilience13myEmptyGlobalAA02MyD6StructVv
// CHECK:         return

// Synthesized getter and setter for our resilient global variable

// CHECK-LABEL: sil @$S17global_resilience13myEmptyGlobalAA02MyD6StructVvg
// CHECK:         function_ref @$S17global_resilience13myEmptyGlobalAA02MyD6StructVvau
// CHECK:         return

// CHECK-LABEL: sil @$S17global_resilience13myEmptyGlobalAA02MyD6StructVvs
// CHECK:         function_ref @$S17global_resilience13myEmptyGlobalAA02MyD6StructVvau
// CHECK:         return

// Mutable addressor for fixed-layout global

// CHECK-LABEL: sil [global_init] @$S17global_resilience19myFixedLayoutGlobalAA13MyEmptyStructVvau
// CHECK:         global_addr @$S17global_resilience19myFixedLayoutGlobalAA13MyEmptyStructVv
// CHECK:         return

// CHECK-OPT-LABEL: sil private @globalinit_{{.*}}_func0
// CHECK-OPT:     alloc_global @$S17global_resilience19myFixedLayoutGlobalAA13MyEmptyStructVv
// CHECK-OPT:     return

// CHECK-OPT-LABEL: sil [global_init] @$S17global_resilience19myFixedLayoutGlobalAA13MyEmptyStructVvau
// CHECK-OPT:     global_addr @$S17global_resilience19myFixedLayoutGlobalAA13MyEmptyStructVvp
// CHECK-OPT:     function_ref @globalinit_{{.*}}_func0
// CHECK-OPT:     return

// Accessing resilient global from our resilience domain --
// call the addressor directly

// CHECK-LABEL: sil @$S17global_resilience16getMyEmptyGlobalAA0dE6StructVyF
// CHECK:         function_ref @$S17global_resilience13myEmptyGlobalAA02MyD6StructVvau
// CHECK:         return
public func getMyEmptyGlobal() -> MyEmptyStruct {
  return myEmptyGlobal
}

// Accessing resilient global from a different resilience domain --
// access it with accessors

// CHECK-LABEL: sil @$S17global_resilience14getEmptyGlobal010resilient_A00D15ResilientStructVyF
// CHECK:         function_ref @$S16resilient_global11emptyGlobalAA20EmptyResilientStructVvg
// CHECK:         return
public func getEmptyGlobal() -> EmptyResilientStruct {
  return emptyGlobal
}

// Accessing fixed-layout global from a different resilience domain --
// call the addressor directly

// CHECK-LABEL: sil @$S17global_resilience20getFixedLayoutGlobal010resilient_A020EmptyResilientStructVyF
// CHECK:         function_ref @$S16resilient_global17fixedLayoutGlobalAA20EmptyResilientStructVvau
// CHECK:         return
public func getFixedLayoutGlobal() -> EmptyResilientStruct {
  return fixedLayoutGlobal
}
