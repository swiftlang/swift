// RUN: %target-swift-frontend -I %S/../Inputs -emit-silgen -enable-source-import -parse-as-library -enable-resilience %s | %FileCheck %s
// RUN: %target-swift-frontend -I %S/../Inputs -emit-sil -enable-source-import -parse-as-library -enable-resilience %s -O | %FileCheck --check-prefix=CHECK-OPT %s

import resilient_global

public struct MyEmptyStruct {}

// CHECK-LABEL: sil_global @_Tv17global_resilience13myEmptyGlobalVS_13MyEmptyStruct : $MyEmptyStruct

public var myEmptyGlobal = MyEmptyStruct()

// CHECK-LABEL: sil_global @_Tv17global_resilience19myFixedLayoutGlobalVS_13MyEmptyStruct : $MyEmptyStruct

@_fixed_layout public var myFixedLayoutGlobal = MyEmptyStruct()

// Mutable addressor for resilient global (should not be public?)

// CHECK-LABEL: sil [global_init] @_TF17global_resilienceau13myEmptyGlobalVS_13MyEmptyStruct : $@convention(thin) () -> Builtin.RawPointer
// CHECK:         global_addr @_Tv17global_resilience13myEmptyGlobalVS_13MyEmptyStruct
// CHECK:         return

// Synthesized getter and setter for our resilient global variable

// CHECK-LABEL: sil @_TF17global_resilienceg13myEmptyGlobalVS_13MyEmptyStruct
// CHECK:         function_ref @_TF17global_resilienceau13myEmptyGlobalVS_13MyEmptyStruct
// CHECK:         return

// CHECK-LABEL: sil @_TF17global_resiliences13myEmptyGlobalVS_13MyEmptyStruct
// CHECK:         function_ref @_TF17global_resilienceau13myEmptyGlobalVS_13MyEmptyStruct
// CHECK:         return

// Mutable addressor for fixed-layout global

// CHECK-LABEL: sil [global_init] @_TF17global_resilienceau19myFixedLayoutGlobalVS_13MyEmptyStruct
// CHECK:         global_addr @_Tv17global_resilience19myFixedLayoutGlobalVS_13MyEmptyStruct
// CHECK:         return

// CHECK-OPT-LABEL: sil private @globalinit_{{.*}}_func0
// CHECK-OPT:     alloc_global @_Tv17global_resilience19myFixedLayoutGlobalVS_13MyEmptyStruct
// CHECK-OPT:     return

// CHECK-OPT-LABEL: sil [global_init] @_TF17global_resilienceau19myFixedLayoutGlobalVS_13MyEmptyStruct
// CHECK-OPT:     global_addr @_Tv17global_resilience19myFixedLayoutGlobalVS_13MyEmptyStruct
// CHECK-OPT:     function_ref @globalinit_{{.*}}_func0
// CHECK-OPT:     return

// Accessing resilient global from our resilience domain --
// call the addressor directly

// CHECK-LABEL: sil @_TF17global_resilience16getMyEmptyGlobalFT_VS_13MyEmptyStruct
// CHECK:         function_ref @_TF17global_resilienceau13myEmptyGlobalVS_13MyEmptyStruct
// CHECK:         return
public func getMyEmptyGlobal() -> MyEmptyStruct {
  return myEmptyGlobal
}

// Accessing resilient global from a different resilience domain --
// access it with accessors

// CHECK-LABEL: sil @_TF17global_resilience14getEmptyGlobalFT_V16resilient_global20EmptyResilientStruct
// CHECK:         function_ref @_TF16resilient_globalg11emptyGlobalVS_20EmptyResilientStruct
// CHECK:         return
public func getEmptyGlobal() -> EmptyResilientStruct {
  return emptyGlobal
}

// Accessing fixed-layout global from a different resilience domain --
// call the addressor directly

// CHECK-LABEL: sil @_TF17global_resilience20getFixedLayoutGlobalFT_V16resilient_global20EmptyResilientStruct
// CHECK:         function_ref @_TF16resilient_globalau17fixedLayoutGlobalVS_20EmptyResilientStruct
// CHECK:         return
public func getFixedLayoutGlobal() -> EmptyResilientStruct {
  return fixedLayoutGlobal
}
