// RUN: %target-swift-frontend %s -parse-as-library -module-name=test -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend %s -parse-as-library -O -module-name=test -emit-sil | %FileCheck %s

struct S: Hashable, Sendable {}

public let metatype1 = Int.self
public let metatype2: Any.Type = Int.self
public let metatype3: Any.Type = S.self
public let metatype4: any (Hashable & Sendable).Type = Int.self
public let metatype5: any (Hashable & Sendable).Type = S.self
public let metatype6: Any.Type = Array<Int>.self
public let metatype7: any (Hashable & Sendable).Type = Array<Int>.self

// CHECK:       sil_global [let] @$s4test9metatype1Simvp : $@thin Int.Type = {
// CHECK-NEXT:   %initval = metatype $@thin Int.Type
// CHECK-NEXT: }

// CHECK:       sil_global [let] @$s4test9metatype2ypXpvp : $@thick any Any.Type = {
// CHECK-NEXT:   %0 = metatype $@thick Int.Type                  
// CHECK-NEXT:   %initval = init_existential_metatype %0, $@thick any Any.Type
// CHECK-NEXT: }

// CHECK:       sil_global [let] @$s4test9metatype3ypXpvp : $@thick any Any.Type = {
// CHECK-NEXT:   %0 = metatype $@thick S.Type                    
// CHECK-NEXT:   %initval = init_existential_metatype %0, $@thick any Any.Type
// CHECK-NEXT: }

// CHECK:       sil_global [let] @$s4test9metatype4SH_s8SendablepXpvp : $@thick any (Hashable & Sendable).Type = {
// CHECK-NEXT:   %0 = metatype $@thick Int.Type                  
// CHECK-NEXT:   %initval = init_existential_metatype %0, $@thick any (Hashable & Sendable).Type
// CHECK-NEXT: }

// CHECK:       sil_global [let] @$s4test9metatype5SH_s8SendablepXpvp : $@thick any (Hashable & Sendable).Type = {
// CHECK-NEXT:   %0 = metatype $@thick S.Type                    
// CHECK-NEXT:   %initval = init_existential_metatype %0, $@thick any (Hashable & Sendable).Type
// CHECK-NEXT: }

// CHECK:       sil_global [let] @$s4test9metatype6ypXpvp : $@thick any Any.Type = {
// CHECK-NEXT:   %0 = metatype $@thick Array<Int>.Type           
// CHECK-NEXT:   %initval = init_existential_metatype %0, $@thick any Any.Type
// CHECK-NEXT: }

// CHECK:       sil_global [let] @$s4test9metatype7SH_s8SendablepXpvp : $@thick any (Hashable & Sendable).Type = {
// CHECK-NEXT:   %0 = metatype $@thick Array<Int>.Type           
// CHECK-NEXT:   %initval = init_existential_metatype %0, $@thick any (Hashable & Sendable).Type
// CHECK-NEXT: }

// These are not constant folded and stay as lazily-initialized
public let metatypeA: Any.Type = (Bool.random() ? Array<Int>.self : Array<Bool>.self).self
public let metatypeB: Any.Type = Mirror.self // resilient

// CHECK: sil_global private @$s4test9metatypeA_Wz : $Builtin.Word
// CHECK: sil_global [let] @$s4test9metatypeAypXpvp : $@thick any Any.Type
// CHECK: sil_global private @$s4test9metatypeB_Wz : $Builtin.Word
// CHECK: sil_global [let] @$s4test9metatypeBypXpvp : $@thick any Any.Type
// CHECK: sil private [global_init_once_fn] @$s4test9metatypeA_WZ : $@convention(c) (Builtin.RawPointer) -> () {
// CHECK: sil private [global_init_once_fn] @$s4test9metatypeB_WZ : $@convention(c) (Builtin.RawPointer) -> () {
