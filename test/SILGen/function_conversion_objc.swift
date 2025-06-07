// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name function_conversion_objc -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -enable-objc-interop -verify | %FileCheck %s

import Foundation

// ==== Metatype to object conversions

// CHECK-LABEL: sil hidden [ossa] @$s24function_conversion_objc20convMetatypeToObjectyySo8NSObjectCmADcF
func convMetatypeToObject(_ f: @escaping (NSObject) -> NSObject.Type) {
// CHECK:         function_ref @$sSo8NSObjectCABXMTIeggd_AByXlIeggo_TR
// CHECK:         partial_apply
  let _: (NSObject) -> AnyObject = f
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSo8NSObjectCABXMTIeggd_AByXlIeggo_TR : $@convention(thin) (@guaranteed NSObject, @guaranteed @callee_guaranteed (@guaranteed NSObject) -> @thick NSObject.Type) -> @owned AnyObject {
// CHECK:         apply %1(%0)
// CHECK:         thick_to_objc_metatype {{.*}} : $@thick NSObject.Type to $@objc_metatype NSObject.Type
// CHECK:         objc_metatype_to_object {{.*}} : $@objc_metatype NSObject.Type to $AnyObject
// CHECK:         return

@objc protocol NSBurrito {}

// CHECK-LABEL: sil hidden [ossa] @$s24function_conversion_objc31convExistentialMetatypeToObjectyyAA9NSBurrito_pXpAaC_pcF
func convExistentialMetatypeToObject(_ f: @escaping (NSBurrito) -> NSBurrito.Type) {
// CHECK:         function_ref @$s24function_conversion_objc9NSBurrito_pAaB_pXmTIeggd_AaB_pyXlIeggo_TR
// CHECK:         partial_apply
  let _: (NSBurrito) -> AnyObject = f
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s24function_conversion_objc9NSBurrito_pAaB_pXmTIeggd_AaB_pyXlIeggo_TR : $@convention(thin) (@guaranteed any NSBurrito, @guaranteed @callee_guaranteed (@guaranteed any NSBurrito) -> @thick any NSBurrito.Type) -> @owned AnyObject
// CHECK:         apply %1(%0)
// CHECK:         thick_to_objc_metatype {{.*}} : $@thick any NSBurrito.Type to $@objc_metatype any NSBurrito.Type
// CHECK:         objc_existential_metatype_to_object {{.*}} : $@objc_metatype any NSBurrito.Type to $AnyObject
// CHECK:         return

// CHECK-LABEL: sil hidden [ossa] @$s24function_conversion_objc28convProtocolMetatypeToObjectyyAA9NSBurrito_pmycF
func convProtocolMetatypeToObject(_ f: @escaping () -> NSBurrito.Protocol) {
// CHECK:         function_ref @$s24function_conversion_objc9NSBurrito_pXMtIegd_So8ProtocolCIego_TR
// CHECK:         partial_apply
  let _: () -> Protocol = f
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s24function_conversion_objc9NSBurrito_pXMtIegd_So8ProtocolCIego_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> @thin (any NSBurrito).Type) -> @owned Protocol
// CHECK:         apply %0() : $@callee_guaranteed () -> @thin (any NSBurrito).Type
// CHECK:         objc_protocol #NSBurrito : $Protocol
// CHECK:         copy_value
// CHECK:         return

// ==== Representation conversions

// CHECK-LABEL: sil hidden [ossa] @$s24function_conversion_objc11funcToBlockyyyXByycF : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> @owned @convention(block) () -> ()
// CHECK:         [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage
// CHECK:         [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]]
// CHECK:         [[COPY:%.*]] = copy_block [[BLOCK]] : $@convention(block) () -> ()
// CHECK:         return [[COPY]]
func funcToBlock(_ x: @escaping () -> ()) -> @convention(block) () -> () {
  return x
}

// CHECK-LABEL: sil hidden [ossa] @$s24function_conversion_objc11blockToFuncyyycyyXBF : $@convention(thin) (@guaranteed @convention(block) () -> ()) -> @owned @callee_guaranteed () -> ()
// CHECK: bb0([[ARG:%.*]] : @guaranteed $@convention(block) () -> ()):
// CHECK:   [[COPIED:%.*]] = copy_block [[ARG]]
// CHECK:   [[BORROWED_COPIED:%.*]] = begin_borrow [[COPIED]]
// CHECK:   [[COPIED_2:%.*]] = copy_value [[BORROWED_COPIED]]
// CHECK:   [[THUNK:%.*]] = function_ref @$sIeyB_Ieg_TR
// CHECK:   [[FUNC:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[COPIED_2]])
// CHECK:   end_borrow [[BORROWED_COPIED]]
// CHECK:   destroy_value [[COPIED]]
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return [[FUNC]]
func blockToFunc(_ x: @escaping @convention(block) () -> ()) -> () -> () {
  return x
}

// ==== Representation change + function type conversion

// CHECK-LABEL: sil hidden [ossa] @$s24function_conversion_objc22blockToFuncExistentialyypycSiyXBF : $@convention(thin) (@guaranteed @convention(block) () -> Int) -> @owned @callee_guaranteed () -> @out Any
// CHECK:         function_ref @$sSiIeyBd_SiIegd_TR
// CHECK:         partial_apply
// CHECK:         function_ref @$sSiIegd_ypIegr_TR
// CHECK:         partial_apply
// CHECK:         return
func blockToFuncExistential(_ x: @escaping @convention(block) () -> Int) -> () -> Any {
  return x
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSiIeyBd_SiIegd_TR : $@convention(thin) (@guaranteed @convention(block) () -> Int) -> Int

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSiIegd_ypIegr_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> Int) -> @out Any

// C function pointer conversions

class A : NSObject {}
class B : A {}

// CHECK-LABEL: sil hidden [ossa] @$s24function_conversion_objc18cFuncPtrConversionyyAA1BCXCyAA1ACXCF
func cFuncPtrConversion(_ x: @escaping @convention(c) (A) -> ()) -> @convention(c) (B) -> () {
// CHECK:         convert_function %0 : $@convention(c) (A) -> () to $@convention(c) (B) -> ()
// CHECK:         return
  return x
}

func cFuncPtr(_ a: A) {}

// CHECK-LABEL: sil hidden [ossa] @$s24function_conversion_objc19cFuncDeclConversionyAA1BCXCyF
func cFuncDeclConversion() -> @convention(c) (B) -> () {
// CHECK:         function_ref @$s24function_conversion_objc8cFuncPtryyAA1ACFTo : $@convention(c) (A) -> ()
// CHECK:         convert_function %0 : $@convention(c) (A) -> () to $@convention(c) (B) -> ()
// CHECK:         return
  return cFuncPtr
}

func cFuncPtrConversionUnsupported(_ x: @escaping @convention(c) (@convention(block) () -> ()) -> ())
    -> @convention(c) (@convention(c) () -> ()) -> () {
  return x  // expected-error{{C function pointer signature '@convention(c) (@convention(block) () -> ()) -> ()' is not compatible with expected type '@convention(c) (@convention(c) () -> ()) -> ()'}}
}
