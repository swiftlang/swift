// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -emit-silgen -verify | %FileCheck %s

import Foundation

// REQUIRES: objc_interop

// ==== Metatype to object conversions

// CHECK-LABEL: sil hidden @_T024function_conversion_objc20convMetatypeToObjectySo8NSObjectCmADcF
func convMetatypeToObject(_ f: @escaping (NSObject) -> NSObject.Type) {
// CHECK:         function_ref @_T0So8NSObjectCABXMTIxxd_ABs9AnyObject_pIxxo_TR
// CHECK:         partial_apply
  let _: (NSObject) -> AnyObject = f
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0So8NSObjectCABXMTIxxd_ABs9AnyObject_pIxxo_TR : $@convention(thin) (@owned NSObject, @owned @callee_owned (@owned NSObject) -> @thick NSObject.Type) -> @owned AnyObject {
// CHECK:         apply %1(%0)
// CHECK:         thick_to_objc_metatype {{.*}} : $@thick NSObject.Type to $@objc_metatype NSObject.Type
// CHECK:         objc_metatype_to_object {{.*}} : $@objc_metatype NSObject.Type to $AnyObject
// CHECK:         return

@objc protocol NSBurrito {}

// CHECK-LABEL: sil hidden @_T024function_conversion_objc31convExistentialMetatypeToObjectyAA9NSBurrito_pXpAaC_pcF
func convExistentialMetatypeToObject(_ f: @escaping (NSBurrito) -> NSBurrito.Type) {
// CHECK:         function_ref @_T024function_conversion_objc9NSBurrito_pAaB_pXmTIxxd_AaB_ps9AnyObject_pIxxo_TR
// CHECK:         partial_apply
  let _: (NSBurrito) -> AnyObject = f
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T024function_conversion_objc9NSBurrito_pAaB_pXmTIxxd_AaB_ps9AnyObject_pIxxo_TR : $@convention(thin) (@owned NSBurrito, @owned @callee_owned (@owned NSBurrito) -> @thick NSBurrito.Type) -> @owned AnyObject
// CHECK:         apply %1(%0)
// CHECK:         thick_to_objc_metatype {{.*}} : $@thick NSBurrito.Type to $@objc_metatype NSBurrito.Type
// CHECK:         objc_existential_metatype_to_object {{.*}} : $@objc_metatype NSBurrito.Type to $AnyObject
// CHECK:         return

// CHECK-LABEL: sil hidden @_T024function_conversion_objc28convProtocolMetatypeToObjectyAA9NSBurrito_pmycF
func convProtocolMetatypeToObject(_ f: @escaping () -> NSBurrito.Protocol) {
// CHECK:         function_ref @_T024function_conversion_objc9NSBurrito_pXMtIxd_So8ProtocolCIxo_TR
// CHECK:         partial_apply
  let _: () -> Protocol = f
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T024function_conversion_objc9NSBurrito_pXMtIxd_So8ProtocolCIxo_TR : $@convention(thin) (@owned @callee_owned () -> @thin NSBurrito.Protocol) -> @owned Protocol
// CHECK:         apply %0() : $@callee_owned () -> @thin NSBurrito.Protocol
// CHECK:         objc_protocol #NSBurrito : $Protocol
// CHECK:         copy_value
// CHECK:         return

// ==== Representation conversions

// CHECK-LABEL: sil hidden @_T024function_conversion_objc11funcToBlockyyXByycF : $@convention(thin) (@owned @callee_owned () -> ()) -> @owned @convention(block) () -> ()
// CHECK:         [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage
// CHECK:         [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]]
// CHECK:         [[COPY:%.*]] = copy_block [[BLOCK]] : $@convention(block) () -> ()
// CHECK:         return [[COPY]]
func funcToBlock(_ x: @escaping () -> ()) -> @convention(block) () -> () {
  return x
}

// CHECK-LABEL: sil hidden @_T024function_conversion_objc11blockToFuncyycyyXBF : $@convention(thin) (@owned @convention(block) () -> ()) -> @owned @callee_owned () -> ()
// CHECK: bb0([[ARG:%.*]] : $@convention(block) () -> ()):
// CHECK:   [[COPIED:%.*]] = copy_block [[ARG]]
// CHECK:   [[BORROWED_COPIED:%.*]] = begin_borrow [[COPIED]]
// CHECK:   [[COPIED_2:%.*]] = copy_value [[BORROWED_COPIED]]
// CHECK:   [[THUNK:%.*]] = function_ref @_T0IyB_Ix_TR
// CHECK:   [[FUNC:%.*]] = partial_apply [[THUNK]]([[COPIED_2]])
// CHECK:   end_borrow [[BORROWED_COPIED]] from [[COPIED]]
// CHECK:   destroy_value [[COPIED]]
// CHECK:   destroy_value [[ARG]]
// CHECK:   return [[FUNC]]
func blockToFunc(_ x: @escaping @convention(block) () -> ()) -> () -> () {
  return x
}

// ==== Representation change + function type conversion

// CHECK-LABEL: sil hidden @_T024function_conversion_objc22blockToFuncExistentialypycSiyXBF : $@convention(thin) (@owned @convention(block) () -> Int) -> @owned @callee_owned () -> @out Any
// CHECK:         function_ref @_T0SiIyBd_SiIxd_TR
// CHECK:         partial_apply
// CHECK:         function_ref @_T0SiIxd_ypIxr_TR
// CHECK:         partial_apply
// CHECK:         return
func blockToFuncExistential(_ x: @escaping @convention(block) () -> Int) -> () -> Any {
  return x
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0SiIyBd_SiIxd_TR : $@convention(thin) (@owned @convention(block) () -> Int) -> Int

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0SiIxd_ypIxr_TR : $@convention(thin) (@owned @callee_owned () -> Int) -> @out Any

// C function pointer conversions

class A : NSObject {}
class B : A {}

// CHECK-LABEL: sil hidden @_T024function_conversion_objc18cFuncPtrConversionyAA1BCXCyAA1ACXCF
func cFuncPtrConversion(_ x: @escaping @convention(c) (A) -> ()) -> @convention(c) (B) -> () {
// CHECK:         convert_function %0 : $@convention(c) (A) -> () to $@convention(c) (B) -> ()
// CHECK:         return
  return x
}

func cFuncPtr(_ a: A) {}

// CHECK-LABEL: sil hidden @_T024function_conversion_objc19cFuncDeclConversionyAA1BCXCyF
func cFuncDeclConversion() -> @convention(c) (B) -> () {
// CHECK:         function_ref @_T024function_conversion_objc8cFuncPtryAA1ACFTo : $@convention(c) (A) -> ()
// CHECK:         convert_function %0 : $@convention(c) (A) -> () to $@convention(c) (B) -> ()
// CHECK:         return
  return cFuncPtr
}

func cFuncPtrConversionUnsupported(_ x: @escaping @convention(c) (@convention(block) () -> ()) -> ())
    -> @convention(c) (@convention(c) () -> ()) -> () {
  return x  // expected-error{{C function pointer signature '@convention(c) (@convention(block) () -> ()) -> ()' is not compatible with expected type '@convention(c) (@convention(c) () -> ()) -> ()'}}
}
