// RUN: %target-swift-frontend -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -emit-silgen -verify | %FileCheck %s

import Foundation

// REQUIRES: objc_interop

// ==== Metatype to object conversions

// CHECK-LABEL: sil hidden @_TF24function_conversion_objc20convMetatypeToObjectFFCSo8NSObjectMS0_T_
func convMetatypeToObject(_ f: @escaping (NSObject) -> NSObject.Type) {
// CHECK:         function_ref @_TTRXFo_oCSo8NSObject_dXMTS__XFo_oS__oPs9AnyObject__
// CHECK:         partial_apply
  let _: (NSObject) -> AnyObject = f
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo_oCSo8NSObject_dXMTS__XFo_oS__oPs9AnyObject__ : $@convention(thin) (@owned NSObject, @owned @callee_owned (@owned NSObject) -> @thick NSObject.Type) -> @owned AnyObject {
// CHECK:         apply %1(%0)
// CHECK:         thick_to_objc_metatype {{.*}} : $@thick NSObject.Type to $@objc_metatype NSObject.Type
// CHECK:         objc_metatype_to_object {{.*}} : $@objc_metatype NSObject.Type to $AnyObject
// CHECK:         return

@objc protocol NSBurrito {}

// CHECK-LABEL: sil hidden @_TF24function_conversion_objc31convExistentialMetatypeToObjectFFPS_9NSBurrito_PMPS0__T_
func convExistentialMetatypeToObject(_ f: @escaping (NSBurrito) -> NSBurrito.Type) {
// CHECK:         function_ref @_TTRXFo_oP24function_conversion_objc9NSBurrito__dXPMTPS0___XFo_oPS0___oPs9AnyObject__
// CHECK:         partial_apply
  let _: (NSBurrito) -> AnyObject = f
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo_oP24function_conversion_objc9NSBurrito__dXPMTPS0___XFo_oPS0___oPs9AnyObject__ : $@convention(thin) (@owned NSBurrito, @owned @callee_owned (@owned NSBurrito) -> @thick NSBurrito.Type) -> @owned AnyObject
// CHECK:         apply %1(%0)
// CHECK:         thick_to_objc_metatype {{.*}} : $@thick NSBurrito.Type to $@objc_metatype NSBurrito.Type
// CHECK:         objc_existential_metatype_to_object {{.*}} : $@objc_metatype NSBurrito.Type to $AnyObject
// CHECK:         return

// CHECK-LABEL: sil hidden @_TF24function_conversion_objc28convProtocolMetatypeToObjectFFT_MPS_9NSBurrito_T_
func convProtocolMetatypeToObject(_ f: @escaping () -> NSBurrito.Protocol) {
// CHECK:         function_ref @_TTRXFo__dXMtP24function_conversion_objc9NSBurrito__XFo__oCSo8Protocol_
// CHECK:         partial_apply
  let _: () -> Protocol = f
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo__dXMtP24function_conversion_objc9NSBurrito__XFo__oCSo8Protocol_ : $@convention(thin) (@owned @callee_owned () -> @thin NSBurrito.Protocol) -> @owned Protocol
// CHECK:         apply %0() : $@callee_owned () -> @thin NSBurrito.Protocol
// CHECK:         objc_protocol #NSBurrito : $Protocol
// CHECK:         strong_retain
// CHECK:         return

// ==== Representation conversions

// CHECK-LABEL: sil hidden @_TF24function_conversion_objc11funcToBlockFFT_T_bT_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> @owned @convention(block) () -> ()
// CHECK:         [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage
// CHECK:         [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]]
// CHECK:         [[COPY:%.*]] = copy_block [[BLOCK]] : $@convention(block) () -> ()
// CHECK:         return [[COPY]]
func funcToBlock(_ x: @escaping () -> ()) -> @convention(block) () -> () {
  return x
}

// CHECK-LABEL: sil hidden @_TF24function_conversion_objc11blockToFuncFbT_T_FT_T_ : $@convention(thin) (@owned @convention(block) () -> ()) -> @owned @callee_owned () -> ()
// CHECK:         [[COPIED:%.*]] = copy_block %0
// CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFdCb___XFo___
// CHECK:         [[FUNC:%.*]] = partial_apply [[THUNK]]([[COPIED]])
// CHECK:         return [[FUNC]]
func blockToFunc(_ x: @escaping @convention(block) () -> ()) -> () -> () {
  return x
}

// ==== Representation change + function type conversion

// CHECK-LABEL: sil hidden @_TF24function_conversion_objc22blockToFuncExistentialFbT_SiFT_P_ : $@convention(thin) (@owned @convention(block) () -> Int) -> @owned @callee_owned () -> @out Any
// CHECK:         function_ref @_TTRXFdCb__dSi_XFo__dSi_
// CHECK:         partial_apply
// CHECK:         function_ref @_TTRXFo__dSi_XFo__iP__
// CHECK:         partial_apply
// CHECK:         return
func blockToFuncExistential(_ x: @escaping @convention(block) () -> Int) -> () -> Any {
  return x
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFdCb__dSi_XFo__dSi_ : $@convention(thin) (@owned @convention(block) () -> Int) -> Int

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo__dSi_XFo__iP__ : $@convention(thin) (@owned @callee_owned () -> Int) -> @out Any

// C function pointer conversions

class A : NSObject {}
class B : A {}

// CHECK-LABEL: sil hidden @_TF24function_conversion_objc18cFuncPtrConversionFcCS_1AT_cCS_1BT_
func cFuncPtrConversion(_ x: @escaping @convention(c) (A) -> ()) -> @convention(c) (B) -> () {
// CHECK:         convert_function %0 : $@convention(c) (A) -> () to $@convention(c) (B) -> ()
// CHECK:         return
  return x
}

func cFuncPtr(_ a: A) {}

// CHECK-LABEL: sil hidden @_TF24function_conversion_objc19cFuncDeclConversionFT_cCS_1BT_
func cFuncDeclConversion() -> @convention(c) (B) -> () {
// CHECK:         function_ref @_TToF24function_conversion_objc8cFuncPtrFCS_1AT_ : $@convention(c) (A) -> ()
// CHECK:         convert_function %0 : $@convention(c) (A) -> () to $@convention(c) (B) -> ()
// CHECK:         return
  return cFuncPtr
}

func cFuncPtrConversionUnsupported(_ x: @escaping @convention(c) (@convention(block) () -> ()) -> ())
    -> @convention(c) (@convention(c) () -> ()) -> () {
  return x  // expected-error{{C function pointer signature '@convention(c) (@convention(block) () -> ()) -> ()' is not compatible with expected type '@convention(c) (@convention(c) () -> ()) -> ()'}}
}
