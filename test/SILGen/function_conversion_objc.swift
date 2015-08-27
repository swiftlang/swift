// RUN: %target-swift-frontend -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -emit-silgen | FileCheck %s

import Foundation

// REQUIRES: objc_interop

// CHECK-LABEL: sil hidden @_TF24function_conversion_objc20convMetatypeToObjectFFCSo8NSObjectMS0_T_
func convMetatypeToObject(f: NSObject -> NSObject.Type) {
// CHECK:         function_ref @_TTRXFo_oCSo8NSObject_dXMTS__XFo_oS__oPSs9AnyObject__
// CHECK:         partial_apply
  let _: NSObject -> AnyObject = f
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo_oCSo8NSObject_dXMTS__XFo_oS__oPSs9AnyObject__ : $@convention(thin) (@owned NSObject, @owned @callee_owned (@owned NSObject) -> @thick NSObject.Type) -> @owned AnyObject {
// CHECK:         apply %1(%0)
// CHECK:         thick_to_objc_metatype {{.*}} : $@thick NSObject.Type to $@objc_metatype NSObject.Type
// CHECK:         objc_metatype_to_object {{.*}} : $@objc_metatype NSObject.Type to $AnyObject
// CHECK:         return

@objc protocol NSBurrito {}

// CHECK-LABEL: sil hidden @_TF24function_conversion_objc31convExistentialMetatypeToObjectFFPS_9NSBurrito_PMPS0__T_
func convExistentialMetatypeToObject(f: NSBurrito -> NSBurrito.Type) {
// CHECK:         function_ref @_TTRXFo_oP24function_conversion_objc9NSBurrito__dXPMTPS0___XFo_oPS0___oPSs9AnyObject__
// CHECK:         partial_apply
  let _: NSBurrito -> AnyObject = f
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo_oP24function_conversion_objc9NSBurrito__dXPMTPS0___XFo_oPS0___oPSs9AnyObject__ : $@convention(thin) (@owned NSBurrito, @owned @callee_owned (@owned NSBurrito) -> @thick NSBurrito.Type) -> @owned AnyObject
// CHECK:         apply %1(%0)
// CHECK:         thick_to_objc_metatype {{.*}} : $@thick NSBurrito.Type to $@objc_metatype NSBurrito.Type
// CHECK:         objc_existential_metatype_to_object {{.*}} : $@objc_metatype NSBurrito.Type to $AnyObject
// CHECK:         return

// CHECK-LABEL: sil hidden @_TF24function_conversion_objc28convProtocolMetatypeToObjectFFT_MPS_9NSBurrito_T_
func convProtocolMetatypeToObject(f: () -> NSBurrito.Protocol) {
// CHECK:         function_ref @_TTRXFo__dXMtP24function_conversion_objc9NSBurrito__XFo__oCSo8Protocol_
// CHECK:         partial_apply
  let _: () -> Protocol = f
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo__dXMtP24function_conversion_objc9NSBurrito__XFo__oCSo8Protocol_ : $@convention(thin) (@owned @callee_owned () -> @thin NSBurrito.Protocol) -> @owned Protocol
// CHECK:         apply %0() : $@callee_owned () -> @thin NSBurrito.Protocol
// CHECK:         objc_protocol #NSBurrito : $Protocol
// CHECK:         strong_retain
// CHECK:         return
