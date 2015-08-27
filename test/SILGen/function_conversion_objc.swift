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

@objc protocol NSHipster {}

// CHECK-LABEL: sil hidden @_TF24function_conversion_objc31convExistentialMetatypeToObjectFFPS_9NSHipster_PMPS0__T_
func convExistentialMetatypeToObject(f: NSHipster -> NSHipster.Type) {
// CHECK:         function_ref @_TTRXFo_oP24function_conversion_objc9NSHipster__dXPMTPS0___XFo_oPS0___oPSs9AnyObject__
// CHECK:         partial_apply
  let _: NSHipster -> AnyObject = f
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo_oP24function_conversion_objc9NSHipster__dXPMTPS0___XFo_oPS0___oPSs9AnyObject__ : $@convention(thin) (@owned NSHipster, @owned @callee_owned (@owned NSHipster) -> @thick NSHipster.Type) -> @owned AnyObject
// CHECK:         apply %1(%0)
// CHECK:         thick_to_objc_metatype {{.*}} : $@thick NSHipster.Type to $@objc_metatype NSHipster.Type
// CHECK:         objc_existential_metatype_to_object {{.*}} : $@objc_metatype NSHipster.Type to $AnyObject
// CHECK:         return

// CHECK-LABEL: sil hidden @_TF24function_conversion_objc28convProtocolMetatypeToObjectFFT_MPS_9NSHipster_T_
func convProtocolMetatypeToObject(f: () -> NSHipster.Protocol) {
// CHECK:         function_ref @_TTRXFo__dXMtP24function_conversion_objc9NSHipster__XFo__oCSo8Protocol_
// CHECK:         partial_apply
  let _: () -> Protocol = f
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo__dXMtP24function_conversion_objc9NSHipster__XFo__oCSo8Protocol_ : $@convention(thin) (@owned @callee_owned () -> @thin NSHipster.Protocol) -> @owned Protocol
// CHECK:         apply %0() : $@callee_owned () -> @thin NSHipster.Protocol
// CHECK:         objc_protocol #NSHipster : $Protocol
// CHECK:         strong_retain
// CHECK:         return
