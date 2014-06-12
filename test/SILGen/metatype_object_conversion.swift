// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -enable-metatype-object-conversions -emit-silgen -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

import Foundation

class C {}

@class_protocol protocol CP {}

@objc protocol OP {}

// CHECK-LABEL: sil @_TF26metatype_object_conversion16metatypeToObjectFMCS_1CPSs9AnyObject_ 
func metatypeToObject(x: C.Type) -> AnyObject {
  // CHECK: bb0([[THICK:%.*]] : $@thick C.Type):
  // CHECK:   [[OBJC:%.*]] = thick_to_objc_metatype [[THICK]]
  // CHECK:   [[OBJECT:%.*]] = objc_metatype_to_object [[OBJC]]
  // CHECK:   return [[OBJECT]]
  return x
}

// CHECK-LABEL: sil @_TF26metatype_object_conversion27existentialMetatypeToObjectFPMPS_2CP_PSs9AnyObject_
func existentialMetatypeToObject(x: CP.Type) -> AnyObject {
  // CHECK: bb0([[THICK:%.*]] : $@thick CP.Type):
  // CHECK:   [[OBJC:%.*]] = thick_to_objc_metatype [[THICK]]
  // CHECK:   [[OBJECT:%.*]] = objc_existential_metatype_to_object [[OBJC]]
  // CHECK:   return [[OBJECT]]
  return x
}

// CHECK-LABEL: sil @_TF26metatype_object_conversion23protocolToProtocolClassFT_CSo8Protocol
func protocolToProtocolClass() -> Protocol {
  // CHECK: [[PROTO:%.*]] = objc_protocol #OP
  // CHECK: return [[PROTO]]
  return OP.self
}
