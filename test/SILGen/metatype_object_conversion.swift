// RUN: %target-swift-emit-silgen -enable-sil-ownership -sdk %S/Inputs -I %S/Inputs -enable-source-import -enable-objc-interop %s | %FileCheck %s

import Foundation

class C {}

protocol CP : class {}

@objc protocol OP {}

// CHECK-LABEL: sil hidden @$s26metatype_object_conversion0A8ToObjectyyXlAA1CCmF
func metatypeToObject(_ x: C.Type) -> AnyObject {
  // CHECK: bb0([[THICK:%.*]] : $@thick C.Type):
  // CHECK:   [[OBJC:%.*]] = thick_to_objc_metatype [[THICK]]
  // CHECK:   [[OBJECT:%.*]] = objc_metatype_to_object [[OBJC]]
  // CHECK:   return [[OBJECT]]
  return x
}

// CHECK-LABEL: sil hidden @$s26metatype_object_conversion27existentialMetatypeToObjectyyXlAA2CP_pXpF
func existentialMetatypeToObject(_ x: CP.Type) -> AnyObject {
  // CHECK: bb0([[THICK:%.*]] : $@thick CP.Type):
  // CHECK:   [[OBJC:%.*]] = thick_to_objc_metatype [[THICK]]
  // CHECK:   [[OBJECT:%.*]] = objc_existential_metatype_to_object [[OBJC]]
  // CHECK:   return [[OBJECT]]
  return x
}

// CHECK-LABEL: sil hidden @$s26metatype_object_conversion23protocolToProtocolClassSo0F0CyF
func protocolToProtocolClass() -> Protocol {
  // CHECK: [[PROTO:%.*]] = objc_protocol #OP
  // CHECK: [[COPIED_PROTO:%.*]] = copy_value [[PROTO]]
  // CHECK: return [[COPIED_PROTO]]
  return OP.self
}
