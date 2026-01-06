// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_objc.swift -disable-objc-attr-requires-foundation-module -enable-objc-interop
// RUN: llvm-bcanalyzer %t/def_objc.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -module-name objc -Xllvm -sil-print-types -emit-silgen -I %t %s -o - | %FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_objc

// SIL: sil hidden [ossa] @$s4objc9testProto3objy04def_A09ObjCProto_p_tF : $@convention(thin) (@guaranteed any ObjCProto) -> () {
func testProto(obj obj: ObjCProto) {
  // SIL: = objc_method {{%.*}} : $@opened({{.*}}, any ObjCProto) Self, #ObjCProto.doSomething!foreign
  obj.doSomething()
}

// SIL: sil hidden [ossa] @$s4objc9testClass3objy04def_A09ObjCClassC_tF : $@convention(thin) (@guaranteed ObjCClass) -> () {
func testClass(obj obj: ObjCClass) {
  // SIL: = objc_method %{{.+}} : $ObjCClass, #ObjCClass.implicitlyObjC!foreign
  obj.implicitlyObjC()

  // SIL: = objc_method %{{.+}} : $@objc_metatype ObjCClass.Type, #ObjCClass.classMethod!foreign
  ObjCClass.classMethod()
}

// SIL: sil hidden [ossa] @$s4objc15testNativeClass3objy04def_A012NonObjCClassC_tF : $@convention(thin) (@guaranteed NonObjCClass) -> () {
func testNativeClass(obj obj: NonObjCClass) {
  // SIL: = objc_method %{{.+}} : $NonObjCClass, #NonObjCClass.doSomething!foreign
  // SIL: = objc_method %{{.+}} : $NonObjCClass, #NonObjCClass.objcMethod!foreign
  obj.doSomething()
  obj.objcMethod()

  // SIL: objc_method [[OBJ:%[0-9]+]] : $NonObjCClass, #NonObjCClass.objcProp!getter.foreign
  var x = obj.objcProp
  
  // SIL: objc_method [[OBJ:%[0-9]+]] : $NonObjCClass, #NonObjCClass.subscript!getter.foreign
  _ = obj[42]
}

