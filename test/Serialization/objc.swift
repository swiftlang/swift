// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_objc.swift -disable-objc-attr-requires-foundation-module
// RUN: llvm-bcanalyzer %t/def_objc.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -I %t %s -o - | %FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_objc

// SIL: sil hidden @_T04objc9testProtoy04def_A09ObjCProto_p3obj_tF : $@convention(thin) (@owned ObjCProto) -> () {
func testProto(obj obj: ObjCProto) {
  // SIL: = witness_method [volatile] $@opened({{.*}}) ObjCProto, #ObjCProto.doSomething!1.foreign
  obj.doSomething()
}

// SIL: sil hidden @_T04objc9testClassy04def_A09ObjCClassC3obj_tF : $@convention(thin) (@owned ObjCClass) -> () {
func testClass(obj obj: ObjCClass) {
  // SIL: = class_method [volatile] %{{.+}} : $ObjCClass, #ObjCClass.implicitlyObjC!1.foreign
  obj.implicitlyObjC()

  // SIL: = class_method [volatile] %{{.+}} : $@thick ObjCClass.Type, #ObjCClass.classMethod!1.foreign
  ObjCClass.classMethod()
}

// SIL: sil hidden @_T04objc15testNativeClassy04def_A012NonObjCClassC3obj_tF : $@convention(thin) (@owned NonObjCClass) -> () {
func testNativeClass(obj obj: NonObjCClass) {
  // SIL: = class_method [volatile] %{{.+}} : $NonObjCClass, #NonObjCClass.doSomething!1.foreign
  // SIL: = class_method [volatile] %{{.+}} : $NonObjCClass, #NonObjCClass.objcMethod!1.foreign
  obj.doSomething()
  obj.objcMethod()

  // SIL: class_method [volatile] [[OBJ:%[0-9]+]] : $NonObjCClass, #NonObjCClass.objcProp!getter.1.foreign
  var x = obj.objcProp
  
  // SIL: class_method [volatile] [[OBJ:%[0-9]+]] : $NonObjCClass, #NonObjCClass.subscript!getter.1.foreign
  _ = obj[42]
}

