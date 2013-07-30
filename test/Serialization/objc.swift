// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o %t/def_objc.swiftmodule %S/Inputs/def_objc.swift
// RUN: llvm-bcanalyzer %t/def_objc.swiftmodule | FileCheck %s
// RUN: %swift -emit-silgen -I=%t %s -o - | FileCheck %s -check-prefix=SIL

// CHECK-NOT: FALL_BACK_TO_TRANSLATION_UNIT
// CHECK-NOT: UnknownCode

import def_objc

// SIL: sil @_T4objc9testProtoFT3objPSo9ObjCProto__T_ : $[thin] (obj : ObjCProto) -> () {
func testProto(obj : ObjCProto) {
  // SIL: = protocol_method [volatile] %{{.+}} : $ObjCProto, #ObjCProto.doSomething!1.objc
  obj.doSomething()
}

// SIL: sil @_T4objc9testClassFT3objCSo9ObjCClass_T_ : $[thin] (obj : ObjCClass) -> () {
func testClass(obj : ObjCClass) {
  // SIL: = class_method [volatile] %{{.+}} : $ObjCClass, #ObjCClass.implicitlyObjC!1.objc
  obj.implicitlyObjC()
  
  // SIL: = class_method [volatile] %{{.+}} : $ObjCClass.metatype, #ObjCClass.classMethod!1.objc
  ObjCClass.classMethod()
}

// SIL: sil @_T4objc15testNativeClassFT3objC8def_objc12NonObjCClass_T_ : $[thin] (obj : NonObjCClass) -> () {
func testNativeClass(obj : NonObjCClass) {
  // SIL: = class_method [volatile] %{{.+}} : $NonObjCClass, #NonObjCClass.doSomething!1.objc
  // SIL: = class_method [volatile] %{{.+}} : $NonObjCClass, #NonObjCClass.objcMethod!1.objc
  obj.doSomething()
  obj.objcMethod()

  // FIXME: This should not be performing direct access.
  // SIL: ref_element_addr %{{.+}} : $NonObjCClass, #objcProp
  var x = obj.objcProp
  
  // FIXME: This should be using the ObjC entry point.
  // SIL: = function_ref @_TC8def_objc12NonObjCClass11__subscriptFT1iSi_T_g
  var _ = obj[42]
}

