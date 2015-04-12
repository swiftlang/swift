// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation
import gizmo

func curry_pod(x: CurryTest) -> Int -> Int {
  return x.pod
}
// CHECK-LABEL: sil hidden @_TF13objc_currying9curry_podFCSo9CurryTestFSiSi : $@thin (@owned CurryTest) -> @owned @callee_owned (Int) -> Int
// CHECK:      bb0([[ARG1:%.*]] : $CurryTest):
// CHECK:         strong_retain [[ARG1]]
// CHECK:         [[THUNK:%.*]] = function_ref [[THUNK_FOO_1:@_TTOFCSo9CurryTest3podFS_FSiSi]] : $@thin (@owned CurryTest) -> @owned @callee_owned (Int) -> Int
// CHECK:         [[FN:%.*]] = apply [[THUNK]](%0)
// CHECK:         strong_release [[ARG1]]
// CHECK:         return [[FN]]

// CHECK: sil shared [[THUNK_FOO_1]] : $@thin (@owned CurryTest) -> @owned @callee_owned (Int) -> Int
// CHECK:   [[THUNK:%.*]] = function_ref [[THUNK_FOO_2:@_TTOFCSo9CurryTest3podfS_FSiSi]]
// CHECK:   [[FN:%.*]] = partial_apply [[THUNK]](%0)
// CHECK:   return [[FN]]

// CHECK: sil shared [[THUNK_FOO_2]] : $@cc(method) @thin (Int, @guaranteed CurryTest) -> Int
// CHECK: bb0([[ARG1:%.*]] : $Int, [[ARG2:%.*]] : $CurryTest):
// CHECK:   strong_retain [[ARG2]]
// CHECK:   [[METHOD:%.*]] = class_method [volatile] %1 : $CurryTest, #CurryTest.pod!1.foreign
// CHECK:   [[RESULT:%.*]] = apply [[METHOD]](%0, %1)
// CHECK:   strong_release [[ARG2]]
// CHECK:   return [[RESULT]]

func curry_bridged(x: CurryTest) -> String! -> String! {
  return x.bridged
}
// CHECK-LABEL: sil hidden @_TF13objc_currying13curry_bridgedFCSo9CurryTestFGSQSS_GSQSS_ : $@thin (@owned CurryTest) -> @owned @callee_owned (@owned ImplicitlyUnwrappedOptional<String>) -> @owned ImplicitlyUnwrappedOptional<String>
// CHECK:         [[THUNK:%.*]] = function_ref [[THUNK_BAR_1:@_TTOFCSo9CurryTest7bridgedFS_FGSQSS_GSQSS_]]
// CHECK:         [[FN:%.*]] = apply [[THUNK]](%0)
// CHECK:         return [[FN]]

// CHECK: sil shared [[THUNK_BAR_1]] : $@thin (@owned CurryTest) -> @owned @callee_owned (@owned ImplicitlyUnwrappedOptional<String>) -> @owned ImplicitlyUnwrappedOptional<String>
// CHECK:   [[THUNK:%.*]] = function_ref [[THUNK_BAR_2:@_TTOFCSo9CurryTest7bridgedfS_FGSQSS_GSQSS_]]
// CHECK:   [[FN:%.*]] = partial_apply [[THUNK]](%0)
// CHECK:   return [[FN]]

// CHECK: sil shared [[THUNK_BAR_2]] : $@cc(method) @thin (@owned ImplicitlyUnwrappedOptional<String>, @guaranteed CurryTest) -> @owned ImplicitlyUnwrappedOptional<String>
// CHECK:   function_ref @swift_StringToNSString
// CHECK:   [[METHOD:%.*]] = class_method [volatile] %1 : $CurryTest, #CurryTest.bridged!1.foreign
// CHECK:   [[RES:%.*]] = apply [[METHOD]]({{%.*}}, %1) : $@cc(objc_method) @thin (ImplicitlyUnwrappedOptional<NSString>, CurryTest) -> @autoreleased ImplicitlyUnwrappedOptional<NSString>
// CHECK:   strong_retain_autoreleased [[RES]]
// CHECK:   function_ref @swift_NSStringToString
// CHECK:   strong_release %1
// CHECK:   return {{%.*}} : $ImplicitlyUnwrappedOptional<String>

func curry_returnsInnerPointer(x: CurryTest) -> () -> UnsafeMutablePointer<Void> {
  return x.returnsInnerPointer
}
// CHECK-LABEL: sil hidden @_TF13objc_currying25curry_returnsInnerPointerFCSo9CurryTestFT_GVSs20UnsafeMutablePointerT__ : $@thin (@owned CurryTest) -> @owned @callee_owned () -> UnsafeMutablePointer<()> {
// CHECK:         [[THUNK:%.*]] = function_ref [[THUNK_RETURNSINNERPOINTER:@_TTOFCSo9CurryTest19returnsInnerPointerFS_FT_GVSs20UnsafeMutablePointerT__]]
// CHECK:         [[FN:%.*]] = apply [[THUNK]](%0)
// CHECK:         return [[FN]]

// CHECK: sil shared [[THUNK_RETURNSINNERPOINTER]] : $@thin (@owned CurryTest) -> @owned @callee_owned () -> UnsafeMutablePointer<()>
// CHECK:   [[THUNK:%.*]] = function_ref [[THUNK_RETURNSINNERPOINTER_2:@_TTOFCSo9CurryTest19returnsInnerPointerfS_FT_GVSs20UnsafeMutablePointerT__]]
// CHECK:   [[FN:%.*]] = partial_apply [[THUNK]](%0)
// CHECK:   return [[FN]]

// CHECK: sil shared @_TTOFCSo9CurryTest19returnsInnerPointerfS_FT_GVSs20UnsafeMutablePointerT__ : $@cc(method) @thin (@guaranteed CurryTest) -> UnsafeMutablePointer<()>
// CHECK:  bb0([[ARG1:%.*]] : 
// CHECK:   strong_retain [[ARG1]]
// CHECK:   [[METHOD:%.*]] = class_method [volatile] %0 : $CurryTest, #CurryTest.returnsInnerPointer!1.foreign
// CHECK:   [[RES:%.*]] = apply [[METHOD]](%0) : $@cc(objc_method) @thin (CurryTest) -> @unowned_inner_pointer UnsafeMutablePointer<()>
// CHECK:   autorelease_value %0
// CHECK:   return [[RES]]

// CHECK-LABEL: sil hidden @_TF13objc_currying19curry_pod_AnyObjectFPSs9AnyObject_FSiSi : $@thin (@owned AnyObject) -> @owned @callee_owned (Int) -> Int
// CHECK:         dynamic_method_br [[SELF:%.*]] : $@opened({{.*}}) AnyObject, #CurryTest.pod!1.foreign, [[HAS_METHOD:bb[0-9]+]]
// CHECK:       [[HAS_METHOD]]([[METHOD:%.*]] : $@cc(objc_method) @thin (Int, @opened({{.*}}) AnyObject) -> Int):
// CHECK:         partial_apply [[METHOD]]([[SELF]])
func curry_pod_AnyObject(x: AnyObject) -> Int -> Int {
  return x.pod!
}

// normalOwnership requires a thunk to bring the method to Swift conventions
// CHECK-LABEL: sil hidden @_TF13objc_currying31curry_normalOwnership_AnyObjectFPSs9AnyObject_FGSQCSo9CurryTest_GSQS1__
// CHECK:         dynamic_method_br [[SELF:%.*]] : $@opened({{.*}}) AnyObject, #CurryTest.normalOwnership!1.foreign, [[HAS_METHOD:bb[0-9]+]]
// CHECK:       [[HAS_METHOD]]([[METHOD:%.*]] : $@cc(objc_method) @thin (ImplicitlyUnwrappedOptional<CurryTest>, @opened({{.*}}) AnyObject) -> @autoreleased ImplicitlyUnwrappedOptional<CurryTest>):
// CHECK:         [[PA:%.*]] = partial_apply [[METHOD]]([[SELF]])
// CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFo_dGSQCSo9CurryTest__aGSQS___XFo_oGSQS___oGSQS___
// CHECK:         partial_apply [[THUNK]]([[PA]])
func curry_normalOwnership_AnyObject(x: AnyObject) -> CurryTest! -> CurryTest! {
  return x.normalOwnership!
}

// weirdOwnership is NS_RETURNS_RETAINED and NS_CONSUMES_SELF so already
// follows Swift conventions
// CHECK-LABEL: sil hidden @_TF13objc_currying30curry_weirdOwnership_AnyObjectFPSs9AnyObject_FGSQCSo9CurryTest_GSQS1__ : $@thin (@owned AnyObject) -> @owned @callee_owned (@owned ImplicitlyUnwrappedOptional<CurryTest>) -> @owned ImplicitlyUnwrappedOptional<CurryTest> 
// CHECK:         dynamic_method_br [[SELF:%.*]] : $@opened({{.*}}) AnyObject, #CurryTest.weirdOwnership!1.foreign, [[HAS_METHOD:bb[0-9]+]]
// CHECK:       bb1([[METHOD:%.*]] : $@cc(objc_method) @thin (@owned ImplicitlyUnwrappedOptional<CurryTest>, @owned @opened({{.*}}) AnyObject) -> @owned ImplicitlyUnwrappedOptional<CurryTest>):
// CHECK:         partial_apply [[METHOD]]([[SELF]])
func curry_weirdOwnership_AnyObject(x: AnyObject) -> CurryTest! -> CurryTest! {
  return x.weirdOwnership!
}

// bridged requires a thunk to handle bridging conversions
// CHECK-LABEL: sil hidden @_TF13objc_currying23curry_bridged_AnyObjectFPSs9AnyObject_FGSQSS_GSQSS_ : $@thin (@owned AnyObject) -> @owned @callee_owned (@owned ImplicitlyUnwrappedOptional<String>) -> @owned ImplicitlyUnwrappedOptional<String>
// CHECK:         dynamic_method_br [[SELF:%.*]] : $@opened({{.*}}) AnyObject, #CurryTest.bridged!1.foreign, [[HAS_METHOD:bb[0-9]+]]
// CHECK:       [[HAS_METHOD]]([[METHOD:%.*]] : $@cc(objc_method) @thin (ImplicitlyUnwrappedOptional<NSString>, @opened({{.*}}) AnyObject) -> @autoreleased ImplicitlyUnwrappedOptional<NSString>):
// CHECK:         [[PA:%.*]] = partial_apply [[METHOD]]([[SELF]])
// CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFo_dGSQCSo8NSString__aGSQS___XFo_oGSQSS__oGSQSS__ 
// CHECK:         partial_apply [[THUNK]]([[PA]])
func curry_bridged_AnyObject(x: AnyObject) -> String! -> String! {
  return x.bridged!
}

// check that we substitute Self = AnyObject correctly for Self-returning
// methods
// CHECK-LABEL: sil hidden @_TF13objc_currying27curry_returnsSelf_AnyObjectFPSs9AnyObject_FT_GSQPS0___
// CHECK:         dynamic_method_br [[SELF:%.*]] : $@opened({{.*}}) AnyObject, #CurryTest.returnsSelf!1.foreign, [[HAS_METHOD:bb[0-9]+]]
// CHECK:       [[HAS_METHOD]]([[METHOD:%.*]] : $@cc(objc_method) @thin (@opened({{.*}}) AnyObject) -> @autoreleased ImplicitlyUnwrappedOptional<AnyObject>):
// CHECK:         [[PA:%.*]] = partial_apply [[METHOD]]([[SELF]])
// CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFo__aGSQPSs9AnyObject___XFo__oGSQPS____
// CHECK:         partial_apply [[THUNK]]([[PA]])
func curry_returnsSelf_AnyObject(x: AnyObject) -> () -> AnyObject! {
  return x.returnsSelf!
}

// CHECK-LABEL: sil hidden @_TF13objc_currying35curry_returnsInnerPointer_AnyObjectFPSs9AnyObject_FT_GVSs20UnsafeMutablePointerT__
// CHECK:         dynamic_method_br [[SELF:%.*]] : $@opened({{.*}}) AnyObject, #CurryTest.returnsInnerPointer!1.foreign, [[HAS_METHOD:bb[0-9]+]]
// CHECK:       [[HAS_METHOD]]([[METHOD:%.*]] : $@cc(objc_method) @thin (@opened({{.*}}) AnyObject) -> @unowned_inner_pointer UnsafeMutablePointer<()>):
// CHECK:         [[PA:%.*]] = partial_apply [[METHOD]]([[SELF]])
// CHECK:         [[PA]]{{.*}}@owned @callee_owned () -> UnsafeMutablePointer<()>

func curry_returnsInnerPointer_AnyObject(x: AnyObject) -> () -> UnsafeMutablePointer<Void> {
  return x.returnsInnerPointer!
}
