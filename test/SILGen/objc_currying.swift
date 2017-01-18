// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-silgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-silgen %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import gizmo

func curry_pod(_ x: CurryTest) -> (Int) -> Int {
  return x.pod
}
// CHECK-LABEL: sil hidden @_TF13objc_currying9curry_podFCSo9CurryTestFSiSi : $@convention(thin) (@owned CurryTest) -> @owned @callee_owned (Int) -> Int
// CHECK:      bb0([[ARG1:%.*]] : $CurryTest):
// CHECK:         [[THUNK:%.*]] = function_ref @[[THUNK_FOO_1:_TTOFCSo9CurryTest3podFSiSi]] : $@convention(thin) (@owned CurryTest) -> @owned @callee_owned (Int) -> Int
// CHECK:         [[COPIED_ARG1:%.*]] = copy_value [[ARG1]]
// CHECK:         [[FN:%.*]] = apply [[THUNK]]([[COPIED_ARG1]])
// CHECK:         destroy_value [[ARG1]]
// CHECK:         return [[FN]]
// CHECK: } // end sil function '_TF13objc_currying9curry_podFCSo9CurryTestFSiSi'

// CHECK: sil shared [thunk] @[[THUNK_FOO_1]] : $@convention(thin) (@owned CurryTest) -> @owned @callee_owned (Int) -> Int
// CHECK:   [[THUNK:%.*]] = function_ref @[[THUNK_FOO_2:_TTOFCSo9CurryTest3podfSiSi]]
// CHECK:   [[FN:%.*]] = partial_apply [[THUNK]](%0)
// CHECK:   return [[FN]]
// CHECK: } // end sil function '[[THUNK_FOO_1]]'

// CHECK: sil shared [thunk] @[[THUNK_FOO_2]] : $@convention(method) (Int, @guaranteed CurryTest) -> Int
// CHECK: bb0([[ARG1:%.*]] : $Int, [[ARG2:%.*]] : $CurryTest):
// CHECK:   [[COPIED_ARG2:%.*]] = copy_value [[ARG2]]
// CHECK:   [[METHOD:%.*]] = class_method [volatile] [[COPIED_ARG2]] : $CurryTest, #CurryTest.pod!1.foreign
// CHECK:   [[RESULT:%.*]] = apply [[METHOD]]([[ARG1]], [[COPIED_ARG2]])
// CHECK:   destroy_value [[COPIED_ARG2]]
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '[[THUNK_FOO_2]]'

func curry_bridged(_ x: CurryTest) -> (String!) -> String! {
  return x.bridged
}
// CHECK-LABEL: sil hidden @_TF13objc_currying13curry_bridgedFCSo9CurryTestFGSQSS_GSQSS_ : $@convention(thin) (@owned CurryTest) -> @owned @callee_owned (@owned Optional<String>) -> @owned Optional<String>
// CHECK: bb0([[ARG1:%.*]] : $CurryTest):
// CHECK:   [[THUNK:%.*]] = function_ref @[[THUNK_BAR_1:_TTOFCSo9CurryTest7bridgedFGSQSS_GSQSS_]]
// CHECK:   [[ARG1_COPY:%.*]] = copy_value [[ARG1]]
// CHECK:   [[FN:%.*]] = apply [[THUNK]]([[ARG1_COPY]])
// CHECK:   destroy_value [[ARG1]]
// CHECK:   return [[FN]]
// CHECK: } // end sil function '_TF13objc_currying13curry_bridgedFCSo9CurryTestFGSQSS_GSQSS_'

// CHECK: sil shared [thunk] @[[THUNK_BAR_1]] : $@convention(thin) (@owned CurryTest) -> @owned @callee_owned (@owned Optional<String>) -> @owned Optional<String>
// CHECK: bb0([[ARG1:%.*]] : $CurryTest):
// CHECK:   [[THUNK:%.*]] = function_ref @[[THUNK_BAR_2:_TTOFCSo9CurryTest7bridgedfGSQSS_GSQSS_]]
// CHECK:   [[FN:%.*]] = partial_apply [[THUNK]]([[ARG1]])
// CHECK:   return [[FN]]
// CHECK: } // end sil function '[[THUNK_BAR_1]]'

// CHECK: sil shared [thunk] @[[THUNK_BAR_2]] : $@convention(method) (@owned Optional<String>, @guaranteed CurryTest) -> @owned Optional<String>
// CHECK: bb0([[OPT_STRING:%.*]] : $Optional<String>, [[SELF:%.*]] : $CurryTest):
// CHECK:   cond_br {{%.*}}, bb1, bb2
// CHECK: bb1:
// CHECK:   [[STRING:%.*]] = unchecked_enum_data [[OPT_STRING]]
// CHECK:   [[BRIDGING_FUNC:%.*]] = function_ref @_TFE10FoundationSS19_bridgeToObjectiveCfT_CSo8NSString
// CHECK:   [[NSSTRING:%.*]] = apply [[BRIDGING_FUNC]]([[STRING]])
// CHECK:   [[OPT_NSSTRING:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[NSSTRING]] : $NSString
// CHECK:   destroy_value [[STRING]]
// CHECK:   br bb3([[OPT_NSSTRING]] : $Optional<NSString>)

// CHECK: bb2:
// CHECK:   [[OPT_NONE:%.*]] = enum $Optional<NSString>, #Optional.none!enumelt
// CHECK:   br bb3([[OPT_NONE]] : $Optional<NSString>)

// CHECK: bb3([[OPT_NSSTRING:%.*]] : $Optional<NSString>):
// CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK:   [[METHOD:%.*]] = class_method [volatile] [[SELF_COPY]] : $CurryTest, #CurryTest.bridged!1.foreign
// CHECK:   [[RESULT_OPT_NSSTRING:%.*]] = apply [[METHOD]]([[OPT_NSSTRING]], [[SELF_COPY]]) : $@convention(objc_method) (Optional<NSString>, CurryTest) -> @autoreleased Optional<NSString>
// CHECK:   [[IS_OPT:%.*]] = select_enum [[RESULT_OPT_NSSTRING]]
// CHECK:   cond_br [[IS_OPT]], bb4, bb5

// CHECK: bb4:
// CHECK:   [[RESULT_NSSTRING:%.*]] = unchecked_enum_data [[RESULT_OPT_NSSTRING]]
// CHECK:   [[BRIDGE_FUNC:%.*]] = function_ref @_TZFE10FoundationSS36_unconditionallyBridgeFromObjectiveCfGSqCSo8NSString_SS
// CHECK:   [[REWRAP_RESULT_NSSTRING:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[RESULT_NSSTRING]]
// CHECK:   [[RESULT_STRING:%.*]] = apply [[BRIDGE_FUNC]]([[REWRAP_RESULT_NSSTRING]]
// CHECK:   [[WRAPPED_RESULT_STRING:%.*]] = enum $Optional<String>, #Optional.some!enumelt.1, [[RESULT_STRING]]
// CHECK:   br bb6([[WRAPPED_RESULT_STRING]] : $Optional<String>)

// CHECK: bb5:
// CHECK:   [[OPT_NONE:%.*]] = enum $Optional<String>, #Optional.none!enumelt
// CHECK:   br bb6([[OPT_NONE]] : $Optional<String>)

// CHECK: bb6([[FINAL_RESULT:%.*]] : $Optional<String>):
// CHECK:   destroy_value [[SELF_COPY]]
// CHECK:   destroy_value [[OPT_NSSTRING]]
// CHECK:   return [[FINAL_RESULT]] : $Optional<String>
// CHECK: } // end sil function '[[THUNK_BAR_2]]'

func curry_returnsInnerPointer(_ x: CurryTest) -> () -> UnsafeMutableRawPointer! {
  return x.returnsInnerPointer
}
// CHECK-LABEL: sil hidden @_TF13objc_currying25curry_returnsInnerPointerFCSo9CurryTestFT_GSQSv_ : $@convention(thin) (@owned CurryTest) -> @owned @callee_owned () -> Optional<UnsafeMutableRawPointer> {
// CHECK: bb0([[SELF:%.*]] : $CurryTest):
// CHECK:   [[THUNK:%.*]] = function_ref @[[THUNK_RETURNSINNERPOINTER:_TTOFCSo9CurryTest19returnsInnerPointerFT_GSQSv_]]
// CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK:   [[FN:%.*]] = apply [[THUNK]]([[SELF_COPY]])
// CHECK:   destroy_value [[SELF]]
// CHECK:   return [[FN]]
// CHECK: } // end sil function '_TF13objc_currying25curry_returnsInnerPointerFCSo9CurryTestFT_GSQSv_'

// CHECK: sil shared [thunk] @[[THUNK_RETURNSINNERPOINTER]] : $@convention(thin) (@owned CurryTest) -> @owned @callee_owned () -> Optional<UnsafeMutableRawPointer>
// CHECK:   [[THUNK:%.*]] = function_ref @[[THUNK_RETURNSINNERPOINTER_2:_TTOFCSo9CurryTest19returnsInnerPointerfT_GSQSv_]]
// CHECK:   [[FN:%.*]] = partial_apply [[THUNK]](%0)
// CHECK:   return [[FN]]
// CHECK: } // end sil function '[[THUNK_RETURNSINNERPOINTER]]'

// CHECK: sil shared [thunk] @[[THUNK_RETURNSINNERPOINTER_2]] : $@convention(method) (@guaranteed CurryTest) -> Optional<UnsafeMutableRawPointer>
// CHECK:  bb0([[ARG1:%.*]] : $CurryTest):
// CHECK:   [[ARG1_COPY:%.*]] = copy_value [[ARG1]]
// CHECK:   [[METHOD:%.*]] = class_method [volatile] [[ARG1_COPY]] : $CurryTest, #CurryTest.returnsInnerPointer!1.foreign
// CHECK:   [[RES:%.*]] = apply [[METHOD]]([[ARG1_COPY]]) : $@convention(objc_method) (CurryTest) -> @unowned_inner_pointer Optional<UnsafeMutableRawPointer>
// CHECK:   autorelease_value 
// CHECK:   return [[RES]]
// CHECK: } // end sil function '[[THUNK_RETURNSINNERPOINTER_2]]'

// CHECK-LABEL: sil hidden @_TF13objc_currying19curry_pod_AnyObjectFPs9AnyObject_FSiSi : $@convention(thin) (@owned AnyObject) -> @owned @callee_owned (Int) -> Int
// CHECK: bb0([[ANY:%.*]] : $AnyObject):
// CHECK:   [[OPENED_ANY:%.*]] = open_existential_ref [[ANY]]
// CHECK:   [[OPENED_ANY_COPY:%.*]] = copy_value [[OPENED_ANY]]
// CHECK:   dynamic_method_br [[OPENED_ANY_COPY]] : $@opened({{.*}}) AnyObject, #CurryTest.pod!1.foreign, [[HAS_METHOD:bb[0-9]+]]
// CHECK:   [[HAS_METHOD]]([[METHOD:%.*]] : $@convention(objc_method) (Int, @opened({{.*}}) AnyObject) -> Int):
// CHECK:   [[OPENED_ANY_COPY_2:%.*]] = copy_value [[OPENED_ANY_COPY]]
// CHECK:   partial_apply [[METHOD]]([[OPENED_ANY_COPY_2]])
// CHECK: } // end sil function '_TF13objc_currying19curry_pod_AnyObjectFPs9AnyObject_FSiSi'
func curry_pod_AnyObject(_ x: AnyObject) -> (Int) -> Int {
  return x.pod!
}

// normalOwnership requires a thunk to bring the method to Swift conventions
// CHECK-LABEL: sil hidden @_TF13objc_currying31curry_normalOwnership_AnyObjectFPs9AnyObject_FGSQCSo9CurryTest_GSQS1__ : $@convention(thin) (@owned AnyObject) -> @owned @callee_owned (@owned Optional<CurryTest>) -> @owned Optional<CurryTest> {
// CHECK: bb0([[ANY:%.*]] : $AnyObject):
// CHECK:   [[OPENED_ANY:%.*]] = open_existential_ref [[ANY]]
// CHECK:   [[OPENED_ANY_COPY:%.*]] = copy_value [[OPENED_ANY]]
// CHECK:   dynamic_method_br [[OPENED_ANY_COPY]] : $@opened({{.*}}) AnyObject, #CurryTest.normalOwnership!1.foreign, [[HAS_METHOD:bb[0-9]+]]
// CHECK: [[HAS_METHOD]]([[METHOD:%.*]] : $@convention(objc_method) (Optional<CurryTest>, @opened({{.*}}) AnyObject) -> @autoreleased Optional<CurryTest>):
// CHECK:   [[OPENED_ANY_COPY_2:%.*]] = copy_value [[OPENED_ANY_COPY]]
// CHECK:   [[PA:%.*]] = partial_apply [[METHOD]]([[OPENED_ANY_COPY_2]])
// CHECK:   [[THUNK:%.*]] = function_ref @_TTRXFo_dGSqCSo9CurryTest__oGSqS___XFo_oGSqS___oGSqS___
// CHECK:   partial_apply [[THUNK]]([[PA]])
// CHECK: } // end sil function '_TF13objc_currying31curry_normalOwnership_AnyObjectFPs9AnyObject_FGSQCSo9CurryTest_GSQS1__'
func curry_normalOwnership_AnyObject(_ x: AnyObject) -> (CurryTest!) -> CurryTest! {
  return x.normalOwnership!
}

// weirdOwnership is NS_RETURNS_RETAINED and NS_CONSUMES_SELF so already
// follows Swift conventions
// CHECK-LABEL: sil hidden @_TF13objc_currying30curry_weirdOwnership_AnyObjectFPs9AnyObject_FGSQCSo9CurryTest_GSQS1__ : $@convention(thin) (@owned AnyObject) -> @owned @callee_owned (@owned Optional<CurryTest>) -> @owned Optional<CurryTest>
// CHECK: bb0([[ANY:%.*]] : $AnyObject):
// CHECK:   [[OPENED_ANY:%.*]] = open_existential_ref [[ANY]]
// CHECK:   [[OPENED_ANY_COPY:%.*]] = copy_value [[OPENED_ANY]]
// CHECK:   dynamic_method_br [[SELF:%.*]] : $@opened({{.*}}) AnyObject, #CurryTest.weirdOwnership!1.foreign, [[HAS_METHOD:bb[0-9]+]]
// CHECK: bb1([[METHOD:%.*]] : $@convention(objc_method) (@owned Optional<CurryTest>, @owned @opened({{.*}}) AnyObject) -> @owned Optional<CurryTest>):
// CHECK:   [[OPENED_ANY_COPY_2:%.*]] = copy_value [[OPENED_ANY_COPY]]
// CHECK:   partial_apply [[METHOD]]([[OPENED_ANY_COPY_2]])
// CHECK: } // end sil function '_TF13objc_currying30curry_weirdOwnership_AnyObjectFPs9AnyObject_FGSQCSo9CurryTest_GSQS1__'
func curry_weirdOwnership_AnyObject(_ x: AnyObject) -> (CurryTest!) -> CurryTest! {
  return x.weirdOwnership!
}

// bridged requires a thunk to handle bridging conversions
// CHECK-LABEL: sil hidden @_TF13objc_currying23curry_bridged_AnyObjectFPs9AnyObject_FGSQSS_GSQSS_ : $@convention(thin) (@owned AnyObject) -> @owned @callee_owned (@owned Optional<String>) -> @owned Optional<String>
// CHECK: bb0([[ANY:%.*]] : $AnyObject):
// CHECK:   [[OPENED_ANY:%.*]] = open_existential_ref [[ANY]]
// CHECK:   [[OPENED_ANY_COPY:%.*]] = copy_value [[OPENED_ANY]]
// CHECK:   dynamic_method_br [[OPENED_ANY_COPY]] : $@opened({{.*}}) AnyObject, #CurryTest.bridged!1.foreign, [[HAS_METHOD:bb[0-9]+]]
// CHECK: [[HAS_METHOD]]([[METHOD:%.*]] : $@convention(objc_method) (Optional<NSString>, @opened({{.*}}) AnyObject) -> @autoreleased Optional<NSString>):
// CHECK:   [[OPENED_ANY_COPY_2:%.*]] = copy_value [[OPENED_ANY_COPY]]
// CHECK:   [[PA:%.*]] = partial_apply [[METHOD]]([[OPENED_ANY_COPY_2]])
// CHECK:   [[THUNK:%.*]] = function_ref @_TTRXFo_dGSqCSo8NSString__oGSqS___XFo_oGSqSS__oGSqSS__
// CHECK:   partial_apply [[THUNK]]([[PA]])
// CHECK: } // end sil function '_TF13objc_currying23curry_bridged_AnyObjectFPs9AnyObject_FGSQSS_GSQSS_'
func curry_bridged_AnyObject(_ x: AnyObject) -> (String!) -> String! {
  return x.bridged!
}

// check that we substitute Self = AnyObject correctly for Self-returning
// methods
// CHECK-LABEL: sil hidden @_TF13objc_currying27curry_returnsSelf_AnyObjectFPs9AnyObject_FT_GSQPS0___ : $@convention(thin) (@owned AnyObject) -> @owned @callee_owned () -> @owned Optional<AnyObject> {
// CHECK: bb0([[ANY:%.*]] : $AnyObject):
// CHECK:   [[OPENED_ANY:%.*]] = open_existential_ref [[ANY]]
// CHECK:   [[OPENED_ANY_COPY:%.*]] = copy_value [[OPENED_ANY]]
// CHECK:   dynamic_method_br [[OPENED_ANY_COPY]] : $@opened({{.*}}) AnyObject, #CurryTest.returnsSelf!1.foreign, [[HAS_METHOD:bb[0-9]+]]
// CHECK: [[HAS_METHOD]]([[METHOD:%.*]] : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> @autoreleased Optional<AnyObject>):
// CHECK:   [[OPENED_ANY_COPY_2:%.*]] = copy_value [[OPENED_ANY_COPY]]
// CHECK:   [[PA:%.*]] = partial_apply [[METHOD]]([[OPENED_ANY_COPY_2]])
// CHECK: } // end sil function '_TF13objc_currying27curry_returnsSelf_AnyObjectFPs9AnyObject_FT_GSQPS0___'
func curry_returnsSelf_AnyObject(_ x: AnyObject) -> () -> AnyObject! {
  return x.returnsSelf!
}

// CHECK-LABEL: sil hidden @_TF13objc_currying35curry_returnsInnerPointer_AnyObjectFPs9AnyObject_FT_GSQSv_ : $@convention(thin) (@owned AnyObject) -> @owned @callee_owned () -> Optional<UnsafeMutableRawPointer> {
// CHECK: bb0([[ANY:%.*]] : $AnyObject):
// CHECK:   [[OPENED_ANY:%.*]] = open_existential_ref [[ANY]]
// CHECK:   [[OPENED_ANY_COPY:%.*]] = copy_value [[OPENED_ANY]]
// CHECK:   dynamic_method_br [[OPENED_ANY_COPY]] : $@opened({{.*}}) AnyObject, #CurryTest.returnsInnerPointer!1.foreign, [[HAS_METHOD:bb[0-9]+]]
// CHECK: [[HAS_METHOD]]([[METHOD:%.*]] : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> @unowned_inner_pointer Optional<UnsafeMutableRawPointer>):
// CHECK:   [[OPENED_ANY_COPY_2:%.*]] = copy_value [[OPENED_ANY_COPY]]
// CHECK:   [[PA:%.*]] = partial_apply [[METHOD]]([[OPENED_ANY_COPY_2]])
// CHECK: } // end sil function '_TF13objc_currying35curry_returnsInnerPointer_AnyObjectFPs9AnyObject_FT_GSQSv_'

func curry_returnsInnerPointer_AnyObject(_ x: AnyObject) -> () -> UnsafeMutableRawPointer! {
  return x.returnsInnerPointer!
}

