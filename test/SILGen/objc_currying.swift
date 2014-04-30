// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -emit-silgen -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

import Foundation
import gizmo

func curry_foo(x: CurryTest) -> Int -> Int {
  return x.foo
}
// CHECK-LABEL: sil @_TF13objc_currying9curry_fooFCSo9CurryTestFSiSi : $@thin (@owned CurryTest) -> @owned @callee_owned (Int) -> Int
// CHECK:         [[THUNK:%.*]] = function_ref [[THUNK_FOO_1:@_TTOFCSo9CurryTest3fooFS_FSiSi]]
// CHECK:         [[FN:%.*]] = apply [[THUNK]](%0)
// CHECK:         return [[FN]]

// CHECK: sil shared [[THUNK_FOO_1]] : $@thin (@owned CurryTest) -> @owned @callee_owned (Int) -> Int
// CHECK:   [[THUNK:%.*]] = function_ref [[THUNK_FOO_2:@_TTOFCSo9CurryTest3foofS_FSiSi]]
// CHECK:   [[FN:%.*]] = partial_apply [[THUNK]](%0)
// CHECK:   return [[FN]]

// CHECK: sil shared [[THUNK_FOO_2]] : $@cc(method) @thin (Int, @owned CurryTest) -> Int
// CHECK:   [[METHOD:%.*]] = class_method [volatile] %1 : $CurryTest, #CurryTest.foo!1.foreign
// CHECK:   [[RESULT:%.*]] = apply [[METHOD]](%0, %1)
// CHECK:   strong_release %1
// CHECK:   return [[RESULT]]

func curry_bar(x: CurryTest) -> String! -> String! {
  return x.bar
}
// CHECK-LABEL: sil @_TF13objc_currying9curry_barFCSo9CurryTestFGSQSS_GSQSS_ : $@thin (@owned CurryTest) -> @owned @callee_owned (@owned UncheckedOptional<String>) -> @owned UncheckedOptional<String>
// CHECK:         [[THUNK:%.*]] = function_ref [[THUNK_BAR_1:@_TTOFCSo9CurryTest3barFS_FGSQSS_GSQSS_]]
// CHECK:         [[FN:%.*]] = apply [[THUNK]](%0)
// CHECK:         return [[FN]]

// CHECK: sil shared [[THUNK_BAR_1]] : $@thin (@owned CurryTest) -> @owned @callee_owned (@owned UncheckedOptional<String>) -> @owned UncheckedOptional<String>
// CHECK:   [[THUNK:%.*]] = function_ref [[THUNK_BAR_2:@_TTOFCSo9CurryTest3barfS_FGSQSS_GSQSS_]]
// CHECK:   [[FN:%.*]] = partial_apply [[THUNK]](%0)
// CHECK:   return [[FN]]

// CHECK: sil shared [[THUNK_BAR_2]] : $@cc(method) @thin (@owned UncheckedOptional<String>, @owned CurryTest) -> @owned UncheckedOptional<String>
// CHECK:   function_ref @swift_StringToNSString
// CHECK:   [[METHOD:%.*]] = class_method [volatile] %1 : $CurryTest, #CurryTest.bar!1.foreign
// CHECK:   [[RES:%.*]] = apply [[METHOD]]({{%.*}}, %1) : $@cc(objc_method) @thin (UncheckedOptional<NSString>, CurryTest) -> @autoreleased UncheckedOptional<NSString>
// CHECK:   strong_retain_autoreleased [[RES]]
// CHECK:   function_ref @swift_NSStringToString
// CHECK:   strong_release %1
// CHECK:   return {{%.*}} : $UncheckedOptional<String>

