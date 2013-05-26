// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -nsstring-is-string -emit-sil -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s | FileCheck %s

import Foundation

func getDescription(o:NSObject) -> String {
  return o.description()
}
// CHECK: sil @_T20objc_nsstring_bridge14getDescriptionFT1oCSo8NSObject_SS : $[thin] (o : NSObject) -> String {
// CHECK: bb0({{%.*}} : $NSObject):
// CHECK:  [[DESCRIPTION:%.*]] = class_method {{%.*}}, @description.1.objc
// CHECK:  [[RES_BRIDGED:%.*]] = apply [[DESCRIPTION]]({{%.*}})
// CHECK:  retain_autoreleased [[RES_BRIDGED]]
// CHECK:  [[NSSTRING_TO_STRING:%.*]] = function_ref ${{.*}}, @swift_NSStringToString
// CHECK:  [[NATIVE_BUF:%.*]] = alloc_var stack $String
// CHECK:  initialize_var [[NATIVE_BUF]]
// CHECK:  apply [[NSSTRING_TO_STRING]]([[RES_BRIDGED]], [[NATIVE_BUF]])
// CHECK:  [[RES_NATIVE:%.*]] = load [[NATIVE_BUF]]
// CHECK:  return [[RES_NATIVE]]
// CHECK:}

func getUppercaseString(s:NSString) -> String {
  return s.uppercaseString()
}
// CHECK: sil @_T20objc_nsstring_bridge18getUppercaseStringFT1sCSo8NSString_SS : $[thin] (s : NSString) -> String {
// CHECK: bb0({{%.*}} : $NSString):
// -- The 'self' argument of NSString methods doesn't bridge.
// CHECK-NOT: function_ref ${{.*}}, @swift_NSStringToString
// CHECK-NOT: function_ref ${{.*}}, @swift_StringToNSString
// CHECK:   [[UPPERCASE_STRING:%.*]] = class_method {{%.*}}, @uppercaseString.1.objc
// CHECK:   [[RES_BRIDGED:%.*]] = apply [[UPPERCASE_STRING]]({{%.*}})
// CHECK:   retain_autoreleased [[RES_BRIDGED]]
// CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref ${{.*}}, @swift_NSStringToString
// CHECK:   [[NATIVE_BUF:%.*]] = alloc_var stack $String
// CHECK:   initialize_var [[NATIVE_BUF]]
// CHECK:   apply [[NSSTRING_TO_STRING]]([[RES_BRIDGED]], [[NATIVE_BUF]])
// CHECK:   [[RES_NATIVE:%.*]] = load [[NATIVE_BUF]]
// CHECK:   return [[RES_NATIVE]]
// CHECK: }

// @interface Foo -(void) setFoo: (NSString*)s; @end
func setFoo(f:Foo, s:String) {
  f.setFoo(s)
}
// CHECK: sil @_T20objc_nsstring_bridge6setFooFT1fCSo3Foo1sSS_T_ : $[thin] (f : Foo, s : String) -> () {
// CHECK: bb0({{%.*}} : $Foo, {{%.*}} : $String):
// CHECK:   [[SET_FOO:%.*]] = class_method [[F:%.*]], @setFoo.1.objc
// CHECK:   [[NATIVE_IN:%.*]] = load {{%.*}} : $*String
// CHECK:   [[NATIVE_STR_VALUE:%.*]] = struct_extract [[NATIVE_IN]], @str_value
// CHECK:   [[NATIVE_STR_OWNER:%.*]] = struct_extract [[NATIVE_STR_VALUE]], @owner
// CHECK:   retain [[NATIVE_STR_OWNER]]
// CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref ${{.*}}, @swift_StringToNSString
// CHECK:   [[NATIVE_TMP:%.*]] = alloc_var stack $String
// CHECK:   store [[NATIVE_IN]] to [[NATIVE_TMP]]
// CHECK:   [[BRIDGED:%.*]] = apply [[STRING_TO_NSSTRING]]([[NATIVE_TMP]])
// CHECK:   apply [[SET_FOO]]([[F]], [[BRIDGED]])
// CHECK:   release [[BRIDGED]]
// CHECK:   dealloc_var stack [[NATIVE_TMP]]
// CHECK:   [[NATIVE_STR_VALUE:%.*]] = struct_extract [[NATIVE_IN]], @str_value
// CHECK:   [[NATIVE_STR_OWNER:%.*]] = struct_extract [[NATIVE_STR_VALUE]], @owner
// CHECK:   release [[NATIVE_STR_OWNER]]
// CHECK:   release [[F]]
// CHECK: }

// NSString *bar(int);
func callBar() -> String {
  return bar(0)
}
// CHECK: sil @_T20objc_nsstring_bridge7callBarFT_SS : $[thin] () -> String {
// CHECK: bb0:
// CHECK:   [[BAR:%.*]] = function_ref ${{.*}}, @bar
// CHECK:   [[RES_BRIDGED:%.*]] = apply [[BAR]]({{%.*}})
// CHECK:   retain_autoreleased [[RES_BRIDGED]]
// CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref ${{.*}}, @swift_NSStringToString
// CHECK:   [[NATIVE_BUF:%.*]] = alloc_var stack $String
// CHECK:   initialize_var [[NATIVE_BUF]]
// CHECK:   apply [[NSSTRING_TO_STRING]]([[RES_BRIDGED]], [[NATIVE_BUF]])
// CHECK:   [[RES_NATIVE:%.*]] = load [[NATIVE_BUF]]
// CHECK:   return [[RES_NATIVE]]
// CHECK: }

// void setBar(NSString *s);
func callSetBar(s:String) {
  setBar(s)
}
// CHECK: sil @_T20objc_nsstring_bridge10callSetBarFT1sSS_T_ : $[thin] (s : String) -> () {
// CHECK: bb0({{%.*}} : $String):
// CHECK:   [[SET_BAR:%.*]] = function_ref ${{.*}}, @setBar
// CHECK:   [[NATIVE_IN:%.*]] = load {{%.*}} : $*String
// CHECK:   [[NATIVE_STR_VALUE:%.*]] = struct_extract [[NATIVE_IN]], @str_value
// CHECK:   [[NATIVE_STR_OWNER:%.*]] = struct_extract [[NATIVE_STR_VALUE]], @owner
// CHECK:   retain [[NATIVE_STR_OWNER]]
// CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref ${{.*}}, @swift_StringToNSString
// CHECK:   [[NATIVE_TMP:%.*]] = alloc_var stack $String
// CHECK:   store [[NATIVE_IN]] to [[NATIVE_TMP]]
// CHECK:   [[BRIDGED:%.*]] = apply [[STRING_TO_NSSTRING]]([[NATIVE_TMP]])
// CHECK:   apply [[SET_BAR]]([[BRIDGED]])
// CHECK:   release [[BRIDGED]]
// CHECK:   dealloc_var stack [[NATIVE_TMP]]
// CHECK:   [[NATIVE_STR_VALUE:%.*]] = struct_extract [[NATIVE_IN]], @str_value
// CHECK:   [[NATIVE_STR_OWNER:%.*]] = struct_extract [[NATIVE_STR_VALUE]], @owner
// CHECK:   release [[NATIVE_STR_OWNER]]
// CHECK: }
