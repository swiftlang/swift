// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -emit-sil -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s | FileCheck %s

import Foundation

func getDescription(o:NSObject) -> String {
  return o.description()
}
// CHECK: sil @_T13objc_bridging14getDescriptionFT1oCSo8NSObject_SS : $[thin] (o : NSObject) -> String {
// CHECK: bb0({{%.*}} : $NSObject):
// CHECK:  [[DESCRIPTION:%.*]] = class_method [volatile] {{%.*}}, @description.1.objc
// CHECK:  [[RES_BRIDGED:%.*]] = apply [[DESCRIPTION]]({{%.*}})
// CHECK:  retain_autoreleased [[RES_BRIDGED]]
// CHECK:  [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
// CHECK:  [[NATIVE_BUF:%.*]] = alloc_var stack $String
// CHECK:  initialize_var [[NATIVE_BUF]]
// CHECK:  apply [[NSSTRING_TO_STRING]]([[RES_BRIDGED]], [[NATIVE_BUF]])
// CHECK:  [[RES_NATIVE:%.*]] = load [[NATIVE_BUF]]
// CHECK:  return [[RES_NATIVE]]
// CHECK:}

func getUppercaseString(s:NSString) -> String {
  return s.uppercaseString()
}
// CHECK: sil @_T13objc_bridging18getUppercaseStringFT1sCSo8NSString_SS : $[thin] (s : NSString) -> String {
// CHECK: bb0({{%.*}} : $NSString):
// -- The 'self' argument of NSString methods doesn't bridge.
// CHECK-NOT: function_ref @swift_NSStringToString
// CHECK-NOT: function_ref @swift_StringToNSString
// CHECK:   [[UPPERCASE_STRING:%.*]] = class_method [volatile] {{%.*}}, @uppercaseString.1.objc
// CHECK:   [[RES_BRIDGED:%.*]] = apply [[UPPERCASE_STRING]]({{%.*}})
// CHECK:   retain_autoreleased [[RES_BRIDGED]]
// CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
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
// CHECK: sil @_T13objc_bridging6setFooFT1fCSo3Foo1sSS_T_ : $[thin] (f : Foo, s : String) -> () {
// CHECK: bb0({{%.*}} : $Foo, {{%.*}} : $String):
// CHECK:   [[SET_FOO:%.*]] = class_method [volatile] [[F:%.*]], @setFoo.1.objc
// CHECK:   [[NATIVE_IN:%.*]] = load {{%.*}} : $*String
// CHECK:   [[NATIVE_STR_VALUE:%.*]] = struct_extract [[NATIVE_IN]], @str_value
// CHECK:   [[NATIVE_STR_OWNER:%.*]] = struct_extract [[NATIVE_STR_VALUE]], @owner
// CHECK:   retain [[NATIVE_STR_OWNER]]
// CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
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

// @interface Foo -(BOOL) zim; @end
func getZim(f:Foo) -> Bool {
  return f.zim()
}
// CHECK: sil @_T13objc_bridging6getZimFT1fCSo3Foo_Sb : $[thin] (f : Foo) -> Bool {
// CHECK:   [[OBJC_BOOL:%.*]] = apply {{.*}} : $[cc(objc_method), thin] (Foo, ()) -> ObjCBool
// CHECK:   [[CONVERT:%.*]] = function_ref @swift_ObjCBoolToBool : $[thin] (x : ObjCBool) -> Bool
// CHECK:   [[SWIFT_BOOL:%.*]] = apply [[CONVERT]]([[OBJC_BOOL]]) : $[thin] (x : ObjCBool) -> Bool
// CHECK:   return [[SWIFT_BOOL]] : $Bool
// CHECK: }

// @interface Foo -(void) setZim: (BOOL)b; @end
func setZim(f:Foo, b:Bool) {
  f.setZim(b)
}
// CHECK: sil @_T13objc_bridging6setZimFT1fCSo3Foo1bSb_T_ : $[thin] (f : Foo, b : Bool) -> () {
// CHECK:   [[CONVERT:%.*]] = function_ref @swift_BoolToObjCBool : $[thin] (x : Bool) -> ObjCBool
// CHECK:   [[OBJC_BOOL:%.*]] = apply [[CONVERT]]({{%.*}}) : $[thin] (x : Bool) -> ObjCBool
// CHECK:   apply {{%.*}}({{%.*}}, [[OBJC_BOOL]]) : $[cc(objc_method), thin] (Foo, (b : ObjCBool)) -> ()
// CHECK: }

// NSString *bar(void);
func callBar() -> String {
  return bar()
}
// CHECK: sil @_T13objc_bridging7callBarFT_SS : $[thin] () -> String {
// CHECK: bb0:
// CHECK:   [[BAR:%.*]] = function_ref @bar
// CHECK:   [[RES_BRIDGED:%.*]] = apply [[BAR]]()
// CHECK:   retain_autoreleased [[RES_BRIDGED]]
// CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
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
// CHECK: sil @_T13objc_bridging10callSetBarFT1sSS_T_ : $[thin] (s : String) -> () {
// CHECK: bb0({{%.*}} : $String):
// CHECK:   [[SET_BAR:%.*]] = function_ref @setBar
// CHECK:   [[NATIVE_IN:%.*]] = load {{%.*}} : $*String
// CHECK:   [[NATIVE_STR_VALUE:%.*]] = struct_extract [[NATIVE_IN]], @str_value
// CHECK:   [[NATIVE_STR_OWNER:%.*]] = struct_extract [[NATIVE_STR_VALUE]], @owner
// CHECK:   retain [[NATIVE_STR_OWNER]]
// CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
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

var NSS : NSString

// -- NSString methods don't convert 'this'
extension NSString {
  var nsstrFakeProp : NSString {
    get: return NSS
    set:
  }
  // CHECK: sil @_TToCSo8NSString13nsstrFakePropS_g
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }
  // CHECK: sil @_TToCSo8NSString13nsstrFakePropS_s
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

  func nsstrResult() -> NSString { return NSS }
  // CHECK: sil @_TToCSo8NSString11nsstrResultfS_FT_S_
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

  func nsstrArg(s:NSString) { }
  // CHECK: sil @_TToCSo8NSString8nsstrArgfS_FT1sS__T_
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

}

class Bas : NSObject {
  // -- Bridging thunks for String properties convert between NSString
  var strRealProp : String
  // CHECK: sil @_TToCSo3Bas11strRealPropSSg : $[cc(objc_method), thin] (Bas, ()) -> NSString {
  // CHECK: bb0([[THIS:%.*]] : $Bas):
  // CHECK:   [[PROP_ADDR:%.*]] = ref_element_addr [[THIS]], @strRealProp
  // CHECK:   [[PROP:%.*]] = load [[PROP_ADDR]]
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
  // CHECK:   [[PROP_TEMP:%.*]] = alloc_var stack $String
  // CHECK:   store [[PROP]] to [[PROP_TEMP]]
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[PROP_TEMP]])
  // CHECK:   autorelease_return [[NSSTR]]
  // CHECK: }

  // CHECK: sil @_TToCSo3Bas11strRealPropSSs : $[cc(objc_method), thin] (Bas, (value : NSString)) -> () {
  // CHECK: bb0([[THIS:%.*]] : $Bas, [[VALUE:%.*]] : $NSString):
  // CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
  // CHECK:   [[STR_TEMP:%.*]] = alloc_var stack $String
  // CHECK:   apply [[NSSTRING_TO_STRING]]([[VALUE]], [[STR_TEMP]])
  // CHECK:   [[STR:%.*]] = load [[STR_TEMP]]
  // CHECK:   [[STR_ADDR:%.*]] = ref_element_addr [[THIS]], @strRealProp
  // CHECK:   store [[STR]] to [[STR_ADDR]]
  // CHECK: }

  var strFakeProp : String {
    get: return ""
    set:
  }
  // CHECK: sil @_TToCSo3Bas11strFakePropSSg : $[cc(objc_method), thin] (Bas, ()) -> NSString {
  // CHECK: bb0([[THIS:%.*]] : $Bas):
  // CHECK:   [[GETTER:%.*]] = function_ref @_TCSo3Bas11strFakePropSSg
  // CHECK:   [[STR:%.*]] = apply [[GETTER]]([[THIS]])
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
  // CHECK:   [[STR_TEMP:%.*]] = alloc_var stack $String
  // CHECK:   store [[STR]] to [[STR_TEMP]]
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[STR_TEMP]])
  // CHECK:   autorelease_return [[NSSTR]]
  // CHECK: }

  // CHECK: sil @_TToCSo3Bas11strFakePropSSs : $[cc(objc_method), thin] (Bas, (value : NSString)) -> () {
  // CHECK: bb0([[THIS:%.*]] : $Bas, [[NSSTR:%.*]] : $NSString):
  // CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
  // CHECK:   [[STR_TEMP:%.*]] = alloc_var stack $String
  // CHECK:   apply [[NSSTRING_TO_STRING]]([[NSSTR]], [[STR_TEMP]])
  // CHECK:   [[STR:%.*]] = load [[STR_TEMP]]
  // CHECK:   [[SETTER:%.*]] = function_ref @_TCSo3Bas11strFakePropSSs
  // CHECK:   apply [[SETTER]]([[STR]], [[THIS]])
  // CHECK: }

  // -- Bridging thunks for explicitly NSString properties don't convert
  var nsstrRealProp : NSString
  var nsstrFakeProp : NSString {
    get: return NSS
    set:
  }
  // CHECK: sil @_TToCSo3Bas13nsstrRealPropCSo8NSStringg : $[cc(objc_method), thin] (Bas, ()) -> NSString {
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

  // CHECK: sil @_TToCSo3Bas13nsstrRealPropCSo8NSStrings : $[cc(objc_method), thin] (Bas, (value : NSString)) ->
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

  // -- Bridging thunks for String methods convert between NSString
  func strResult() -> String { return "" }
  // CHECK: sil @_TToCSo3Bas9strResultfS_FT_SS : $[cc(objc_method), thin] (Bas, ()) -> NSString {
  // CHECK: bb0([[THIS:%.*]] : $Bas):
  // CHECK:   [[METHOD:%.*]] = function_ref @_TCSo3Bas9strResultfS_FT_SS
  // CHECK:   [[STR:%.*]] = apply [[METHOD]]([[THIS]])
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
  // CHECK:   [[STR_TEMP:%.*]] = alloc_var stack $String
  // CHECK:   store [[STR]] to [[STR_TEMP]]
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[STR_TEMP]])
  // CHECK:   autorelease_return [[NSSTR]]
  // CHECK: }
  func strArg(s:String) { }
  // CHECK: sil @_TToCSo3Bas6strArgfS_FT1sSS_T_ : $[cc(objc_method), thin] (Bas, (s : NSString)) -> () {
  // CHECK: bb0([[THIS:%.*]] : $Bas, [[NSSTR:%.*]] : $NSString):
  // CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
  // CHECK:   [[STR_TEMP:%.*]] = alloc_var stack $String
  // CHECK:   apply [[NSSTRING_TO_STRING]]([[NSSTR]], [[STR_TEMP]])
  // CHECK:   [[STR:%.*]] = load [[STR_TEMP]]
  // CHECK:   [[METHOD:%.*]] = function_ref @_TCSo3Bas6strArgfS_FT1sSS_T_
  // CHECK:   apply [[METHOD]]([[STR]], [[THIS]])
  // CHECK: }

  // -- Bridging thunks for explicitly NSString properties don't convert
  func nsstrResult() -> NSString { return NSS }
  // CHECK: sil @_TToCSo3Bas11nsstrResultfS_FT_CSo8NSString : $[cc(objc_method), thin] (Bas, ()) -> NSString {
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }
  func nsstrArg(s:NSString) { }
  // CHECK: sil @_TCSo3Bas8nsstrArgfS_FT1sCSo8NSString_T_ : $[cc(method), thin] ((s : NSString), Bas) -> () {
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }
}

