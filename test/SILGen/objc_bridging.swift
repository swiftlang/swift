// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-cpu --check-prefix=CHECK-%target-os-%target-cpu

// REQUIRES: objc_interop

import Foundation

func getDescription(o: NSObject) -> String {
  return o.description
}
// CHECK-LABEL: sil hidden @_TF13objc_bridging14getDescription
// CHECK: bb0({{%.*}} : $NSObject):
// CHECK:  [[DESCRIPTION:%.*]] = class_method [volatile] {{%.*}} : {{.*}}, #NSObject.description!getter.1.foreign
// CHECK:  [[OPT_BRIDGED:%.*]] = apply [[DESCRIPTION]]({{%.*}})
// CHECK:  strong_retain_autoreleased [[OPT_BRIDGED]]
// CHECK:  select_enum [[OPT_BRIDGED]]
// CHECK:  [[BRIDGED:%.*]] = unchecked_enum_data [[OPT_BRIDGED]]
// CHECK:  [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
// CHECK:  [[BRIDGED_BOX:%.*]] = enum $Optional<NSString>, #Optional.Some!enumelt.1, [[BRIDGED]]
// CHECK:  [[NATIVE:%.*]] = apply [[NSSTRING_TO_STRING]]([[BRIDGED_BOX]])
// CHECK:  [[OPT_NATIVE:%.*]] = enum $ImplicitlyUnwrappedOptional<String>, #ImplicitlyUnwrappedOptional.Some!enumelt.1, [[NATIVE]]
// CHECK:  [[T0:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
// CHECK:  apply [[T0]]<String>([[NATIVE_BUF:%.*]]#1,
// CHECK:  [[NATIVE:%.*]] = load [[NATIVE_BUF]]
// CHECK:  return [[NATIVE]] 
// CHECK:}

func getUppercaseString(s: NSString) -> String {
  return s.uppercaseString()
}
// CHECK-LABEL: sil hidden @_TF13objc_bridging18getUppercaseString
// CHECK: bb0({{%.*}} : $NSString):
// -- The 'self' argument of NSString methods doesn't bridge.
// CHECK-NOT: function_ref @swift_NSStringToString
// CHECK-NOT: function_ref @swift_StringToNSString
// CHECK:   [[UPPERCASE_STRING:%.*]] = class_method [volatile] {{%.*}} : {{.*}}, #NSString.uppercaseString!1.foreign
// CHECK:   [[OPT_BRIDGED:%.*]] = apply [[UPPERCASE_STRING]]({{%.*}})
// CHECK:   retain_autoreleased [[OPT_BRIDGED]]
// CHECK:   select_enum [[OPT_BRIDGED]]
// CHECK:   [[BRIDGED:%.*]] = unchecked_enum_data [[OPT_BRIDGED]]
// CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
// CHECK:   [[BRIDGED_BOX:%.*]] = enum $Optional<NSString>, #Optional.Some!enumelt.1, [[BRIDGED]]
// CHECK:   [[NATIVE:%.*]] = apply [[NSSTRING_TO_STRING]]([[BRIDGED_BOX]])
// CHECK:   [[OPT_NATIVE:%.*]] = enum $ImplicitlyUnwrappedOptional<String>, #ImplicitlyUnwrappedOptional.Some!enumelt.1, [[NATIVE]]
// CHECK:   [[T0:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
// CHECK:   apply [[T0]]<String>([[NATIVE_BUF:%.*]]#1,
// CHECK:   [[NATIVE:%.*]] = load [[NATIVE_BUF]]
// CHECK:   return [[NATIVE]]
// CHECK: }

// @interface Foo -(void) setFoo: (NSString*)s; @end
func setFoo(f: Foo, var s: String) {
  f.setFoo(s)
}
// CHECK-LABEL: sil hidden @_TF13objc_bridging6setFoo
// CHECK: bb0({{%.*}} : $Foo, {{%.*}} : $String):
// CHECK:   [[SET_FOO:%.*]] = class_method [volatile] [[F:%.*]] : {{.*}}, #Foo.setFoo!1.foreign
// CHECK:   [[NATIVE_BUF:%.*]] = init_enum_data_addr [[OPT_NATIVE_BUF:%[0-9]+]]
// CHECK:   copy_addr {{%.*}} to [initialization] [[NATIVE_BUF]] : $*String
// CHECK:   inject_enum_addr [[OPT_NATIVE_BUF]]
// CHECK:   [[OPT_NATIVE:%.*]] = load [[OPT_NATIVE_BUF]]
// CHECK:   select_enum [[OPT_NATIVE]]
// CHECK:   [[NATIVE:%.*]] = unchecked_enum_data [[OPT_NATIVE]]
// CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
// CHECK:   [[BRIDGED:%.*]] = apply [[STRING_TO_NSSTRING]]([[NATIVE]])
// CHECK:    = enum $ImplicitlyUnwrappedOptional<NSString>, #ImplicitlyUnwrappedOptional.Some!enumelt.1, [[BRIDGED]]
// CHECK:   bb3([[OPT_BRIDGED:%.*]] : $ImplicitlyUnwrappedOptional<NSString>):
// CHECK:   apply [[SET_FOO]]([[OPT_BRIDGED]], %0)
// CHECK:   release_value [[OPT_BRIDGED]]
// CHECK: }

// @interface Foo -(BOOL) zim; @end
func getZim(f: Foo) -> Bool {
  return f.zim()
}
// CHECK-i386-LABEL: sil hidden @_TF13objc_bridging6getZim
// CHECK-i386:   [[OBJC_BOOL:%.*]] = apply {{.*}} : $@convention(objc_method) (Foo) -> ObjCBool
// CHECK-i386:   [[CONVERT:%.*]] = function_ref @swift_ObjCBoolToBool : $@convention(thin) (ObjCBool) -> Bool
// CHECK-i386:   [[SWIFT_BOOL:%.*]] = apply [[CONVERT]]([[OBJC_BOOL]]) : $@convention(thin) (ObjCBool) -> Bool
// CHECK-i386:   return [[SWIFT_BOOL]] : $Bool
// CHECK-i386: }

// CHECK-macosx-x86_64-LABEL: sil hidden @_TF13objc_bridging6getZim
// CHECK-macosx-x86_64:   [[OBJC_BOOL:%.*]] = apply {{.*}} : $@convention(objc_method) (Foo) -> ObjCBool
// CHECK-macosx-x86_64:   [[CONVERT:%.*]] = function_ref @swift_ObjCBoolToBool : $@convention(thin) (ObjCBool) -> Bool
// CHECK-macosx-x86_64:   [[SWIFT_BOOL:%.*]] = apply [[CONVERT]]([[OBJC_BOOL]]) : $@convention(thin) (ObjCBool) -> Bool
// CHECK-macosx-x86_64:   return [[SWIFT_BOOL]] : $Bool
// CHECK-macosx-x86_64: }

// CHECK-ios-x86_64-LABEL: sil hidden @_TF13objc_bridging6getZim
// CHECK-ios-x86_64:   [[SWIFT_BOOL:%.*]] = apply {{.*}} : $@convention(objc_method) (Foo) -> Bool
// CHECK-ios-x86_64:   return [[SWIFT_BOOL]] : $Bool
// CHECK-ios-x86_64: }

// CHECK-arm64-LABEL: sil hidden @_TF13objc_bridging6getZim
// CHECK-arm64:   [[SWIFT_BOOL:%.*]] = apply {{.*}} : $@convention(objc_method) (Foo) -> Bool
// CHECK-arm64:   return [[SWIFT_BOOL]] : $Bool
// CHECK-arm64: }

// @interface Foo -(void) setZim: (BOOL)b; @end
func setZim(f: Foo, b: Bool) {
  f.setZim(b)
}
// CHECK-i386-LABEL: sil hidden @_TF13objc_bridging6setZim
// CHECK-i386:   [[CONVERT:%.*]] = function_ref @swift_BoolToObjCBool : $@convention(thin) (Bool) -> ObjCBool
// CHECK-i386:   [[OBJC_BOOL:%.*]] = apply [[CONVERT]]({{%.*}}) : $@convention(thin) (Bool) -> ObjCBool
// CHECK-i386:   apply {{%.*}}([[OBJC_BOOL]], {{%.*}}) : $@convention(objc_method) (ObjCBool, Foo) -> ()
// CHECK-i386: }

// CHECK-macosx-x86_64-LABEL: sil hidden @_TF13objc_bridging6setZim
// CHECK-macosx-x86_64:   [[CONVERT:%.*]] = function_ref @swift_BoolToObjCBool : $@convention(thin) (Bool) -> ObjCBool
// CHECK-macosx-x86_64:   [[OBJC_BOOL:%.*]] = apply [[CONVERT]]({{%.*}}) : $@convention(thin) (Bool) -> ObjCBool
// CHECK-macosx-x86_64:   apply {{%.*}}([[OBJC_BOOL]], {{%.*}}) : $@convention(objc_method) (ObjCBool, Foo) -> ()
// CHECK-macosx-x86_64: }

// CHECK-ios-x86_64-LABEL: sil hidden @_TF13objc_bridging6setZim
// CHECK-ios-x86_64: bb0([[FOO_OBJ:%[0-9]+]] : $Foo, [[SWIFT_BOOL:%[0-9]+]] : $Bool):
// CHECK-ios-x86_64:   apply {{%.*}}([[SWIFT_BOOL]], [[FOO_OBJ]]) : $@convention(objc_method) (Bool, Foo) -> ()
// CHECK-ios-x86_64: }

// CHECK-arm64-LABEL: sil hidden @_TF13objc_bridging6setZim
// CHECK-arm64: bb0([[FOO_OBJ:%[0-9]+]] : $Foo, [[SWIFT_BOOL:%[0-9]+]] : $Bool):
// CHECK-arm64:   apply {{%.*}}([[SWIFT_BOOL]], [[FOO_OBJ]]) : $@convention(objc_method) (Bool, Foo) -> ()
// CHECK-arm64: }

// @interface Foo -(_Bool) zang; @end
func getZang(f: Foo) -> Bool {
  return f.zang()
}
// CHECK-LABEL: sil hidden @_TF13objc_bridging7getZangFCSo3FooSb
// CHECK:   [[BOOL:%.*]] = apply {{%.*}}(%0) : $@convention(objc_method) (Foo) -> Bool
// CHECK:   return [[BOOL]]

// @interface Foo -(void) setZang: (_Bool)b; @end
func setZang(f: Foo, b: Bool) {
  f.setZang(b)
}
// CHECK-LABEL: sil hidden @_TF13objc_bridging7setZangFTCSo3FooSb_T_
// CHECK:   apply {{%.*}}(%1, %0) : $@convention(objc_method) (Bool, Foo) -> ()

// NSString *bar(void);
func callBar() -> String {
  return bar()
}
// CHECK-LABEL: sil hidden @_TF13objc_bridging7callBar
// CHECK: bb0:
// CHECK:   [[BAR:%.*]] = function_ref @bar
// CHECK:   [[OPT_BRIDGED:%.*]] = apply [[BAR]]()
// CHECK:   retain_autoreleased [[OPT_BRIDGED]]
// CHECK:   select_enum [[OPT_BRIDGED]]
// CHECK:   [[BRIDGED:%.*]] = unchecked_enum_data [[OPT_BRIDGED]]
// CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
// CHECK:   [[BRIDGED_BOX:%.*]] = enum $Optional<NSString>, #Optional.Some!enumelt.1, [[BRIDGED]]
// CHECK:   [[NATIVE:%.*]] = apply [[NSSTRING_TO_STRING]]([[BRIDGED_BOX]])
// CHECK:   [[OPT_NATIVE:%.*]] = enum $ImplicitlyUnwrappedOptional<String>, #ImplicitlyUnwrappedOptional.Some!enumelt.1, [[NATIVE]]
// CHECK:   [[T0:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
// CHECK:   apply [[T0]]<String>([[NATIVE_BUF:%.*]]#1,
// CHECK:   [[NATIVE:%.*]] = load [[NATIVE_BUF]]
// CHECK:   return [[NATIVE]]
// CHECK: }

// void setBar(NSString *s);
func callSetBar(var s: String) {
  setBar(s)
}
// CHECK-LABEL: sil hidden @_TF13objc_bridging10callSetBar
// CHECK: bb0({{%.*}} : $String):
// CHECK:   [[SET_BAR:%.*]] = function_ref @setBar
// CHECK:   [[NATIVE_BUF:%.*]] = init_enum_data_addr [[OPT_NATIVE_BUF:%[0-9]+]]
// CHECK:   copy_addr {{%.*}} to [initialization] [[NATIVE_BUF]] : $*String
// CHECK:   inject_enum_addr [[OPT_NATIVE_BUF]]
// CHECK:   [[OPT_NATIVE:%.*]] = load [[OPT_NATIVE_BUF]]
// CHECK:   select_enum [[OPT_NATIVE]]
// CHECK:   [[NATIVE:%.*]] = unchecked_enum_data [[OPT_NATIVE]]
// CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
// CHECK:   [[BRIDGED:%.*]] = apply [[STRING_TO_NSSTRING]]([[NATIVE]])
// CHECK:    = enum $ImplicitlyUnwrappedOptional<NSString>, #ImplicitlyUnwrappedOptional.Some!enumelt.1, [[BRIDGED]]
// CHECK: bb3([[OPT_BRIDGED:%.*]] : $ImplicitlyUnwrappedOptional<NSString>):
// CHECK:   apply [[SET_BAR]]([[OPT_BRIDGED]])
// CHECK:   release_value [[OPT_BRIDGED]]
// CHECK: }

var NSS: NSString

// -- NSString methods don't convert 'self'
extension NSString {
  var nsstrFakeProp: NSString {
    get { return NSS }
    set {}
  }
  // CHECK-LABEL: sil hidden @_TToFE13objc_bridgingCSo8NSStringg13nsstrFakePropS0_
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }
  // CHECK-LABEL: sil hidden @_TToFE13objc_bridgingCSo8NSStrings13nsstrFakePropS0_
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

  func nsstrResult() -> NSString { return NSS }
  // CHECK-LABEL: sil hidden @_TToFE13objc_bridgingCSo8NSString11nsstrResultfS0_FT_S0_
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

  func nsstrArg(s: NSString) { }
  // CHECK-LABEL: sil hidden @_TToFE13objc_bridgingCSo8NSString8nsstrArgfS0_FS0_T_
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

}

class Bas : NSObject {
  // -- Bridging thunks for String properties convert between NSString
  var strRealProp: String = "Hello"
  // CHECK-LABEL: sil hidden [transparent] @_TToFC13objc_bridging3Basg11strRealPropSS : $@convention(objc_method) (Bas) -> @autoreleased NSString {
  // CHECK: bb0([[THIS:%.*]] : $Bas):
  // CHECK:   strong_retain [[THIS]] : $Bas
  // CHECK:   // function_ref objc_bridging.Bas.strRealProp.getter
  // CHECK:   [[PROPIMPL:%.*]] = function_ref @_TFC13objc_bridging3Basg11strRealPropSS
  // CHECK:   [[PROP_COPY:%.*]] = apply [[PROPIMPL]]([[THIS]]) : $@convention(method) (@guaranteed Bas) -> @owned String
  // CHECK:   strong_release [[THIS]]
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[PROP_COPY]])
  // CHECK:   autorelease_return [[NSSTR]]
  // CHECK: }


  // CHECK-LABEL: sil hidden [transparent] @_TFC13objc_bridging3Basg11strRealPropSS
  // CHECK:   [[PROP_ADDR:%.*]] = ref_element_addr %0 : {{.*}}, #Bas.strRealProp
  // CHECK:   [[PROP:%.*]] = load [[PROP_ADDR]]
  // CHECK:   retain_value [[PROP]] : $String


  // CHECK-LABEL: sil hidden [transparent]  @_TToFC13objc_bridging3Bass11strRealPropSS : $@convention(objc_method) (NSString, Bas) -> () {
  // CHECK: bb0([[VALUE:%.*]] : $NSString, [[THIS:%.*]] : $Bas):
  // CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
  // CHECK:   [[VALUE_BOX:%.*]] = enum $Optional<NSString>, #Optional.Some!enumelt.1, [[VALUE]]
  // CHECK:   [[STR:%.*]] = apply [[NSSTRING_TO_STRING]]([[VALUE_BOX]])
  
  // CHECK:   [[SETIMPL:%.*]] = function_ref @_TFC13objc_bridging3Bass11strRealPropSS
  // CHECK:   apply [[SETIMPL]]([[STR]], %1)

  // CHECK-LABEL: sil hidden [transparent] @_TFC13objc_bridging3Bass11strRealPropSS
  // CHECK: bb0(%0 : $String, %1 : $Bas):

  // CHECK:   [[STR_ADDR:%.*]] = ref_element_addr %1 : {{.*}}, #Bas.strRealProp
  // CHECK:   assign {{.*}} to [[STR_ADDR]]
  // CHECK: }

  var strFakeProp: String {
    get { return "" }
    set {}
  }
  // CHECK-LABEL: sil hidden @_TToFC13objc_bridging3Basg11strFakePropSS : $@convention(objc_method) (Bas) -> @autoreleased NSString {
  // CHECK: bb0([[THIS:%.*]] : $Bas):
  // CHECK:   [[GETTER:%.*]] = function_ref @_TFC13objc_bridging3Basg11strFakePropSS
  // CHECK:   [[STR:%.*]] = apply [[GETTER]]([[THIS]])
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[STR]])
  // CHECK:   autorelease_return [[NSSTR]]
  // CHECK: }

  // CHECK-LABEL: sil hidden @_TToFC13objc_bridging3Bass11strFakePropSS : $@convention(objc_method) (NSString, Bas) -> () {
  // CHECK: bb0([[NSSTR:%.*]] : $NSString, [[THIS:%.*]] : $Bas):
  // CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
  // CHECK:   [[NSSTR_BOX:%.*]] = enum $Optional<NSString>, #Optional.Some!enumelt.1, [[NSSTR]]
  // CHECK:   [[STR:%.*]] = apply [[NSSTRING_TO_STRING]]([[NSSTR_BOX]])
  // CHECK:   [[SETTER:%.*]] = function_ref @_TFC13objc_bridging3Bass11strFakePropSS
  // CHECK:   apply [[SETTER]]([[STR]], [[THIS]])
  // CHECK: }

  // -- Bridging thunks for explicitly NSString properties don't convert
  var nsstrRealProp: NSString
  var nsstrFakeProp: NSString {
    get { return NSS }
    set {}
  }
  // CHECK-LABEL: sil hidden [transparent] @_TToFC13objc_bridging3Basg13nsstrRealPropCSo8NSString : $@convention(objc_method) (Bas) -> @autoreleased NSString {
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

  // CHECK-LABEL: sil hidden [transparent]  @_TToFC13objc_bridging3Bass13nsstrRealPropCSo8NSString : $@convention(objc_method) (NSString, Bas) ->
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

  // -- Bridging thunks for String methods convert between NSString
  func strResult() -> String { return "" }
  // CHECK-LABEL: sil hidden @_TToFC13objc_bridging3Bas9strResultfS0_FT_SS : $@convention(objc_method) (Bas) -> @autoreleased NSString {
  // CHECK: bb0([[THIS:%.*]] : $Bas):
  // CHECK:   [[METHOD:%.*]] = function_ref @_TFC13objc_bridging3Bas9strResultfS0_FT_SS
  // CHECK:   [[STR:%.*]] = apply [[METHOD]]([[THIS]])
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[STR]])
  // CHECK:   autorelease_return [[NSSTR]]
  // CHECK: }
  func strArg(s: String) { }
  // CHECK-LABEL: sil hidden @_TToFC13objc_bridging3Bas6strArg
  // CHECK: bb0([[NSSTR:%.*]] : $NSString, [[THIS:%.*]] : $Bas):
  // CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
  // CHECK:   [[NSSTR_BOX:%.*]] = enum $Optional<NSString>, #Optional.Some!enumelt.1, [[NSSTR]]
  // CHECK:   [[STR:%.*]] = apply [[NSSTRING_TO_STRING]]([[NSSTR_BOX]])
  // CHECK:   [[METHOD:%.*]] = function_ref @_TFC13objc_bridging3Bas6strArg
  // CHECK:   apply [[METHOD]]([[STR]], [[THIS]])
  // CHECK: }

  // -- Bridging thunks for explicitly NSString properties don't convert
  func nsstrResult() -> NSString { return NSS }
  // CHECK-LABEL: sil hidden @_TToFC13objc_bridging3Bas11nsstrResultfS0_FT_CSo8NSString : $@convention(objc_method) (Bas) -> @autoreleased NSString {
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }
  func nsstrArg(s: NSString) { }
  // CHECK-LABEL: sil hidden @_TFC13objc_bridging3Bas8nsstrArg
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

  init(str: NSString) {
    nsstrRealProp = str
    super.init()
  }

  // CHECK-LABEL: sil hidden @_TToFC13objc_bridging3Bas8arrayArg{{.*}} : $@convention(objc_method) (NSArray, Bas) -> ()
  // CHECK: bb0([[NSARRAY:%[0-9]+]] : $NSArray, [[SELF:%[0-9]+]] : $Bas):
  // CHECK:   strong_retain [[NSARRAY]] : $NSArray
  // CHECK:   strong_retain [[SELF]] : $Bas
  // CHECK:   [[CONV_FN:%[0-9]+]] = function_ref @_TF10Foundation22_convertNSArrayToArray{{.*}} : $@convention(thin) <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK-NEXT: [[OPT_NSARRAY:%[0-9]+]] = enum $Optional<NSArray>, #Optional.Some!enumelt.1, [[NSARRAY]] : $NSArray
  // CHECK-NEXT: [[ARRAY:%[0-9]+]] = apply [[CONV_FN]]<AnyObject>([[OPT_NSARRAY]]) : $@convention(thin) <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @_TFC13objc_bridging3Bas{{.*}} : $@convention(method) (@owned Array<AnyObject>, @guaranteed Bas) -> ()
  // CHECK:   [[RESULT:%[0-9]+]] = apply [[SWIFT_FN]]([[ARRAY]], [[SELF]]) : $@convention(method) (@owned Array<AnyObject>, @guaranteed Bas) -> ()
  // CHECK:   strong_release [[SELF]] : $Bas
  // CHECK:   return [[RESULT]] : $()
  func arrayArg(array: [AnyObject]) { }
  
  // CHECK-LABEL: sil hidden @_TToFC13objc_bridging3Bas11arrayResult{{.*}} : $@convention(objc_method) (Bas) -> @autoreleased NSArray
  // CHECK: bb0([[SELF:%[0-9]+]] : $Bas):
  // CHECK:   strong_retain [[SELF]] : $Bas
  // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @_TFC13objc_bridging3Bas11arrayResult{{.*}} : $@convention(method) (@guaranteed Bas) -> @owned Array<AnyObject>
  // CHECK:   [[ARRAY:%[0-9]+]] = apply [[SWIFT_FN]]([[SELF]]) : $@convention(method) (@guaranteed Bas) -> @owned Array<AnyObject>
  // CHECK:   strong_release [[SELF]]
  // CHECK:   [[CONV_FN:%[0-9]+]] = function_ref @_TF10Foundation22_convertArrayToNSArray{{.*}} : $@convention(thin) <τ_0_0> (@owned Array<τ_0_0>) -> @owned NSArray
  // CHECK:   [[NSARRAY:%[0-9]+]] = apply [[CONV_FN]]<AnyObject>([[ARRAY]]) : $@convention(thin) <τ_0_0> (@owned Array<τ_0_0>) -> @owned NSArray
  // CHECK:   autorelease_return [[NSARRAY]]
  func arrayResult() -> [AnyObject] { return [] }

  // CHECK-LABEL: sil hidden [transparent] @_TToFC13objc_bridging3Basg9arrayPropGSaSS_ : $@convention(objc_method) (Bas) -> @autoreleased NSArray
  // CHECK-LABEL: sil hidden [transparent] @_TToFC13objc_bridging3Bass9arrayPropGSaSS_ : $@convention(objc_method) (NSArray, Bas) -> ()
  var arrayProp: [String] = []
}

// CHECK-LABEL: sil hidden @_TF13objc_bridging16applyStringBlock
func applyStringBlock(f: @convention(block) String -> String, x: String) -> String {
  // CHECK: [[BLOCK:%.*]] = copy_block %0
  // CHECK: [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
  // CHECK: [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]
  // CHECK: [[RES:%.*]] = apply [[BLOCK]]([[NSSTR]]) : $@convention(block) (NSString) -> @autoreleased NSString
  // CHECK: function_ref @swift_NSStringToString
  // CHECK: return {{%.*}} : $String
  return f(x)
}

// CHECK-LABEL: sil hidden @_TF13objc_bridging15bridgeCFunction
func bridgeCFunction() -> (String!) -> (String!) {
  // CHECK: [[THUNK:%.*]] = function_ref @_TTOFSC18NSStringFromStringFGSQSS_GSQSS_ : $@convention(thin) (@owned ImplicitlyUnwrappedOptional<String>) -> @owned ImplicitlyUnwrappedOptional<String>
  // CHECK: [[THICK:%.*]] = thin_to_thick_function [[THUNK]]
  // CHECK: return [[THICK]]
  return NSStringFromString
}

func forceNSArrayMembers() -> (NSArray, NSArray) {
  let x = NSArray(objects: nil, count: 0)
  return (x, x)
}

// Check that the allocating initializer shim for initializers that take pointer
// arguments lifetime-extends the bridged pointer for the right duration.
// <rdar://problem/16738050>

// CHECK-LABEL: sil shared @_TFCSo7NSArrayCfMS_FT7objectsGVSs13UnsafePointerGSqPSs9AnyObject___5countVSs5Int32_S_
// CHECK:         [[SELF:%.*]] = alloc_ref_dynamic
// CHECK:         [[METHOD:%.*]] = function_ref @_TTOFCSo7NSArraycfMS_FT7objectsGVSs13UnsafePointerGSqPSs9AnyObject___5countVSs5Int32_S_
// CHECK:         [[RESULT:%.*]] = apply [[METHOD]]
// CHECK:         return [[RESULT]]

// Check that type lowering preserves the bool/BOOL distinction when bridging
// imported C functions.

// CHECK-i386-LABEL: sil hidden @_TF13objc_bridging5boolsFSbTSbSb_
// CHECK-i386:         function_ref @useBOOL : $@convention(c) (ObjCBool) -> ()
// CHECK-i386:         function_ref @useBool : $@convention(c) (Bool) -> ()
// CHECK-i386:         function_ref @getBOOL : $@convention(c) () -> ObjCBool
// CHECK-i386:         function_ref @getBool : $@convention(c) () -> Bool

// CHECK-macosx-x86_64-LABEL: sil hidden @_TF13objc_bridging5boolsFSbTSbSb_
// CHECK-macosx-x86_64:         function_ref @useBOOL : $@convention(c) (ObjCBool) -> ()
// CHECK-macosx-x86_64:         function_ref @useBool : $@convention(c) (Bool) -> ()
// CHECK-macosx-x86_64:         function_ref @getBOOL : $@convention(c) () -> ObjCBool
// CHECK-macosx-x86_64:         function_ref @getBool : $@convention(c) () -> Bool

// FIXME: no distinction on x86_64 and arm64, since SILGen looks at the
// underlying Clang decl of the bridged decl to decide whether it needs
// bridging.
//
// CHECK-ios-x86_64-LABEL: sil hidden @_TF13objc_bridging5boolsFSbTSbSb_
// CHECK-ios-x86_64:         function_ref @useBOOL : $@convention(c) (Bool) -> ()
// CHECK-ios-x86_64:         function_ref @useBool : $@convention(c) (Bool) -> ()
// CHECK-ios-x86_64:         function_ref @getBOOL : $@convention(c) () -> Bool
// CHECK-ios-x86_64:         function_ref @getBool : $@convention(c) () -> Bool

// CHECK-arm64-LABEL: sil hidden @_TF13objc_bridging5boolsFSbTSbSb_
// CHECK-arm64:         function_ref @useBOOL : $@convention(c) (Bool) -> ()
// CHECK-arm64:         function_ref @useBool : $@convention(c) (Bool) -> ()
// CHECK-arm64:         function_ref @getBOOL : $@convention(c) () -> Bool
// CHECK-arm64:         function_ref @getBool : $@convention(c) () -> Bool

func bools(x: Bool) -> (Bool, Bool) {
  useBOOL(x)
  useBool(x)

  return (getBOOL(), getBool())
}


