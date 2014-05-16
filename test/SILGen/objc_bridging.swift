// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -emit-silgen -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

import Foundation

func getDescription(o: NSObject) -> String {
  return o.description
}
// CHECK-LABEL: sil  @_TF13objc_bridging14getDescription
// CHECK: bb0({{%.*}} : $NSObject):
// CHECK:  [[DESCRIPTION:%.*]] = class_method [volatile] {{%.*}} : {{.*}}, #NSObject.description!getter.1.foreign
// CHECK:  [[OPT_BRIDGED:%.*]] = apply [[DESCRIPTION]]({{%.*}})
// CHECK:  retain_autoreleased [[OPT_BRIDGED]]
// CHECK:  store [[OPT_BRIDGED]] to [[OPT_BRIDGED_BUF:%.*]]#1
// CHECK:  [[T0:%.*]] = function_ref @_TFSs41_doesImplicitlyUnwrappedOptionalHaveValueU__FRGSQQ__Bi1_
// CHECK:  apply [transparent] [[T0]]<NSString>([[OPT_BRIDGED_BUF]]#1)
// CHECK:  [[T0:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
// CHECK:  apply [transparent] [[T0]]<NSString>([[BRIDGED_BUF:%.*]]#1, [[OPT_BRIDGED_BUF]]#1)
// CHECK:  [[BRIDGED:%.*]] = load [[BRIDGED_BUF]]
// CHECK:  [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
// CHECK:  [[NATIVE:%.*]] = apply [[NSSTRING_TO_STRING]]([[BRIDGED]])
// CHECK:  store [[NATIVE]] to [[NATIVE_BUF:%.*]]#1
// CHECK:  [[T0:%.*]] = function_ref @_TFSs43_injectValueIntoImplicitlyUnwrappedOptionalU__FQ_GSQQ__
// CHECK:  apply [transparent] [[T0]]<String>([[OPT_NATIVE_BUF:%.*]]#1, [[NATIVE_BUF]]#1)
// CHECK:  [[OPT_NATIVE:%.*]] = load [[OPT_NATIVE_BUF]]#1
// CHECK:  [[T0:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
// CHECK:  apply [transparent] [[T0]]<String>([[NATIVE_BUF:%.*]]#1,
// CHECK:  [[NATIVE:%.*]] = load [[NATIVE_BUF]]
// CHECK:  return [[NATIVE]] 
// CHECK:}

func getUppercaseString(s: NSString) -> String {
  return s.uppercaseString()
}
// CHECK-LABEL: sil  @_TF13objc_bridging18getUppercaseString
// CHECK: bb0({{%.*}} : $NSString):
// -- The 'self' argument of NSString methods doesn't bridge.
// CHECK-NOT: function_ref @swift_NSStringToString
// CHECK-NOT: function_ref @swift_StringToNSString
// CHECK:   [[UPPERCASE_STRING:%.*]] = class_method [volatile] {{%.*}} : {{.*}}, #NSString.uppercaseString!1.foreign
// CHECK:   [[OPT_BRIDGED:%.*]] = apply [[UPPERCASE_STRING]]({{%.*}})
// CHECK:   retain_autoreleased [[OPT_BRIDGED]]
// CHECK:   store [[OPT_BRIDGED]] to [[OPT_BRIDGED_BUF:%.*]]#1
// CHECK:   [[T0:%.*]] = function_ref @_TFSs41_doesImplicitlyUnwrappedOptionalHaveValueU__FRGSQQ__Bi1_
// CHECK:   apply [transparent] [[T0]]<NSString>([[OPT_BRIDGED_BUF]]#1)
// CHECK:   [[T0:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
// CHECK:   apply [transparent] [[T0]]<NSString>([[BRIDGED_BUF:%.*]]#1, [[OPT_BRIDGED_BUF]]#1)
// CHECK:   [[BRIDGED:%.*]] = load [[BRIDGED_BUF]]
// CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
// CHECK:   [[NATIVE:%.*]] = apply [[NSSTRING_TO_STRING]]([[BRIDGED]])
// CHECK:   store [[NATIVE]] to [[NATIVE_BUF:%.*]]#1
// CHECK:   [[T0:%.*]] = function_ref @_TFSs43_injectValueIntoImplicitlyUnwrappedOptionalU__FQ_GSQQ__
// CHECK:   apply [transparent] [[T0]]<String>([[OPT_NATIVE_BUF:%.*]]#1, [[NATIVE_BUF]]#1)
// CHECK:   [[OPT_NATIVE:%.*]] = load [[OPT_NATIVE_BUF]]#1
// CHECK:   [[T0:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
// CHECK:   apply [transparent] [[T0]]<String>([[NATIVE_BUF:%.*]]#1,
// CHECK:   [[NATIVE:%.*]] = load [[NATIVE_BUF]]
// CHECK:   return [[NATIVE]]
// CHECK: }

// @interface Foo -(void) setFoo: (NSString*)s; @end
func setFoo(var f: Foo, var s: String) {
  f.setFoo(s)
}
// CHECK-LABEL: sil  @_TF13objc_bridging6setFoo
// CHECK: bb0({{%.*}} : $Foo, {{%.*}} : $String):
// CHECK:   [[SET_FOO:%.*]] = class_method [volatile] [[F:%.*]] : {{.*}}, #Foo.setFoo!1.foreign
// CHECK:   copy_addr {{%.*}} to [initialization] [[NATIVE_BUF:%.*]]#1 : $*String
// CHECK:   [[T0:%.*]] = function_ref @_TFSs43_injectValueIntoImplicitlyUnwrappedOptionalU__FQ_GSQQ__
// CHECK:   apply [transparent] [[T0]]<String>([[OPT_NATIVE_BUF:%.*]]#1, [[NATIVE_BUF]]#1)
// CHECK:   [[OPT_NATIVE:%.*]] = load [[OPT_NATIVE_BUF]]
// CHECK:   store [[OPT_NATIVE]] to [[OPT_NATIVE_BUF:%.*]]#1
// CHECK:   [[T0:%.*]] = function_ref @_TFSs41_doesImplicitlyUnwrappedOptionalHaveValueU__FRGSQQ__Bi1_
// CHECK:   apply [transparent] [[T0]]<String>([[OPT_NATIVE_BUF]]#1)
// CHECK:   [[T0:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
// CHECK:   apply [transparent] [[T0]]<String>([[NATIVE_BUF:%.*]]#1, [[OPT_NATIVE_BUF]]#1)
// CHECK:   [[NATIVE:%.*]] = load [[NATIVE_BUF]]#1
// CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
// CHECK:   [[BRIDGED:%.*]] = apply [[STRING_TO_NSSTRING]]([[NATIVE]])
// CHECK:   store [[BRIDGED]] to [[BRIDGED_BUF:%.*]]#1
// CHECK:   [[T0:%.*]] = function_ref @_TFSs43_injectValueIntoImplicitlyUnwrappedOptionalU__FQ_GSQQ__
// CHECK:   apply [transparent] [[T0]]<NSString>([[OPT_BRIDGED_BUF:%.*]]#1, [[BRIDGED_BUF]]#1)
// CHECK:   [[OPT_BRIDGED:%.*]] = load [[OPT_BRIDGED_BUF]]#1
// CHECK:   apply [[SET_FOO]]([[OPT_BRIDGED]], [[F]])
// CHECK:   release_value [[OPT_BRIDGED]]
// CHECK: }

// @interface Foo -(BOOL) zim; @end
func getZim(f: Foo) -> Bool {
  return f.zim()
}
// CHECK-LABEL: sil  @_TF13objc_bridging6getZim
// CHECK:   [[OBJC_BOOL:%.*]] = apply {{.*}} : $@cc(objc_method) @thin (Foo) -> ObjCBool
// CHECK:   [[CONVERT:%.*]] = function_ref @swift_ObjCBoolToBool : $@thin (ObjCBool) -> Bool
// CHECK:   [[SWIFT_BOOL:%.*]] = apply [[CONVERT]]([[OBJC_BOOL]]) : $@thin (ObjCBool) -> Bool
// CHECK:   return [[SWIFT_BOOL]] : $Bool
// CHECK: }

// @interface Foo -(void) setZim: (BOOL)b; @end
func setZim(f: Foo, b: Bool) {
  f.setZim(b)
}
// CHECK-LABEL: sil  @_TF13objc_bridging6setZim
// CHECK:   [[CONVERT:%.*]] = function_ref @swift_BoolToObjCBool : $@thin (Bool) -> ObjCBool
// CHECK:   [[OBJC_BOOL:%.*]] = apply [[CONVERT]]({{%.*}}) : $@thin (Bool) -> ObjCBool
// CHECK:   apply {{%.*}}([[OBJC_BOOL]], {{%.*}}) : $@cc(objc_method) @thin (ObjCBool, Foo) -> ()
// CHECK: }

// NSString *bar(void);
func callBar() -> String {
  return bar()
}
// CHECK-LABEL: sil  @_TF13objc_bridging7callBar
// CHECK: bb0:
// CHECK:   [[BAR:%.*]] = function_ref @bar
// CHECK:   [[OPT_BRIDGED:%.*]] = apply [[BAR]]()
// CHECK:   retain_autoreleased [[OPT_BRIDGED]]
// CHECK:   store [[OPT_BRIDGED]] to [[OPT_BRIDGED_BUF:%.*]]#1
// CHECK:   [[T0:%.*]] = function_ref @_TFSs41_doesImplicitlyUnwrappedOptionalHaveValueU__FRGSQQ__Bi1_
// CHECK:   apply [transparent] [[T0]]<NSString>([[OPT_BRIDGED_BUF]]#1)
// CHECK:   [[T0:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
// CHECK:   apply [transparent] [[T0]]<NSString>([[BRIDGED_BUF:%.*]]#1, [[OPT_BRIDGED_BUF]]#1)
// CHECK:   [[BRIDGED:%.*]] = load [[BRIDGED_BUF]]
// CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
// CHECK:   [[NATIVE:%.*]] = apply [[NSSTRING_TO_STRING]]([[BRIDGED]])
// CHECK:   store [[NATIVE]] to [[NATIVE_BUF:%.*]]#1
// CHECK:   [[T0:%.*]] = function_ref @_TFSs43_injectValueIntoImplicitlyUnwrappedOptionalU__FQ_GSQQ__
// CHECK:   apply [transparent] [[T0]]<String>([[OPT_NATIVE_BUF:%.*]]#1, [[NATIVE_BUF]]#1)
// CHECK:   [[OPT_NATIVE:%.*]] = load [[OPT_NATIVE_BUF]]#1
// CHECK:   [[T0:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
// CHECK:   apply [transparent] [[T0]]<String>([[NATIVE_BUF:%.*]]#1,
// CHECK:   [[NATIVE:%.*]] = load [[NATIVE_BUF]]
// CHECK:   return [[NATIVE]]
// CHECK: }

// void setBar(NSString *s);
func callSetBar(var s: String) {
  setBar(s)
}
// CHECK-LABEL: sil  @_TF13objc_bridging10callSetBar
// CHECK: bb0({{%.*}} : $String):
// CHECK:   [[SET_BAR:%.*]] = function_ref @setBar
// CHECK:   copy_addr {{%.*}} to [initialization] [[NATIVE_BUF:%.*]]#1 : $*String
// CHECK:   [[T0:%.*]] = function_ref @_TFSs43_injectValueIntoImplicitlyUnwrappedOptionalU__FQ_GSQQ__
// CHECK:   apply [transparent] [[T0]]<String>([[OPT_NATIVE_BUF:%.*]]#1, [[NATIVE_BUF]]#1)
// CHECK:   [[OPT_NATIVE:%.*]] = load [[OPT_NATIVE_BUF]]
// CHECK:   store [[OPT_NATIVE]] to [[OPT_NATIVE_BUF:%.*]]#1
// CHECK:   [[T0:%.*]] = function_ref @_TFSs41_doesImplicitlyUnwrappedOptionalHaveValueU__FRGSQQ__Bi1_
// CHECK:   apply [transparent] [[T0]]<String>([[OPT_NATIVE_BUF]]#1)
// CHECK:   [[T0:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
// CHECK:   apply [transparent] [[T0]]<String>([[NATIVE_BUF:%.*]]#1, [[OPT_NATIVE_BUF]]#1)
// CHECK:   [[NATIVE:%.*]] = load [[NATIVE_BUF]]#1
// CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
// CHECK:   [[BRIDGED:%.*]] = apply [[STRING_TO_NSSTRING]]([[NATIVE]])
// CHECK:   store [[BRIDGED]] to [[BRIDGED_BUF:%.*]]#1
// CHECK:   [[T0:%.*]] = function_ref @_TFSs43_injectValueIntoImplicitlyUnwrappedOptionalU__FQ_GSQQ__
// CHECK:   apply [transparent] [[T0]]<NSString>([[OPT_BRIDGED_BUF:%.*]]#1, [[BRIDGED_BUF]]#1)
// CHECK:   [[OPT_BRIDGED:%.*]] = load [[OPT_BRIDGED_BUF]]#1
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
  // CHECK-LABEL: sil  @_TToFCSo8NSStringg13nsstrFakePropS_
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }
  // CHECK-LABEL: sil  @_TToFCSo8NSStrings13nsstrFakePropS_
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

  func nsstrResult() -> NSString { return NSS }
  // CHECK-LABEL: sil  @_TToFCSo8NSString11nsstrResultfS_FT_S_
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

  func nsstrArg(s: NSString) { }
  // CHECK-LABEL: sil  @_TToFCSo8NSString8nsstrArg
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

}

class Bas : NSObject {
  // -- Bridging thunks for String properties convert between NSString
  var strRealProp: String = "Hello"
  // CHECK-LABEL: sil [transparent] @_TToFC13objc_bridging3Basg11strRealPropSS : $@cc(objc_method) @thin (Bas) -> @autoreleased NSString {
  // CHECK: bb0([[THIS:%.*]] : $Bas):
  // CHECK:   strong_retain [[THIS]] : $Bas
  // CHECK:   // function_ref objc_bridging.Bas.strRealProp.getter
  // CHECK:   [[PROPIMPL:%.*]] = function_ref @_TFC13objc_bridging3Basg11strRealPropSS
  // CHECK:   [[PROP_COPY:%.*]] = apply [transparent] [[PROPIMPL]]([[THIS]]) : $@cc(method) @thin (@owned Bas) -> @owned String
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[PROP_COPY]])
  // CHECK:   autorelease_return [[NSSTR]]
  // CHECK: }


  // CHECK-LABEL: sil [transparent] @_TFC13objc_bridging3Basg11strRealPropSS
  // CHECK:   [[PROP_ADDR:%.*]] = ref_element_addr %0 : {{.*}}, #Bas.strRealProp
  // CHECK:   [[PROP:%.*]] = load [[PROP_ADDR]]
  // CHECK:   retain_value [[PROP]] : $String


  // CHECK-LABEL: sil [transparent]  @_TToFC13objc_bridging3Bass11strRealPropSS : $@cc(objc_method) @thin (NSString, Bas) -> () {
  // CHECK: bb0([[VALUE:%.*]] : $NSString, [[THIS:%.*]] : $Bas):
  // CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
  // CHECK:   [[STR:%.*]] = apply [[NSSTRING_TO_STRING]]([[VALUE]])
  
  // CHECK:   [[SETIMPL:%.*]] = function_ref @_TFC13objc_bridging3Bass11strRealPropSS
  // CHECK:   apply [transparent] [[SETIMPL]]([[STR]], %1)

  // CHECK-LABEL: sil [transparent] @_TFC13objc_bridging3Bass11strRealPropSS
  // CHECK: bb0(%0 : $String, %1 : $Bas):

  // CHECK:   [[STR_ADDR:%.*]] = ref_element_addr %1 : {{.*}}, #Bas.strRealProp
  // CHECK:   assign {{.*}} to [[STR_ADDR]]
  // CHECK: }

  var strFakeProp: String {
    get { return "" }
    set {}
  }
  // CHECK-LABEL: sil  @_TToFC13objc_bridging3Basg11strFakePropSS : $@cc(objc_method) @thin (Bas) -> @autoreleased NSString {
  // CHECK: bb0([[THIS:%.*]] : $Bas):
  // CHECK:   [[GETTER:%.*]] = function_ref @_TFC13objc_bridging3Basg11strFakePropSS
  // CHECK:   [[STR:%.*]] = apply [[GETTER]]([[THIS]])
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[STR]])
  // CHECK:   autorelease_return [[NSSTR]]
  // CHECK: }

  // CHECK-LABEL: sil  @_TToFC13objc_bridging3Bass11strFakePropSS : $@cc(objc_method) @thin (NSString, Bas) -> () {
  // CHECK: bb0([[NSSTR:%.*]] : $NSString, [[THIS:%.*]] : $Bas):
  // CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
  // CHECK:   [[STR:%.*]] = apply [[NSSTRING_TO_STRING]]([[NSSTR]])
  // CHECK:   [[SETTER:%.*]] = function_ref @_TFC13objc_bridging3Bass11strFakePropSS
  // CHECK:   apply [[SETTER]]([[STR]], [[THIS]])
  // CHECK: }

  // -- Bridging thunks for explicitly NSString properties don't convert
  var nsstrRealProp: NSString
  var nsstrFakeProp: NSString {
    get { return NSS }
    set {}
  }
  // CHECK-LABEL: sil [transparent] @_TToFC13objc_bridging3Basg13nsstrRealPropCSo8NSString : $@cc(objc_method) @thin (Bas) -> @autoreleased NSString {
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

  // CHECK-LABEL: sil [transparent]  @_TToFC13objc_bridging3Bass13nsstrRealPropCSo8NSString : $@cc(objc_method) @thin (NSString, Bas) ->
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

  // -- Bridging thunks for String methods convert between NSString
  func strResult() -> String { return "" }
  // CHECK-LABEL: sil  @_TToFC13objc_bridging3Bas9strResultfS0_FT_SS : $@cc(objc_method) @thin (Bas) -> @autoreleased NSString {
  // CHECK: bb0([[THIS:%.*]] : $Bas):
  // CHECK:   [[METHOD:%.*]] = function_ref @_TFC13objc_bridging3Bas9strResultfS0_FT_SS
  // CHECK:   [[STR:%.*]] = apply [[METHOD]]([[THIS]])
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[STR]])
  // CHECK:   autorelease_return [[NSSTR]]
  // CHECK: }
  func strArg(s: String) { }
  // CHECK-LABEL: sil  @_TToFC13objc_bridging3Bas6strArg
  // CHECK: bb0([[NSSTR:%.*]] : $NSString, [[THIS:%.*]] : $Bas):
  // CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @swift_NSStringToString
  // CHECK:   [[STR:%.*]] = apply [[NSSTRING_TO_STRING]]([[NSSTR]])
  // CHECK:   [[METHOD:%.*]] = function_ref @_TFC13objc_bridging3Bas6strArg
  // CHECK:   apply [[METHOD]]([[STR]], [[THIS]])
  // CHECK: }

  // -- Bridging thunks for explicitly NSString properties don't convert
  func nsstrResult() -> NSString { return NSS }
  // CHECK-LABEL: sil  @_TToFC13objc_bridging3Bas11nsstrResultfS0_FT_CSo8NSString : $@cc(objc_method) @thin (Bas) -> @autoreleased NSString {
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }
  func nsstrArg(s: NSString) { }
  // CHECK-LABEL: sil  @_TFC13objc_bridging3Bas8nsstrArg
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: swift_NSStringToString
  // CHECK: }

  init(str: NSString) {
    nsstrRealProp = str
    super.init()
  }

  // CHECK-LABEL: sil @_TToFC13objc_bridging3Bas8arrayArg{{.*}} : $@cc(objc_method) @thin (NSArray, Bas) -> ()
  // CHECK: bb0([[NSARRAY:%[0-9]+]] : $NSArray, [[SELF:%[0-9]+]] : $Bas):
  // CHECK:   strong_retain [[NSARRAY]] : $NSArray
  // CHECK:   strong_retain [[SELF]] : $Bas
  // CHECK:   [[CONV_FN:%[0-9]+]] = function_ref @_TF10Foundation22_convertNSArrayToArray{{.*}} : $@thin <τ_0_0> (@owned NSArray) -> @owned Array<τ_0_0>
  // CHECK-NEXT: [[ARRAY:%[0-9]+]] = apply [[CONV_FN]]<AnyObject>([[NSARRAY]]) : $@thin <τ_0_0> (@owned NSArray) -> @owned Array<τ_0_0>
  // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @_TFC13objc_bridging3Bas{{.*}} : $@cc(method) @thin (@owned Array<AnyObject>, @owned Bas) -> ()
  // CHECK:   [[RESULT:%[0-9]+]] = apply [[SWIFT_FN]]([[ARRAY]], [[SELF]]) : $@cc(method) @thin (@owned Array<AnyObject>, @owned Bas) -> ()
  // CHECK:   return [[RESULT]] : $()
  func arrayArg(array: AnyObject[]) { }
  
  // CHECK-LABEL: sil @_TToFC13objc_bridging3Bas11arrayResult{{.*}} : $@cc(objc_method) @thin (Bas) -> @autoreleased NSArray
  // CHECK: bb0([[SELF:%[0-9]+]] : $Bas):
  // CHECK:   strong_retain [[SELF]] : $Bas
  // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @_TFC13objc_bridging3Bas11arrayResult{{.*}} : $@cc(method) @thin (@owned Bas) -> @owned Array<AnyObject>
  // CHECK:   [[ARRAY:%[0-9]+]] = apply [[SWIFT_FN]]([[SELF]]) : $@cc(method) @thin (@owned Bas) -> @owned Array<AnyObject>
  // CHECK:   [[CONV_FN:%[0-9]+]] = function_ref @_TF10Foundation22_convertArrayToNSArray{{.*}} : $@thin <τ_0_0> (@owned Array<τ_0_0>) -> @owned NSArray
  // CHECK:   [[NSARRAY:%[0-9]+]] = apply [[CONV_FN]]<AnyObject>([[ARRAY]]) : $@thin <τ_0_0> (@owned Array<τ_0_0>) -> @owned NSArray
  func arrayResult() -> AnyObject[] { return [] }

  // CHECK-LABEL: sil [transparent] @_TToFC13objc_bridging3Basg9arrayPropGSaSS_ : $@cc(objc_method) @thin (Bas) -> @autoreleased NSArray
  // CHECK-LABEL: sil [transparent] @_TToFC13objc_bridging3Bass9arrayPropGSaSS_ : $@cc(objc_method) @thin (NSArray, Bas) -> ()
  var arrayProp: String[] = []
}

// CHECK-LABEL: sil @_TF13objc_bridging16applyStringBlock
func applyStringBlock(f: @objc_block String -> String, x: String) -> String {
  // CHECK: [[BLOCK:%.*]] = copy_block %0
  // CHECK: [[STRING_TO_NSSTRING:%.*]] = function_ref @swift_StringToNSString
  // CHECK: [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]
  // CHECK: [[RES:%.*]] = apply [[BLOCK]]([[NSSTR]]) : $@cc(cdecl) @objc_block (NSString) -> @autoreleased NSString
  // CHECK: function_ref @swift_NSStringToString
  // CHECK: return {{%.*}} : $String
  return f(x)
}

// CHECK-LABEL: sil @_TF13objc_bridging15bridgeCFunction
func bridgeCFunction() -> (String!) -> (String!) {
  // CHECK: [[THUNK:%.*]] = function_ref @_TTOFSC18NSStringFromStringFGSQSS_GSQSS_ : $@thin (@owned ImplicitlyUnwrappedOptional<String>) -> @owned ImplicitlyUnwrappedOptional<String>
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

// CHECK-LABEL: sil shared @_TFCSo7NSArrayCfMS_FT7objectsGVSs13CConstPointerGSqPSs9AnyObject___5countVSs5Int32_S_
// CHECK:         [[SELF:%.*]] = alloc_ref_dynamic
// CHECK:         [[METHOD:%.*]] = function_ref @_TTOFCSo7NSArraycfMS_FT7objectsGVSs13CConstPointerGSqPSs9AnyObject___5countVSs5Int32_S_
// CHECK:         [[RESULT:%.*]] = apply [[METHOD]]
// CHECK:         return [[RESULT]]
