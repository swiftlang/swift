
// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-module -o %t -I %S/../Inputs/ObjCBridging %S/../Inputs/ObjCBridging/Appliances.swift
// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) -module-name objc_bridging -I %S/../Inputs/ObjCBridging -Xllvm -sil-full-demangle %s -enable-sil-ownership | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-cpu --check-prefix=CHECK-%target-os-%target-cpu

// REQUIRES: objc_interop

import Foundation
import Appliances


func getDescription(_ o: NSObject) -> String {
  return o.description
}
// CHECK-LABEL: sil hidden @$s13objc_bridging14getDescription{{.*}}F
// CHECK: bb0([[ARG:%.*]] : @guaranteed $NSObject):
// CHECK:   [[DESCRIPTION:%.*]] = objc_method [[ARG]] : $NSObject, #NSObject.description!getter.1.foreign
// CHECK:   [[OPT_BRIDGED:%.*]] = apply [[DESCRIPTION]]([[ARG]])
// CHECK:   switch_enum [[OPT_BRIDGED]] : $Optional<NSString>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]([[BRIDGED:%.*]] : @owned $NSString):
// CHECK-NOT:   unchecked_enum_data
// CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @$sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
// CHECK:   [[BRIDGED_BOX:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[BRIDGED]]
// CHECK:   [[NATIVE:%.*]] = apply [[NSSTRING_TO_STRING]]([[BRIDGED_BOX]],
// CHECK:   [[OPT_NATIVE:%.*]] = enum $Optional<String>, #Optional.some!enumelt.1, [[NATIVE]]
// CHECK:   br [[CONT_BB:bb[0-9]+]]([[OPT_NATIVE]] : $Optional<String>)
//
// CHECK: [[NONE_BB]]:
// CHECK:   [[OPT_NATIVE:%.*]] = enum $Optional<String>, #Optional.none!enumelt
// CHECK:   br [[CONT_BB]]([[OPT_NATIVE]] : $Optional<String>)
//
// CHECK: [[CONT_BB]]([[OPT_NATIVE:%.*]] : @owned $Optional<String>):
// CHECK:   switch_enum [[OPT_NATIVE]] : $Optional<String>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
// CHECK: [[NONE_BB]]:
// CHECK:   unreachable
//
// CHECK: [[SOME_BB]]([[NATIVE:%.*]] : @owned $String):
// CHECK-NOT:    destroy_value [[ARG]]
// CHECK:    return [[NATIVE]] 
// CHECK:}

func getUppercaseString(_ s: NSString) -> String {
  return s.uppercase()
}
// CHECK-LABEL: sil hidden @$s13objc_bridging18getUppercaseString{{.*}}F
// CHECK: bb0([[ARG:%.*]] : @guaranteed $NSString):
// -- The 'self' argument of NSString methods doesn't bridge.
// CHECK-NOT: function_ref @$sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
// CHECK-NOT: function_ref @swift_StringToNSString
// CHECK:   [[UPPERCASE_STRING:%.*]] = objc_method [[ARG]] : $NSString, #NSString.uppercase!1.foreign
// CHECK:   [[OPT_BRIDGED:%.*]] = apply [[UPPERCASE_STRING]]([[ARG]]) : $@convention(objc_method) (NSString) -> @autoreleased Optional<NSString>
// CHECK:   switch_enum [[OPT_BRIDGED]] : $Optional<NSString>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
//
// CHECK: [[SOME_BB]]([[BRIDGED:%.*]] :
// CHECK-NOT:  unchecked_enum_data
// CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @$sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
// CHECK:   [[BRIDGED_BOX:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[BRIDGED]]
// CHECK:   [[NATIVE:%.*]] = apply [[NSSTRING_TO_STRING]]([[BRIDGED_BOX]]
// CHECK:   [[OPT_NATIVE:%.*]] = enum $Optional<String>, #Optional.some!enumelt.1, [[NATIVE]]
// CHECK:   br [[CONT_BB:bb[0-9]+]]([[OPT_NATIVE]] : $Optional<String>)
//
// CHECK: [[NONE_BB]]:
// CHECK:   [[OPT_NATIVE:%.*]] = enum $Optional<String>, #Optional.none!enumelt
// CHECK:   br [[CONT_BB]]([[OPT_NATIVE]] : $Optional<String>)
//
// CHECK: [[CONT_BB]]([[OPT_NATIVE:%.*]] : @owned $Optional<String>):
// CHECK:   switch_enum [[OPT_NATIVE]] : $Optional<String>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
// CHECK: [[NONE_BB]]:
// CHECK:   unreachable
//
// CHECK: [[SOME_BB]]([[NATIVE:%.*]] : @owned $String):
// CHECK:   return [[NATIVE]]
// CHECK: }

// @interface Foo -(void) setFoo: (NSString*)s; @end
func setFoo(_ f: Foo, s: String) {
  var s = s
  f.setFoo(s)
}
// CHECK-LABEL: sil hidden @$s13objc_bridging6setFoo{{.*}}F
// CHECK: bb0([[ARG0:%.*]] : @guaranteed $Foo, {{%.*}} : @guaranteed $String):
// CHECK:   [[NATIVE:%.*]] = load
// CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
// CHECK:   [[BORROWED_NATIVE:%.*]] = begin_borrow [[NATIVE]]
// CHECK:   [[BRIDGED:%.*]] = apply [[STRING_TO_NSSTRING]]([[BORROWED_NATIVE]])
// CHECK:   [[OPT_BRIDGED:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[BRIDGED]]
// CHECK:   [[SET_FOO:%.*]] = objc_method [[ARG0]] : $Foo, #Foo.setFoo!1.foreign
// CHECK:   apply [[SET_FOO]]([[OPT_BRIDGED]], [[ARG0]]) : $@convention(objc_method) (Optional<NSString>, Foo) -> ()
// CHECK:   destroy_value [[OPT_BRIDGED]]
// CHECK-NOT:   destroy_value [[ARG0]]
// CHECK: }

// @interface Foo -(BOOL) zim; @end
func getZim(_ f: Foo) -> Bool {
  return f.zim()
}

// CHECK-ios-i386-LABEL: sil hidden @$s13objc_bridging6getZim{{.*}}F
// CHECK-ios-i386: bb0([[SELF:%.*]] : @guaranteed $Foo):
// CHECK-ios-i386:   [[METHOD:%.*]] = objc_method [[SELF]] : $Foo, #Foo.zim!1.foreign : (Foo) -> () -> Bool
// CHECK-ios-i386:   [[OBJC_BOOL:%.*]] = apply [[METHOD]]([[SELF]])  : $@convention(objc_method) (Foo) -> ObjCBool
// CHECK-ios-i386:   [[CONVERT:%.*]] = function_ref @swift_ObjCBoolToBool : $@convention(thin) (ObjCBool) -> Bool
// CHECK-ios-i386:   [[SWIFT_BOOL:%.*]] = apply [[CONVERT]]([[OBJC_BOOL]]) : $@convention(thin) (ObjCBool) -> Bool
// CHECK-ios-i386:   return [[SWIFT_BOOL]] : $Bool
// CHECK-ios-i386: }

// CHECK-watchos-i386-LABEL: sil hidden @$s13objc_bridging6getZim{{.*}}F
// CHECK-watchos-i386: bb0([[SELF:%.*]] : @guaranteed $Foo):
// CHECK-watchos-i386:   [[METHOD:%.*]] = objc_method [[SELF]] : $Foo, #Foo.zim!1.foreign : (Foo) -> () -> Boo
// CHECK-watchos-i386:   [[BOOL:%.*]] = apply [[METHOD]]([[SELF]]) : $@convention(objc_method) (Foo) -> Bool
// CHECK-watchos-i386:   return [[BOOL]] : $Bool
// CHECK-watchos-i386: }

// CHECK-macosx-x86_64-LABEL: sil hidden @$s13objc_bridging6getZim{{.*}}F
// CHECK-macosx-x86_64: bb0([[SELF:%.*]] : @guaranteed $Foo):
// CHECK-macosx-x86_64:   [[METHOD:%.*]] = objc_method [[SELF]] : $Foo, #Foo.zim!1.foreign : (Foo) -> () -> Bool
// CHECK-macosx-x86_64:   [[OBJC_BOOL:%.*]] = apply [[METHOD]]([[SELF]])  : $@convention(objc_method) (Foo) -> ObjCBool
// CHECK-macosx-x86_64:   [[CONVERT:%.*]] = function_ref @swift_ObjCBoolToBool : $@convention(thin) (ObjCBool) -> Bool
// CHECK-macosx-x86_64:   [[SWIFT_BOOL:%.*]] = apply [[CONVERT]]([[OBJC_BOOL]]) : $@convention(thin) (ObjCBool) -> Bool
// CHECK-macosx-x86_64:   return [[SWIFT_BOOL]] : $Bool
// CHECK-macosx-x86_64: }

// CHECK-ios-x86_64-LABEL: sil hidden @$s13objc_bridging6getZim{{.*}}F
// CHECK-ios-x86_64: bb0([[SELF:%.*]] : @guaranteed $Foo):
// CHECK-ios-x86_64:   [[METHOD:%.*]] = objc_method [[SELF]] : $Foo, #Foo.zim!1.foreign : (Foo) -> () -> Boo
// CHECK-ios-x86_64:   [[BOOL:%.*]] = apply [[METHOD]]([[SELF]]) : $@convention(objc_method) (Foo) -> Bool
// CHECK-ios-x86_64:   return [[BOOL]] : $Bool
// CHECK-ios-x86_64: }

// CHECK-arm64-LABEL: sil hidden @$s13objc_bridging6getZim{{.*}}F
// CHECK-arm64: bb0([[SELF:%.*]] : @guaranteed $Foo):
// CHECK-arm64:   [[METHOD:%.*]] = objc_method [[SELF]] : $Foo, #Foo.zim!1.foreign : (Foo) -> () -> Boo
// CHECK-arm64:   [[BOOL:%.*]] = apply [[METHOD]]([[SELF]]) : $@convention(objc_method) (Foo) -> Bool
// CHECK-arm64:   return [[BOOL]] : $Bool
// CHECK-arm64: }

// @interface Foo -(void) setZim: (BOOL)b; @end
func setZim(_ f: Foo, b: Bool) {
  f.setZim(b)
}
// CHECK-ios-i386-LABEL: sil hidden @$s13objc_bridging6setZim{{.*}}F
// CHECK-ios-i386: bb0([[ARG0:%.*]] : @guaranteed $Foo, [[ARG1:%.*]] : @trivial $Bool):
// CHECK-ios-i386:   [[CONVERT:%.*]] = function_ref @swift_BoolToObjCBool : $@convention(thin) (Bool) -> ObjCBool
// CHECK-ios-i386:   [[OBJC_BOOL:%.*]] = apply [[CONVERT]]([[ARG1]]) : $@convention(thin) (Bool) -> ObjCBool
// CHECK-ios-i386:   [[METHOD:%.*]] = objc_method [[ARG0]] : $Foo, #Foo.setZim!1.foreign
// CHECK-ios-i386:   apply [[METHOD]]([[OBJC_BOOL]], [[ARG0]]) : $@convention(objc_method) (ObjCBool, Foo) -> ()
// CHECK-ios-i386-NOT:   destroy_value [[ARG0]]
// CHECK-ios-i386: }

// CHECK-macosx-x86_64-LABEL: sil hidden @$s13objc_bridging6setZim{{.*}}F
// CHECK-macosx-x86_64: bb0([[ARG0:%.*]] : @guaranteed $Foo, [[ARG1:%.*]] : @trivial $Bool):
// CHECK-macosx-x86_64:   [[CONVERT:%.*]] = function_ref @swift_BoolToObjCBool : $@convention(thin) (Bool) -> ObjCBool
// CHECK-macosx-x86_64:   [[OBJC_BOOL:%.*]] = apply [[CONVERT]]([[ARG1]]) : $@convention(thin) (Bool) -> ObjCBool
// CHECK-macosx-x86_64:   [[METHOD:%.*]] = objc_method [[ARG0]] : $Foo, #Foo.setZim!1.foreign
// CHECK-macosx-x86_64:   apply [[METHOD]]([[OBJC_BOOL]], [[ARG0]]) : $@convention(objc_method) (ObjCBool, Foo) -> ()
// CHECK-macosx-x86_64-NOT:   destroy_value [[ARG0]]
// CHECK-macosx-x86_64: }

// CHECK-ios-x86_64-LABEL: sil hidden @$s13objc_bridging6setZim{{.*}}F
// CHECK-ios-x86_64: bb0([[ARG0:%.*]] : @guaranteed $Foo, [[ARG1:%.*]] : @trivial $Bool):
// CHECK-ios-x86_64:   [[METHOD:%.*]] = objc_method [[ARG0]] : $Foo, #Foo.setZim!1.foreign
// CHECK-ios-x86_64:   apply [[METHOD]]([[ARG1]], [[ARG0]]) : $@convention(objc_method) (Bool, Foo) -> ()
// CHECK-ios-x86_64-NOT:   destroy_value [[ARG0]]
// CHECK-ios-x86_64: }

// CHECK-arm64-LABEL: sil hidden @$s13objc_bridging6setZim{{.*}}F
// CHECK-arm64: bb0([[ARG0:%.*]] : @guaranteed $Foo, [[ARG1:%.*]] : @trivial $Bool):
// CHECK-arm64:   [[METHOD:%.*]] = objc_method [[ARG0]] : $Foo, #Foo.setZim!1.foreign
// CHECK-arm64:   apply [[METHOD]]([[ARG1]], [[ARG0]]) : $@convention(objc_method) (Bool, Foo) -> ()
// CHECK-arm64-NOT:   destroy_value [[ARG0]]
// CHECK-arm64: }

// CHECK-watchos-i386-LABEL: sil hidden @$s13objc_bridging6setZim{{.*}}F
// CHECK-watchos-i386: bb0([[ARG0:%.*]] : @guaranteed $Foo, [[ARG1:%.*]] : @trivial $Bool):
// CHECK-watchos-i386:   [[METHOD:%.*]] = objc_method [[ARG0]] : $Foo, #Foo.setZim!1.foreign
// CHECK-watchos-i386:   apply [[METHOD]]([[ARG1]], [[ARG0]]) : $@convention(objc_method) (Bool, Foo) -> ()
// CHECK-watchos-i386-NOT:   destroy_value [[ARG0]]
// CHECK-watchos-i386: }

// @interface Foo -(_Bool) zang; @end
func getZang(_ f: Foo) -> Bool {
  return f.zang()
}
// CHECK-LABEL: sil hidden @$s13objc_bridging7getZangySbSo3FooCF
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Foo)
// CHECK:   [[METHOD:%.*]] = objc_method [[ARG]] : $Foo, #Foo.zang!1.foreign
// CHECK:   [[BOOL:%.*]] = apply [[METHOD]]([[ARG]]) : $@convention(objc_method) (Foo) -> Bool
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return [[BOOL]]

// @interface Foo -(void) setZang: (_Bool)b; @end
func setZang(_ f: Foo, _ b: Bool) {
  f.setZang(b)
}
// CHECK-LABEL: sil hidden @$s13objc_bridging7setZangyySo3FooC_SbtF
// CHECK: bb0([[ARG0:%.*]] : @guaranteed $Foo, [[ARG1:%.*]] : @trivial $Bool):
// CHECK:   [[METHOD:%.*]] = objc_method [[ARG0]] : $Foo, #Foo.setZang!1.foreign
// CHECK:   apply [[METHOD]]([[ARG1]], [[ARG0]]) : $@convention(objc_method) (Bool, Foo) -> ()
// CHECK-NOT:   destroy_value [[ARG0]]
// CHECK: } // end sil function '$s13objc_bridging7setZangyySo3FooC_SbtF'

// NSString *bar(void);
func callBar() -> String {
  return bar()
}
// CHECK-LABEL: sil hidden @$s13objc_bridging7callBar{{.*}}F
// CHECK: bb0:
// CHECK:   [[BAR:%.*]] = function_ref @bar
// CHECK:   [[OPT_BRIDGED:%.*]] = apply [[BAR]]() : $@convention(c) () -> @autoreleased Optional<NSString>
// CHECK:   switch_enum [[OPT_BRIDGED]] : $Optional<NSString>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]

// CHECK: [[SOME_BB]]([[BRIDGED:%.*]] : @owned $NSString):
// CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @$sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
// CHECK:   [[BRIDGED_BOX:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[BRIDGED]]
// CHECK:   [[NATIVE:%.*]] = apply [[NSSTRING_TO_STRING]]([[BRIDGED_BOX]]
// CHECK:   [[OPT_NATIVE:%.*]] = enum $Optional<String>, #Optional.some!enumelt.1, [[NATIVE]]
// CHECK:   bb5([[NATIVE:%.*]] : @owned $String):
// CHECK:   return [[NATIVE]]
// CHECK: }

// void setBar(NSString *s);
func callSetBar(_ s: String) {
  var s = s
  setBar(s)
}
// CHECK-LABEL: sil hidden @$s13objc_bridging10callSetBar{{.*}}F
// CHECK: bb0({{%.*}} : @guaranteed $String):
// CHECK:   [[NATIVE:%.*]] = load
// CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
// CHECK:   [[BORROWED_NATIVE:%.*]] = begin_borrow [[NATIVE]]
// CHECK:   [[BRIDGED:%.*]] = apply [[STRING_TO_NSSTRING]]([[BORROWED_NATIVE]])
// CHECK:   [[OPT_BRIDGED:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[BRIDGED]]
// CHECK:   end_borrow [[BORROWED_NATIVE]]
// CHECK:   [[SET_BAR:%.*]] = function_ref @setBar
// CHECK:   apply [[SET_BAR]]([[OPT_BRIDGED]])
// CHECK:   destroy_value [[OPT_BRIDGED]]
// CHECK: }

var NSS: NSString

// -- NSString methods don't convert 'self'
extension NSString {
  @objc var nsstrFakeProp: NSString {
    get { return NSS }
    set {}
  }
  // CHECK-LABEL: sil hidden [thunk] @$sSo8NSStringC13objc_bridgingE13nsstrFakePropABvgTo
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: $sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  // CHECK: }
  // CHECK-LABEL: sil hidden [thunk] @$sSo8NSStringC13objc_bridgingE13nsstrFakePropABvsTo
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: $sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  // CHECK: }

  @objc func nsstrResult() -> NSString { return NSS }
  // CHECK-LABEL: sil hidden [thunk] @$sSo8NSStringC13objc_bridgingE11nsstrResultAByFTo
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: $sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  // CHECK: }

  @objc func nsstrArg(_ s: NSString) { }
  // CHECK-LABEL: sil hidden [thunk] @$sSo8NSStringC13objc_bridgingE8nsstrArgyyABFTo
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: $sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  // CHECK: }

}

class Bas : NSObject {
  // -- Bridging thunks for String properties convert between NSString
  @objc var strRealProp: String = "Hello"
  // CHECK-LABEL: sil hidden [thunk] @$s13objc_bridging3BasC11strRealPropSSvgTo : $@convention(objc_method) (Bas) -> @autoreleased NSString {
  // CHECK: bb0([[THIS:%.*]] : @unowned $Bas):
  // CHECK:   [[THIS_COPY:%.*]] = copy_value [[THIS]] : $Bas
  // CHECK:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK:   // function_ref objc_bridging.Bas.strRealProp.getter
  // CHECK:   [[PROPIMPL:%.*]] = function_ref @$s13objc_bridging3BasC11strRealPropSSvg
  // CHECK:   [[PROP_COPY:%.*]] = apply [[PROPIMPL]]([[BORROWED_THIS_COPY]]) : $@convention(method) (@guaranteed Bas) -> @owned String
  // CHECK:   end_borrow [[BORROWED_THIS_COPY]]
  // CHECK:   destroy_value [[THIS_COPY]]
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
  // CHECK:   [[BORROWED_PROP_COPY:%.*]] = begin_borrow [[PROP_COPY]]
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[BORROWED_PROP_COPY]])
  // CHECK:   end_borrow [[BORROWED_PROP_COPY]]
  // CHECK:   destroy_value [[PROP_COPY]]
  // CHECK:   return [[NSSTR]]
  // CHECK: }


  // CHECK-LABEL: sil hidden @$s13objc_bridging3BasC11strRealPropSSvg
  // CHECK:   [[PROP_ADDR:%.*]] = ref_element_addr %0 : {{.*}}, #Bas.strRealProp
  // CHECK:   [[READ:%.*]] = begin_access [read] [dynamic] [[PROP_ADDR]] : $*String
  // CHECK:   [[PROP:%.*]] = load [copy] [[READ]]


  // CHECK-LABEL: sil hidden [thunk] @$s13objc_bridging3BasC11strRealPropSSvsTo : $@convention(objc_method) (NSString, Bas) -> () {
  // CHECK: bb0([[VALUE:%.*]] : @unowned $NSString, [[THIS:%.*]] : @unowned $Bas):
  // CHECK:   [[VALUE_COPY:%.*]] = copy_value [[VALUE]]
  // CHECK:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @$sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  // CHECK:   [[VALUE_BOX:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[VALUE_COPY]]
  // CHECK:   [[STR:%.*]] = apply [[NSSTRING_TO_STRING]]([[VALUE_BOX]]

  // CHECK:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK:   [[SETIMPL:%.*]] = function_ref @$s13objc_bridging3BasC11strRealPropSSvs
  // CHECK:   apply [[SETIMPL]]([[STR]], [[BORROWED_THIS_COPY]])
  // CHECK:   end_borrow [[BORROWED_THIS_COPY]]
  // CHECK:   destroy_value [[THIS_COPY]]
  // CHECK: } // end sil function '$s13objc_bridging3BasC11strRealPropSSvsTo'

  // CHECK-LABEL: sil hidden @$s13objc_bridging3BasC11strRealPropSSvs
  // CHECK: bb0(%0 : @owned $String, %1 : @guaranteed $Bas):

  // CHECK:   [[STR_ADDR:%.*]] = ref_element_addr %1 : {{.*}}, #Bas.strRealProp
  // CHECK:   [[WRITE:%.*]] = begin_access [modify] [dynamic] [[STR_ADDR]] : $*String
  // CHECK:   assign {{.*}} to [[WRITE]]
  // CHECK: }

  @objc var strFakeProp: String {
    get { return "" }
    set {}
  }
  // CHECK-LABEL: sil hidden [thunk] @$s13objc_bridging3BasC11strFakePropSSvgTo : $@convention(objc_method) (Bas) -> @autoreleased NSString {
  // CHECK: bb0([[THIS:%.*]] : @unowned $Bas):
  // CHECK:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK:   [[GETTER:%.*]] = function_ref @$s13objc_bridging3BasC11strFakePropSSvg
  // CHECK:   [[STR:%.*]] = apply [[GETTER]]([[BORROWED_THIS_COPY]])
  // CHECK:   end_borrow [[BORROWED_THIS_COPY]]
  // CHECK:   destroy_value [[THIS_COPY]]
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
  // CHECK:   [[BORROWED_STR:%.*]] = begin_borrow [[STR]]
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[BORROWED_STR]])
  // CHECK:   end_borrow [[BORROWED_STR]]
  // CHECK:   destroy_value [[STR]]
  // CHECK:   return [[NSSTR]]
  // CHECK: }

  // CHECK-LABEL: sil hidden [thunk] @$s13objc_bridging3BasC11strFakePropSSvsTo : $@convention(objc_method) (NSString, Bas) -> () {
  // CHECK: bb0([[NSSTR:%.*]] : @unowned $NSString, [[THIS:%.*]] : @unowned $Bas):
  // CHECK:   [[NSSTR_COPY:%.*]] = copy_value [[NSSTR]]
  // CHECK:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @$sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  // CHECK:   [[NSSTR_BOX:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[NSSTR_COPY]]
  // CHECK:   [[STR:%.*]] = apply [[NSSTRING_TO_STRING]]([[NSSTR_BOX]]
  // CHECK:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK:   [[SETTER:%.*]] = function_ref @$s13objc_bridging3BasC11strFakePropSSvs
  // CHECK:   apply [[SETTER]]([[STR]], [[BORROWED_THIS_COPY]])
  // CHECK:   end_borrow [[BORROWED_THIS_COPY]]
  // CHECK:   destroy_value [[THIS_COPY]]
  // CHECK: } // end sil function '$s13objc_bridging3BasC11strFakePropSSvsTo'

  // -- Bridging thunks for explicitly NSString properties don't convert
  @objc var nsstrRealProp: NSString
  @objc var nsstrFakeProp: NSString {
    get { return NSS }
    set {}
  }
  // CHECK-LABEL: sil hidden [thunk] @$s13objc_bridging3BasC13nsstrRealPropSo8NSStringCvgTo : $@convention(objc_method) (Bas) -> @autoreleased NSString {
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: $sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  // CHECK: }

  // CHECK-LABEL: sil hidden [thunk] @$s13objc_bridging3BasC13nsstrRealPropSo8NSStringCvsTo : $@convention(objc_method) (NSString, Bas) ->
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: $sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  // CHECK: }

  // -- Bridging thunks for String methods convert between NSString
  @objc func strResult() -> String { return "" }
  // CHECK-LABEL: sil hidden [thunk] @$s13objc_bridging3BasC9strResultSSyFTo
  // CHECK: bb0([[THIS:%.*]] : @unowned $Bas):
  // CHECK:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK:   [[METHOD:%.*]] = function_ref @$s13objc_bridging3BasC9strResultSSyF
  // CHECK:   [[STR:%.*]] = apply [[METHOD]]([[BORROWED_THIS_COPY]])
  // CHECK:   end_borrow [[BORROWED_THIS_COPY]]
  // CHECK:   destroy_value [[THIS_COPY]]
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
  // CHECK:   [[BORROWED_STR:%.*]] = begin_borrow [[STR]]
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[BORROWED_STR]])
  // CHECK:   end_borrow [[BORROWED_STR]]
  // CHECK:   destroy_value [[STR]]
  // CHECK:   return [[NSSTR]]
  // CHECK: }
  @objc func strArg(_ s: String) { }
  // CHECK-LABEL: sil hidden [thunk] @$s13objc_bridging3BasC6strArgyySSFTo
  // CHECK: bb0([[NSSTR:%.*]] : @unowned $NSString, [[THIS:%.*]] : @unowned $Bas):
  // CHECK:   [[NSSTR_COPY:%.*]] = copy_value [[NSSTR]]
  // CHECK:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @$sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  // CHECK:   [[NSSTR_BOX:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[NSSTR_COPY]]
  // CHECK:   [[STR:%.*]] = apply [[NSSTRING_TO_STRING]]([[NSSTR_BOX]]
  // CHECK:   [[BORROWED_STR:%.*]] = begin_borrow [[STR]]
  // CHECK:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK:   [[METHOD:%.*]] = function_ref @$s13objc_bridging3BasC6strArgyySSF
  // CHECK:   apply [[METHOD]]([[BORROWED_STR]], [[BORROWED_THIS_COPY]])
  // CHECK:   end_borrow [[BORROWED_THIS_COPY]]
  // CHECK:   destroy_value [[THIS_COPY]]
  // CHECK: } // end sil function '$s13objc_bridging3BasC6strArgyySSFTo'

  // -- Bridging thunks for explicitly NSString properties don't convert
  @objc func nsstrResult() -> NSString { return NSS }
  // CHECK-LABEL: sil hidden [thunk] @$s13objc_bridging3BasC11nsstrResultSo8NSStringCyFTo
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: $sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  // CHECK: }
  @objc func nsstrArg(_ s: NSString) { }
  // CHECK-LABEL: sil hidden @$s13objc_bridging3BasC8nsstrArgyySo8NSStringCF
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: $sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  // CHECK: }

  @objc init(str: NSString) {
    nsstrRealProp = str
    super.init()
  }

  // CHECK-LABEL: sil hidden [thunk] @$s13objc_bridging3BasC8arrayArgyySayyXlGFTo : $@convention(objc_method) (NSArray, Bas) -> ()
  // CHECK: bb0([[NSARRAY:%[0-9]+]] : @unowned $NSArray, [[SELF:%[0-9]+]] : @unowned $Bas):
  // CHECK:   [[NSARRAY_COPY:%.*]] = copy_value [[NSARRAY]] : $NSArray
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Bas
  // CHECK:   [[CONV_FN:%[0-9]+]] = function_ref @$sSa10FoundationE36_unconditionallyBridgeFromObjectiveCySayxGSo7NSArrayCSgFZ
  // CHECK:   [[OPT_NSARRAY:%[0-9]+]] = enum $Optional<NSArray>, #Optional.some!enumelt.1, [[NSARRAY_COPY]] : $NSArray
  // CHECK:   [[ARRAY_META:%[0-9]+]] = metatype $@thin Array<AnyObject>.Type
  // CHECK:   [[ARRAY:%[0-9]+]] = apply [[CONV_FN]]<AnyObject>([[OPT_NSARRAY]], [[ARRAY_META]])
  // CHECK:   [[BORROWED_ARRAY:%.*]] = begin_borrow [[ARRAY]]
  // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @$s13objc_bridging3BasC8arrayArgyySayyXlGF : $@convention(method) (@guaranteed Array<AnyObject>, @guaranteed Bas) -> ()
  // CHECK:   [[RESULT:%[0-9]+]] = apply [[SWIFT_FN]]([[BORROWED_ARRAY]], [[BORROWED_SELF_COPY]]) : $@convention(method) (@guaranteed Array<AnyObject>, @guaranteed Bas) -> ()
  // CHECK:   end_borrow [[BORROWED_SELF_COPY]]
  // CHECK:   destroy_value [[SELF_COPY]] : $Bas
  // CHECK:   return [[RESULT]] : $()
  @objc func arrayArg(_ array: [AnyObject]) { }
  
  // CHECK-LABEL: sil hidden [thunk] @$s13objc_bridging3BasC11arrayResultSayyXlGyFTo : $@convention(objc_method) (Bas) -> @autoreleased NSArray
  // CHECK: bb0([[SELF:%[0-9]+]] : @unowned $Bas):
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Bas
  // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @$s13objc_bridging3BasC11arrayResultSayyXlGyF : $@convention(method) (@guaranteed Bas) -> @owned Array<AnyObject>
  // CHECK:   [[ARRAY:%[0-9]+]] = apply [[SWIFT_FN]]([[BORROWED_SELF_COPY]]) : $@convention(method) (@guaranteed Bas) -> @owned Array<AnyObject>
  // CHECK:   end_borrow [[BORROWED_SELF_COPY]]
  // CHECK:   destroy_value [[SELF_COPY]]
  // CHECK:   [[CONV_FN:%[0-9]+]] = function_ref @$sSa10FoundationE19_bridgeToObjectiveCSo7NSArrayCyF
  // CHECK:   [[BORROWED_ARRAY:%.*]] = begin_borrow [[ARRAY]]
  // CHECK:   [[NSARRAY:%[0-9]+]] = apply [[CONV_FN]]<AnyObject>([[BORROWED_ARRAY]]) : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @owned NSArray
  // CHECK:   end_borrow [[BORROWED_ARRAY]]
  // CHECK:   destroy_value [[ARRAY]]
  // CHECK:   return [[NSARRAY]]
  @objc func arrayResult() -> [AnyObject] { return [] }

  // CHECK-LABEL: sil hidden [thunk] @$s13objc_bridging3BasC9arrayPropSaySSGvgTo : $@convention(objc_method) (Bas) -> @autoreleased NSArray
  // CHECK-LABEL: sil hidden [thunk] @$s13objc_bridging3BasC9arrayPropSaySSGvsTo : $@convention(objc_method) (NSArray, Bas) -> ()
  @objc var arrayProp: [String] = []
}

// CHECK-LABEL: sil hidden @$s13objc_bridging16applyStringBlock_1xS3SXB_SStF
func applyStringBlock(_ f: @convention(block) (String) -> String, x: String) -> String {
  // CHECK: bb0([[BLOCK:%.*]] : @guaranteed $@convention(block) @noescape (NSString) -> @autoreleased NSString, [[STRING:%.*]] : @guaranteed $String):
  // CHECK:   [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  // CHECK:   [[BORROWED_BLOCK_COPY:%.*]] = begin_borrow [[BLOCK_COPY]]
  // CHECK:   [[BLOCK_COPY_COPY:%.*]] = copy_value [[BORROWED_BLOCK_COPY]]
  // CHECK:   [[STRING_COPY:%.*]] = copy_value [[STRING]]
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
  // CHECK:   [[BORROWED_STRING_COPY:%.*]] = begin_borrow [[STRING_COPY]]
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[BORROWED_STRING_COPY]]) : $@convention(method) (@guaranteed String)
  // CHECK:   end_borrow [[BORROWED_STRING_COPY]]
  // CHECK:   destroy_value [[STRING_COPY]]
  // CHECK:   [[RESULT_NSSTR:%.*]] = apply [[BLOCK_COPY_COPY]]([[NSSTR]]) : $@convention(block) @noescape (NSString) -> @autoreleased NSString
  // CHECK:   destroy_value [[NSSTR]]
  // CHECK:   [[FINAL_BRIDGE:%.*]] = function_ref @$sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  // CHECK:   [[OPTIONAL_NSSTR:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[RESULT_NSSTR]]
  // CHECK:   [[RESULT:%.*]] = apply [[FINAL_BRIDGE]]([[OPTIONAL_NSSTR]], {{.*}}) : $@convention(method) (@guaranteed Optional<NSString>, @thin String.Type) -> @owned String
  // CHECK:   destroy_value [[BLOCK_COPY_COPY]]
  // CHECK-NOT:   destroy_value [[STRING]]
  // CHECK:   destroy_value [[BLOCK_COPY]]
  // CHECK-NOT:   destroy_value [[BLOCK]]
  // CHECK:   return [[RESULT]] : $String
  return f(x)
}
// CHECK: } // end sil function '$s13objc_bridging16applyStringBlock_1xS3SXB_SStF'

// CHECK-LABEL: sil hidden @$s13objc_bridging15bridgeCFunction{{.*}}F
func bridgeCFunction() -> (String?) -> (String?) {
  // CHECK: [[THUNK:%.*]] = function_ref @$sSo18NSStringFromStringySSSgABFTO : $@convention(thin) (@guaranteed Optional<String>) -> @owned Optional<String>
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

// CHECK-LABEL: sil shared [serializable] @$sSo7NSArrayC7objects5countABSPyyXlSgGSg_s5Int32VtcfC
// CHECK:         [[SELF:%.*]] = alloc_ref_dynamic
// CHECK:         [[METHOD:%.*]] = function_ref @$sSo7NSArrayC7objects5countABSPyyXlSgGSg_s5Int32VtcfcTO
// CHECK:         [[RESULT:%.*]] = apply [[METHOD]]
// CHECK:         return [[RESULT]]

// Check that type lowering preserves the bool/BOOL distinction when bridging
// imported C functions.

// CHECK-ios-i386-LABEL: sil hidden @$s13objc_bridging5boolsySb_SbtSbF
// CHECK-ios-i386:         function_ref @useBOOL : $@convention(c) (ObjCBool) -> ()
// CHECK-ios-i386:         function_ref @useBool : $@convention(c) (Bool) -> ()
// CHECK-ios-i386:         function_ref @getBOOL : $@convention(c) () -> ObjCBool
// CHECK-ios-i386:         function_ref @getBool : $@convention(c) () -> Bool

// CHECK-macosx-x86_64-LABEL: sil hidden @$s13objc_bridging5boolsySb_SbtSbF
// CHECK-macosx-x86_64:         function_ref @useBOOL : $@convention(c) (ObjCBool) -> ()
// CHECK-macosx-x86_64:         function_ref @useBool : $@convention(c) (Bool) -> ()
// CHECK-macosx-x86_64:         function_ref @getBOOL : $@convention(c) () -> ObjCBool
// CHECK-macosx-x86_64:         function_ref @getBool : $@convention(c) () -> Bool

// FIXME: no distinction on x86_64, arm64 or watchos-i386, since SILGen looks
// at the underlying Clang decl of the bridged decl to decide whether it needs
// bridging.
//
// CHECK-watchos-i386-LABEL: sil hidden @$s13objc_bridging5boolsySb_SbtSbF
// CHECK-watchos-i386:         function_ref @useBOOL : $@convention(c) (Bool) -> ()
// CHECK-watchos-i386:         function_ref @useBool : $@convention(c) (Bool) -> ()
// CHECK-watchos-i386:         function_ref @getBOOL : $@convention(c) () -> Bool
// CHECK-watchos-i386:         function_ref @getBool : $@convention(c) () -> Bool

// CHECK-ios-x86_64-LABEL: sil hidden @$s13objc_bridging5boolsySb_SbtSbF
// CHECK-ios-x86_64:         function_ref @useBOOL : $@convention(c) (Bool) -> ()
// CHECK-ios-x86_64:         function_ref @useBool : $@convention(c) (Bool) -> ()
// CHECK-ios-x86_64:         function_ref @getBOOL : $@convention(c) () -> Bool
// CHECK-ios-x86_64:         function_ref @getBool : $@convention(c) () -> Bool

// CHECK-arm64-LABEL: sil hidden @$s13objc_bridging5boolsySb_SbtSbF
// CHECK-arm64:         function_ref @useBOOL : $@convention(c) (Bool) -> ()
// CHECK-arm64:         function_ref @useBool : $@convention(c) (Bool) -> ()
// CHECK-arm64:         function_ref @getBOOL : $@convention(c) () -> Bool
// CHECK-arm64:         function_ref @getBool : $@convention(c) () -> Bool

func bools(_ x: Bool) -> (Bool, Bool) {
  useBOOL(x)
  useBool(x)

  return (getBOOL(), getBool())
}

// CHECK-LABEL: sil hidden @$s13objc_bridging9getFridge{{.*}}F
// CHECK: bb0([[HOME:%[0-9]+]] : @guaranteed $APPHouse):
func getFridge(_ home: APPHouse) -> Refrigerator {
  // CHECK: [[GETTER:%[0-9]+]] = objc_method [[HOME]] : $APPHouse, #APPHouse.fridge!getter.1.foreign
  // CHECK: [[OBJC_RESULT:%[0-9]+]] = apply [[GETTER]]([[HOME]])
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @$s10Appliances12RefrigeratorV36_unconditionallyBridgeFromObjectiveCyACSo15APPRefrigeratorCSgFZ
  // CHECK: [[REFRIGERATOR_META:%[0-9]+]] = metatype $@thin Refrigerator.Type
  // CHECK: [[RESULT:%[0-9]+]] = apply [[BRIDGE_FN]]([[OBJC_RESULT]], [[REFRIGERATOR_META]])
  // CHECK-NOT: destroy_value [[HOME]] : $APPHouse
  // CHECK: return [[RESULT]] : $Refrigerator
  return home.fridge
}

// FIXME(integers): the following checks should be updated for the new integer
// protocols. <rdar://problem/29939484>
// XCHECK-LABEL: sil hidden @$s13objc_bridging16updateFridgeTemp{{.*}}F
// XCHECK: bb0([[HOME:%[0-9]+]] : $APPHouse, [[DELTA:%[0-9]+]] : $Double):
func updateFridgeTemp(_ home: APPHouse, delta: Double) {
  // +=
  // XCHECK: [[PLUS_EQ:%[0-9]+]] = function_ref @$ss2peoiyySdz_SdtF

  // Temporary fridge
  // XCHECK: [[TEMP_FRIDGE:%[0-9]+]]  = alloc_stack $Refrigerator

  // Get operation
  // CHECK: [[GETTER:%[0-9]+]] = objc_method [[HOME]] : $APPHouse, #APPHouse.fridge!getter.1.foreign
  // CHECK: [[OBJC_FRIDGE:%[0-9]+]] = apply [[GETTER]]([[HOME]])
  // CHECK: [[BRIDGE_FROM_FN:%[0-9]+]] = function_ref @$s10Appliances12RefrigeratorV36_unconditionallyBridgeFromObjectiveCyACSo15APPRefrigeratorCSgFZ
  // CHECK: [[REFRIGERATOR_META:%[0-9]+]] = metatype $@thin Refrigerator.Type
  // CHECK: [[FRIDGE:%[0-9]+]] = apply [[BRIDGE_FROM_FN]]([[OBJC_FRIDGE]], [[REFRIGERATOR_META]])

  // Addition
  // XCHECK: [[TEMP:%[0-9]+]] = struct_element_addr [[TEMP_FRIDGE]] : $*Refrigerator, #Refrigerator.temperature
  // XCHECK: apply [[PLUS_EQ]]([[TEMP]], [[DELTA]])

  // Setter
  // XCHECK: [[FRIDGE:%[0-9]+]] = load [trivial] [[TEMP_FRIDGE]] : $*Refrigerator
  // XCHECK: [[SETTER:%[0-9]+]] = objc_method [[BORROWED_HOME]] : $APPHouse, #APPHouse.fridge!setter.1.foreign
  // XCHECK: [[BRIDGE_TO_FN:%[0-9]+]] = function_ref @$s10Appliances12RefrigeratorV19_bridgeToObjectiveCSo15APPRefrigeratorCyF
  // XCHECK: [[OBJC_ARG:%[0-9]+]] = apply [[BRIDGE_TO_FN]]([[FRIDGE]])
  // XCHECK: apply [[SETTER]]([[OBJC_ARG]], [[BORROWED_HOME]]) : $@convention(objc_method) (APPRefrigerator, APPHouse) -> ()
  // XCHECK: destroy_value [[OBJC_ARG]]
  // XCHECK: end_borrow [[BORROWED_HOME]]
  // XCHECK: destroy_value [[HOME]]
  home.fridge.temperature += delta
}

// CHECK-LABEL: sil hidden @$s13objc_bridging20callNonStandardBlock5valueySi_tF
func callNonStandardBlock(value: Int) {
  // CHECK: enum $Optional<@convention(block) () -> @owned Optional<AnyObject>>
  takesNonStandardBlock { return value }
}

func takeTwoAnys(_ lhs: Any, _ rhs: Any) -> Any { return lhs }

// CHECK-LABEL: sil hidden @$s13objc_bridging22defineNonStandardBlock1xyyp_tF
func defineNonStandardBlock(x: Any) {
  // CHECK: function_ref @$s13objc_bridging22defineNonStandardBlock1xyyp_tFypypcfU_
  // CHECK: function_ref @$sypypIegnr_yXlyXlIeyBya_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed (@in_guaranteed Any) -> @out Any, AnyObject) -> @autoreleased AnyObject

  let fn : @convention(block) (Any) -> Any = { y in takeTwoAnys(x, y) }
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sypypIegnr_yXlyXlIeyBya_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed (@in_guaranteed Any) -> @out Any, AnyObject) -> @autoreleased AnyObject
// CHECK: bb0(%0 : @trivial $*@block_storage @callee_guaranteed (@in_guaranteed Any) -> @out Any, %1 : @unowned $AnyObject):
// CHECK:   [[T0:%.*]] = copy_value %1 : $AnyObject
// CHECK:   [[T1:%.*]] = open_existential_ref [[T0]] : $AnyObject
// CHECK:   [[ARG:%.*]] = alloc_stack $Any
// CHECK:   [[T2:%.*]] = init_existential_addr [[ARG]]
// CHECK:   store [[T1]] to [init] [[T2]]
// CHECK:   [[RESULT:%.*]] = alloc_stack $Any
// CHECK:   apply {{.*}}([[RESULT]], [[ARG]])

// CHECK-LABEL: sil hidden @$s13objc_bridging15castToCFunction3ptrySV_tF : $@convention(thin) (UnsafeRawPointer) -> () {
func castToCFunction(ptr: UnsafeRawPointer) {
  // CHECK: [[OUT:%.*]] = alloc_stack $@convention(c) (Optional<AnyObject>) -> ()
  // CHECK: [[IN:%.]] = alloc_stack $UnsafeRawPointer
  // CHECK: store %0 to [trivial] [[IN]] : $*UnsafeRawPointer
  // CHECK: [[META:%.*]] = metatype $@thick (@convention(c) (Optional<AnyObject>) -> ()).Type
  // CHECK: [[CASTFN:%.*]] = function_ref @$ss13unsafeBitCast_2toq_x_q_mtr0_lF
  // CHECK: apply [[CASTFN]]<UnsafeRawPointer, @convention(c) (AnyObject?) -> ()>([[OUT]], [[IN]], [[META]]) : $@convention(thin) <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0, @thick τ_0_1.Type) -> @out τ_0_1
  // CHECK: [[RESULT:%.*]] = load [trivial] [[OUT]] : $*@convention(c) (Optional<AnyObject>) -> ()
  typealias Fn = @convention(c) (AnyObject?) -> Void
  unsafeBitCast(ptr, to: Fn.self)(nil)
}
