// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-silgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-module -o %t -I %S/../Inputs/ObjCBridging %S/../Inputs/ObjCBridging/Appliances.swift
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -I %S/../Inputs/ObjCBridging -Xllvm -sil-full-demangle -emit-silgen %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-cpu --check-prefix=CHECK-%target-os-%target-cpu

// REQUIRES: objc_interop

import Foundation
import Appliances


func getDescription(_ o: NSObject) -> String {
  return o.description
}
// CHECK-LABEL: sil hidden @_T013objc_bridging14getDescription{{.*}}F
// CHECK: bb0([[ARG:%.*]] : $NSObject):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[DESCRIPTION:%.*]] = class_method [volatile] [[BORROWED_ARG]] : $NSObject, #NSObject.description!getter.1.foreign
// CHECK:   [[OPT_BRIDGED:%.*]] = apply [[DESCRIPTION]]([[BORROWED_ARG]])
// CHECK:   switch_enum [[OPT_BRIDGED]] : $Optional<NSString>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]([[BRIDGED:%.*]] : $NSString):
// CHECK-NOT:   unchecked_enum_data
// CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @_T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
// CHECK:   [[BRIDGED_BOX:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[BRIDGED]]
// CHECK:   [[NATIVE:%.*]] = apply [[NSSTRING_TO_STRING]]([[BRIDGED_BOX]],
// CHECK:   [[OPT_NATIVE:%.*]] = enum $Optional<String>, #Optional.some!enumelt.1, [[NATIVE]]
// CHECK:   br [[CONT_BB:bb[0-9]+]]([[OPT_NATIVE]] : $Optional<String>)
//
// CHECK: [[NONE_BB]]:
// CHECK:   [[OPT_NATIVE:%.*]] = enum $Optional<String>, #Optional.none!enumelt
// CHECK:   br [[CONT_BB]]([[OPT_NATIVE]] : $Optional<String>)
//
// CHECK: [[CONT_BB]]([[OPT_NATIVE:%.*]] : $Optional<String>):
// CHECK:   switch_enum [[OPT_NATIVE]] : $Optional<String>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
// CHECK: [[NONE_BB]]:
// CHECK:   unreachable
//
// CHECK: [[SOME_BB]]([[NATIVE:%.*]] : $String):
// CHECK:    end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:    destroy_value [[ARG]]
// CHECK:    return [[NATIVE]] 
// CHECK:}

func getUppercaseString(_ s: NSString) -> String {
  return s.uppercase()
}
// CHECK-LABEL: sil hidden @_T013objc_bridging18getUppercaseString{{.*}}F
// CHECK: bb0([[ARG:%.*]] : $NSString):
// -- The 'self' argument of NSString methods doesn't bridge.
// CHECK-NOT: function_ref @_T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
// CHECK-NOT: function_ref @swift_StringToNSString
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[UPPERCASE_STRING:%.*]] = class_method [volatile] [[BORROWED_ARG]] : $NSString, #NSString.uppercase!1.foreign
// CHECK:   [[OPT_BRIDGED:%.*]] = apply [[UPPERCASE_STRING]]([[BORROWED_ARG]]) : $@convention(objc_method) (NSString) -> @autoreleased Optional<NSString>
// CHECK:   switch_enum [[OPT_BRIDGED]] : $Optional<NSString>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
//
// CHECK: [[SOME_BB]]([[BRIDGED:%.*]] :
// CHECK-NOT:  unchecked_enum_data
// CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @_T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
// CHECK:   [[BRIDGED_BOX:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[BRIDGED]]
// CHECK:   [[NATIVE:%.*]] = apply [[NSSTRING_TO_STRING]]([[BRIDGED_BOX]]
// CHECK:   [[OPT_NATIVE:%.*]] = enum $Optional<String>, #Optional.some!enumelt.1, [[NATIVE]]
// CHECK:   br [[CONT_BB:bb[0-9]+]]([[OPT_NATIVE]] : $Optional<String>)
//
// CHECK: [[NONE_BB]]:
// CHECK:   [[OPT_NATIVE:%.*]] = enum $Optional<String>, #Optional.none!enumelt
// CHECK:   br [[CONT_BB]]([[OPT_NATIVE]] : $Optional<String>)
//
// CHECK: [[CONT_BB]]([[OPT_NATIVE:%.*]] : $Optional<String>):
// CHECK:   switch_enum [[OPT_NATIVE]] : $Optional<String>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
// CHECK: [[NONE_BB]]:
// CHECK:   unreachable
//
// CHECK: [[SOME_BB]]([[NATIVE:%.*]] : $String):
// CHECK:   return [[NATIVE]]
// CHECK: }

// @interface Foo -(void) setFoo: (NSString*)s; @end
func setFoo(_ f: Foo, s: String) {
  var s = s
  f.setFoo(s)
}
// CHECK-LABEL: sil hidden @_T013objc_bridging6setFoo{{.*}}F
// CHECK: bb0([[ARG0:%.*]] : $Foo, {{%.*}} : $String):
// CHECK:   [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
// CHECK:   [[SET_FOO:%.*]] = class_method [volatile] [[BORROWED_ARG0]] : $Foo, #Foo.setFoo!1.foreign
// CHECK:   [[NV:%.*]] = load
// CHECK:   [[OPT_NATIVE:%.*]] = enum $Optional<String>, #Optional.some!enumelt.1, [[NV]]
// CHECK:   switch_enum [[OPT_NATIVE]] : $Optional<String>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]([[NATIVE:%.*]] : $String):
// CHECK-NOT: unchecked_enum_data
// CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @_T0SS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
// CHECK:   [[BORROWED_NATIVE:%.*]] = begin_borrow [[NATIVE]]
// CHECK:   [[BRIDGED:%.*]] = apply [[STRING_TO_NSSTRING]]([[BORROWED_NATIVE]])
// CHECK:   [[OPT_BRIDGED:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[BRIDGED]]
// CHECK:   end_borrow [[BORROWED_NATIVE]] from [[NATIVE]]
// CHECK:   destroy_value [[NATIVE]]
// CHECK:   br [[CONT_BB:bb[0-9]+]]([[OPT_BRIDGED]] : $Optional<NSString>)
//
// CHECK: [[NONE_BB]]:
// CHECK:   [[OPT_BRIDGED:%.*]] = enum $Optional<NSString>, #Optional.none!enumelt
// CHECK:   br [[CONT_BB]]([[OPT_BRIDGED]] : $Optional<NSString>)
//
// CHECK: [[CONT_BB]]([[OPT_BRIDGED:%.*]] : $Optional<NSString>):
// CHECK:   apply [[SET_FOO]]([[OPT_BRIDGED]], [[BORROWED_ARG0]]) : $@convention(objc_method) (Optional<NSString>, Foo) -> ()
// CHECK:   destroy_value [[OPT_BRIDGED]]
// CHECK:   end_borrow [[BORROWED_ARG0]] from [[ARG0]]
// CHECK:   destroy_value [[ARG0]]
// CHECK: }

// @interface Foo -(BOOL) zim; @end
func getZim(_ f: Foo) -> Bool {
  return f.zim()
}

// CHECK-ios-i386-LABEL: sil hidden @_T013objc_bridging6getZim{{.*}}F
// CHECK-ios-i386: bb0([[SELF:%.*]] : $Foo):
// CHECK-ios-i386:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
// CHECK-ios-i386:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $Foo, #Foo.zim!1.foreign : (Foo) -> () -> Bool
// CHECK-ios-i386:   [[OBJC_BOOL:%.*]] = apply [[METHOD]]([[BORROWED_SELF]])  : $@convention(objc_method) (Foo) -> ObjCBool
// CHECK-ios-i386:   [[CONVERT:%.*]] = function_ref @swift_ObjCBoolToBool : $@convention(thin) (ObjCBool) -> Bool
// CHECK-ios-i386:   [[SWIFT_BOOL:%.*]] = apply [[CONVERT]]([[OBJC_BOOL]]) : $@convention(thin) (ObjCBool) -> Bool
// CHECK-ios-i386:   end_borrow [[BORROWED_SELF]] from [[SELF]]
// CHECK-ios-i386:   return [[SWIFT_BOOL]] : $Bool
// CHECK-ios-i386: }

// CHECK-watchos-i386-LABEL: sil hidden @_T013objc_bridging6getZim{{.*}}F
// CHECK-watchos-i386: bb0([[SELF:%.*]] : $Foo):
// CHECK-watchos-i386:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
// CHECK-watchos-i386:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $Foo, #Foo.zim!1.foreign : (Foo) -> () -> Boo
// CHECK-watchos-i386:   [[BOOL:%.*]] = apply [[METHOD]]([[BORROWED_SELF]]) : $@convention(objc_method) (Foo) -> Bool
// CHECK-watchos-i386:   end_borrow [[BORROWED_SELF]] from [[SELF]]
// CHECK-watchos-i386:   return [[BOOL]] : $Bool
// CHECK-watchos-i386: }

// CHECK-macosx-x86_64-LABEL: sil hidden @_T013objc_bridging6getZim{{.*}}F
// CHECK-macosx-x86_64: bb0([[SELF:%.*]] : $Foo):
// CHECK-macosx-x86_64:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
// CHECK-macosx-x86_64:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $Foo, #Foo.zim!1.foreign : (Foo) -> () -> Bool
// CHECK-macosx-x86_64:   [[OBJC_BOOL:%.*]] = apply [[METHOD]]([[BORROWED_SELF]])  : $@convention(objc_method) (Foo) -> ObjCBool
// CHECK-macosx-x86_64:   [[CONVERT:%.*]] = function_ref @swift_ObjCBoolToBool : $@convention(thin) (ObjCBool) -> Bool
// CHECK-macosx-x86_64:   [[SWIFT_BOOL:%.*]] = apply [[CONVERT]]([[OBJC_BOOL]]) : $@convention(thin) (ObjCBool) -> Bool
// CHECK-macosx-x86_64:   end_borrow [[BORROWED_SELF]] from [[SELF]]
// CHECK-macosx-x86_64:   return [[SWIFT_BOOL]] : $Bool
// CHECK-macosx-x86_64: }

// CHECK-ios-x86_64-LABEL: sil hidden @_T013objc_bridging6getZim{{.*}}F
// CHECK-ios-x86_64: bb0([[SELF:%.*]] : $Foo):
// CHECK-ios-x86_64:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
// CHECK-ios-x86_64:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $Foo, #Foo.zim!1.foreign : (Foo) -> () -> Boo
// CHECK-ios-x86_64:   [[BOOL:%.*]] = apply [[METHOD]]([[BORROWED_SELF]]) : $@convention(objc_method) (Foo) -> Bool
// CHECK-ios-x86_64:   end_borrow [[BORROWED_SELF]] from [[SELF]]
// CHECK-ios-x86_64:   return [[BOOL]] : $Bool
// CHECK-ios-x86_64: }

// CHECK-arm64-LABEL: sil hidden @_T013objc_bridging6getZim{{.*}}F
// CHECK-arm64: bb0([[SELF:%.*]] : $Foo):
// CHECK-arm64:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
// CHECK-arm64:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $Foo, #Foo.zim!1.foreign : (Foo) -> () -> Boo
// CHECK-arm64:   [[BOOL:%.*]] = apply [[METHOD]]([[BORROWED_SELF]]) : $@convention(objc_method) (Foo) -> Bool
// CHECK-arm64:   end_borrow [[BORROWED_SELF]] from [[SELF]]
// CHECK-arm64:   return [[BOOL]] : $Bool
// CHECK-arm64: }

// @interface Foo -(void) setZim: (BOOL)b; @end
func setZim(_ f: Foo, b: Bool) {
  f.setZim(b)
}
// CHECK-ios-i386-LABEL: sil hidden @_T013objc_bridging6setZim{{.*}}F
// CHECK-ios-i386: bb0([[ARG0:%.*]] : $Foo, [[ARG1:%.*]] : $Bool):
// CHECK-ios-i386:   [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
// CHECK-ios-i386:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_ARG0]] : $Foo, #Foo.setZim!1.foreign
// CHECK-ios-i386:   [[CONVERT:%.*]] = function_ref @swift_BoolToObjCBool : $@convention(thin) (Bool) -> ObjCBool
// CHECK-ios-i386:   [[OBJC_BOOL:%.*]] = apply [[CONVERT]]([[ARG1]]) : $@convention(thin) (Bool) -> ObjCBool
// CHECK-ios-i386:   apply [[METHOD]]([[OBJC_BOOL]], [[BORROWED_ARG0]]) : $@convention(objc_method) (ObjCBool, Foo) -> ()
// CHECK-ios-i386:   end_borrow [[BORROWED_ARG0]] from [[ARG0]]
// CHECK-ios-i386:   destroy_value [[ARG0]]
// CHECK-ios-i386: }

// CHECK-macosx-x86_64-LABEL: sil hidden @_T013objc_bridging6setZim{{.*}}F
// CHECK-macosx-x86_64: bb0([[ARG0:%.*]] : $Foo, [[ARG1:%.*]] : $Bool):
// CHECK-macosx-x86_64:   [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
// CHECK-macosx-x86_64:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_ARG0]] : $Foo, #Foo.setZim!1.foreign
// CHECK-macosx-x86_64:   [[CONVERT:%.*]] = function_ref @swift_BoolToObjCBool : $@convention(thin) (Bool) -> ObjCBool
// CHECK-macosx-x86_64:   [[OBJC_BOOL:%.*]] = apply [[CONVERT]]([[ARG1]]) : $@convention(thin) (Bool) -> ObjCBool
// CHECK-macosx-x86_64:   apply [[METHOD]]([[OBJC_BOOL]], [[BORROWED_ARG0]]) : $@convention(objc_method) (ObjCBool, Foo) -> ()
// CHECK-macosx-x86_64:   end_borrow [[BORROWED_ARG0]] from [[ARG0]]
// CHECK-macosx-x86_64:   destroy_value [[ARG0]]
// CHECK-macosx-x86_64: }

// CHECK-ios-x86_64-LABEL: sil hidden @_T013objc_bridging6setZim{{.*}}F
// CHECK-ios-x86_64: bb0([[ARG0:%.*]] : $Foo, [[ARG1:%.*]] : $Bool):
// CHECK-ios-x86_64:   [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
// CHECK-ios-x86_64:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_ARG0]] : $Foo, #Foo.setZim!1.foreign
// CHECK-ios-x86_64:   apply [[METHOD]]([[ARG1]], [[BORROWED_ARG0]]) : $@convention(objc_method) (Bool, Foo) -> ()
// CHECK-ios-x86_64:   end_borrow [[BORROWED_ARG0]] from [[ARG0]]
// CHECK-ios-x86_64:   destroy_value [[ARG0]]
// CHECK-ios-x86_64: }

// CHECK-arm64-LABEL: sil hidden @_T013objc_bridging6setZim{{.*}}F
// CHECK-arm64: bb0([[ARG0:%.*]] : $Foo, [[ARG1:%.*]] : $Bool):
// CHECK-arm64:   [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
// CHECK-arm64:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_ARG0]] : $Foo, #Foo.setZim!1.foreign
// CHECK-arm64:   apply [[METHOD]]([[ARG1]], [[BORROWED_ARG0]]) : $@convention(objc_method) (Bool, Foo) -> ()
// CHECK-arm64:   end_borrow [[BORROWED_ARG0]] from [[ARG0]]
// CHECK-arm64:   destroy_value [[ARG0]]
// CHECK-arm64: }

// CHECK-watchos-i386-LABEL: sil hidden @_T013objc_bridging6setZim{{.*}}F
// CHECK-watchos-i386: bb0([[ARG0:%.*]] : $Foo, [[ARG1:%.*]] : $Bool):
// CHECK-watchos-i386:   [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
// CHECK-watchos-i386:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_ARG0]] : $Foo, #Foo.setZim!1.foreign
// CHECK-watchos-i386:   apply [[METHOD]]([[ARG1]], [[BORROWED_ARG0]]) : $@convention(objc_method) (Bool, Foo) -> ()
// CHECK-watchos-i386:   end_borrow [[BORROWED_ARG0]] from [[ARG0]]
// CHECK-watchos-i386:   destroy_value [[ARG0]]
// CHECK-watchos-i386: }

// @interface Foo -(_Bool) zang; @end
func getZang(_ f: Foo) -> Bool {
  return f.zang()
}
// CHECK-LABEL: sil hidden @_T013objc_bridging7getZangSbSo3FooCF
// CHECK: bb0([[ARG:%.*]] : $Foo)
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_ARG]] : $Foo, #Foo.zang!1.foreign
// CHECK:   [[BOOL:%.*]] = apply [[METHOD]]([[BORROWED_ARG]]) : $@convention(objc_method) (Foo) -> Bool
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]]
// CHECK:   return [[BOOL]]

// @interface Foo -(void) setZang: (_Bool)b; @end
func setZang(_ f: Foo, _ b: Bool) {
  f.setZang(b)
}
// CHECK-LABEL: sil hidden @_T013objc_bridging7setZangySo3FooC_SbtF
// CHECK: bb0([[ARG0:%.*]] : $Foo, [[ARG1:%.*]] : $Bool):
// CHECK:   [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
// CHECK:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_ARG0]] : $Foo, #Foo.setZang!1.foreign
// CHECK:   apply [[METHOD]]([[ARG1]], [[BORROWED_ARG0]]) : $@convention(objc_method) (Bool, Foo) -> ()
// CHECK:   end_borrow [[BORROWED_ARG0]] from [[ARG0]]
// CHECK:   destroy_value [[ARG0]]
// CHECK: } // end sil function '_T013objc_bridging7setZangySo3FooC_SbtF' 

// NSString *bar(void);
func callBar() -> String {
  return bar()
}
// CHECK-LABEL: sil hidden @_T013objc_bridging7callBar{{.*}}F
// CHECK: bb0:
// CHECK:   [[BAR:%.*]] = function_ref @bar
// CHECK:   [[OPT_BRIDGED:%.*]] = apply [[BAR]]() : $@convention(c) () -> @autoreleased Optional<NSString>
// CHECK:   switch_enum [[OPT_BRIDGED]] : $Optional<NSString>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]

// CHECK: [[SOME_BB]]([[BRIDGED:%.*]] : $NSString):
// CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @_T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
// CHECK:   [[BRIDGED_BOX:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[BRIDGED]]
// CHECK:   [[NATIVE:%.*]] = apply [[NSSTRING_TO_STRING]]([[BRIDGED_BOX]]
// CHECK:   [[OPT_NATIVE:%.*]] = enum $Optional<String>, #Optional.some!enumelt.1, [[NATIVE]]
// CHECK:   bb5([[NATIVE:%.*]] : $String):
// CHECK:   return [[NATIVE]]
// CHECK: }

// void setBar(NSString *s);
func callSetBar(_ s: String) {
  var s = s
  setBar(s)
}
// CHECK-LABEL: sil hidden @_T013objc_bridging10callSetBar{{.*}}F
// CHECK: bb0({{%.*}} : $String):
// CHECK:   [[SET_BAR:%.*]] = function_ref @setBar
// CHECK:   [[NV:%.*]] = load
// CHECK:   [[OPT_NATIVE:%.*]] = enum $Optional<String>, #Optional.some!enumelt.1, [[NV]]
// CHECK:   switch_enum [[OPT_NATIVE]] : $Optional<String>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]

// CHECK: [[SOME_BB]]([[NATIVE:%.*]] : $String):
// CHECK-NOT: unchecked_enum_data
// CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @_T0SS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
// CHECK:   [[BORROWED_NATIVE:%.*]] = begin_borrow [[NATIVE]]
// CHECK:   [[BRIDGED:%.*]] = apply [[STRING_TO_NSSTRING]]([[BORROWED_NATIVE]])
// CHECK:    = enum $Optional<NSString>, #Optional.some!enumelt.1, [[BRIDGED]]
// CHECK:   end_borrow [[BORROWED_NATIVE]] from [[NATIVE]]
// CHECK: bb3([[OPT_BRIDGED:%.*]] : $Optional<NSString>):
// CHECK:   apply [[SET_BAR]]([[OPT_BRIDGED]])
// CHECK:   destroy_value [[OPT_BRIDGED]]
// CHECK: }

var NSS: NSString

// -- NSString methods don't convert 'self'
extension NSString {
  var nsstrFakeProp: NSString {
    get { return NSS }
    set {}
  }
  // CHECK-LABEL: sil hidden [thunk] @_T0So8NSStringC13objc_bridgingE13nsstrFakePropABfgTo
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: _T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
  // CHECK: }
  // CHECK-LABEL: sil hidden [thunk] @_T0So8NSStringC13objc_bridgingE13nsstrFakePropABfsTo
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: _T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
  // CHECK: }

  func nsstrResult() -> NSString { return NSS }
  // CHECK-LABEL: sil hidden [thunk] @_T0So8NSStringC13objc_bridgingE11nsstrResultAByFTo
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: _T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
  // CHECK: }

  func nsstrArg(_ s: NSString) { }
  // CHECK-LABEL: sil hidden [thunk] @_T0So8NSStringC13objc_bridgingE8nsstrArgyABFTo
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: _T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
  // CHECK: }

}

class Bas : NSObject {
  // -- Bridging thunks for String properties convert between NSString
  var strRealProp: String = "Hello"
  // CHECK-LABEL: sil hidden [thunk] @_T013objc_bridging3BasC11strRealPropSSfgTo : $@convention(objc_method) (Bas) -> @autoreleased NSString {
  // CHECK: bb0([[THIS:%.*]] : $Bas):
  // CHECK:   [[THIS_COPY:%.*]] = copy_value [[THIS]] : $Bas
  // CHECK:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK:   // function_ref objc_bridging.Bas.strRealProp.getter
  // CHECK:   [[PROPIMPL:%.*]] = function_ref @_T013objc_bridging3BasC11strRealPropSSfg
  // CHECK:   [[PROP_COPY:%.*]] = apply [[PROPIMPL]]([[BORROWED_THIS_COPY]]) : $@convention(method) (@guaranteed Bas) -> @owned String
  // CHECK:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK:   destroy_value [[THIS_COPY]]
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @_T0SS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
  // CHECK:   [[BORROWED_PROP_COPY:%.*]] = begin_borrow [[PROP_COPY]]
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[BORROWED_PROP_COPY]])
  // CHECK:   end_borrow [[BORROWED_PROP_COPY]] from [[PROP_COPY]]
  // CHECK:   destroy_value [[PROP_COPY]]
  // CHECK:   return [[NSSTR]]
  // CHECK: }


  // CHECK-LABEL: sil hidden @_T013objc_bridging3BasC11strRealPropSSfg
  // CHECK:   [[PROP_ADDR:%.*]] = ref_element_addr %0 : {{.*}}, #Bas.strRealProp
  // CHECK:   [[PROP:%.*]] = load [copy] [[PROP_ADDR]]


  // CHECK-LABEL: sil hidden [thunk] @_T013objc_bridging3BasC11strRealPropSSfsTo : $@convention(objc_method) (NSString, Bas) -> () {
  // CHECK: bb0([[VALUE:%.*]] : $NSString, [[THIS:%.*]] : $Bas):
  // CHECK:   [[VALUE_COPY:%.*]] = copy_value [[VALUE]]
  // CHECK:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @_T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
  // CHECK:   [[VALUE_BOX:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[VALUE_COPY]]
  // CHECK:   [[STR:%.*]] = apply [[NSSTRING_TO_STRING]]([[VALUE_BOX]]

  // CHECK:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK:   [[SETIMPL:%.*]] = function_ref @_T013objc_bridging3BasC11strRealPropSSfs
  // CHECK:   apply [[SETIMPL]]([[STR]], [[BORROWED_THIS_COPY]])
  // CHECK:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK:   destroy_value [[THIS_COPY]]
  // CHECK: } // end sil function '_T013objc_bridging3BasC11strRealPropSSfsTo'

  // CHECK-LABEL: sil hidden @_T013objc_bridging3BasC11strRealPropSSfs
  // CHECK: bb0(%0 : $String, %1 : $Bas):

  // CHECK:   [[STR_ADDR:%.*]] = ref_element_addr %1 : {{.*}}, #Bas.strRealProp
  // CHECK:   assign {{.*}} to [[STR_ADDR]]
  // CHECK: }

  var strFakeProp: String {
    get { return "" }
    set {}
  }
  // CHECK-LABEL: sil hidden [thunk] @_T013objc_bridging3BasC11strFakePropSSfgTo : $@convention(objc_method) (Bas) -> @autoreleased NSString {
  // CHECK: bb0([[THIS:%.*]] : $Bas):
  // CHECK:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK:   [[GETTER:%.*]] = function_ref @_T013objc_bridging3BasC11strFakePropSSfg
  // CHECK:   [[STR:%.*]] = apply [[GETTER]]([[BORROWED_THIS_COPY]])
  // CHECK:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK:   destroy_value [[THIS_COPY]]
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @_T0SS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
  // CHECK:   [[BORROWED_STR:%.*]] = begin_borrow [[STR]]
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[BORROWED_STR]])
  // CHECK:   end_borrow [[BORROWED_STR]] from [[STR]]
  // CHECK:   destroy_value [[STR]]
  // CHECK:   return [[NSSTR]]
  // CHECK: }

  // CHECK-LABEL: sil hidden [thunk] @_T013objc_bridging3BasC11strFakePropSSfsTo : $@convention(objc_method) (NSString, Bas) -> () {
  // CHECK: bb0([[NSSTR:%.*]] : $NSString, [[THIS:%.*]] : $Bas):
  // CHECK:   [[NSSTR_COPY:%.*]] = copy_value [[NSSTR]]
  // CHECK:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @_T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
  // CHECK:   [[NSSTR_BOX:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[NSSTR_COPY]]
  // CHECK:   [[STR:%.*]] = apply [[NSSTRING_TO_STRING]]([[NSSTR_BOX]]
  // CHECK:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK:   [[SETTER:%.*]] = function_ref @_T013objc_bridging3BasC11strFakePropSSfs
  // CHECK:   apply [[SETTER]]([[STR]], [[BORROWED_THIS_COPY]])
  // CHECK:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK:   destroy_value [[THIS_COPY]]
  // CHECK: } // end sil function '_T013objc_bridging3BasC11strFakePropSSfsTo'

  // -- Bridging thunks for explicitly NSString properties don't convert
  var nsstrRealProp: NSString
  var nsstrFakeProp: NSString {
    get { return NSS }
    set {}
  }
  // CHECK-LABEL: sil hidden [thunk] @_T013objc_bridging3BasC13nsstrRealPropSo8NSStringCfgTo : $@convention(objc_method) (Bas) -> @autoreleased NSString {
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: _T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
  // CHECK: }

  // CHECK-LABEL: sil hidden [thunk] @_T013objc_bridging3BasC13nsstrRealPropSo8NSStringCfsTo : $@convention(objc_method) (NSString, Bas) ->
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: _T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
  // CHECK: }

  // -- Bridging thunks for String methods convert between NSString
  func strResult() -> String { return "" }
  // CHECK-LABEL: sil hidden [thunk] @_T013objc_bridging3BasC9strResultSSyFTo
  // CHECK: bb0([[THIS:%.*]] : $Bas):
  // CHECK:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK:   [[METHOD:%.*]] = function_ref @_T013objc_bridging3BasC9strResultSSyF
  // CHECK:   [[STR:%.*]] = apply [[METHOD]]([[BORROWED_THIS_COPY]])
  // CHECK:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK:   destroy_value [[THIS_COPY]]
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @_T0SS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
  // CHECK:   [[BORROWED_STR:%.*]] = begin_borrow [[STR]]
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[BORROWED_STR]])
  // CHECK:   end_borrow [[BORROWED_STR]] from [[STR]]
  // CHECK:   destroy_value [[STR]]
  // CHECK:   return [[NSSTR]]
  // CHECK: }
  func strArg(_ s: String) { }
  // CHECK-LABEL: sil hidden [thunk] @_T013objc_bridging3BasC6strArgySSFTo
  // CHECK: bb0([[NSSTR:%.*]] : $NSString, [[THIS:%.*]] : $Bas):
  // CHECK:   [[NSSTR_COPY:%.*]] = copy_value [[NSSTR]]
  // CHECK:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK:   [[NSSTRING_TO_STRING:%.*]] = function_ref @_T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
  // CHECK:   [[NSSTR_BOX:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[NSSTR_COPY]]
  // CHECK:   [[STR:%.*]] = apply [[NSSTRING_TO_STRING]]([[NSSTR_BOX]]
  // CHECK:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK:   [[METHOD:%.*]] = function_ref @_T013objc_bridging3BasC6strArgySSF
  // CHECK:   apply [[METHOD]]([[STR]], [[BORROWED_THIS_COPY]])
  // CHECK:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK:   destroy_value [[THIS_COPY]]
  // CHECK: } // end sil function '_T013objc_bridging3BasC6strArgySSFTo'

  // -- Bridging thunks for explicitly NSString properties don't convert
  func nsstrResult() -> NSString { return NSS }
  // CHECK-LABEL: sil hidden [thunk] @_T013objc_bridging3BasC11nsstrResultSo8NSStringCyFTo
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: _T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
  // CHECK: }
  func nsstrArg(_ s: NSString) { }
  // CHECK-LABEL: sil hidden @_T013objc_bridging3BasC8nsstrArgySo8NSStringCF
  // CHECK-NOT: swift_StringToNSString
  // CHECK-NOT: _T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
  // CHECK: }

  init(str: NSString) {
    nsstrRealProp = str
    super.init()
  }

  // CHECK-LABEL: sil hidden [thunk] @_T013objc_bridging3BasC8arrayArgySays9AnyObject_pGFTo : $@convention(objc_method) (NSArray, Bas) -> ()
  // CHECK: bb0([[NSARRAY:%[0-9]+]] : $NSArray, [[SELF:%[0-9]+]] : $Bas):
  // CHECK:   [[NSARRAY_COPY:%.*]] = copy_value [[NSARRAY]] : $NSArray
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Bas
  // CHECK:   [[CONV_FN:%[0-9]+]] = function_ref @_T0Sa10FoundationE36_unconditionallyBridgeFromObjectiveCSayxGSo7NSArrayCSgFZ
  // CHECK:   [[OPT_NSARRAY:%[0-9]+]] = enum $Optional<NSArray>, #Optional.some!enumelt.1, [[NSARRAY_COPY]] : $NSArray
  // CHECK:   [[ARRAY_META:%[0-9]+]] = metatype $@thin Array<AnyObject>.Type
  // CHECK:   [[ARRAY:%[0-9]+]] = apply [[CONV_FN]]<AnyObject>([[OPT_NSARRAY]], [[ARRAY_META]])
  // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @_T013objc_bridging3BasC8arrayArgySays9AnyObject_pGF : $@convention(method) (@owned Array<AnyObject>, @guaranteed Bas) -> ()
  // CHECK:   [[RESULT:%[0-9]+]] = apply [[SWIFT_FN]]([[ARRAY]], [[BORROWED_SELF_COPY]]) : $@convention(method) (@owned Array<AnyObject>, @guaranteed Bas) -> ()
  // CHECK:   end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK:   destroy_value [[SELF_COPY]] : $Bas
  // CHECK:   return [[RESULT]] : $()
  func arrayArg(_ array: [AnyObject]) { }
  
  // CHECK-LABEL: sil hidden [thunk] @_T013objc_bridging3BasC11arrayResultSays9AnyObject_pGyFTo : $@convention(objc_method) (Bas) -> @autoreleased NSArray
  // CHECK: bb0([[SELF:%[0-9]+]] : $Bas):
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Bas
  // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @_T013objc_bridging3BasC11arrayResultSays9AnyObject_pGyF : $@convention(method) (@guaranteed Bas) -> @owned Array<AnyObject>
  // CHECK:   [[ARRAY:%[0-9]+]] = apply [[SWIFT_FN]]([[BORROWED_SELF_COPY]]) : $@convention(method) (@guaranteed Bas) -> @owned Array<AnyObject>
  // CHECK:   end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK:   destroy_value [[SELF_COPY]]
  // CHECK:   [[CONV_FN:%[0-9]+]] = function_ref @_T0Sa10FoundationE19_bridgeToObjectiveCSo7NSArrayCyF
  // CHECK:   [[BORROWED_ARRAY:%.*]] = begin_borrow [[ARRAY]]
  // CHECK:   [[NSARRAY:%[0-9]+]] = apply [[CONV_FN]]<AnyObject>([[BORROWED_ARRAY]]) : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @owned NSArray
  // CHECK:   end_borrow [[BORROWED_ARRAY]] from [[ARRAY]]
  // CHECK:   destroy_value [[ARRAY]]
  // CHECK:   return [[NSARRAY]]
  func arrayResult() -> [AnyObject] { return [] }

  // CHECK-LABEL: sil hidden [thunk] @_T013objc_bridging3BasC9arrayPropSaySSGfgTo : $@convention(objc_method) (Bas) -> @autoreleased NSArray
  // CHECK-LABEL: sil hidden [thunk] @_T013objc_bridging3BasC9arrayPropSaySSGfsTo : $@convention(objc_method) (NSArray, Bas) -> ()
  var arrayProp: [String] = []
}

// CHECK-LABEL: sil hidden @_T013objc_bridging16applyStringBlockS3SXB_SS1xtF
func applyStringBlock(_ f: @convention(block) (String) -> String, x: String) -> String {
  // CHECK: bb0([[BLOCK:%.*]] : $@convention(block) (NSString) -> @autoreleased NSString, [[STRING:%.*]] : $String):
  // CHECK:   [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  // CHECK:   [[BORROWED_BLOCK_COPY:%.*]] = begin_borrow [[BLOCK_COPY]]
  // CHECK:   [[BLOCK_COPY_COPY:%.*]] = copy_value [[BORROWED_BLOCK_COPY]]
  // CHECK:   [[BORROWED_STRING:%.*]] = begin_borrow [[STRING]]
  // CHECK:   [[STRING_COPY:%.*]] = copy_value [[BORROWED_STRING]]
  // CHECK:   [[STRING_TO_NSSTRING:%.*]] = function_ref @_T0SS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
  // CHECK:   [[BORROWED_STRING_COPY:%.*]] = begin_borrow [[STRING_COPY]]
  // CHECK:   [[NSSTR:%.*]] = apply [[STRING_TO_NSSTRING]]([[BORROWED_STRING_COPY]]) : $@convention(method) (@guaranteed String)
  // CHECK:   end_borrow [[BORROWED_STRING_COPY]] from [[STRING_COPY]]
  // CHECK:   destroy_value [[STRING_COPY]]
  // CHECK:   [[RESULT_NSSTR:%.*]] = apply [[BLOCK_COPY_COPY]]([[NSSTR]]) : $@convention(block) (NSString) -> @autoreleased NSString
  // CHECK:   destroy_value [[NSSTR]]
  // CHECK:   [[FINAL_BRIDGE:%.*]] = function_ref @_T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
  // CHECK:   [[OPTIONAL_NSSTR:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[RESULT_NSSTR]]
  // CHECK:   [[RESULT:%.*]] = apply [[FINAL_BRIDGE]]([[OPTIONAL_NSSTR]], {{.*}}) : $@convention(method) (@owned Optional<NSString>, @thin String.Type) -> @owned String
  // CHECK:   destroy_value [[BLOCK_COPY_COPY]]
  // CHECK:   destroy_value [[STRING]]
  // CHECK:   destroy_value [[BLOCK_COPY]]
  // CHECK:   destroy_value [[BLOCK]]
  // CHECK:   return [[RESULT]] : $String
  return f(x)
}
// CHECK: } // end sil function '_T013objc_bridging16applyStringBlockS3SXB_SS1xtF'

// CHECK-LABEL: sil hidden @_T013objc_bridging15bridgeCFunction{{.*}}F
func bridgeCFunction() -> (String!) -> (String!) {
  // CHECK: [[THUNK:%.*]] = function_ref @_T0SC18NSStringFromStringSQySSGABFTO : $@convention(thin) (@owned Optional<String>) -> @owned Optional<String>
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

// CHECK-LABEL: sil shared @_T0So7NSArrayCABSQySPys9AnyObject_pSgGG7objects_s5Int32V5counttcfC
// CHECK:         [[SELF:%.*]] = alloc_ref_dynamic
// CHECK:         [[METHOD:%.*]] = function_ref @_T0So7NSArrayCABSQySPys9AnyObject_pSgGG7objects_s5Int32V5counttcfcTO
// CHECK:         [[RESULT:%.*]] = apply [[METHOD]]
// CHECK:         return [[RESULT]]

// Check that type lowering preserves the bool/BOOL distinction when bridging
// imported C functions.

// CHECK-ios-i386-LABEL: sil hidden @_T013objc_bridging5boolsSb_SbtSbF
// CHECK-ios-i386:         function_ref @useBOOL : $@convention(c) (ObjCBool) -> ()
// CHECK-ios-i386:         function_ref @useBool : $@convention(c) (Bool) -> ()
// CHECK-ios-i386:         function_ref @getBOOL : $@convention(c) () -> ObjCBool
// CHECK-ios-i386:         function_ref @getBool : $@convention(c) () -> Bool

// CHECK-macosx-x86_64-LABEL: sil hidden @_T013objc_bridging5boolsSb_SbtSbF
// CHECK-macosx-x86_64:         function_ref @useBOOL : $@convention(c) (ObjCBool) -> ()
// CHECK-macosx-x86_64:         function_ref @useBool : $@convention(c) (Bool) -> ()
// CHECK-macosx-x86_64:         function_ref @getBOOL : $@convention(c) () -> ObjCBool
// CHECK-macosx-x86_64:         function_ref @getBool : $@convention(c) () -> Bool

// FIXME: no distinction on x86_64, arm64 or watchos-i386, since SILGen looks
// at the underlying Clang decl of the bridged decl to decide whether it needs
// bridging.
//
// CHECK-watchos-i386-LABEL: sil hidden @_T013objc_bridging5boolsSb_SbtSbF
// CHECK-watchos-i386:         function_ref @useBOOL : $@convention(c) (Bool) -> ()
// CHECK-watchos-i386:         function_ref @useBool : $@convention(c) (Bool) -> ()
// CHECK-watchos-i386:         function_ref @getBOOL : $@convention(c) () -> Bool
// CHECK-watchos-i386:         function_ref @getBool : $@convention(c) () -> Bool

// CHECK-ios-x86_64-LABEL: sil hidden @_T013objc_bridging5boolsSb_SbtSbF
// CHECK-ios-x86_64:         function_ref @useBOOL : $@convention(c) (Bool) -> ()
// CHECK-ios-x86_64:         function_ref @useBool : $@convention(c) (Bool) -> ()
// CHECK-ios-x86_64:         function_ref @getBOOL : $@convention(c) () -> Bool
// CHECK-ios-x86_64:         function_ref @getBool : $@convention(c) () -> Bool

// CHECK-arm64-LABEL: sil hidden @_T013objc_bridging5boolsSb_SbtSbF
// CHECK-arm64:         function_ref @useBOOL : $@convention(c) (Bool) -> ()
// CHECK-arm64:         function_ref @useBool : $@convention(c) (Bool) -> ()
// CHECK-arm64:         function_ref @getBOOL : $@convention(c) () -> Bool
// CHECK-arm64:         function_ref @getBool : $@convention(c) () -> Bool

func bools(_ x: Bool) -> (Bool, Bool) {
  useBOOL(x)
  useBool(x)

  return (getBOOL(), getBool())
}

// CHECK-LABEL: sil hidden @_T013objc_bridging9getFridge{{.*}}F
// CHECK: bb0([[HOME:%[0-9]+]] : $APPHouse):
func getFridge(_ home: APPHouse) -> Refrigerator {
  // CHECK: [[BORROWED_HOME:%.*]] = begin_borrow [[HOME]]
  // CHECK: [[GETTER:%[0-9]+]] = class_method [volatile] [[BORROWED_HOME]] : $APPHouse, #APPHouse.fridge!getter.1.foreign
  // CHECK: [[OBJC_RESULT:%[0-9]+]] = apply [[GETTER]]([[BORROWED_HOME]])
  // CHECK: [[BRIDGE_FN:%[0-9]+]] = function_ref @_T010Appliances12RefrigeratorV36_unconditionallyBridgeFromObjectiveCACSo15APPRefrigeratorCSgFZ
  // CHECK: [[REFRIGERATOR_META:%[0-9]+]] = metatype $@thin Refrigerator.Type
  // CHECK: [[RESULT:%[0-9]+]] = apply [[BRIDGE_FN]]([[OBJC_RESULT]], [[REFRIGERATOR_META]])
  // CHECK: end_borrow [[BORROWED_HOME]] from [[HOME]]
  // CHECK: destroy_value [[HOME]] : $APPHouse
  // CHECK: return [[RESULT]] : $Refrigerator
  return home.fridge
}

// CHECK-LABEL: sil hidden @_T013objc_bridging16updateFridgeTemp{{.*}}F
// CHECK: bb0([[HOME:%[0-9]+]] : $APPHouse, [[DELTA:%[0-9]+]] : $Double):
func updateFridgeTemp(_ home: APPHouse, delta: Double) {
  // +=
  // CHECK: [[PLUS_EQ:%[0-9]+]] = function_ref @_T0s2peoiySdz_SdtF

  // Borrowed home
  // CHECK: [[BORROWED_HOME:%.*]] = begin_borrow [[HOME]]

  // Temporary fridge
  // CHECK: [[TEMP_FRIDGE:%[0-9]+]]  = alloc_stack $Refrigerator

  // Get operation
  // CHECK: [[GETTER:%[0-9]+]] = class_method [volatile] [[BORROWED_HOME]] : $APPHouse, #APPHouse.fridge!getter.1.foreign
  // CHECK: [[OBJC_FRIDGE:%[0-9]+]] = apply [[GETTER]]([[BORROWED_HOME]])
  // CHECK: [[BRIDGE_FROM_FN:%[0-9]+]] = function_ref @_T010Appliances12RefrigeratorV36_unconditionallyBridgeFromObjectiveCACSo15APPRefrigeratorCSgFZ
  // CHECK: [[REFRIGERATOR_META:%[0-9]+]] = metatype $@thin Refrigerator.Type
  // CHECK: [[FRIDGE:%[0-9]+]] = apply [[BRIDGE_FROM_FN]]([[OBJC_FRIDGE]], [[REFRIGERATOR_META]])

  // Addition
  // CHECK: [[TEMP:%[0-9]+]] = struct_element_addr [[TEMP_FRIDGE]] : $*Refrigerator, #Refrigerator.temperature
  // CHECK: apply [[PLUS_EQ]]([[TEMP]], [[DELTA]])

  // Setter
  // CHECK: [[FRIDGE:%[0-9]+]] = load [trivial] [[TEMP_FRIDGE]] : $*Refrigerator
  // CHECK: [[SETTER:%[0-9]+]] = class_method [volatile] [[BORROWED_HOME]] : $APPHouse, #APPHouse.fridge!setter.1.foreign
  // CHECK: [[BRIDGE_TO_FN:%[0-9]+]] = function_ref @_T010Appliances12RefrigeratorV19_bridgeToObjectiveCSo15APPRefrigeratorCyF
  // CHECK: [[OBJC_ARG:%[0-9]+]] = apply [[BRIDGE_TO_FN]]([[FRIDGE]])
  // CHECK: apply [[SETTER]]([[OBJC_ARG]], [[BORROWED_HOME]]) : $@convention(objc_method) (APPRefrigerator, APPHouse) -> ()
  // CHECK: destroy_value [[OBJC_ARG]]
  // CHECK: end_borrow [[BORROWED_HOME]] from [[HOME]]
  // CHECK: destroy_value [[HOME]]
  home.fridge.temperature += delta
}
