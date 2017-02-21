// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs/ -I %S/Inputs -enable-source-import %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import objc_extensions_helper

class Sub : Base {}

extension Sub {
  override var prop: String! {
    didSet {
      // Ignore it.
    }

    // Make sure that we are generating the @objc thunk and are calling the actual method.
    //
    // CHECK-LABEL: sil hidden [thunk] @_TToFC15objc_extensions3Subg4propGSQSS_ : $@convention(objc_method) (Sub) -> @autoreleased Optional<NSString> {
    // CHECK: bb0([[SELF:%.*]] : $Sub):
    // CHECK: [[SELF_COPY:%.*]] = copy_value [[SELF]]
    // CHECK: [[GETTER_FUNC:%.*]] = function_ref @_TFC15objc_extensions3Subg4propGSQSS_ : $@convention(method) (@guaranteed Sub) -> @owned Optional<String>
    // CHECK: apply [[GETTER_FUNC]]([[SELF_COPY]])
    // CHECK: destroy_value [[SELF_COPY]]
    // CHECK: } // end sil function '_TToFC15objc_extensions3Subg4propGSQSS_'

    // Then check the body of the getter calls the super_method.
    // CHECK-LABEL: sil hidden @_TFC15objc_extensions3Subg4propGSQSS_ : $@convention(method) (@guaranteed Sub) -> @owned Optional<String> {
    // CHECK: bb0([[SELF:%.*]] : $Sub):
    // CHECK: [[SELF_COPY:%.*]] = copy_value [[SELF]]
    // CHECK: [[SELF_COPY_CAST:%.*]] = upcast [[SELF_COPY]] : $Sub to $Base
    // CHECK: [[SUPER_METHOD:%.*]] = super_method [volatile] [[SELF_COPY]] : $Sub, #Base.prop!getter.1.foreign
    // CHECK: [[RESULT:%.*]] = apply [[SUPER_METHOD]]([[SELF_COPY_CAST]])
    // CHECK: bb3(
    // CHECK: destroy_value [[SELF_COPY]]
    // CHECK: } // end sil function '_TFC15objc_extensions3Subg4propGSQSS_'

    // Then check the setter @objc thunk.
    //
    // TODO: This codegens using a select_enum + cond_br. It would be better to
    // just use a switch_enum so we can consume the value. This change will be
    // necessary in a semantic ARC world.
    //
    // CHECK-LABEL: sil hidden [thunk] @_TToFC15objc_extensions3Subs4propGSQSS_ : $@convention(objc_method) (Optional<NSString>, Sub) -> () {
    // CHECK: bb0([[NEW_VALUE:%.*]] : $Optional<NSString>, [[SELF:%.*]] : $Sub):
    // CHECK: [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Sub
    // CHECK: bb1:
    // CHECK: bb3([[BRIDGED_NEW_VALUE:%.*]] : $Optional<String>):
    // CHECK:   [[NORMAL_FUNC:%.*]] = function_ref @_TFC15objc_extensions3Subs4propGSQSS_ : $@convention(method) (@owned Optional<String>, @guaranteed Sub) -> ()
    // CHECK:   apply [[NORMAL_FUNC]]([[BRIDGED_NEW_VALUE]], [[SELF_COPY]])
    // CHECK:   destroy_value [[SELF_COPY]]
    // CHECK: } // end sil function '_TToFC15objc_extensions3Subs4propGSQSS_'

    // Then check the body of the actually setter value and make sure that we
    // call the didSet function.
    // CHECK-LABEL: sil hidden @_TFC15objc_extensions3Subs4propGSQSS_ : $@convention(method) (@owned Optional<String>, @guaranteed Sub) -> () {

    // First we get the old value.
    // CHECK: bb0([[NEW_VALUE:%.*]] : $Optional<String>, [[SELF:%.*]] : $Sub):
    // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
    // CHECK:   [[UPCAST_SELF_COPY:%.*]] = upcast [[SELF_COPY]] : $Sub to $Base
    // CHECK:   [[GET_SUPER_METHOD:%.*]] = super_method [volatile] [[SELF_COPY]] : $Sub, #Base.prop!getter.1.foreign : (Base) -> () -> String! , $@convention(objc_method) (Base) -> @autoreleased Optional<NSString>
    // CHECK:   [[OLD_NSSTRING:%.*]] = apply [[GET_SUPER_METHOD]]([[UPCAST_SELF_COPY]])

    // CHECK: bb3([[OLD_NSSTRING_BRIDGED:%.*]] : $Optional<String>):
    // This next line is completely not needed. But we are emitting it now.
    // CHECK:   destroy_value [[SELF_COPY]]
    // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
    // CHECK:   [[UPCAST_SELF_COPY:%.*]] = upcast [[SELF_COPY]] : $Sub to $Base
    // CHECK:   [[SET_SUPER_METHOD:%.*]] = super_method [volatile] [[SELF_COPY]] : $Sub, #Base.prop!setter.1.foreign : (Base) -> (String!) -> () , $@convention(objc_method) (Optional<NSString>, Base) -> ()
    // CHECK: bb4:
    // CHECK: bb6([[BRIDGED_NEW_STRING:%.*]] : $Optional<NSString>):
    // CHECK:    apply [[SET_SUPER_METHOD]]([[BRIDGED_NEW_STRING]], [[UPCAST_SELF_COPY]])
    // CHECK:    destroy_value [[BRIDGED_NEW_STRING]]
    // CHECK:    destroy_value [[SELF_COPY]]
    // CHECK:    [[DIDSET_NOTIFIER:%.*]] = function_ref @_TFC15objc_extensions3SubW4propGSQSS_ : $@convention(method) (@owned Optional<String>, @guaranteed Sub) -> ()
    // CHECK:    [[COPIED_OLD_NSSTRING_BRIDGED:%.*]] = copy_value [[OLD_NSSTRING_BRIDGED]]
    // This is an identity cast that should be eliminated by SILGen peepholes.
    // CHECK:    apply [[DIDSET_NOTIFIER]]([[COPIED_OLD_NSSTRING_BRIDGED]], [[SELF]])
    // CHECK:    destroy_value [[OLD_NSSTRING_BRIDGED]]
    // CHECK:    destroy_value [[NEW_VALUE]]
    // CHECK: } // end sil function '_TFC15objc_extensions3Subs4propGSQSS_'

  }

  func foo() {
  }

  override func objCBaseMethod() {}
}

// CHECK-LABEL: sil hidden @_TF15objc_extensions20testOverridePropertyFCS_3SubT_
func testOverrideProperty(_ obj: Sub) {
  // CHECK: = class_method [volatile] %0 : $Sub, #Sub.prop!setter.1.foreign : (Sub) -> (String!) -> ()
  obj.prop = "abc"
} // CHECK: }

testOverrideProperty(Sub())

// CHECK-LABEL: sil shared [thunk] @_TFC15objc_extensions3Sub3fooFT_T_
// CHECK:         function_ref @_TTDFC15objc_extensions3Sub3foofT_T_
// CHECK: } // end sil function '_TFC15objc_extensions3Sub3fooFT_T_'
// CHECK:       sil shared [transparent] [thunk] @_TTDFC15objc_extensions3Sub3foofT_T_
// CHECK:       bb0([[SELF:%.*]] : $Sub):
// CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK:         class_method [volatile] [[SELF_COPY]] : $Sub, #Sub.foo!1.foreign
// CHECK: } // end sil function '_TTDFC15objc_extensions3Sub3foofT_T_'
func testCurry(_ x: Sub) {
  _ = x.foo
}

extension Sub {
  var otherProp: String {
    get { return "hello" }
    set { }
  }
}

class SubSub : Sub {
  // CHECK-LABEL: sil hidden @_TFC15objc_extensions6SubSub14objCBaseMethodfT_T_
  // CHECK: bb0([[SELF:%.*]] : $SubSub):
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:   super_method [volatile] [[SELF_COPY]] : $SubSub, #Sub.objCBaseMethod!1.foreign : (Sub) -> () -> () , $@convention(objc_method) (Sub) -> ()
  // CHECK: } // end sil function '_TFC15objc_extensions6SubSub14objCBaseMethodfT_T_'
  override func objCBaseMethod() {
    super.objCBaseMethod()
  }
}

extension SubSub {
  // CHECK-LABEL: sil hidden @_TFC15objc_extensions6SubSubs9otherPropSS
  // CHECK: bb0([[NEW_VALUE:%.*]] : $String, [[SELF:%.*]] : $SubSub):
  // CHECK:   [[SELF_COPY_1:%.*]] = copy_value [[SELF]]
  // CHECK:   = super_method [volatile] [[SELF_COPY_1]] : $SubSub, #Sub.otherProp!getter.1.foreign
  // CHECK:   [[SELF_COPY_2:%.*]] = copy_value [[SELF]]
  // CHECK:   = super_method [volatile] [[SELF_COPY_2]] : $SubSub, #Sub.otherProp!setter.1.foreign
  // CHECK: } // end sil function '_TFC15objc_extensions6SubSubs9otherPropSS'
  override var otherProp: String {
    didSet {
      // Ignore it.
    }
  }
}

// SR-1025
extension Base {
  fileprivate static var x = 1
}

// CHECK-LABEL: sil hidden @_TF15objc_extensions19testStaticVarAccessFT_T_
func testStaticVarAccess() {
  // CHECK: [[F:%.*]] = function_ref @_TFE15objc_extensionsCSo4BaseauP33_1F05E59585E0BB585FCA206FBFF1A92D1xSi
  // CHECK: [[PTR:%.*]] = apply [[F]]()
  // CHECK: [[ADDR:%.*]] = pointer_to_address [[PTR]]
  _ = Base.x
}
