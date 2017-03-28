// RUN: %target-swift-frontend -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -emit-silgen -enable-swift3-objc-inference | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

class ObjCSubclass : NSObject {
  // CHECK-LABEL: sil hidden [thunk] @_T0016objc_deprecated_A7_thunks12ObjCSubclassCACyt7nothing_tcfcTo : $@convention(objc_method) (@owned ObjCSubclass) -> @owned ObjCSubclass {
  // CHECK: bb0(%0 : $ObjCSubclass):
  // CHECK-NEXT: builtin "swift3ImplicitObjCEntrypoint"() : $()
  init(nothing: ()) { super.init() }
  
  // CHECK-LABEL: sil hidden [thunk] @_T0016objc_deprecated_A7_thunks12ObjCSubclassC3fooyyFTo : $@convention(objc_method) (ObjCSubclass) -> ()
  // CHECK: bb0(%0 : $ObjCSubclass):
  // CHECK-NEXT: builtin "swift3ImplicitObjCEntrypoint"() : $()
  func foo() { }

  // CHECK-LABEL: sil hidden [thunk] @_T0016objc_deprecated_A7_thunks12ObjCSubclassC3barSo8NSObjectCSgfgTo : $@convention(objc_method) (ObjCSubclass) -> @autoreleased Optional<NSObject>
  // CHECK: bb0(%0 : $ObjCSubclass):
  // CHECK-NEXT: builtin "swift3ImplicitObjCEntrypoint"() : $()

  // CHECK-LABEL: sil hidden [thunk] @_T0016objc_deprecated_A7_thunks12ObjCSubclassC3barSo8NSObjectCSgfsTo : $@convention(objc_method) (Optional<NSObject>, ObjCSubclass) -> () {
  // CHECK: %0 : $Optional<NSObject>, %1 : $ObjCSubclass
  // CHECK-NEXT: builtin "swift3ImplicitObjCEntrypoint"() : $()
  var bar: NSObject? = nil

  // CHECK-LABEL: sil hidden [thunk] @_T0016objc_deprecated_A7_thunks12ObjCSubclassC9subscripts9AnyObject_pSicfgTo : $@convention(objc_method) (Int, ObjCSubclass) -> @autoreleased AnyObject 
  // CHECK: bb0(%0 : $Int, %1 : $ObjCSubclass):
  // CHECK-NEXT: builtin "swift3ImplicitObjCEntrypoint"() : $()

  // CHECK-LABEL: sil hidden [thunk] @_T0016objc_deprecated_A7_thunks12ObjCSubclassC9subscripts9AnyObject_pSicfsTo : $@convention(objc_method) (AnyObject, Int, ObjCSubclass) ->
  // CHECK: bb0(%0 : $AnyObject, %1 : $Int, %2 : $ObjCSubclass):
  // CHECK-NEXT: builtin "swift3ImplicitObjCEntrypoint"() : $()
  subscript (i: Int) -> AnyObject { get { return self } set { } } 
}
