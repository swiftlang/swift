// RUN: %target-swift-frontend -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -emit-silgen -enable-swift3-objc-inference -swift-version 4 | %FileCheck -check-prefix CHECK-SWIFT4 %s

// RUN: %target-swift-frontend -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -emit-silgen -swift-version 3 | %FileCheck -check-prefix CHECK-SWIFT3 %s

// REQUIRES: objc_interop

import Foundation

class ObjCSubclass : NSObject {
  // CHECK-SWIFT4-LABEL: sil hidden [thunk] @_T0016objc_deprecated_A7_thunks12ObjCSubclassCACyt7nothing_tcfcTo : $@convention(objc_method) (@owned ObjCSubclass) -> @owned ObjCSubclass {
  // CHECK-SWIFT4: bb0(%0 : $ObjCSubclass):
  // CHECK-SWIFT4-NEXT: builtin "swift3ImplicitObjCEntrypoint"() : $()
  init(nothing: ()) { super.init() }
  
  // CHECK-SWIFT4-LABEL: sil hidden [thunk] @_T0016objc_deprecated_A7_thunks12ObjCSubclassC3fooyyFTo : $@convention(objc_method) (ObjCSubclass) -> ()
  // CHECK-SWIFT4: bb0(%0 : $ObjCSubclass):
  // CHECK-SWIFT4-NEXT: builtin "swift3ImplicitObjCEntrypoint"() : $()
  func foo() { }

  // CHECK-SWIFT4-LABEL: sil hidden [thunk] @_T0016objc_deprecated_A7_thunks12ObjCSubclassC3barSo8NSObjectCSgfgTo : $@convention(objc_method) (ObjCSubclass) -> @autoreleased Optional<NSObject>
  // CHECK-SWIFT4: bb0(%0 : $ObjCSubclass):
  // CHECK-SWIFT4-NEXT: builtin "swift3ImplicitObjCEntrypoint"() : $()

  // CHECK-SWIFT4-LABEL: sil hidden [thunk] @_T0016objc_deprecated_A7_thunks12ObjCSubclassC3barSo8NSObjectCSgfsTo : $@convention(objc_method) (Optional<NSObject>, ObjCSubclass) -> () {
  // CHECK-SWIFT4: %0 : $Optional<NSObject>, %1 : $ObjCSubclass
  // CHECK-SWIFT4-NEXT: builtin "swift3ImplicitObjCEntrypoint"() : $()
  var bar: NSObject? = nil

  // CHECK-SWIFT4-LABEL: sil hidden [thunk] @_T0016objc_deprecated_A7_thunks12ObjCSubclassC9subscriptyXlSicfgTo : $@convention(objc_method) (Int, ObjCSubclass) -> @autoreleased AnyObject 
  // CHECK-SWIFT4: bb0(%0 : $Int, %1 : $ObjCSubclass):
  // CHECK-SWIFT4-NEXT: builtin "swift3ImplicitObjCEntrypoint"() : $()

  // CHECK-SWIFT4-LABEL: sil hidden [thunk] @_T0016objc_deprecated_A7_thunks12ObjCSubclassC9subscriptyXlSicfsTo : $@convention(objc_method) (AnyObject, Int, ObjCSubclass) ->
  // CHECK-SWIFT4: bb0(%0 : $AnyObject, %1 : $Int, %2 : $ObjCSubclass):
  // CHECK-SWIFT4-NEXT: builtin "swift3ImplicitObjCEntrypoint"() : $()
  subscript (i: Int) -> AnyObject { get { return self } set { } } 
}

extension ObjCSubclass {
	// CHECK-SWIFT4-LABEL: sil hidden [thunk] @_T0016objc_deprecated_A7_thunks12ObjCSubclassC13falsePositiveyyFTo : $@convention(objc_method) (ObjCSubclass) -> ()
  // CHECK-SWIFT4: bb0(%0 : $ObjCSubclass):
  // CHECK-SWIFT4-NOT: builtin "swift3ImplicitObjCEntrypoint"() : $()
	// CHECK-SWIFT4: return
  func falsePositive() { }
}

// CHECK-SWIFT3-NOT: builtin "swift3ImplicitObjCEntrypoint"() : $()
