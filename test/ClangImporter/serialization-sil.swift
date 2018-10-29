
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/Test.swiftmodule -emit-sil -o /dev/null -module-name Test %s -sdk "" -import-objc-header %S/Inputs/serialization-sil.h -enable-sil-ownership
// RUN: %target-sil-func-extractor %t/Test.swiftmodule -sil-print-debuginfo  -func='$s4Test16testPartialApplyyySoAA_pF' -o - | %FileCheck %s

// REQUIRES: objc_interop

// @_transparent to force serialization.
@_transparent
public func testPartialApply(_ obj: Test) {
  // CHECK-LABEL: @$s4Test16testPartialApplyyySoAA_pF : $@convention(thin) (@guaranteed Test) -> () {
  if let curried1 = obj.normalObject {
    // CHECK: dynamic_method_br [[CURRIED1_OBJ:%.+]] : $@opened([[CURRIED1_EXISTENTIAL:.+]]) Test, #Test.normalObject!1.foreign, [[CURRIED1_TRUE:[^,]+]], [[CURRIED1_FALSE:[^,]+]]
    // CHECK: [[CURRIED1_FALSE]]:
    // CHECK: [[CURRIED1_TRUE]]([[CURRIED1_METHOD:%.+]] : $@convention(objc_method) (@opened([[CURRIED1_EXISTENTIAL]]) Test) -> @autoreleased AnyObject):
    // CHECK: [[CURRIED1_PARTIAL:%.+]] = partial_apply [callee_guaranteed] [[CURRIED1_METHOD]]([[CURRIED1_OBJ]]) : $@convention(objc_method) (@opened([[CURRIED1_EXISTENTIAL]]) Test) -> @autoreleased AnyObject
    // CHECK: [[CURRIED1_THUNK:%.+]] = function_ref @$syXlIego_ypIegr_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> @owned AnyObject) -> @out Any
    // CHECK: = partial_apply [callee_guaranteed] [[CURRIED1_THUNK]]([[CURRIED1_PARTIAL]])
    curried1()
  }
  if let curried2 = obj.innerPointer {
    // CHECK: dynamic_method_br [[CURRIED2_OBJ:%.+]] : $@opened([[CURRIED2_EXISTENTIAL:.+]]) Test, #Test.innerPointer!1.foreign, [[CURRIED2_TRUE:[^,]+]], [[CURRIED2_FALSE:[^,]+]]
    // CHECK: [[CURRIED2_FALSE]]:
    // CHECK: [[CURRIED2_TRUE]]([[CURRIED2_METHOD:%.+]] : $@convention(objc_method) (@opened([[CURRIED2_EXISTENTIAL]]) Test) -> @unowned_inner_pointer UnsafeMutableRawPointer):
    // CHECK: [[CURRIED2_PARTIAL:%.+]] = partial_apply [callee_guaranteed] [[CURRIED2_METHOD]]([[CURRIED2_OBJ]]) : $@convention(objc_method) (@opened([[CURRIED2_EXISTENTIAL]]) Test) -> @unowned_inner_pointer UnsafeMutableRawPointer
    curried2()
  }
  if let prop1 = obj.normalObjectProp {
    // CHECK: dynamic_method_br [[PROP1_OBJ:%.+]] : $@opened([[PROP1_EXISTENTIAL:.+]]) Test, #Test.normalObjectProp!getter.1.foreign, [[PROP1_TRUE:[^,]+]], [[PROP1_FALSE:[^,]+]]
    // CHECK: [[PROP1_FALSE]]:
    // CHECK: [[PROP1_TRUE]]([[PROP1_METHOD:%.+]] : $@convention(objc_method) (@opened([[PROP1_EXISTENTIAL]]) Test) -> @autoreleased AnyObject):
    // CHECK: [[PROP1_PARTIAL:%.+]] = partial_apply [callee_guaranteed] [[PROP1_METHOD]]([[PROP1_OBJ]]) : $@convention(objc_method) (@opened([[PROP1_EXISTENTIAL]]) Test) -> @autoreleased AnyObject
    // CHECK: = apply [[PROP1_PARTIAL]]() : $@callee_guaranteed () -> @owned AnyObject
    _ = prop1
  }
  if let prop2 = obj.innerPointerProp {
    // CHECK: dynamic_method_br [[PROP2_OBJ:%.+]] : $@opened([[PROP2_EXISTENTIAL:.+]]) Test, #Test.innerPointerProp!getter.1.foreign, [[PROP2_TRUE:[^,]+]], [[PROP2_FALSE:[^,]+]]
    // CHECK: [[PROP2_FALSE]]:
    // CHECK: [[PROP2_TRUE]]([[PROP2_METHOD:%.+]] : $@convention(objc_method) (@opened([[PROP2_EXISTENTIAL]]) Test) -> @unowned_inner_pointer UnsafeMutableRawPointer):
    // CHECK: [[PROP2_PARTIAL:%.+]] = partial_apply [callee_guaranteed] [[PROP2_METHOD]]([[PROP2_OBJ]]) : $@convention(objc_method) (@opened([[PROP2_EXISTENTIAL]]) Test) -> @unowned_inner_pointer UnsafeMutableRawPointer
    // CHECK: = apply [[PROP2_PARTIAL]]() : $@callee_guaranteed () -> UnsafeMutableRawPointer
    _ = prop2
  }
} // CHECK: // end sil function '$s4Test16testPartialApplyyySoAA_pF'
