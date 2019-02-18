// RUN: %target-swift-frontend %s -whole-module-optimization -emit-sil | %FileCheck %s

public class C {
  public func f() {}
}

//CHECK-LABEL: sil [serialized] @$s32devirtualize_inlinable_mandatory1gyyAA1CCF : $@convention(thin) (@guaranteed C) -> () {
//CHECK: class_method %0 : $C, #C.f!1 : (C) -> () -> (), $@convention(method) (@guaranteed C) -> ()
//CHECK-NOT: function_ref
//CHECK: return 
@inlinable public func g(_ x: C) {
  x.f()
}
