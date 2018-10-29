// RUN: %target-swift-frontend -Xllvm -sil-full-demangle %s \
// RUN:   -emit-sil -emit-verbose-sil -Xllvm -sil-print-debuginfo -g -o - \
// RUN:   | %FileCheck %s
public func g<T>(_ t: T) {}
public func f(_ i: Int32) {
  // CHECK: function_ref @$s4main1fyys5Int32VFyycfU_
  // CHECK-SAME: loc "{{.*}}":13:3,
  // CHECK: %[[CLOSURE:.*]] = partial_apply
  // CHECK-SAME: loc "{{.*}}":13:3,{{.*}}auto_gen
  // CHECK: store %[[CLOSURE]]
  // CHECK-SAME: loc "{{.*}}":12:3,  
  var closure = // line 12
  {
    g(i)
  }
  return closure() 
}
