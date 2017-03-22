// RUN: %target-swift-frontend %s -emit-sil -O -o - -verify | %FileCheck %s

// Make sure we do not specialize resilientCallee.

// CHECK-LABEL: sil [fragile] [always_inline] @_T026closure_specialize_fragile0C6CalleryyF : $@convention(thin) () -> ()
// CHECK: function_ref @_T026closure_specialize_fragile15resilientCalleeyyyc2fn_tF : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
// CHECK: return

@inline(__always) public func fragileCaller() {
  resilientCallee {
    print("Hi")
  }
}

// CHECK-LABEL: sil @_T026closure_specialize_fragile15resilientCalleeyyyc2fn_tF : $@convention(thin) (@owned @callee_owned () -> ()) -> ()

public func resilientCallee(fn: () -> ()) {
  fn()
}
