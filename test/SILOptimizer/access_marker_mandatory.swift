
// RUN: %target-swift-frontend -module-name access_marker_mandatory -enable-sil-ownership -parse-as-library -Xllvm -sil-full-demangle -emit-sil -Onone -enforce-exclusivity=checked %s | %FileCheck %s

public struct S {
  var i: Int
  var o: AnyObject
}

// CHECK-LABEL: sil [noinline] @$s23access_marker_mandatory5initSyAA1SVSi_yXltF : $@convention(thin) (Int, @guaranteed AnyObject) -> @owned S {
// CHECK: bb0(%0 : $Int, %1 : $AnyObject):
// CHECK: [[STK:%.*]] = alloc_stack $S, var, name "s"
// CHECK: cond_br %{{.*}}, bb1, bb2
// CHECK: bb1:
// CHECK: [[WRITE:%.*]] = begin_access [modify] [static] [[STK]] : $*S
// CHECK: store %{{.*}} to [[WRITE]] : $*S
// CHECK: end_access [[WRITE]]
// CHECK: bb2:
// CHECK: [[WRITE:%.*]] = begin_access [modify] [static] [[STK]] : $*S
// CHECK: store %{{.*}} to [[WRITE]] : $*S
// CHECK: end_access [[WRITE]]
// CHECK: bb3:
// CHECK: [[READ:%.*]] = begin_access [read] [static] [[STK]] : $*S
// CHECK: [[RET:%.*]] = load [[READ]] : $*S
// CHECK: end_access [[READ]]
// CHECK: destroy_addr [[STK]]
// CHECK: dealloc_stack [[STK]]
// CHECK: return [[RET]] : $S
// CHECK-LABEL: } // end sil function '$s23access_marker_mandatory5initSyAA1SVSi_yXltF'
@inline(never)
public func initS(_ x: Int, _ o: AnyObject) -> S {
  var s: S
  if x == 0 {
    s = S(i: 1, o: o)
  } else {
    s = S(i: x, o: o)
  }
  return s
}

@inline(never)
func takeS(_ s: S) {}

// CHECK-LABEL: sil @$s23access_marker_mandatory14modifyAndReadS1oyyXl_tF : $@convention(thin) (@guaranteed AnyObject) -> () {
// CHECK: bb0(%0 : $AnyObject):
// CHECK: [[STK:%.*]] = alloc_stack $S, var, name "s"
// CHECK: [[FINIT:%.*]] = function_ref @$s23access_marker_mandatory5initSyAA1SVSi_yXltF : $@convention(thin) (Int, @guaranteed AnyObject) -> @owned S
// CHECK: [[INITS:%.*]] = apply [[FINIT]](%{{.*}}, %0) : $@convention(thin) (Int, @guaranteed AnyObject) -> @owned S
// CHECK: store [[INITS]] to [[STK]] : $*S
// CHECK: [[WRITE:%.*]] = begin_access [modify] [static] [[STK]] : $*S
// CHECK: [[ADDRI:%.*]] = struct_element_addr [[WRITE]] : $*S, #S.i
// CHECK: store %{{.*}} to [[ADDRI]] : $*Int
// CHECK: end_access [[WRITE]]
// CHECK: [[READ:%.*]] = begin_access [read] [static] [[STK]] : $*S
// CHECK: end_access [[READ]]
// CHECK: [[FTAKE:%.*]] = function_ref @$s23access_marker_mandatory5takeSyyAA1SVF : $@convention(thin) (@guaranteed S) -> ()
// CHECK: apply [[FTAKE]](%{{.*}}) : $@convention(thin) (@guaranteed S) -> ()
// CHECK-LABEL: } // end sil function '$s23access_marker_mandatory14modifyAndReadS1oyyXl_tF'
public func modifyAndReadS(o: AnyObject) {
  var s = initS(3, o)
  s.i = 42
  takeS(s)
}

// Test capture promotion followed by stack promotion.
// Access enforcement selection must run after capture promotion
// so that we never stack promote something with dynamic access.
// Otherwise, we may try to convert the access to [deinit] which
// doesn't make sense dynamically.
//
// CHECK-LABEL: sil hidden @$s23access_marker_mandatory19captureStackPromoteSiycyF : $@convention(thin) () -> @owned @callee_guaranteed () -> Int {
// CHECK-LABEL: bb0:
// CHECK: [[STK:%.*]] = alloc_stack $Int, var, name "x"
// CHECK: [[WRITE:%.*]] = begin_access [modify] [static] [[STK]] : $*Int
// CHECK: store %{{.*}} to [[WRITE]] : $*Int
// CHECK: end_access [[WRITE]] : $*Int
// CHECK: [[F:%.*]] = function_ref @$s23access_marker_mandatory19captureStackPromoteSiycyFSiycfU_Tf2i_n : $@convention(thin) (Int) -> Int
// CHECK: [[C:%.*]] = partial_apply [callee_guaranteed] [[F]](%{{.*}}) : $@convention(thin) (Int) -> Int
// CHECK: dealloc_stack [[STK]] : $*Int
// CHECK: return [[C]] : $@callee_guaranteed () -> Int
// CHECK-LABEL: } // end sil function '$s23access_marker_mandatory19captureStackPromoteSiycyF'
func captureStackPromote() -> () -> Int {
  var x = 1
  x = 2
  let f = { x }
  return f
}
