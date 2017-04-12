// RUN: %target-swift-frontend -parse-as-library -Xllvm -sil-full-demangle -enforce-exclusivity=checked -emit-sil %s | %FileCheck %s

public struct S {
  var i: Int
  var o: AnyObject
}

// CHECK-LABEL: sil [noinline] @_T023access_marker_mandatory5initSAA1SVSi_s9AnyObject_ptF : $@convention(thin) (Int, @owned AnyObject) -> @owned S {
// CHECK: bb0(%0 : $Int, %1 : $AnyObject):
// CHECK: %[[STK:.*]] = alloc_stack $S, var, name "s"
// CHECK: cond_br %{{.*}}, bb1, bb2
// CHECK: bb1:
// CHECK-NOT: begin_access
// CHECK: store %{{.*}} to %[[STK]] : $*S
// CHECK-NOT: end_access
// CHECK: bb2:
// CHECK-NOT: begin_access
// CHECK: store %{{.*}} to %[[STK]] : $*S
// CHECK-NOT: end_access
// CHECK: bb3:
// CHECK-NOT: begin_access
// CHECK: %[[RET:.*]] = load %[[STK]] : $*S
// CHECK-NOT: end_access
// CHECK: destroy_addr %[[STK]]
// CHECK: dealloc_stack %[[STK]]
// CHECK: return %[[RET]] : $S
// CHECK-LABEL: } // end sil function '_T023access_marker_mandatory5initSAA1SVSi_s9AnyObject_ptF'
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

// CHECK-LABEL: sil @_T023access_marker_mandatory14modifyAndReadSys9AnyObject_p1o_tF : $@convention(thin) (@owned AnyObject) -> () {
// CHECK: bb0(%0 : $AnyObject):
// CHECK: %[[STK:.*]] = alloc_stack $S, var, name "s"
// CHECK: %[[FINIT:.*]] = function_ref @_T023access_marker_mandatory5initSAA1SVSi_s9AnyObject_ptF : $@convention(thin) (Int, @owned AnyObject) -> @owned S
// CHECK: %[[INITS:.*]] = apply %[[FINIT]](%{{.*}}, %0) : $@convention(thin) (Int, @owned AnyObject) -> @owned S
// CHECK-NOT: begin_access
// CHECK: store %[[INITS]] to %[[STK]] : $*S
// CHECK-NOT: end_access
// CHECK-NOT: begin_access
// CHECK: %[[ADDRI:.*]] = struct_element_addr %[[STK]] : $*S, #S.i
// CHECK: store %{{.*}} to %[[ADDRI]] : $*Int
// CHECK-NOT: end_access
// CHECK-NOT: begin_access
// CHECK: %[[FTAKE:.*]] = function_ref @_T023access_marker_mandatory5takeSyAA1SVF : $@convention(thin) (@owned S) -> ()
// CHECK: apply %[[FTAKE]](%{{.*}}) : $@convention(thin) (@owned S) -> ()
// CHECK-LABEL: } // end sil function '_T023access_marker_mandatory14modifyAndReadSys9AnyObject_p1o_tF'
public func modifyAndReadS(o: AnyObject) {
  var s = initS(3, o)
  s.i = 42
  takeS(s)
}
