// RUN: %target-swift-frontend -parse-as-library -Xllvm -sil-full-demangle -emit-sil -enforce-exclusivity=checked %s | %FileCheck %s

public struct S {
  var i: Int
  var o: AnyObject
}

// CHECK-LABEL: sil [noinline] @_T023access_marker_mandatory5initSAA1SVSi_s9AnyObject_ptF : $@convention(thin) (Int, @owned AnyObject) -> @owned S {
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
// CHECK: [[STK:%.*]] = alloc_stack $S, var, name "s"
// CHECK: [[FINIT:%.*]] = function_ref @_T023access_marker_mandatory5initSAA1SVSi_s9AnyObject_ptF : $@convention(thin) (Int, @owned AnyObject) -> @owned S
// CHECK: [[INITS:%.*]] = apply [[FINIT]](%{{.*}}, %0) : $@convention(thin) (Int, @owned AnyObject) -> @owned S
// CHECK: store [[INITS]] to [[STK]] : $*S
// CHECK: [[WRITE:%.*]] = begin_access [modify] [static] [[STK]] : $*S
// CHECK: [[ADDRI:%.*]] = struct_element_addr [[WRITE]] : $*S, #S.i
// CHECK: store %{{.*}} to [[ADDRI]] : $*Int
// CHECK: end_access [[WRITE]]
// CHECK: [[FTAKE:%.*]] = function_ref @_T023access_marker_mandatory5takeSyAA1SVF : $@convention(thin) (@owned S) -> ()
// CHECK: [[READ:%.*]] = begin_access [read] [static] [[STK]] : $*S
// CHECK: end_access [[READ]]
// CHECK: apply [[FTAKE]](%{{.*}}) : $@convention(thin) (@owned S) -> ()
// CHECK-LABEL: } // end sil function '_T023access_marker_mandatory14modifyAndReadSys9AnyObject_p1o_tF'
public func modifyAndReadS(o: AnyObject) {
  var s = initS(3, o)
  s.i = 42
  takeS(s)
}
