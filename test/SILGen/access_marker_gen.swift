// RUN: %target-swift-frontend -parse-as-library -Xllvm -sil-full-demangle -enforce-exclusivity=checked -emit-silgen %s | %FileCheck %s

public struct S {
  var i: Int
  var o: AnyObject?
}

// CHECK-LABEL: sil hidden [noinline] @_T017access_marker_gen5initSAA1SVs9AnyObject_pSgF : $@convention(thin) (@owned Optional<AnyObject>) -> @owned S {
// CHECK: bb0(%0 : $Optional<AnyObject>):
// CHECK: %[[BOX:.*]] = alloc_box ${ var S }, var, name "s"
// CHECK: %[[ADDRS:.*]] = project_box %[[BOX]] : ${ var S }, 0
// CHECK: %[[UNINIT:.*]] = mark_uninitialized [var] %[[ADDRS]] : $*S
// CHECK: cond_br %{{.*}}, bb1, bb2
// CHECK: bb1:
// CHECK: %[[ACCESS1:.*]] = begin_access [modify] [unknown] %[[UNINIT]] : $*S
// CHECK: assign %{{.*}} to %[[ACCESS1]] : $*S
// CHECK: end_access %[[ACCESS1]] : $*S
// CHECK: bb2:
// CHECK: %[[ACCESS2:.*]] = begin_access [modify] [unknown] %[[UNINIT]] : $*S
// CHECK: assign %{{.*}} to %[[ACCESS2]] : $*S
// CHECK: end_access %[[ACCESS2]] : $*S
// CHECK: bb3:
// CHECK: %[[ACCESS3:.*]] = begin_access [read] [unknown] %[[UNINIT]] : $*S
// CHECK: %[[RET:.*]] = load [copy] %[[ACCESS3]] : $*S
// CHECK: end_access %[[ACCESS3]] : $*S
// CHECK: return %[[RET]] : $S
// CHECK-LABEL: } // end sil function '_T017access_marker_gen5initSAA1SVs9AnyObject_pSgF'
@inline(never)
func initS(_ o: AnyObject?) -> S {
  var s: S
  if o == nil {
    s = S(i: 0, o: nil)
  } else {
    s = S(i: 1, o: o)
  }
  return s
}

@inline(never)
func takeS(_ s: S) {}

// CHECK-LABEL: sil @_T017access_marker_gen14modifyAndReadSyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK: %[[BOX:.*]] = alloc_box ${ var S }, var, name "s"
// CHECK: %[[ADDRS:.*]] = project_box %[[BOX]] : ${ var S }, 0
// CHECK: %[[ACCESS1:.*]] = begin_access [modify] [unknown] %[[ADDRS]] : $*S
// CHECK: %[[ADDRI:.*]] = struct_element_addr %15 : $*S, #S.i
// CHECK: assign %{{.*}} to %[[ADDRI]] : $*Int
// CHECK: end_access %[[ACCESS1]] : $*S
// CHECK: %[[ACCESS2:.*]] = begin_access [read] [unknown] %[[ADDRS]] : $*S
// CHECK: %{{.*}} = load [copy] %[[ACCESS2]] : $*S
// CHECK: end_access %[[ACCESS2]] : $*S
// CHECK-LABEL: } // end sil function '_T017access_marker_gen14modifyAndReadSyyF'
public func modifyAndReadS() {
  var s = initS(nil)
  s.i = 42
  takeS(s)
}

var global = S(i: 0, o: nil)

func readGlobal() -> AnyObject? {
  return global.o
}

// CHECK-LABEL: sil hidden @_T017access_marker_gen10readGlobals9AnyObject_pSgyF
// CHECK:         [[ADDRESSOR:%.*]] = function_ref @_T017access_marker_gen6globalAA1SVfau :
// CHECK-NEXT:    [[T0:%.*]] = apply [[ADDRESSOR]]()
// CHECK-NEXT:    [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*S
// CHECK-NEXT:    [[T2:%.*]] = begin_access [read] [dynamic] [[T1]]
// CHECK-NEXT:    [[T3:%.*]] = struct_element_addr [[T2]] : $*S, #S.o
// CHECK-NEXT:    [[T4:%.*]] = load [copy] [[T3]]
// CHECK-NEXT:    end_access [[T2]]
// CHECK-NEXT:    return [[T4]]
