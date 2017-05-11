// RUN: %target-swift-frontend -parse-as-library -Xllvm -sil-full-demangle -enforce-exclusivity=checked -emit-silgen %s | %FileCheck %s

func modify<T>(_ x: inout T) {}

public struct S {
  var i: Int
  var o: AnyObject?
}

// CHECK-LABEL: sil hidden [noinline] @_T017access_marker_gen5initSAA1SVyXlSgF : $@convention(thin) (@owned Optional<AnyObject>) -> @owned S {
// CHECK: bb0(%0 : $Optional<AnyObject>):
// CHECK: [[BOX:%.*]] = alloc_box ${ var S }, var, name "s"
// CHECK: [[MARKED_BOX:%.*]] = mark_uninitialized [var] [[BOX]] : ${ var S }
// CHECK: [[ADDR:%.*]] = project_box [[MARKED_BOX]] : ${ var S }, 0
// CHECK: cond_br %{{.*}}, bb1, bb2
// CHECK: bb1:
// CHECK: [[ACCESS1:%.*]] = begin_access [modify] [unknown] [[ADDR]] : $*S
// CHECK: assign %{{.*}} to [[ACCESS1]] : $*S
// CHECK: end_access [[ACCESS1]] : $*S
// CHECK: bb2:
// CHECK: [[ACCESS2:%.*]] = begin_access [modify] [unknown] [[ADDR]] : $*S
// CHECK: assign %{{.*}} to [[ACCESS2]] : $*S
// CHECK: end_access [[ACCESS2]] : $*S
// CHECK: bb3:
// CHECK: [[ACCESS3:%.*]] = begin_access [read] [unknown] [[ADDR]] : $*S
// CHECK: [[RET:%.*]] = load [copy] [[ACCESS3]] : $*S
// CHECK: end_access [[ACCESS3]] : $*S
// CHECK: return [[RET]] : $S
// CHECK-LABEL: } // end sil function '_T017access_marker_gen5initSAA1SVyXlSgF'
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

// CHECK-LABEL: sil hidden @_T017access_marker_gen10readGlobalyXlSgyF
// CHECK:         [[ADDRESSOR:%.*]] = function_ref @_T017access_marker_gen6globalAA1SVfau :
// CHECK-NEXT:    [[T0:%.*]] = apply [[ADDRESSOR]]()
// CHECK-NEXT:    [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*S
// CHECK-NEXT:    [[T2:%.*]] = begin_access [read] [dynamic] [[T1]]
// CHECK-NEXT:    [[T3:%.*]] = struct_element_addr [[T2]] : $*S, #S.o
// CHECK-NEXT:    [[T4:%.*]] = load [copy] [[T3]]
// CHECK-NEXT:    end_access [[T2]]
// CHECK-NEXT:    return [[T4]]


public struct HasTwoStoredProperties {
  var f: Int = 7
  var g: Int = 9

// CHECK-LABEL: sil hidden @_T017access_marker_gen22HasTwoStoredPropertiesV027noOverlapOnAssignFromPropToM0yyF : $@convention(method) (@inout HasTwoStoredProperties) -> ()
// CHECK:       [[ACCESS1:%.*]] = begin_access [read] [unknown] [[SELF_ADDR:%.*]] : $*HasTwoStoredProperties
// CHECK-NEXT:  [[G_ADDR:%.*]] = struct_element_addr [[ACCESS1]] : $*HasTwoStoredProperties, #HasTwoStoredProperties.g
// CHECK-NEXT:  [[G_VAL:%.*]] = load [trivial] [[G_ADDR]] : $*Int
// CHECK-NEXT:  end_access [[ACCESS1]] : $*HasTwoStoredProperties
// CHECK-NEXT:  [[ACCESS2:%.*]] = begin_access [modify] [unknown] [[SELF_ADDR]] : $*HasTwoStoredProperties
// CHECK-NEXT:  [[F_ADDR:%.*]] = struct_element_addr [[ACCESS2]] : $*HasTwoStoredProperties, #HasTwoStoredProperties.f
// CHECK-NEXT:  assign [[G_VAL]] to [[F_ADDR]] : $*Int
// CHECK-NEXT:  end_access [[ACCESS2]] : $*HasTwoStoredProperties
  mutating func noOverlapOnAssignFromPropToProp() {
    f = g
  }
}

class C {
  final var x: Int = 0
}

func testClassInstanceProperties(c: C) {
  let y = c.x
  c.x = y
}
// CHECK-LABEL: sil hidden @_T017access_marker_gen27testClassInstancePropertiesyAA1CC1c_tF :
// CHECK:       [[C:%.*]] = begin_borrow %0 : $C
// CHECK-NEXT:  [[CX:%.*]] = ref_element_addr [[C]] : $C, #C.x
// CHECK-NEXT:  [[ACCESS:%.*]] = begin_access [read] [dynamic] [[CX]] : $*Int
// CHECK-NEXT:  [[Y:%.*]] = load [trivial] [[ACCESS]]
// CHECK-NEXT:  end_access [[ACCESS]]
// CHECK:       [[C:%.*]] = begin_borrow %0 : $C
// CHECK-NEXT:  [[CX:%.*]] = ref_element_addr [[C]] : $C, #C.x
// CHECK-NEXT:  [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[CX]] : $*Int
// CHECK-NEXT:  assign [[Y]] to [[ACCESS]]
// CHECK-NEXT:  end_access [[ACCESS]]

class D {
  var x: Int = 0
}
//   materializeForSet callback
// CHECK-LABEL: sil private [transparent] @_T017access_marker_gen1DC1xSifmytfU_
// CHECK:       end_unpaired_access [dynamic] %1 : $*Builtin.UnsafeValueBuffer

//   materializeForSet
// CHECK-LABEL: sil hidden [transparent] @_T017access_marker_gen1DC1xSifm
// CHECK:       [[T0:%.*]] = ref_element_addr %2 : $D, #D.x
// CHECK-NEXT:  begin_unpaired_access [modify] [dynamic] [[T0]] : $*Int

func testDispatchedClassInstanceProperty(d: D) {
  modify(&d.x)
}
// CHECK-LABEL: sil hidden @_T017access_marker_gen35testDispatchedClassInstancePropertyyAA1DC1d_tF
// CHECK:       [[D:%.*]] = begin_borrow %0 : $D
// CHECK:       [[METHOD:%.*]] = class_method [[D]] : $D, #D.x!materializeForSet.1
// CHECK:       apply [[METHOD]]({{.*}}, [[D]])
// CHECK-NOT:   begin_access
// CHECK:       end_borrow [[D]] from %0 : $D
