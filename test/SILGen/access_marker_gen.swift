
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name access_marker_gen -parse-as-library -Xllvm -sil-full-demangle -enforce-exclusivity=checked %s | %FileCheck %s

func modify<T>(_ x: inout T) {}

public struct S {
  var i: Int
  var o: AnyObject?
}

// CHECK-LABEL: sil hidden [noinline] [ossa] @$s17access_marker_gen5initSyAA1SVyXlSgF : $@convention(thin) (@guaranteed Optional<AnyObject>) -> @owned S {
// CHECK: bb0(%0 : @guaranteed $Optional<AnyObject>):
// CHECK: [[BOX:%.*]] = alloc_box ${ var S }, var, name "s"
// CHECK: [[MARKED_BOX:%.*]] = mark_uninitialized [var] [[BOX]] : ${ var S }
// CHECK: [[BOX_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[MARKED_BOX]]
// CHECK: [[ADDR:%.*]] = project_box [[BOX_LIFETIME]] : ${ var S }, 0
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
// CHECK-LABEL: } // end sil function '$s17access_marker_gen5initSyAA1SVyXlSgF'
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

// CHECK-LABEL: sil [ossa] @$s17access_marker_gen14modifyAndReadSyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK: %[[BOX:.*]] = alloc_box ${ var S }, var, name "s"
// CHECK: %[[BOX_LIFETIME:[^,]+]] = begin_borrow [lexical] [var_decl] %[[BOX]]
// CHECK: %[[ADDRS:.*]] = project_box %[[BOX_LIFETIME]] : ${ var S }, 0
// CHECK: %[[ACCESS1:.*]] = begin_access [modify] [unknown] %[[ADDRS]] : $*S
// CHECK: %[[ADDRI:.*]] = struct_element_addr %[[ACCESS1]] : $*S, #S.i
// CHECK: assign %{{.*}} to %[[ADDRI]] : $*Int
// CHECK: end_access %[[ACCESS1]] : $*S
// CHECK: %[[ACCESS2:.*]] = begin_access [read] [unknown] %[[ADDRS]] : $*S
// CHECK: %{{.*}} = load [copy] %[[ACCESS2]] : $*S
// CHECK: end_access %[[ACCESS2]] : $*S
// CHECK-LABEL: } // end sil function '$s17access_marker_gen14modifyAndReadSyyF'
public func modifyAndReadS() {
  var s = initS(nil)
  s.i = 42
  takeS(s)
}

var global = S(i: 0, o: nil)

func readGlobal() -> AnyObject? {
  return global.o
}

// CHECK-LABEL: sil hidden [ossa] @$s17access_marker_gen10readGlobalyXlSgyF
// CHECK:         [[ADDRESSOR:%.*]] = function_ref @$s17access_marker_gen6globalAA1SVvau :
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

// CHECK-LABEL: sil hidden [ossa] @$s17access_marker_gen22HasTwoStoredPropertiesV027noOverlapOnAssignFromPropToM0yyF : $@convention(method) (@inout HasTwoStoredProperties) -> ()
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
  let z: Int = 0
}

func testClassInstanceProperties(c: C) {
  let y = c.x
  c.x = y
}
// CHECK-LABEL: sil hidden [ossa] @$s17access_marker_gen27testClassInstanceProperties1cyAA1CC_tF :
// CHECK: bb0([[C:%.*]] : @guaranteed $C
// CHECK-NEXT:  debug_value
// CHECK-NEXT:  [[CX:%.*]] = ref_element_addr [[C]] : $C, #C.x
// CHECK-NEXT:  [[ACCESS:%.*]] = begin_access [read] [dynamic] [[CX]] : $*Int
// CHECK-NEXT:  [[L:%.*]] = load [trivial] [[ACCESS]]
// CHECK-NEXT:  end_access [[ACCESS]]
// CHECK-NEXT:  [[Y:%.*]] = move_value [var_decl] [[L]]
// CHECK-NEXT:  debug_value
// CHECK-NEXT:  [[CX:%.*]] = ref_element_addr [[C]] : $C, #C.x
// CHECK-NEXT:  [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[CX]] : $*Int
// CHECK-NEXT:  assign [[Y]] to [[ACCESS]]
// CHECK-NEXT:  end_access [[ACCESS]]

func testClassLetProperty(c: C) -> Int {
  return c.z
}

// CHECK-LABEL: sil hidden [ossa] @$s17access_marker_gen20testClassLetProperty1cSiAA1CC_tF : $@convention(thin) (@guaranteed C) -> Int {
// CHECK: bb0(%0 : @guaranteed $C):
// CHECK:   [[ADR:%.*]] = ref_element_addr %{{.*}} : $C, #C.z
// CHECK-NOT: begin_access
// CHECK:   %{{.*}} = load [trivial] [[ADR]] : $*Int
// CHECK-NOT: end_access
// CHECK-NOT:   destroy_value %0 : $C
// CHECK:   return %{{.*}} : $Int
// CHECK-LABEL: } // end sil function '$s17access_marker_gen20testClassLetProperty1cSiAA1CC_tF'

class D {
  var x: Int = 0
}

//   modify
// CHECK-LABEL: sil hidden [transparent] [ossa] @$s17access_marker_gen1DC1xSivM
// CHECK:       [[T0:%.*]] = ref_element_addr %0 : $D, #D.x
// CHECK-NEXT:  [[T1:%.*]] = begin_access [modify] [dynamic] [[T0]] : $*Int
// CHECK:       yield [[T1]] : $*Int
// CHECK:       end_access [[T1]] : $*Int
// CHECK:       end_access [[T1]] : $*Int

func testDispatchedClassInstanceProperty(d: D) {
  modify(&d.x)
}
// CHECK-LABEL: sil hidden [ossa] @$s17access_marker_gen35testDispatchedClassInstanceProperty1dyAA1DC_tF
// CHECK:     bb0([[D:%.*]] : @guaranteed $D
// CHECK:       [[METHOD:%.*]] = class_method [[D]] : $D, #D.x!modify
// CHECK:       begin_apply [[METHOD]]([[D]])
// CHECK-NOT:   begin_access
// CHECK:       end_apply

