// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

func takeClosure(fn: () -> Int) {}

class C {
  func f() -> Int { return 42 }
}

struct A {
  unowned var x: C
}
// CHECK-LABEL: sil hidden @_TFV7unowned1ACfMS0_FT1xCS_1C_S0_
// CHECK: bb0([[X:%.*]] : $C, %1 : $@thin A.Type):
// CHECK:   [[X_UNOWNED:%.*]] = ref_to_unowned [[X]] : $C to $@sil_unowned C
// CHECK:   unowned_retain [[X_UNOWNED]]
// CHECK:   strong_release [[X]]
// CHECK:   [[A:%.*]] = struct $A ([[X_UNOWNED]] : $@sil_unowned C)
// CHECK:   return [[A]]
// CHECK: }

protocol P {}

struct AddressOnly {
  unowned var x: C
  var p: P
}
// CHECK-LABEL: sil hidden @_TFV7unowned11AddressOnlyCfMS0_FT1xCS_1C1pPS_1P__S0_
// CHECK: bb0([[RET:%.*]] : $*AddressOnly, [[X:%.*]] : $C, {{.*}}):
// CHECK:   [[X_ADDR:%.*]] = struct_element_addr [[RET]] : $*AddressOnly, #AddressOnly.x
// CHECK:   [[X_UNOWNED:%.*]] = ref_to_unowned [[X]] : $C to $@sil_unowned C
// CHECK:   unowned_retain [[X_UNOWNED]] : $@sil_unowned C
// CHECK:   store [[X_UNOWNED]] to [[X_ADDR]]
// CHECK:   strong_release [[X]]
// CHECK: }

// CHECK-LABEL:    sil hidden @_TF7unowned5test0FT1cCS_1C_T_ : $@thin (@owned C) -> () {
func test0(let #c: C) {
// CHECK:    bb0(%0 : $C):

  var a: A
// CHECK:      [[A1:%.*]] = alloc_box $A
// CHECK:      [[A:%.*]] = mark_uninitialized [var] [[A1]]#1

  unowned var x = c
// CHECK:      [[X:%.*]] = alloc_box $@sil_unowned C
// CHECK-NEXT: [[T2:%.*]] = ref_to_unowned %0 : $C  to $@sil_unowned C
// CHECK-NEXT: unowned_retain [[T2]] : $@sil_unowned C
// CHECK-NEXT: store [[T2]] to [[X]]#1 : $*@sil_unowned C

  a.x = c
// CHECK-NEXT: [[T1:%.*]] = struct_element_addr [[A]] : $*A, #A.x
// CHECK-NEXT: [[T2:%.*]] = ref_to_unowned %0 : $C
// CHECK-NEXT: unowned_retain [[T2]] : $@sil_unowned C
// CHECK-NEXT: assign [[T2]] to [[T1]] : $*@sil_unowned C

  a.x = x
// CHECK-NEXT: [[T2:%.*]] = load [[X]]#1 : $*@sil_unowned C     
// CHECK-NEXT:  strong_retain_unowned  [[T2]] : $@sil_unowned C  
// CHECK-NEXT:  [[T3:%.*]] = unowned_to_ref [[T2]] : $@sil_unowned C to $C
// CHECK-NEXT:  [[XP:%.*]] = struct_element_addr [[A]] : $*A, #A.x
// CHECK-NEXT:  [[T4:%.*]] = ref_to_unowned [[T3]] : $C to $@sil_unowned C
// CHECK-NEXT:  unowned_retain [[T4]] : $@sil_unowned C  
// CHECK-NEXT:  assign [[T4]] to [[XP]] : $*@sil_unowned C
// CHECK-NEXT:  strong_release [[T3]] : $C
}

// CHECK-LABEL: sil hidden @{{.*}}unowned_local
func unowned_local() -> C {
  // CHECK: [[c:%.*]] = apply
  let c = C()

  // CHECK: [[uc:%.*]] = alloc_box $@sil_unowned C  // let uc
  // CHECK-NEXT: [[tmp1:%.*]] = ref_to_unowned [[c]] : $C to $@sil_unowned C
  // CHECK-NEXT: unowned_retain [[tmp1]]
  // CHECK-NEXT: store [[tmp1]] to [[uc]]#1
  unowned let uc = c

  // CHECK-NEXT: [[tmp2:%.*]] = load [[uc]]#1
  // CHECK-NEXT: strong_retain_unowned [[tmp2]]
  // CHECK-NEXT: [[tmp3:%.*]] = unowned_to_ref [[tmp2]]
  return uc

  // CHECK-NEXT: strong_release [[uc]]#0
  // CHECK-NEXT: strong_release [[c]]
  // CHECK-NEXT: return [[tmp3]]
}

// <rdar://problem/16877510> capturing an unowned let crashes in silgen
func test_unowned_let_capture(aC : C) {
  unowned let bC = aC
  takeClosure { bC.f() }
}

// CHECK-LABEL: sil shared @_TFF7unowned24test_unowned_let_captureFCS_1CT_U_FT_Si : $@thin (@owned @sil_unowned C) -> Int {
// CHECK-NEXT: bb0([[ARG:%.*]] : $@sil_unowned C):
// CHECK-NEXT:   strong_retain_unowned [[ARG]] : $@sil_unowned C
// CHECK-NEXT:   [[UNOWNED_ARG:%.*]] = unowned_to_ref [[ARG]] : $@sil_unowned C to $C
// CHECK-NEXT:   [[FUN:%.*]] = class_method [[UNOWNED_ARG]] : $C, #C.f!1 : C -> () -> Int , $@cc(method) @thin (@guaranteed C) -> Int
// CHECK-NEXT:   [[RESULT:%.*]] = apply [[FUN]]([[UNOWNED_ARG]]) : $@cc(method) @thin (@guaranteed C) -> Int
// CHECK-NEXT:   strong_release [[UNOWNED_ARG]]
// CHECK-NEXT:   unowned_release [[ARG]] : $@sil_unowned C
// CHECK-NEXT:   return [[RESULT]] : $Int



// <rdar://problem/16880044> unowned let properties don't work as struct and class members
class TestUnownedMember {
  unowned let member : C
  init(inval: C) {
    self.member = inval
  }
}

// CHECK-LABEL: sil hidden @_TFC7unowned17TestUnownedMembercfMS0_FT5invalCS_1C_S0_
// CHECK-NEXT:  bb0(%0 : $C, %1 : $TestUnownedMember):
// CHECK:  [[SELF:%.*]] = mark_uninitialized [rootself] %1 : $TestUnownedMember
// CHECK:  [[FIELDPTR:%.*]] = ref_element_addr [[SELF]] : $TestUnownedMember, #TestUnownedMember.member
// CHECK:  [[INVAL:%.*]] = ref_to_unowned %0 : $C to $@sil_unowned C
// CHECK:  unowned_retain [[INVAL]] : $@sil_unowned C
// CHECK:  assign [[INVAL]] to [[FIELDPTR]] : $*@sil_unowned C
// CHECK:  strong_release %0 : $C
// CHECK:  return [[SELF]] : $TestUnownedMember
