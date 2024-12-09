
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name unowned %s | %FileCheck %s

func takeClosure(_ fn: () -> Int) {}

class C {
  func f() -> Int { return 42 }
}

struct A {
  unowned var x: C
}
_ = A(x: C())
// CHECK-LABEL: sil hidden [ossa] @$s7unowned1AV{{[_0-9a-zA-Z]*}}fC
// CHECK: bb0([[X:%.*]] : @owned $C, %1 : $@thin A.Type):
// CHECK:   [[X_UNOWNED:%.*]] = ref_to_unowned [[X]] : $C to $@sil_unowned C
// CHECK:   [[X_UNOWNED_COPY:%.*]] = copy_value [[X_UNOWNED]]
// CHECK:   destroy_value [[X]]
// CHECK:   [[A:%.*]] = struct $A ([[X_UNOWNED_COPY]] : $@sil_unowned C)
// CHECK:   return [[A]]
// CHECK: }

protocol P {}
struct X: P {}

struct AddressOnly {
  unowned var x: C
  var p: P
}
_ = AddressOnly(x: C(), p: X())
// CHECK-LABEL: sil hidden [ossa] @$s7unowned11AddressOnlyV{{[_0-9a-zA-Z]*}}fC
// CHECK: bb0([[RET:%.*]] : $*AddressOnly, [[X:%.*]] : @owned $C, {{.*}}):
// CHECK:   [[X_ADDR:%.*]] = struct_element_addr [[RET]] : $*AddressOnly, #AddressOnly.x
// CHECK:   [[X_UNOWNED:%.*]] = ref_to_unowned [[X]] : $C to $@sil_unowned C
// CHECK:   [[X_UNOWNED_COPY:%.*]] = copy_value [[X_UNOWNED]] : $@sil_unowned C
// CHECK:   store [[X_UNOWNED_COPY]] to [init] [[X_ADDR]]
// CHECK:   destroy_value [[X]]
// CHECK: }

// CHECK-LABEL:    sil hidden [ossa] @$s7unowned5test01cyAA1CC_tF : $@convention(thin) (@guaranteed C) -> () {
func test0(c c: C) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $C):

  var a: A
  // CHECK:   [[A1:%.*]] = alloc_box ${ var A }, var, name "a"
  // CHECK:   [[MARKED_A1:%.*]] = mark_uninitialized [var] [[A1]]
  // CHECK:   [[MARKED_A1_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[MARKED_A1]]
  // CHECK:   [[PBA:%.*]] = project_box [[MARKED_A1_LIFETIME]]

  unowned var x = c
  // CHECK:   [[X:%.*]] = alloc_box ${ var @sil_unowned C }
  // CHECK:   [[X_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[X]]
  // CHECK:   [[PBX:%.*]] = project_box [[X_LIFETIME]]
  // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
  // CHECK:   [[T2:%.*]] = ref_to_unowned [[ARG_COPY]] : $C  to $@sil_unowned C
  // CHECK:   [[T2_COPY:%.*]] = copy_value [[T2]] : $@sil_unowned C
  // CHECK:   store [[T2_COPY]] to [init] [[PBX]] : $*@sil_unowned C
  // CHECK:   destroy_value [[ARG_COPY]]

  a.x = c
  // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
  // CHECK:   [[WRITE:%.*]] = begin_access [modify] [unknown] [[PBA]]
  // CHECK:   [[T1:%.*]] = struct_element_addr [[WRITE]] : $*A, #A.x
  // CHECK:   [[T2:%.*]] = ref_to_unowned [[ARG_COPY]] : $C
  // CHECK:   [[T2_COPY:%.*]] = copy_value [[T2]] : $@sil_unowned C
  // CHECK:   assign [[T2_COPY]] to [[T1]] : $*@sil_unowned C
  // CHECK:   destroy_value [[ARG_COPY]]

  a.x = x
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBX]]
  // CHECK:   [[T2:%.*]] = load_borrow [[READ]] : $*@sil_unowned C     
  // CHECK:   [[T3:%.*]] = strong_copy_unowned_value  [[T2]] : $@sil_unowned C  
  // CHECK:   end_borrow [[T2]]
  // CHECK:   [[WRITE:%.*]] = begin_access [modify] [unknown] [[PBA]]
  // CHECK:   [[XP:%.*]] = struct_element_addr [[WRITE]] : $*A, #A.x
  // CHECK:   [[T4:%.*]] = ref_to_unowned [[T3]] : $C to $@sil_unowned C
  // CHECK:   [[T4_COPY:%.*]] = copy_value [[T4]] : $@sil_unowned C  
  // CHECK:   assign [[T4_COPY]] to [[XP]] : $*@sil_unowned C
  // CHECK:   destroy_value [[T3]] : $C
  // CHECK:   end_borrow [[X_LIFETIME]]
  // CHECK:   destroy_value [[X]]
  // CHECK:   end_borrow [[MARKED_A1_LIFETIME]]
  // CHECK:   destroy_value [[MARKED_A1]]
}
// CHECK: } // end sil function '$s7unowned5test01cyAA1CC_tF'

// CHECK-LABEL: sil hidden [ossa] @{{.*}}testunowned_local
func testunowned_local() -> C {
  // CHECK: [[C:%.*]] = apply
  let c = C()

  // CHECK: [[MOVED_C:%.*]] = move_value [lexical] [var_decl] [[C]]
  // CHECK: [[UC:%.*]] = alloc_box ${ var @sil_unowned C }, let, name "uc"
  // CHECK: [[UC_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[UC]]
  // CHECK: [[PB_UC:%.*]] = project_box [[UC_LIFETIME]]
  // CHECK: [[BORROWED_C:%.*]] = begin_borrow [[MOVED_C]]
  // CHECK: [[C_COPY:%.*]] = copy_value [[BORROWED_C]]
  // CHECK: [[tmp1:%.*]] = ref_to_unowned [[C_COPY]] : $C to $@sil_unowned C
  // CHECK: [[tmp1_copy:%.*]] = copy_value [[tmp1]]
  // CHECK: store [[tmp1_copy]] to [init] [[PB_UC]]
  // CHECK: destroy_value [[C_COPY]]
  unowned let uc = c

  // CHECK: [[tmp2:%.*]] = load_borrow [[PB_UC]]
  // CHECK: [[tmp3:%.*]] = strong_copy_unowned_value [[tmp2]]
  // CHECK: end_borrow [[tmp2]]
  return uc

  // CHECK: end_borrow [[UC_LIFETIME]]
  // CHECK: destroy_value [[UC]]
  // CHECK: destroy_value [[MOVED_C]]
  // CHECK: return [[tmp3]]
}

// <rdar://problem/16877510> capturing an unowned let crashes in silgen
func test_unowned_let_capture(_ aC : C) {
  unowned let bC = aC
  takeClosure { bC.f() }
}

// CHECK-LABEL: sil private [ossa] @$s7unowned05test_A12_let_captureyyAA1CCFSiyXEfU_ : $@convention(thin) (@guaranteed @sil_unowned C) -> Int {
// CHECK: bb0([[ARG:%.*]] : @closureCapture @guaranteed $@sil_unowned C):
// CHECK-NEXT:   debug_value %0 : $@sil_unowned C, let, name "bC", argno 1
// CHECK-NEXT:   [[UNOWNED_ARG:%.*]] = strong_copy_unowned_value [[ARG]] : $@sil_unowned C
// CHECK-NEXT:   [[FUN:%.*]] = class_method [[UNOWNED_ARG]] : $C, #C.f : (C) -> () -> Int, $@convention(method) (@guaranteed C) -> Int
// CHECK-NEXT:   [[RESULT:%.*]] = apply [[FUN]]([[UNOWNED_ARG]]) : $@convention(method) (@guaranteed C) -> Int
// CHECK-NEXT:   destroy_value [[UNOWNED_ARG]]
// CHECK-NEXT:   return [[RESULT]] : $Int



// <rdar://problem/16880044> unowned let properties don't work as struct and class members
class TestUnownedMember {
  unowned let member : C
  init(inval: C) {
    self.member = inval
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s7unowned17TestUnownedMemberC5invalAcA1CC_tcfc :
// CHECK: bb0([[ARG1:%.*]] : @owned $C, [[SELF_PARAM:%.*]] : @owned $TestUnownedMember):
// CHECK:   [[SELF:%.*]] = mark_uninitialized [rootself] [[SELF_PARAM]] : $TestUnownedMember
// CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
// CHECK:   [[BORROWED_ARG1:%.*]] = begin_borrow [[ARG1]]
// CHECK:   [[ARG1_COPY:%.*]] = copy_value [[BORROWED_ARG1]]
// CHECK:   [[FIELDPTR:%.*]] = ref_element_addr [[BORROWED_SELF]] : $TestUnownedMember, #TestUnownedMember.member
// CHECK:   [[INVAL:%.*]] = ref_to_unowned [[ARG1_COPY]] : $C to $@sil_unowned C
// CHECK:   [[INVAL_COPY:%.*]] = copy_value [[INVAL]] : $@sil_unowned C
// CHECK:   assign [[INVAL_COPY]] to [[FIELDPTR]] : $*@sil_unowned C
// CHECK:   destroy_value [[ARG1_COPY]] : $C
// CHECK:   end_borrow [[BORROWED_SELF]]
// CHECK:   [[RET_SELF:%.*]] = copy_value [[SELF]]
// CHECK:   destroy_value [[SELF]]
// CHECK:   destroy_value [[ARG1]]
// CHECK:   return [[RET_SELF]] : $TestUnownedMember
// CHECK: } // end sil function '$s7unowned17TestUnownedMemberC5invalAcA1CC_tcfc'

// Just verify that lowering an unowned reference to a type parameter
// doesn't explode.
struct Unowned<T: AnyObject> {
  unowned var object: T
}
func takesUnownedStruct(_ z: Unowned<C>) {}
// CHECK-LABEL: sil hidden [ossa] @$s7unowned18takesUnownedStructyyAA0C0VyAA1CCGF : $@convention(thin) (@guaranteed Unowned<C>) -> ()

// Make sure we don't crash here
struct UnownedGenericCapture<T : AnyObject> {
  var object: T
  var optionalObject: T?

  func f() -> () -> () {
    return { [unowned object] in _ = object }
  }

  func g() -> () -> () {
    return { [unowned optionalObject] in _ = optionalObject }
  }
}
