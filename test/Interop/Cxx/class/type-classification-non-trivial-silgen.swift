// RUN: %target-swift-frontend -I %S/Inputs -enable-cxx-interop -emit-silgen %s | %FileCheck %s

import TypeClassification

// Make sure that "StructWithDestructor" is marked as non-trivial by checking for a
// "destroy_addr".
// CHECK-LABEL: sil [ossa] @$s4main24testStructWithDestructoryyF
// CHECK: [[AS:%.*]] = alloc_stack $StructWithDestructor
// CHECK: [[META:%.*]] = metatype $@thin StructWithDestructor.Type
// CHECK: [[FN:%.*]] = function_ref @$sSo20StructWithDestructorVABycfC : $@convention(method) (@thin StructWithDestructor.Type) -> @out StructWithDestructor
// CHECK: apply [[FN]]([[AS]], [[META]]) : $@convention(method) (@thin StructWithDestructor.Type) -> @out StructWithDestructor
// CHECK: destroy_addr [[AS]]
// CHECK: dealloc_stack %0 : $*StructWithDestructor
// CHECK-LABEL: end sil function '$s4main24testStructWithDestructoryyF'
public func testStructWithDestructor() {
  let d = StructWithDestructor()
}

// StructWithDestructor.init()
// CHECK-LABEL: sil shared [transparent] [serializable] [ossa] @$sSo20StructWithDestructorVABycfC : $@convention(method) (@thin StructWithDestructor.Type) -> @out StructWithDestructor
// CHECK: [[BOX:%.*]] = alloc_box ${ var StructWithDestructor }
// CHECK: [[UBOX:%.*]] = mark_uninitialized [rootself] [[BOX]] : ${ var StructWithDestructor }
// CHECK: [[BOX_ADDR:%.*]] = project_box [[UBOX]] : ${ var StructWithDestructor }, 0
// CHECK: [[SA:%.*]] = alloc_stack $StructWithDestructor
// CHECK: builtin "zeroInitializer"<StructWithDestructor>([[SA]] : $*StructWithDestructor) : $()
// CHECK: [[BA:%.*]] = begin_access [modify] [unknown] [[BOX_ADDR]] : $*StructWithDestructor
// CHECK: copy_addr [take] [[SA]] to [[BA]] : $*StructWithDestructor
// CHECK: copy_addr [[BOX_ADDR]] to [initialization] %0 : $*StructWithDestructor
// CHECK: destroy_value [[UBOX]] : ${ var StructWithDestructor }
// CHECK-LABEL: end sil function '$sSo20StructWithDestructorVABycfC'

// Make sure that "HasMemberWithDestructor" is marked as non-trivial by checking
// for a "destroy_addr".
// CHECK-LABEL: sil [ossa] @$s4main33testStructWithSubobjectDestructoryyF
// CHECK: [[AS:%.*]] = alloc_stack $StructWithSubobjectDestructor
// CHECK: [[META:%.*]] = metatype $@thin StructWithSubobjectDestructor.Type
// CHECK: [[FN:%.*]] = function_ref @$sSo29StructWithSubobjectDestructorVABycfC : $@convention(method) (@thin StructWithSubobjectDestructor.Type) -> @out StructWithSubobjectDestructor
// CHECK: apply [[FN]]([[AS]], [[META]]) : $@convention(method) (@thin StructWithSubobjectDestructor.Type) -> @out StructWithSubobjectDestructor
// CHECK: destroy_addr [[AS]]
// CHECK-LABEL: end sil function '$s4main33testStructWithSubobjectDestructoryyF'
public func testStructWithSubobjectDestructor() {
  let d = StructWithSubobjectDestructor()
}

// StructWithSubobjectDestructor.init()
// CHECK-LABEL: sil shared [transparent] [serializable] [ossa] @$sSo29StructWithSubobjectDestructorVABycfC : $@convention(method) (@thin StructWithSubobjectDestructor.Type) -> @out StructWithSubobjectDestructor
// CHECK: [[BOX:%.*]] = alloc_box ${ var StructWithSubobjectDestructor }
// CHECK: [[UBOX:%.*]] = mark_uninitialized [rootself] [[BOX]] : ${ var StructWithSubobjectDestructor }
// CHECK: [[ADDR:%.*]] = project_box [[UBOX]] : ${ var StructWithSubobjectDestructor }, 0
// CHECK: [[AS:%.*]] = alloc_stack $StructWithSubobjectDestructor
// CHECK: builtin "zeroInitializer"<StructWithSubobjectDestructor>([[AS]] : $*StructWithSubobjectDestructor) : $()
// CHECK: [[BA:%.*]] = begin_access [modify] [unknown] [[ADDR]] : $*StructWithSubobjectDestructor
// CHECK: copy_addr [take] [[AS]] to [[BA]] : $*StructWithSubobjectDestructor
// CHECK: copy_addr [[ADDR]] to [initialization] %0 : $*StructWithSubobjectDestructor
// CHECK: destroy_value [[UBOX]] : ${ var StructWithSubobjectDestructor }
// CHECK-LABEL: end sil function '$sSo29StructWithSubobjectDestructorVABycfC'

// CHECK-LABLE: sil [ossa] @$s4main37testStructWithCopyConstructorAndValueSbyF
// CHECK: [[AS:%.*]] = alloc_stack $StructWithCopyConstructorAndValue
// CHECK: [[META:%.*]] = metatype $@thin StructWithCopyConstructorAndValue.Type
// CHECK: [[FN:%.*]] = function_ref @$sSo33StructWithCopyConstructorAndValueV5valueABs5Int32V_tcfC : $@convention(method) (Int32, @thin StructWithCopyConstructorAndValue.Type) -> @out StructWithCopyConstructorAndValue
// CHECK: apply [[FN]]([[AS]], %{{.*}}, [[META]]) : $@convention(method) (Int32, @thin StructWithCopyConstructorAndValue.Type) -> @out StructWithCopyConstructorAndValue
// CHECK: [[OBJ_VAL_ADDR:%.*]] = struct_element_addr [[AS]] : $*StructWithCopyConstructorAndValue, #StructWithCopyConstructorAndValue.value
// CHECK: [[OBJ_VAL:%.*]] = load [trivial] [[OBJ_VAL_ADDR]] : $*Int32
// CHECK: [[IL_42:%.*]] = integer_literal $Builtin.IntLiteral, 42
// CHECK: [[MAKE_INT_FN:%.*]] = function_ref @$ss5Int32V22_builtinIntegerLiteralABBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int32.Type) -> Int32
// CHECK: [[INT_42:%.*]] = apply [[MAKE_INT_FN]]([[IL_42]], %{{.*}}) : $@convention(method) (Builtin.IntLiteral, @thin Int32.Type) -> Int32
// CHECK: [[CMP_FN:%.*]] = function_ref @$ss5Int32V2eeoiySbAB_ABtFZ : $@convention(method) (Int32, Int32, @thin Int32.Type) -> Bool
// CHECK: [[OUT:%.*]] = apply [[CMP_FN]]([[OBJ_VAL]], [[INT_42]], %{{.*}}) : $@convention(method) (Int32, Int32, @thin Int32.Type) -> Bool
// CHECK: destroy_addr [[AS]] : $*StructWithCopyConstructorAndValue
// CHECK: return [[OUT]] : $Bool
// CHECK-LABLE: end sil function '$s4main37testStructWithCopyConstructorAndValueSbyF'
public func testStructWithCopyConstructorAndValue() -> Bool {
  let obj = StructWithCopyConstructorAndValue(value: 42)
  return obj.value == 42
}

// StructWithCopyConstructorAndValue.init(value:)
// CHECK-LABEL: sil shared [transparent] [serializable] [ossa] @$sSo33StructWithCopyConstructorAndValueV5valueABs5Int32V_tcfC : $@convention(method) (Int32, @thin StructWithCopyConstructorAndValue.Type) -> @out StructWithCopyConstructorAndValue
// CHECK: [[VAL:%.*]] = struct_element_addr %0 : $*StructWithCopyConstructorAndValue, #StructWithCopyConstructorAndValue.value
// CHECK: store %1 to [trivial] [[VAL]] : $*Int32
// CHECK-LABEL: end sil function '$sSo33StructWithCopyConstructorAndValueV5valueABs5Int32V_tcfC'

// CHECK-LABEL: sil [ossa] @$s4main46testStructWithSubobjectCopyConstructorAndValueSbyF : $@convention(thin) () -> Bool
// CHECK: [[MEMBER_0:%.*]] = alloc_stack $StructWithCopyConstructorAndValue
// CHECK: [[MEMBER_META:%.*]] = metatype $@thin StructWithCopyConstructorAndValue.Type
// CHECK: [[MAKE_MEMBER_FN:%.*]] = function_ref @$sSo33StructWithCopyConstructorAndValueV5valueABs5Int32V_tcfC : $@convention(method) (Int32, @thin StructWithCopyConstructorAndValue.Type) -> @out StructWithCopyConstructorAndValue
// CHECK: apply [[MAKE_MEMBER_FN]]([[MEMBER_0]], %{{.*}}, [[MEMBER_META]]) : $@convention(method) (Int32, @thin StructWithCopyConstructorAndValue.Type) -> @out StructWithCopyConstructorAndValue
// CHECK: [[AS:%.*]] = alloc_stack $StructWithSubobjectCopyConstructorAndValue
// CHECK: [[META:%.*]] = metatype $@thin StructWithSubobjectCopyConstructorAndValue.Type
// CHECK: [[MEMBER_1:%.*]] = alloc_stack $StructWithCopyConstructorAndValue
// CHECK: copy_addr %0 to [initialization] [[MEMBER_1]] : $*StructWithCopyConstructorAndValue
// CHECK: [[FN:%.*]] = function_ref @$sSo42StructWithSubobjectCopyConstructorAndValueV6memberABSo0abdefG0V_tcfC : $@convention(method) (@in StructWithCopyConstructorAndValue, @thin StructWithSubobjectCopyConstructorAndValue.Type) -> @out StructWithSubobjectCopyConstructorAndValue
// CHECK: apply [[FN]]([[AS]], [[MEMBER_1]], [[META]]) : $@convention(method) (@in StructWithCopyConstructorAndValue, @thin StructWithSubobjectCopyConstructorAndValue.Type) -> @out StructWithSubobjectCopyConstructorAndValue
// CHECK: [[OBJ_MEMBER:%.*]] = struct_element_addr [[AS]] : $*StructWithSubobjectCopyConstructorAndValue, #StructWithSubobjectCopyConstructorAndValue.member
// CHECK: [[MEMBER_2:%.*]] = alloc_stack $StructWithCopyConstructorAndValue
// CHECK: copy_addr [[OBJ_MEMBER]] to [initialization] [[MEMBER_2]] : $*StructWithCopyConstructorAndValue
// CHECK: [[OBJ_VALUE_ADDR:%.*]] = struct_element_addr [[MEMBER_2]] : $*StructWithCopyConstructorAndValue, #StructWithCopyConstructorAndValue.value
// CHECK: [[OBJ_VALUE:%.*]] = load [trivial] [[OBJ_VALUE_ADDR]] : $*Int32
// CHECK: [[MAKE_INT:%.*]] = function_ref @$ss5Int32V22_builtinIntegerLiteralABBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int32.Type) -> Int32
// CHECK: [[INT_42:%.*]] = apply [[MAKE_INT]]({{.*}}) : $@convention(method) (Builtin.IntLiteral, @thin Int32.Type) -> Int32
// CHECK: [[ICMP:%.*]] = function_ref @$ss5Int32V2eeoiySbAB_ABtFZ : $@convention(method) (Int32, Int32, @thin Int32.Type) -> Bool
// CHECK: [[OUT:%.*]] = apply [[ICMP]]([[OBJ_VALUE]], [[INT_42]], %{{.*}}) : $@convention(method) (Int32, Int32, @thin Int32.Type) -> Bool
// CHECK: return [[OUT]] : $Bool
// CHECK-LABEL: end sil function '$s4main46testStructWithSubobjectCopyConstructorAndValueSbyF'
public func testStructWithSubobjectCopyConstructorAndValue() -> Bool {
  let member = StructWithCopyConstructorAndValue(value: 42)
  let obj = StructWithSubobjectCopyConstructorAndValue(member: member)
  return obj.member.value == 42
}

// StructWithSubobjectCopyConstructorAndValue.init(member:)
// CHECK-LABEL: sil shared [transparent] [serializable] [ossa] @$sSo42StructWithSubobjectCopyConstructorAndValueV6memberABSo0abdefG0V_tcfC : $@convention(method) (@in StructWithCopyConstructorAndValue, @thin StructWithSubobjectCopyConstructorAndValue.Type) -> @out StructWithSubobjectCopyConstructorAndValue
// CHECK: [[MEMBER:%.*]] = struct_element_addr %0 : $*StructWithSubobjectCopyConstructorAndValue, #StructWithSubobjectCopyConstructorAndValue.member
// CHECK: copy_addr [take] %1 to [initialization] [[MEMBER]] : $*StructWithCopyConstructorAndValue
// CHECK-LABEL: end sil function '$sSo42StructWithSubobjectCopyConstructorAndValueV6memberABSo0abdefG0V_tcfC'

// testStructWithCopyConstructorAndSubobjectCopyConstructorAndValue()
// CHECK-LABEL: sil [ossa] @$s4main041testStructWithCopyConstructorAndSubobjectefG5ValueSbyF : $@convention(thin) () -> Bool
// CHECK: [[MEMBER_0:%.*]] = alloc_stack $StructWithCopyConstructorAndValue
// CHECK: [[META_MEMBER:%.*]] = metatype $@thin StructWithCopyConstructorAndValue.Type
// CHECK: [[CREATE_MEMBER_FN:%.*]] = function_ref @$sSo33StructWithCopyConstructorAndValueV5valueABs5Int32V_tcfC : $@convention(method) (Int32, @thin StructWithCopyConstructorAndValue.Type) -> @out StructWithCopyConstructorAndValue
// CHECK: apply [[CREATE_MEMBER_FN]]([[MEMBER_0]], %{{.*}}, [[META_MEMBER]]) : $@convention(method) (Int32, @thin StructWithCopyConstructorAndValue.Type) -> @out StructWithCopyConstructorAndValue
// CHECK: [[AS:%.*]] = alloc_stack $StructWithCopyConstructorAndSubobjectCopyConstructorAndValue
// CHECK: [[META:%.*]] = metatype $@thin StructWithCopyConstructorAndSubobjectCopyConstructorAndValue.Type
// CHECK: [[MEMBER_1:%.*]] = alloc_stack $StructWithCopyConstructorAndValue
// CHECK: copy_addr [[MEMBER_0]] to [initialization] [[MEMBER_1]] : $*StructWithCopyConstructorAndValue
// CHECK: [[FN:%.*]] = function_ref @$sSo037StructWithCopyConstructorAndSubobjectcdE5ValueV6memberABSo0abcdeG0V_tcfC : $@convention(method) (@in StructWithCopyConstructorAndValue, @thin StructWithCopyConstructorAndSubobjectCopyConstructorAndValue.Type) -> @out StructWithCopyConstructorAndSubobjectCopyConstructorAndValue
// CHECK: apply [[FN]]([[AS]], [[MEMBER_1]], [[META]]) : $@convention(method) (@in StructWithCopyConstructorAndValue, @thin StructWithCopyConstructorAndSubobjectCopyConstructorAndValue.Type) -> @out StructWithCopyConstructorAndSubobjectCopyConstructorAndValue
// CHECK: [[OBJ_MEMBER_ADDR:%.*]] = struct_element_addr [[AS]] : $*StructWithCopyConstructorAndSubobjectCopyConstructorAndValue, #StructWithCopyConstructorAndSubobjectCopyConstructorAndValue.member
// CHECK: [[MEMBER_2:%.*]] = alloc_stack $StructWithCopyConstructorAndValue
// CHECK: copy_addr [[OBJ_MEMBER_ADDR]] to [initialization] [[MEMBER_2]] : $*StructWithCopyConstructorAndValue
// CHECK: [[OBJ_MEMBER_VALUE_ADDR:%.*]] = struct_element_addr [[MEMBER_2]] : $*StructWithCopyConstructorAndValue, #StructWithCopyConstructorAndValue.value
// CHECK: [[OBJ_MEMBER_VALUE:%.*]] = load [trivial] [[OBJ_MEMBER_VALUE_ADDR]] : $*Int32
// CHECK: [[ICMP:%.*]] = function_ref @$ss5Int32V2eeoiySbAB_ABtFZ : $@convention(method) (Int32, Int32, @thin Int32.Type) -> Bool
// CHECK: [[OUT:%.*]] = apply [[ICMP]]([[OBJ_MEMBER_VALUE]], %{{.*}}) : $@convention(method) (Int32, Int32, @thin Int32.Type) -> Bool
// CHECK: return [[OUT]] : $Bool
// CHECK-LABEL: end sil function '$s4main041testStructWithCopyConstructorAndSubobjectefG5ValueSbyF'
public func testStructWithCopyConstructorAndSubobjectCopyConstructorAndValue()
-> Bool {
  let member = StructWithCopyConstructorAndValue(value: 42)
  let obj = StructWithCopyConstructorAndSubobjectCopyConstructorAndValue(
    member: member
  )
  return obj.member.value == 42
}

// StructWithCopyConstructorAndSubobjectCopyConstructorAndValue.init(member:)
// CHECK-LABEL: sil shared [transparent] [serializable] [ossa] @$sSo037StructWithCopyConstructorAndSubobjectcdE5ValueV6memberABSo0abcdeG0V_tcfC : $@convention(method) (@in StructWithCopyConstructorAndValue, @thin StructWithCopyConstructorAndSubobjectCopyConstructorAndValue.Type) -> @out StructWithCopyConstructorAndSubobjectCopyConstructorAndValue
// CHECK: [[MEMBER:%.*]] = struct_element_addr %0 : $*StructWithCopyConstructorAndSubobjectCopyConstructorAndValue, #StructWithCopyConstructorAndSubobjectCopyConstructorAndValue.member
// CHECK: copy_addr [take] %1 to [initialization] [[MEMBER]] : $*StructWithCopyConstructorAndValue
// CHECK-LABEL: end sil function '$sSo037StructWithCopyConstructorAndSubobjectcdE5ValueV6memberABSo0abcdeG0V_tcfC'

// CHECK-LABEL: sil [ossa] @$s4main4test3objSbSo33StructWithCopyConstructorAndValueV_tF : $@convention(thin) (@in_guaranteed StructWithCopyConstructorAndValue) -> Bool
// CHECK: [[META_1:%.*]] = metatype $@thin Int32.Type
// CHECK: [[OBJ_VAL_ADDR:%.*]] = struct_element_addr %0 : $*StructWithCopyConstructorAndValue, #StructWithCopyConstructorAndValue.value
// CHECK: [[OBJ_VAL:%.*]] = load [trivial] [[OBJ_VAL_ADDR]] : $*Int32
// CHECK: [[IL_42:%.*]] = integer_literal $Builtin.IntLiteral, 42
// CHECK: [[META_2:%.*]] = metatype $@thin Int32.Type
// CHECK: [[FN:%.*]] = function_ref @$ss5Int32V22_builtinIntegerLiteralABBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int32.Type) -> Int32
// CHECK: [[INT:%.*]] = apply [[FN]]([[IL_42]], [[META_2]]) : $@convention(method) (Builtin.IntLiteral, @thin Int32.Type) -> Int32
// CHECK: [[FN_2:%.*]] = function_ref @$ss5Int32V2eeoiySbAB_ABtFZ : $@convention(method) (Int32, Int32, @thin Int32.Type) -> Bool
// CHECK: [[OUT:%.*]] = apply [[FN_2]]([[OBJ_VAL]], [[INT]], [[META_1]]) : $@convention(method) (Int32, Int32, @thin Int32.Type) -> Bool
// CHECK: return [[OUT]] : $Bool
// CHECK-LABEL: end sil function '$s4main4test3objSbSo33StructWithCopyConstructorAndValueV_tF'
public func test(obj: StructWithCopyConstructorAndValue) -> Bool {
  return obj.value == 42
}

// CHECK-LABEL: sil [ossa] @$s4main4test3objSbSo42StructWithSubobjectCopyConstructorAndValueV_tF : $@convention(thin) (@in_guaranteed StructWithSubobjectCopyConstructorAndValue) -> Bool
// CHECK: [[INT_META:%.*]] = metatype $@thin Int32.Type
// CHECK: [[OBJ_MEMBER:%.*]] = struct_element_addr %0 : $*StructWithSubobjectCopyConstructorAndValue, #StructWithSubobjectCopyConstructorAndValue.member
// CHECK: [[MEMBER_TMP:%.*]] = alloc_stack $StructWithCopyConstructorAndValue
// CHECK: copy_addr [[OBJ_MEMBER]] to [initialization] [[MEMBER_TMP]] : $*StructWithCopyConstructorAndValue
// CHECK: [[OBJ_MEMBER_VALUE_ADDR:%.*]] = struct_element_addr [[MEMBER_TMP]] : $*StructWithCopyConstructorAndValue, #StructWithCopyConstructorAndValue.value
// CHECK: [[OBJ_MEMBER_VALUE:%.*]] = load [trivial] [[OBJ_MEMBER_VALUE_ADDR]] : $*Int32
// CHECK: destroy_addr [[MEMBER_TMP]] : $*StructWithCopyConstructorAndValue
// CHECK: [[IL_42:%.*]] = integer_literal $Builtin.IntLiteral, 42
// CHECK: [[INT_META_2:%.*]] = metatype $@thin Int32.Type
// CHECK: [[MAKE_INT_FN:%.*]] = function_ref @$ss5Int32V22_builtinIntegerLiteralABBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int32.Type) -> Int32
// CHECK: [[INT_42:%.*]] = apply [[MAKE_INT_FN]]([[IL_42]], [[INT_META_2]]) : $@convention(method) (Builtin.IntLiteral, @thin Int32.Type) -> Int32
// CHECK: [[FN:%.*]] = function_ref @$ss5Int32V2eeoiySbAB_ABtFZ : $@convention(method) (Int32, Int32, @thin Int32.Type) -> Bool
// CHECK: [[OUT:%.*]] = apply [[FN]]([[OBJ_MEMBER_VALUE]], [[INT_42]], [[INT_META]]) : $@convention(method) (Int32, Int32, @thin Int32.Type) -> Bool
// CHECK: dealloc_stack [[MEMBER_TMP]] : $*StructWithCopyConstructorAndValue
// CHECK: return [[OUT]] : $Bool
// CHECK-LABEL: end sil function '$s4main4test3objSbSo42StructWithSubobjectCopyConstructorAndValueV_tF'
public func test(obj: StructWithSubobjectCopyConstructorAndValue) -> Bool {
  return obj.member.value == 42
}

// CHECK-LABEL: sil [ossa] @$s4main4test3objSbSo037StructWithCopyConstructorAndSubobjectfgH5ValueV_tF
// CHECK: [[META_INT_1:%.*]] = metatype $@thin Int32.Type
// CHECK: [[OBJ_MEMBER:%.*]] = struct_element_addr %0 : $*StructWithCopyConstructorAndSubobjectCopyConstructorAndValue, #StructWithCopyConstructorAndSubobjectCopyConstructorAndValue.member
// CHECK: [[MEMBER_TMP:%.*]] = alloc_stack $StructWithCopyConstructorAndValue
// CHECK: copy_addr [[OBJ_MEMBER]] to [initialization] [[MEMBER_TMP]] : $*StructWithCopyConstructorAndValue
// CHECK: [[OBJ_MEMBER_VAL_ADDR:%.*]] = struct_element_addr [[MEMBER_TMP]] : $*StructWithCopyConstructorAndValue, #StructWithCopyConstructorAndValue.value
// CHECK: [[OBJ_MEMBER_VAL:%.*]] = load [trivial] [[OBJ_MEMBER_VAL_ADDR]] : $*Int32
// CHECK: destroy_addr [[MEMBER_TMP]] : $*StructWithCopyConstructorAndValue
// CHECK: [[IL_42:%.*]] = integer_literal $Builtin.IntLiteral, 42
// CHECK: [[META_INT_2:%.*]] = metatype $@thin Int32.Type
// CHECK: [[MAKE_INT_FN:%.*]] = function_ref @$ss5Int32V22_builtinIntegerLiteralABBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int32.Type) -> Int32
// CHECK: [[INT_42:%.*]] = apply [[MAKE_INT_FN]]([[IL_42]], [[META_INT_2]]) : $@convention(method) (Builtin.IntLiteral, @thin Int32.Type) -> Int32
// CHECK: [[ICMP_FN:%.*]] = function_ref @$ss5Int32V2eeoiySbAB_ABtFZ : $@convention(method) (Int32, Int32, @thin Int32.Type) -> Bool
// CHECK: [[OUT:%.*]] = apply [[ICMP_FN]]([[OBJ_MEMBER_VAL]], [[INT_42]], [[META_INT_1]]) : $@convention(method) (Int32, Int32, @thin Int32.Type) -> Bool
// CHECK: dealloc_stack [[MEMBER_TMP]] : $*StructWithCopyConstructorAndValue
// CHECK: return [[OUT]] : $Bool
// CHECK-LABEL: end sil function '$s4main4test3objSbSo037StructWithCopyConstructorAndSubobjectfgH5ValueV_tF'
public func test(
  obj: StructWithCopyConstructorAndSubobjectCopyConstructorAndValue
) -> Bool {
  return obj.member.value == 42
}
