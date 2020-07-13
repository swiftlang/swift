// RUN: %target-swift-frontend -I %S/Inputs -enable-cxx-interop -emit-silgen %s | %FileCheck %s

import SynthesizedInitializers

// CHECK-LABEL: sil shared [transparent] [serializable] [ossa] @$sSo11EmptyStructVABycfC : $@convention(method) (@thin EmptyStruct.Type) -> EmptyStruct
// CHECK: bb0(%{{[0-9]+}} : $@thin EmptyStruct.Type):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box ${ var EmptyStruct }
// CHECK-NEXT: [[UNINIT:%.*]] = mark_uninitialized [rootself] [[BOX]] : ${ var EmptyStruct }
// CHECK-NEXT: [[PTR:%.*]] = project_box [[UNINIT]] : ${ var EmptyStruct }, 0
// CHECK-NEXT: [[OBJ:%.*]] = builtin "zeroInitializer"<EmptyStruct>() : $EmptyStruct
// CHECK-NEXT: [[PA:%.*]] = begin_access [modify] [unknown] [[PTR]] : $*EmptyStruct
// CHECK-NEXT: assign [[OBJ]] to [[PA]]
// CHECK-NEXT: end_access [[PA]]
// CHECK-NEXT: [[OUT:%.*]] = load [trivial] [[PTR]]
// CHECK-NEXT: destroy_value [[UNINIT]]
// CHECK-NEXT: return [[OUT]]
// CHECK-LABEL: end sil function '$sSo11EmptyStructVABycfC'
public func emptyTypeNoArgInit() {
  let e = EmptyStruct()
}

// CHECK-LABEL: sil shared [transparent] [serializable] [ossa] @$sSo6IntBoxVABycfC : $@convention(method) (@thin IntBox.Type) -> IntBox
// CHECK: bb0(%{{[0-9]+}} : $@thin IntBox.Type):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box ${ var IntBox }
// CHECK-NEXT: [[UNINIT:%.*]] = mark_uninitialized [rootself] [[BOX]] : ${ var IntBox }
// CHECK-NEXT: [[PTR:%.*]] = project_box [[UNINIT]] : ${ var IntBox }, 0
// CHECK-NEXT: [[OBJ:%.*]] = builtin "zeroInitializer"<IntBox>() : $IntBox
// CHECK-NEXT: [[PA:%.*]] = begin_access [modify] [unknown] [[PTR]] : $*IntBox
// CHECK-NEXT: assign [[OBJ]] to [[PA]]
// CHECK-NEXT: end_access [[PA]]
// CHECK-NEXT: [[OUT:%.*]] = load [trivial] [[PTR]]
// CHECK-NEXT: destroy_value [[UNINIT]]
// CHECK-NEXT: return [[OUT]]
// CHECK-LABEL: end sil function '$sSo6IntBoxVABycfC'
public func singleMemberTypeNoArgInit() {
  let i = IntBox()
}

// CHECK-LABEL: sil shared [transparent] [serializable] [ossa] @$sSo6IntBoxV1xABs5Int32V_tcfC : $@convention(method) (Int32, @thin IntBox.Type) -> IntBox
// CHECK: bb0([[I:%[0-9]+]] : $Int32, %{{[0-9]+}} : $@thin IntBox.Type):
// CHECK-NEXT: [[S:%.*]] = struct $IntBox ([[I]] : $Int32)
// CHECK-NEXT: return [[S]]
// CHECK-LABEL: end sil function '$sSo6IntBoxV1xABs5Int32V_tcfC'
public func singleMemberTypeValueInit() {
  let i = IntBox(x: 42)
}
