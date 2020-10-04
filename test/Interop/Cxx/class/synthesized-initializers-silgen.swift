// RUN: %target-swift-frontend -I %S/Inputs -enable-cxx-interop -emit-silgen %s | %FileCheck %s

import SynthesizedInitializers
import Constructors

// CHECK-LABEL: sil [ossa] @$s4main18emptyTypeNoArgInityyF : $@convention(thin) () -> ()
// CHECK: [[AS:%.*]] = alloc_stack $EmptyStruct
// CHECK: [[FN:%.*]] = function_ref @{{_ZN11EmptyStructC1Ev|\?\?0EmptyStruct@@QEAA@XZ}} : $@convention(c) () -> @out EmptyStruct
// CHECK: apply [[FN]]([[AS]]) : $@convention(c) () -> @out EmptyStruct
// CHECK-LABEL: end sil function '$s4main18emptyTypeNoArgInityyF'

// CHECL-LABEL: sil [clang EmptyStruct.init] @{{_ZN11EmptyStructC1Ev|\?\?0EmptyStruct@@QEAA@XZ}} : $@convention(c) () -> @out EmptyStruct
public func emptyTypeNoArgInit() {
  let e = EmptyStruct()
}

// CHECK-LABEL: sil [ossa] @$s4main25singleMemberTypeNoArgInityyF : $@convention(thin) () -> ()
// CHECK: [[AS:%.*]] = alloc_stack $IntBox
// CHECK: [[FN:%.*]] = function_ref @{{_ZN6IntBoxC1Ev|\?\?0IntBox@@QEAA@XZ}} : $@convention(c) () -> @out IntBox
// CHECK: apply [[FN]]([[AS]]) : $@convention(c) () -> @out IntBox
// CHECK-LABEL: end sil function '$s4main25singleMemberTypeNoArgInityyF'

//CHECK-LABEL: sil [clang IntBox.init] @{{_ZN6IntBoxC1Ev|\?\?0IntBox@@QEAA@XZ}} : $@convention(c) () -> @out IntBox
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

// CHECK-LABEL: sil shared [transparent] [serializable] [ossa] @$sSo25DefaultConstructorDeletedV1aABSpys5Int32VG_tcfC : $@convention(method) (UnsafeMutablePointer<Int32>, @thin DefaultConstructorDeleted.Type) -> DefaultConstructorDeleted
// CHECK: bb0([[A:%.*]] : $UnsafeMutablePointer<Int32>
// CHECK-NEXT: [[OUT:%.*]] = struct $DefaultConstructorDeleted ([[A]] : $UnsafeMutablePointer<Int32>)
// CHECK-NEXT: return [[OUT]] : $DefaultConstructorDeleted
// CHECK-LABEL:  end sil function '$sSo25DefaultConstructorDeletedV1aABSpys5Int32VG_tcfC'
public func deletedConstructor(a: UnsafeMutablePointer<Int32>) {
  let deletedExplicitly = DefaultConstructorDeleted(a: a)
}
