// RUN: %target-swift-frontend -I %S/Inputs -enable-cxx-interop -emit-silgen %s | %FileCheck %s

import SynthesizedInitializers

// CHECK-LABEL: sil [ossa] @$s4main18emptyTypeNoArgInityyF : $@convention(thin) () -> ()
// CHECK: [[AS:%.*]] = alloc_stack $EmptyStruct
// CHECK: [[META:%.*]] = metatype $@thin EmptyStruct.Type
// CHECK: [[FN:%.*]] = function_ref @{{_ZN11EmptyStructC1Ev|\?\?0EmptyStruct@@QEAA@XZ}} : $@convention(c) (@thin EmptyStruct.Type) -> @out EmptyStruct
// CHECK: apply [[FN]]([[AS]], [[META]]) : $@convention(c) (@thin EmptyStruct.Type) -> @out EmptyStruct
// CHECK-LABEL: end sil function '$s4main18emptyTypeNoArgInityyF'

// CHECL-LABEL: sil [clang EmptyStruct.init] @{{_ZN11EmptyStructC1Ev|\?\?0EmptyStruct@@QEAA@XZ}} : $@convention(c) (@thin EmptyStruct.Type) -> @out EmptyStruct
public func emptyTypeNoArgInit() {
  let e = EmptyStruct()
}

// CHECK-LABEL: sil [ossa] @$s4main25singleMemberTypeNoArgInityyF : $@convention(thin) () -> ()
// CHECK: [[AS:%.*]] = alloc_stack $IntBox
// CHECK: [[META:%.*]] = metatype $@thin IntBox.Type
// CHECK: [[FN:%.*]] = function_ref @{{_ZN6IntBoxC1Ev|\?\?0IntBox@@QEAA@XZ}} : $@convention(c) (@thin IntBox.Type) -> @out IntBox
// CHECK: apply [[FN]]([[AS]], [[META]]) : $@convention(c) (@thin IntBox.Type) -> @out IntBox
// CHECK-LABEL: end sil function '$s4main25singleMemberTypeNoArgInityyF'

//CHECK-LABEL: sil [clang IntBox.init] @{{_ZN6IntBoxC1Ev|\?\?0IntBox@@QEAA@XZ}} : $@convention(c) (@thin IntBox.Type) -> @out IntBox
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
