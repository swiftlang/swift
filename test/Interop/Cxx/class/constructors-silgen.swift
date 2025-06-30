// RUN: %target-swiftxx-frontend -I %S/Inputs -Xllvm -sil-print-types -emit-silgen %s | %FileCheck %s
// XFAIL: OS=linux-androideabi

import Constructors

// The most important thing to test here is that the constructor result is
// returned with an @out attribute.
// CHECK-LABEL: sil [ossa] @$s4main24testConstructorWithParamyyF : $@convention(thin) () -> ()
// CHECK: [[VAR:%[0-9]+]] = alloc_stack $ConstructorWithParam
// CHECK: [[LITERAL:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 42
// CHECK: [[INT:%[0-9]+]] = apply %{{[0-9]+}}([[LITERAL]], %{{[0-9]+}})
// CHECK: [[FUNC:%[0-9]+]] = function_ref @{{_ZN20ConstructorWithParamC1Ei|\?\?0ConstructorWithParam@@QEAA@H@Z}} : $@convention(c) (Int32) -> @out ConstructorWithParam
// CHECK: %{{[0-9]+}} = apply [[FUNC]]([[VAR]], [[INT]]) : $@convention(c) (Int32) -> @out ConstructorWithParam
// CHECK-LABEL: end sil function '$s4main24testConstructorWithParamyyF'

// CHECK-LABEL: sil [clang ConstructorWithParam.init] @{{_ZN20ConstructorWithParamC1Ei|\?\?0ConstructorWithParam@@QEAA@H@Z}} : $@convention(c) (Int32) -> @out ConstructorWithParam
public func testConstructorWithParam() {
  let c = ConstructorWithParam(42)
}

// CHECK-LABEL: sil [ossa] @$s4main18emptyTypeNoArgInityyF : $@convention(thin) () -> ()
// CHECK: [[AS:%.*]] = alloc_stack $EmptyStruct
// CHECK: [[FN:%.*]] = function_ref @{{_ZN11EmptyStructC1Ev|\?\?0EmptyStruct@@QEAA@XZ}} : $@convention(c) () -> @out EmptyStruct
// CHECK: apply [[FN]]([[AS]]) : $@convention(c) () -> @out EmptyStruct
// CHECK-LABEL: end sil function '$s4main18emptyTypeNoArgInityyF'

// CHECK-LABEL: sil [clang EmptyStruct.init] @{{_ZN11EmptyStructC1Ev|\?\?0EmptyStruct@@QEAA@XZ}} : $@convention(c) () -> @out EmptyStruct
public func emptyTypeNoArgInit() {
  let e = EmptyStruct()
}

// CHECK-LABEL: sil [ossa] @$s4main25singleMemberTypeNoArgInityyF : $@convention(thin) () -> ()
// CHECK: [[AS:%.*]] = alloc_stack $IntWrapper
// CHECK: [[FN:%.*]] = function_ref @{{_ZN10IntWrapperC1Ev|\?\?0IntWrapper@@QEAA@XZ}} : $@convention(c) () -> @out IntWrapper
// CHECK: apply [[FN]]([[AS]]) : $@convention(c) () -> @out IntWrapper
// CHECK-LABEL: end sil function '$s4main25singleMemberTypeNoArgInityyF'

//CHECK-LABEL: sil [clang IntWrapper.init] @{{_ZN10IntWrapperC1Ev|\?\?0IntWrapper@@QEAA@XZ}} : $@convention(c) () -> @out IntWrapper
public func singleMemberTypeNoArgInit() {
  let i = IntWrapper()
}

// CHECK-LABEL: sil shared [transparent] [serialized] [ossa] @$sSo10IntWrapperV1xABs5Int32V_tcfC : $@convention(method) (Int32, @thin IntWrapper.Type) -> IntWrapper
// CHECK: bb0([[I:%[0-9]+]] : $Int32, %{{[0-9]+}} : $@thin IntWrapper.Type):
// CHECK-NEXT: [[S:%.*]] = struct $IntWrapper ([[I]] : $Int32)
// CHECK-NEXT: return [[S]]
// CHECK-LABEL: end sil function '$sSo10IntWrapperV1xABs5Int32V_tcfC'
public func singleMemberTypeValueInit() {
  let i = IntWrapper(x: 42)
}

// CHECK-LABEL: sil shared [transparent] [serialized] [ossa] @$sSo25DefaultConstructorDeletedV1aABSpys5Int32VG_tcfC : $@convention(method) (UnsafeMutablePointer<Int32>, @thin DefaultConstructorDeleted.Type) -> DefaultConstructorDeleted
// CHECK: bb0([[A:%.*]] : $UnsafeMutablePointer<Int32>
// CHECK-NEXT: [[OUT:%.*]] = struct $DefaultConstructorDeleted ([[A]] : $UnsafeMutablePointer<Int32>)
// CHECK-NEXT: return [[OUT]] : $DefaultConstructorDeleted
// CHECK-LABEL:  end sil function '$sSo25DefaultConstructorDeletedV1aABSpys5Int32VG_tcfC'
public func deletedConstructor(a: UnsafeMutablePointer<Int32>) {
  let deletedExplicitly = DefaultConstructorDeleted(a: a)
}

// CHECK-LABEL: sil [ossa] @$s4main20templatedConstructoryyF : $@convention(thin) () -> ()
// CHECK: [[TEMPL:%.*]] = alloc_stack $TemplatedConstructor
// CHECK: [[ARG:%.*]] = alloc_stack $ArgType
// CHECK: [[ARG_VAL:%.*]] = load [trivial] [[ARG]] : $*ArgType
// CHECK: [[FN:%.*]] = function_ref @{{_ZN20TemplatedConstructorC1I7ArgTypeEET_|\?\?\$\?0UArgType@@@TemplatedConstructor@@QEAA@UArgType@@@Z}} : $@convention(c) (ArgType) -> @out TemplatedConstructor
// CHECK: apply [[FN]]([[TEMPL]], [[ARG_VAL]]) : $@convention(c) (ArgType) -> @out TemplatedConstructor
// CHECK-LABEL: end sil function '$s4main20templatedConstructoryyF'

// CHECK-LABEL: sil [clang TemplatedConstructor.init] @{{_ZN20TemplatedConstructorC1I7ArgTypeEET_|\?\?\$\?0UArgType@@@TemplatedConstructor@@QEAA@UArgType@@@Z}} : $@convention(c) (ArgType) -> @out TemplatedConstructor
public func templatedConstructor() {
  let templated = TemplatedConstructor(ArgType())
}
