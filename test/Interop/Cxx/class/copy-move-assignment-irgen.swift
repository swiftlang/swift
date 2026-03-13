// RUN: %target-swift-frontend -I %S/Inputs -disable-llvm-merge-functions-pass -enable-experimental-cxx-interop -emit-ir %s -Xcc -fignore-exceptions -O | %FileCheck %s

import CopyMoveAssignment

@inline(never)
func takeValue<T>(_ x: T) {
    let _ = x
}

@inline(never)
public func copyAssign() {
    var instance = NonTrivialCopyAndCopyMoveAssign()
    let instance2 = NonTrivialCopyAndCopyMoveAssign()
    instance = instance2
    takeValue(instance2)
    takeValue(instance)
}

// CHECK-LABEL: define {{.*}}copyAssign
// CHECK: call {{void|ptr}} @{{_ZN31NonTrivialCopyAndCopyMoveAssignC1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignC2Ev|"\?\?0NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(ptr {{(nonnull )?}}%[[COPY_INSTANCE:.*]])
// CHECK: call {{void|ptr}} @{{_ZN31NonTrivialCopyAndCopyMoveAssignC1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignC2Ev|"\?\?0NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(ptr {{(nonnull )?}}%[[COPY_INSTANCE2:.*]])
// CHECK: call {{void|ptr}} @{{_ZN31NonTrivialCopyAndCopyMoveAssignD1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignD2Ev|"\?\?1NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(ptr {{(nonnull )?}}%[[COPY_INSTANCE]])
// CHECK: call {{void|(noundef )?ptr}} @{{_ZN31NonTrivialCopyAndCopyMoveAssignC1ERKS_|_ZN31NonTrivialCopyAndCopyMoveAssignC2EOS_|_ZN31NonTrivialCopyAndCopyMoveAssignC1ERKS|_ZN31NonTrivialCopyAndCopyMoveAssignC2ERKS_|"\?\?0NonTrivialCopyAndCopyMoveAssign@@QEAA@AEBU0@@Z"}}(
// CHECK-SAME: %[[COPY_INSTANCE]],
// CHECK-SAME: ptr
// CHECK-SAME: %[[COPY_INSTANCE2]])
// CHECK: call {{.*}} @{{_ZN31NonTrivialCopyAndCopyMoveAssignD1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignD2Ev|"\?\?1NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(ptr {{(nonnull )?}}%[[COPY_INSTANCE2]])
// CHECK: call {{.*}} @{{_ZN31NonTrivialCopyAndCopyMoveAssignD1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignD2Ev|"\?\?1NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(ptr {{(nonnull )?}}%[[COPY_INSTANCE]])

// CHECK-LABEL: define {{.*}}takeAssign
// CHECK: call {{void|ptr}} @{{_ZN31NonTrivialCopyAndCopyMoveAssignC1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignC2Ev|"\?\?0NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(ptr {{(nonnull )?}}%[[MOVE_INSTANCE:.*]])
// CHECK: call {{.*}} @{{_ZN31NonTrivialCopyAndCopyMoveAssignD1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignD2Ev|"\?\?1NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(ptr {{(nonnull )?}}%[[MOVE_INSTANCE]])
// CHECK: call {{.*}} @{{_ZN31NonTrivialCopyAndCopyMoveAssignD1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignD2Ev|"\?\?1NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(ptr {{(nonnull )?}}%[[MOVE_INSTANCE]])

// CHECK-LABEL: }

// CHECK-LABEL: }

@inline(never)
public func takeAssign() {
    var instance = NonTrivialCopyAndCopyMoveAssign()
    instance = NonTrivialCopyAndCopyMoveAssign()
    takeValue(instance)
}

copyAssign()
takeAssign()
