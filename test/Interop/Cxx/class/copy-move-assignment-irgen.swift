// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs -enable-experimental-cxx-interop -emit-ir %s -Xcc -fignore-exceptions -O | %FileCheck %s

import CopyMoveAssignment

@inline(never)
func takeValue<T>(_ x: T) {
    let _ = x
}

public func copyAssign() {
    var instance = NonTrivialCopyAndCopyMoveAssign()
    let instance2 = NonTrivialCopyAndCopyMoveAssign()
    instance = instance2
    takeValue(instance2)
    takeValue(instance)
}

// CHECK-LABEL: define {{.*}}copyAssign
// CHECK: call %struct.NonTrivialCopyAndCopyMoveAssign* @{{_ZN31NonTrivialCopyAndCopyMoveAssignC1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignC2Ev|"\?\?0NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(%struct.NonTrivialCopyAndCopyMoveAssign* %[[#COPY_INSTANCE:]])
// CHECK: call %struct.NonTrivialCopyAndCopyMoveAssign* @{{_ZN31NonTrivialCopyAndCopyMoveAssignC1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignC2Ev|"\?\?0NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(%struct.NonTrivialCopyAndCopyMoveAssign* %[[#COPY_INSTANCE2:]])
// CHECK: call %struct.NonTrivialCopyAndCopyMoveAssign* @{{_ZN31NonTrivialCopyAndCopyMoveAssignD1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignD2Ev|"\?\?1NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(%struct.NonTrivialCopyAndCopyMoveAssign* %[[#COPY_INSTANCE]])
// CHECK: call %struct.NonTrivialCopyAndCopyMoveAssign* @{{_ZN31NonTrivialCopyAndCopyMoveAssignC1ERKS_|_ZN31NonTrivialCopyAndCopyMoveAssignC2ERKS_|"\?\?4NonTrivialCopyAndCopyMoveAssign@@QEAAAEAU0@AEBU0@@Z"}}(%struct.NonTrivialCopyAndCopyMoveAssign* %[[#COPY_INSTANCE]], %struct.NonTrivialCopyAndCopyMoveAssign* %[[#COPY_INSTANCE2]])
// CHECK: call swiftcc
// CHECK: call {{.*}}{{_ZN31NonTrivialCopyAndCopyMoveAssignD1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignD2Ev|"\?\?1NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(%struct.NonTrivialCopyAndCopyMoveAssign* %[[#COPY_INSTANCE2]])
// CHECK: call swiftcc
// CHECK: call {{.*}} @{{_ZN31NonTrivialCopyAndCopyMoveAssignD1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignD2Ev|"\?\?1NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(%struct.NonTrivialCopyAndCopyMoveAssign* %[[#COPY_INSTANCE]])
// CHECK-NOT: NonTrivialCopyAndCopyMoveAssign

// CHECK-LABEL: define {{.*}}takeAssign
// CHECK: call %struct.NonTrivialCopyAndCopyMoveAssign* @{{_ZN31NonTrivialCopyAndCopyMoveAssignC1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignC2Ev|"\?\?0NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(%struct.NonTrivialCopyAndCopyMoveAssign* %[[#MOVE_INSTANCE:]])
// CHECK: call {{.*}} @{{_ZN31NonTrivialCopyAndCopyMoveAssignD1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignD2Ev|"\?\?1NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(%struct.NonTrivialCopyAndCopyMoveAssign* %[[#MOVE_INSTANCE]])
// CHECK: call {{.*}} @{{_ZN31NonTrivialCopyAndCopyMoveAssignC1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignC2Ev|"\?\?0NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(%struct.NonTrivialCopyAndCopyMoveAssign* %[[#MOVE_INSTANCE:]])
// CHECK: call swiftcc
// CHECK: call {{.*}} @{{_ZN31NonTrivialCopyAndCopyMoveAssignD1Ev|_ZN31NonTrivialCopyAndCopyMoveAssignD2Ev|"\?\?1NonTrivialCopyAndCopyMoveAssign@@QEAA@XZ"}}(%struct.NonTrivialCopyAndCopyMoveAssign* %[[#MOVE_INSTANCE]])
// CHECK-NOT: NonTrivialCopyAndCopyMoveAssign

// CHECK-LABEL: }

public func takeAssign() {
    var instance = NonTrivialCopyAndCopyMoveAssign()
    instance = NonTrivialCopyAndCopyMoveAssign()
    takeValue(instance)
}

copyAssign()
takeAssign()
