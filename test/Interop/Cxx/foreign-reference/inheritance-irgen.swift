// RUN: %target-swift-emit-ir -Onone %s -I %S/Inputs -cxx-interoperability-mode=default -validate-tbd-against-ir=none -disable-llvm-verify -Xcc -fignore-exceptions -disable-availability-checking | %FileCheck %s

import Inheritance

func foo (x: ValueType, y: RefType, z: DerivedFromRefType) {
}

// CHECK: call ptr @{{.*}}returnValueType{{.*}}()
// CHECK-NOT: call void @{{.*}}RCRetain@{{.*}}ValueType(ptr @{{.*}})
var x = returnValueType()

// CHECK: call ptr @{{.*}}returnRefType{{.*}}()
// CHECK: call void @{{.*}}RCRetain{{.*}}RefType(ptr {{.*}})
var y = returnRefType()

// CHECK: call ptr @{{.*}}returnDerivedFromRefType{{.*}}()
// CHECK: call void @{{.*}}RCRetain{{.*}}RefType(ptr {{.*}})
var z = returnDerivedFromRefType()

// CHECK: load ptr, ptr @{{.*}}RefType{{.*}}, align 8
// CHECK: call void @{{.*}}RefType(ptr {{.*}})

// CHECK: load ptr, ptr @{{.*}}DerivedFromRefType{{.*}}, align 8
// CHECK: call void @{{.*}}RCRetain{{.*}}RefType(ptr {{.*}})

foo(x:x.pointee, y:y, z:z)

// CHECK: call void @{{.*}}RCRelease{{.*}}RefType(ptr {{.*}}) #6
// CHECK: call void @{{.*}}RCRelease{{.*}}RefType(ptr {{.*}}) #6
