// RUN: %target-swift-emit-ir -Onone %s -I %S/Inputs -cxx-interoperability-mode=default -validate-tbd-against-ir=none -disable-llvm-verify -Xcc -fignore-exceptions -disable-availability-checking | %FileCheck %s

// This ensured we do not crash during IRGen for inherited C++ foreign reference types.

import Inheritance

@inline(never)
public func blackHole<T>(_ _: T) {}

let x = DerivedOutOfOrder.getInstance()

blackHole(x.baseField)
blackHole(x.derivedField)
blackHole(x.leafField)

func foo (x1: ValueType, x2: RefType, x3: DerivedFromRefType) {
}

// CHECK: call ptr @{{.*}}returnValueType{{.*}}()
// CHECK-NOT: call void @{{.*}}RCRetain@{{.*}}ValueType(ptr @{{.*}})
var x1 = returnValueType()

// CHECK: call ptr @{{.*}}returnRefType{{.*}}()
// CHECK: call void @{{.*}}RCRetain{{.*}}RefType(ptr {{.*}})
var x2 = returnRefType()

// CHECK: call ptr @{{.*}}returnDerivedFromRefType{{.*}}()
// CHECK: call void @{{.*}}RCRetain{{.*}}RefType(ptr {{.*}})
var x3 = returnDerivedFromRefType()

// CHECK: load ptr, ptr @{{.*}}RefType{{.*}}, align 8
// CHECK: call void @{{.*}}RefType(ptr {{.*}})

// CHECK: load ptr, ptr @{{.*}}DerivedFromRefType{{.*}}, align 8
// CHECK: call void @{{.*}}RCRetain{{.*}}RefType(ptr {{.*}})

foo(x1:x1.pointee, x2:x2, x3:x3)

// CHECK: call void @{{.*}}RCRelease{{.*}}RefType(ptr {{.*}}) #6
// CHECK: call void @{{.*}}RCRelease{{.*}}RefType(ptr {{.*}}) #6
