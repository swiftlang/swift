// RUN: %target-swift-emit-ir -Onone %s -I %S/Inputs -cxx-interoperability-mode=default -validate-tbd-against-ir=none -disable-llvm-verify -Xcc -fignore-exceptions -disable-availability-checking | %FileCheck %s

// This ensured we do not crash during IRGen for inherited C++ foreign reference types.

import Inheritance

@inline(never)
public func blackHole<T>(_ _: T) {}

let x = DerivedOutOfOrder.getInstance()

blackHole(x.baseField)
blackHole(x.derivedField)
blackHole(x.leafField)

let y = DerivedUsesBaseTailPadding.getInstance()

blackHole(y.field2)
blackHole(y.field4)

// CHECK: call ptr @{{.*}}returnValueType{{.*}}
// CHECK-NOT: call void @{{.*}}RCRetain@{{.*}}ValueType(ptr @{{.*}})
var x1 = BasicInheritanceExample.returnValueType()

// CHECK: call ptr @{{.*}}returnRefType{{.*}}
// CHECK: call void @{{.*}}RCRetain{{.*}}RefType{{.*}}(ptr {{.*}})
var x2 = BasicInheritanceExample.returnRefType()

// CHECK: call ptr @{{.*}}returnDerivedFromRefType{{.*}}
// CHECK: call void @{{.*}}RCRetain{{.*}}RefType{{.*}}(ptr {{.*}})
var x3 = BasicInheritanceExample.returnDerivedFromRefType()

func foo(
  x1: BasicInheritanceExample.ValueType, x2: BasicInheritanceExample.RefType,
  x3: BasicInheritanceExample.DerivedFromRefType
) {
}

foo(x1: x1.pointee, x2: x2, x3: x3)

// CHECK: call void @{{.*}}RCRelease{{.*}}RefType{{.*}}(ptr {{.*}})
// CHECK: call void @{{.*}}RCRelease{{.*}}RefType{{.*}}(ptr {{.*}})
