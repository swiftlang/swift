// RUN: %target-swift-emit-sil %s -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking | %FileCheck %s

import Inheritance

func foo (x: ValueType, y: RefType, z: DerivedFromRefType) {
}

var x = returnValueType()
// CHECK: function_ref {{.*}}returnValueType{{.*}} : $@convention(c) () -> UnsafeMutablePointer<ValueType>
// CHECK-NOT: strong_retain {{.*}} : $ValueType

var y = returnRefType()
// CHECK: function_ref {{.*}}returnRefType{{.*}} : $@convention(c) () -> RefType
// CHECK: strong_retain {{.*}} : $RefType
 
var z = returnDerivedFromRefType()
// CHECK: function_ref {{.*}}returnDerivedFromRefType{{.*}} : $@convention(c) () -> DerivedFromRefType
// CHECK: strong_retain {{.*}} : $DerivedFromRefType

// CHECK-NOT: strong_retain {{.*}} : $ValueType
// CHECK: strong_retain {{.*}} : $RefType
// CHECK: strong_retain {{.*}} : $DerivedFromRefType

foo(x:x.pointee, y:y, z:z)
// CHECK: function_ref {{.*}}foo{{.*}} : $@convention(thin) (ValueType, RefType, DerivedFromRefType)
// CHECK-NOT: strong_release {{.*}} : $ValueType
// CHECK: strong_release {{.*}} : $DerivedFromRefType
// CHECK: strong_release {{.*}} : $RefType
