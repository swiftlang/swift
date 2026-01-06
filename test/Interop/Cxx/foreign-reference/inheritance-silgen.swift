// RUN: %target-swift-emit-sil %s -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking | %FileCheck %s

import Inheritance

var x = BasicInheritanceExample.returnValueType()
// CHECK: function_ref {{.*}}returnValueType{{.*}} : $@convention(c) () -> UnsafeMutablePointer<BasicInheritanceExample.ValueType>
// CHECK-NEXT: apply {{.*}} $@convention(c) () -> UnsafeMutablePointer<BasicInheritanceExample.ValueType>
// CHECK-NOT: strong_retain {{.*}}

var y = BasicInheritanceExample.returnRefType()
// CHECK: function_ref {{.*}}returnRefType{{.*}} : $@convention(c) () -> BasicInheritanceExample.RefType
// CHECK-NEXT: apply {{.*}} $@convention(c) () -> BasicInheritanceExample.RefType
// CHECK-NEXT: strong_retain {{.*}}

var z = BasicInheritanceExample.returnDerivedFromRefType()
// CHECK: function_ref {{.*}}returnDerivedFromRefType{{.*}} : $@convention(c) () -> BasicInheritanceExample.DerivedFromRefType
// CHECK-NEXT: apply {{.*}} $@convention(c) () -> BasicInheritanceExample.DerivedFromRefType
// CHECK-NEXT: strong_retain {{.*}}

func foo(
  x: BasicInheritanceExample.ValueType, y: BasicInheritanceExample.RefType,
  z: BasicInheritanceExample.DerivedFromRefType
) {
}

foo(x: x.pointee, y: y, z: z)
// CHECK: function_ref {{.*}}foo{{.*}} : $@convention(thin) (BasicInheritanceExample.ValueType, BasicInheritanceExample.RefType, BasicInheritanceExample.DerivedFromRefType)
