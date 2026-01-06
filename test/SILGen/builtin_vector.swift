// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -disable-experimental-parser-round-trip -disable-availability-checking -enable-experimental-feature BuiltinModule %s | %FileCheck %s

// REQUIRES: swift_feature_BuiltinModule

import Builtin

struct MyVector<let N: Int, T: ~Copyable>: ~Copyable {
    var storage: Builtin.FixedArray<N, T>
}
extension MyVector: Copyable where T: Copyable {}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}13trivial_fixed
// CHECK: bb0(%0 : $MyVector<4, Int>):
// CHECK:   tuple (%0 : {{.*}}, %0 : {{.*}})
func trivial_fixed(a: MyVector<4, Int>) -> (MyVector<4, Int>, MyVector<4, Int>){
    return (a, a)
}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}14loadable_fixed
// CHECK: bb0(%0 : @guaranteed $MyVector<4, AnyObject>):
// CHECK:   [[COPY1:%.*]] = copy_value %0
// CHECK:   [[COPY2:%.*]] = copy_value %0
// CHECK:   tuple ([[COPY1]] : {{.*}}, [[COPY2]] : {{.*}})
func loadable_fixed(a: MyVector<4, AnyObject>)
    -> (MyVector<4, AnyObject>, MyVector<4, AnyObject>)
{
    return (a, a)
}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}8ao_fixed
// CHECK: bb0(%0 : $*MyVector<4, Any>, %1 : $*MyVector<4, Any>, %2 : $*MyVector<4, Any>):
// CHECK:   copy_addr %2 to [init] %0
// CHECK:   copy_addr %2 to [init] %1
func ao_fixed(a: MyVector<4, Any>)
    -> (MyVector<4, Any>, MyVector<4, Any>)
{
    return (a, a)
}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}11trivial_dep
// CHECK: bb0(%0 : $*MyVector<N, Int>, %1 : $*MyVector<N, Int>, %2 : $*MyVector<N, Int>):
// CHECK:   copy_addr %2 to [init] %0
// CHECK:   copy_addr %2 to [init] %1
func trivial_dep<let N: Int>(a: MyVector<N, Int>)
    -> (MyVector<N, Int>, MyVector<N, Int>)
{
    return (a, a)
}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}12loadable_dep
// CHECK: bb0(%0 : $*MyVector<N, AnyObject>, %1 : $*MyVector<N, AnyObject>, %2 : $*MyVector<N, AnyObject>):
// CHECK:   copy_addr %2 to [init] %0
// CHECK:   copy_addr %2 to [init] %1
func loadable_dep<let N: Int>(a: MyVector<N, AnyObject>)
    -> (MyVector<N, AnyObject>, MyVector<N, AnyObject>)
{
    return (a, a)
}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}6ao_dep
// CHECK: bb0(%0 : $*MyVector<N, Any>, %1 : $*MyVector<N, Any>, %2 : $*MyVector<N, Any>):
// CHECK:   copy_addr %2 to [init] %0
// CHECK:   copy_addr %2 to [init] %1
func ao_dep<let N: Int>(a: MyVector<N, Any>)
    -> (MyVector<N, Any>, MyVector<N, Any>)
{
    return (a, a)
}

