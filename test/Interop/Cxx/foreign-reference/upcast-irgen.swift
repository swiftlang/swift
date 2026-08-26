// RUN: %target-swift-emit-irgen %s -I %S/Inputs -cxx-interoperability-mode=default -validate-tbd-against-ir=none -disable-llvm-verify -Xcc -fignore-exceptions -disable-availability-checking | %FileCheck %s

import Upcast

public func passDerivedAsBase() -> Int32 {
  let d = Derived.create()
  return getBaseValueFromBase(d)
}
// CHECK-LABEL: define {{.*}}swiftcc i32 @"$s4main17passDerivedAsBases5Int32VyF"
// CHECK: [[PTR:%[0-9]+]] = call ptr @{{_ZN7Derived6createEv|"\?create@Derived@@SAAEAU1@XZ"}}
// CHECK: call {{.*}}i32 @{{_Z20getBaseValueFromBaseRK4Base|"\?getBaseValueFromBase@@YAHAEBUBase@@@Z"}}(ptr [[PTR]])

public func passRefCountedDerivedAsBase() -> Int32 {
  let d = RefCountedDerived.create()
  return getBaseValueFromRefCountedBase(d)
}
// CHECK-LABEL: define {{.*}}swiftcc i32 @"$s4main27passRefCountedDerivedAsBases5Int32VyF"
// CHECK: [[RCPTR:%[0-9]+]] = call ptr @{{_ZN17RefCountedDerived6createEv|"\?create@RefCountedDerived@@SAAEAU1@XZ"}}
// CHECK: call {{.*}}@{{_ZN17RefCountedDerived37__synthesized_lifetimeAccessor_retainEv|"\?__synthesized_lifetimeAccessor_retain@RefCountedDerived@@QEAAXXZ"}}(ptr [[RCPTR]])
// CHECK: call {{.*}}i32 @{{_Z30getBaseValueFromRefCountedBaseRK14RefCountedBase|"\?getBaseValueFromRefCountedBase@@YAHAEBURefCountedBase@@@Z"}}(ptr [[RCPTR]])
