// A borrowed foreign reference type parameter is not retained around its uses,
// even across a call that could release other references.

// RUN: %target-swift-emit-ir -O %s -I %S/Inputs -cxx-interoperability-mode=default -target %target-swift-5.8-abi-triple | %FileCheck %s

import ParamConventions

// CHECK-LABEL: define {{.*}} @"$s4main8twoCallsySbSo6SharedVF"(ptr
// CHECK-NOT:     call {{.*}}retainShared
// CHECK-NOT:     call {{.*}}releaseShared
// CHECK:         ret i1
public func twoCalls(_ s: Shared) -> Bool {
  return s.virtualValue() != s.virtualValue()
}

// CHECK-LABEL: define {{.*}} @"$s4main11callThenUseys5Int32VSo6SharedVF"(ptr
// CHECK-NOT:     call {{.*}}retainShared
// CHECK-NOT:     call {{.*}}releaseShared
// CHECK:         ret i32
public func callThenUse(_ s: Shared) -> Int32 {
  opaque()
  return s.virtualValue()
}

// CHECK-LABEL: define {{.*}} @"$sSo6SharedV4mainE14twoCallsOnSelfSbyF"(ptr
// CHECK-NOT:     call {{.*}}retainShared
// CHECK-NOT:     call {{.*}}releaseShared
// CHECK:         ret i1
extension Shared {
  public func twoCallsOnSelf() -> Bool {
    return virtualValue() != virtualValue()
  }
}
