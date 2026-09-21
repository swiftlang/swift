// RUN: %target-swift-emit-silgen %s -I %S/Inputs -cxx-interoperability-mode=default -target %target-swift-5.8-abi-triple | %FileCheck %s

import SuperStaticDispatch

extension Derived {
  public func callSuper() -> Int32 {
    return super.nonVirtualMethod()
  }
}

// CHECK-LABEL: sil {{.*}}callSuper{{.*}}
// CHECK-NOT: objc_super_method
// CHECK: Base.nonVirtualMethod()
// CHECK-NOT: Derived.nonVirtualMethod()
// CHECK: end sil function

// CHECK: [clang Base.nonVirtualMethod]
// CHECK-NOT: [clang Derived.nonVirtualMethod]
