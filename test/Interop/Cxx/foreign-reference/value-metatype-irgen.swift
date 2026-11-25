// RUN: %target-swift-emit-ir -O %s -I %S/Inputs -cxx-interoperability-mode=default -target %target-swift-5.8-abi-triple | %FileCheck %s

import POD

public func dynamicType<T>(_ x: T) -> Any.Type { type(of: x) }

public func getDynamicTypeOfEmpty() -> Any.Type {
  dynamicType(Empty.create())
}

// CHECK-LABEL: define {{.*}} @"$s4main21getDynamicTypeOfEmptyypXpyF"
// CHECK-NOT:   swift_getObjectType
// CHECK-NOT:   swift_getDynamicType
// CHECK:       ret
