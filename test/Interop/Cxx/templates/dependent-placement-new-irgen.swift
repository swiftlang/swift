// RUN: %target-swift-emit-ir %s -I %S/Inputs -cxx-interoperability-mode=default | %FileCheck %s

// Referencing makeWidget() forces IRGen to scan the referenced C++ decls,
// including the generic lambda inside makeWidget() whose templated call
// operator contains a placement-new of a dependent type. IRGen used to crash
// traversing that uninstantiated template pattern; now it only looks at the
// concrete instantiation and emits IR successfully.

import DependentPlacementNew

public func test() {
  _ = makeWidget()
}

// CHECK: define {{.*}} @{{.*}}makeWidget
