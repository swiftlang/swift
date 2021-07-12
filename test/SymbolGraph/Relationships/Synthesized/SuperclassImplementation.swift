// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SuperclassImplementation -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SuperclassImplementation -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SuperclassImplementation.symbols.json

// This test references code that has been removed. The implementation that synthesized superclass
// methods was inconsistent; it failed to generate symbols for superclasses from another module.
// From a symbol-relation perspective, the `inheritsFrom` relation from a subclass to its superclass
// still exists, which already implies that all of the superclass's methods are available on the
// subclass. The synthesized methods for subclasses were removed to provide consistency between
// superclasses from the same module and those from a different one. If the implementation is
// brought back, ensure that it consistently adds synthesized methods for superclasses from
// different modules.

public class Base {
  public init() {}
  public func foo() {}
}

public class Derived: Base {
  // CHECK-NOT: "precise": "s:24SuperclassImplementation4BaseC3fooyyF::SYNTHESIZED::s:24SuperclassImplementation7DerivedC"
}

public class DerivedDerived: Derived {
  // CHECK-NOT: "precise": "s:24SuperclassImplementation4BaseC3fooyyF::SYNTHESIZED::s:24SuperclassImplementation07DerivedC0C"
}
