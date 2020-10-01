// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SuperclassImplementation -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SuperclassImplementation -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SuperclassImplementation.symbols.json

public class Base {
  public init() {}
  public func foo() {}
}

public class Derived: Base {
  // CHECK-DAG: "precise": "s:24SuperclassImplementation4BaseC3fooyyF::SYNTHESIZED::s:24SuperclassImplementation7DerivedC"
}

public class DerivedDerived: Derived {
  // CHECK-DAG: "precise": "s:24SuperclassImplementation4BaseC3fooyyF::SYNTHESIZED::s:24SuperclassImplementation07DerivedC0C"
}
