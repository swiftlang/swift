// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SkipsInheritedDocs -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SkipsInheritedDocs -I %t -pretty-print -skip-inherited-docs -output-dir %t
// RUN: %FileCheck %s --input-file %t/SkipsInheritedDocs.symbols.json

// CHECK-NOT: houldappear 

public class Parent {
    /// Parent foo docs
    func foo() { }
}

public class ShouldAppear: Parent  {
    /// Child foo docs
    func foo() { }
}
