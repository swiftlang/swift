// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SkipInheritedDocs -emit-module-path %t/SkipInheritedDocs.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name SkipInheritedDocs -I %t -pretty-print -skip-inherited-docs -output-dir %t
// RUN: %FileCheck %s --input-file %t/SkipInheritedDocs.symbols.json

/// Parent docs.
public protocol P1 {
  /// Parent func docs.
  func p2()
}

/// Child docs
public class C1: P1 {
  public func p2() {}
}

// CHECK: "Parent func docs."
// CHECK-NOT: "Parent func docs."

