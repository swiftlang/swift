// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name GetFromExtension -emit-module-path %t/GetFromExtension.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name GetFromExtension -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/GetFromExtension.symbols.json

public struct Outer<T> {
  public var x: T
  public init(x: T) {
    self.x = x
  } 
}

extension Outer where T == Int {
  // This type's swiftExtension mixin should not include generic
  // constraints regarding U.

  // CHECK: swiftExtension
  // CHECK: constraints
  // CHECK: "kind": "sameType"
  // CHECK: "lhs": "T"
  // CHECK: "rhs": "Int"
  public struct Inner<U: Sequence> {
    public var x: U
    public init(x: U) {
      self.x = x
    } 
  }
}
