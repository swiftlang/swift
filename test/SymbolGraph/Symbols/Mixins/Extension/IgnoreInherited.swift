// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name IgnoreInherited -emit-module-path %t/IgnoreInherited.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name IgnoreInherited -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/IgnoreInherited.symbols.json

public protocol P {
  associatedtype T
  static func foo() -> T
}

public struct S<T> {}

extension S: P where T: Sequence, T.Element == Int {
  public static func foo() -> AnySequence<Int> {
    return AnySequence([0])
  }
}

// CHECK-LABEL: "precise": "s:15IgnoreInherited1SVAASTRzSi7ElementRtzlE3foos11AnySequenceVySiGyFZ"
// CHECK: swiftExtension
// CHECK: "constraints": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "conformance",
// CHECK-NEXT:     "lhs": "T",
// CHECK-NEXT:     "rhs": "Sequence"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "sameType",
// CHECK-NEXT:     "lhs": "T.Element",
// CHECK-NEXT:     "rhs": "Int"
// CHECK-NEXT:   }
// CHECK-NEXT: ]
